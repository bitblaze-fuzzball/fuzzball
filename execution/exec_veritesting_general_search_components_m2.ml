module V = Vine
module VOpt = Vine_opt

open Exec_exceptions
open Exec_options
open Frag_simplify
open Fragment_machine
open Exec_run_common

type 'a progn = {
  eip : int64; (*where was this decoded from? *)
  decls : V.decl list;
  stmts : V.stmt list;
  mutable next : 'a option;
}

type 'a branch = {
  source_eip : int64;
  test : V.exp;
  true_child : 'a;
  false_child: 'a;
}


type 'a finished_type =
| ExternalLoop of int64 (* instruction jumps to ancestor eip *)
| InternalLoop of int64 (* instruction contains cycle in statement list *)
| Return of int64 (* statement list had a return, exit to the eip containing that statement *)
| Halt of int64   (* as above, with halt. *)
| FunCall of int64 (* etc. *)
| SysCall of int64
| Special of int64
| SearchLimit of int64 (* search went too deep, halt with the next instruction being this EIP *)
| Branch of 'a branch     (* encode the test and continuation *)
| Segment of 'a progn


type veritesting_node =
| Undecoded of int64
| Raw of veritesting_node progn
| Completed of veritesting_node finished_type

type vineLabel =
| X86eip of int64
| Internal of int

let key (ft : veritesting_node finished_type) =
  match ft with
  | ExternalLoop eip
  | InternalLoop eip
  | Return eip
  | Halt eip
  | FunCall eip
  | SysCall eip
  | Special eip
  | SearchLimit eip -> eip
  | Branch b -> Int64.neg b.source_eip (* don't want it to colide with prev. block *)
  | Segment progn -> progn.eip

let key (node : veritesting_node) =
  match node with
  | Undecoded eip -> eip
  | Raw progn -> progn.eip
  | Completed ft -> key ft

let equal (a : veritesting_node) (b : veritesting_node) =
  match a,b with
  | Undecoded aeip, Undecoded beip -> aeip = beip
  | Raw aprgn, Raw bprgn -> aprgn.eip = bprgn.eip
  | Completed _, Completed _ -> (key a) = (key b)
  | _, _ -> false

let finished_type_to_string = function
| ExternalLoop eip -> (Printf.sprintf "ExternalLoop @ 0x%Lx" eip)
| InternalLoop eip -> (Printf.sprintf "ExternalLoop @ 0x%Lx" eip)
| Return eip -> (Printf.sprintf "ExternalLoop @ 0x%Lx" eip)
| Halt eip -> (Printf.sprintf "ExternalLoop @ 0x%Lx" eip)
| FunCall eip -> (Printf.sprintf "ExternalLoop @ 0x%Lx" eip)
| SysCall eip -> (Printf.sprintf "ExternalLoop @ 0x%Lx" eip)
| Special eip -> (Printf.sprintf "ExternalLoop @ 0x%Lx" eip)
| SearchLimit eip -> (Printf.sprintf "ExternalLoop @ 0x%Lx" eip)
| Branch b -> (Printf.sprintf "Branch @ 0x%Lx" b.source_eip)
| Segment s -> (Printf.sprintf "Code Segment @ 0x%Lx" s.eip)

let node_to_string = function
  | Undecoded eip -> (Printf.sprintf "Undecoded Region @ 0x%Lx" eip)
  | Raw progn -> (Printf.sprintf "Raw Region @ 0x%Lx" progn.eip)
  | Completed c -> finished_type_to_string c

let decode_eip str =
  if (str.[0] = 'L' && str.[1] = '_')
  then (let num = String.sub str 2 ((String.length str) - 2) in
	Internal (int_of_string num))
  else X86eip (label_to_eip str)

let ordered_p (a : veritesting_node finished_type) (b : veritesting_node finished_type) =
  match a,b with
  | _, _ -> true

let ordered_p (a : veritesting_node) (b : veritesting_node) =
  match a,b with
  | Undecoded aeip, Undecoded beip -> aeip < beip
  | Raw aprgn, Raw bprgn -> aprgn.eip < bprgn.eip
  | Completed ac, Completed bc -> ordered_p ac bc
  | _, _ -> true

let rec print_region_ft ?offset:(offset = 1) (node : veritesting_node finished_type) =
  match node with
  | Branch b ->
    begin
      print_region ~offset:(offset+1) b.true_child;
      print_region ~offset:(offset+1) b.false_child
    end
  | Segment progn ->
    (match progn.next with
    | Some child -> print_region ~offset:(offset+1) child;
    | None -> ())
  | _ -> ()


and print_region ?offset:(offset = 1) (node : veritesting_node) =
  (for i=1 to offset do Printf.printf "\t" done);
  Printf.printf "%s\n" (node_to_string node);
  match node with
  | Undecoded _
  | Raw _ -> ()
  | Completed c -> print_region_ft ~offset c



exception UnexpectedEIP

let walk_to_label target_label tail =
  let rec walk_to_label_help target_label = function
    | [] -> None
    | V.ExpStmt(V.Name(label))::tail ->
      begin
        match decode_eip label with
        | X86eip(_) -> raise UnexpectedEIP
        | Internal(ilabel) ->
          if (target_label = ilabel)
	  then Some tail
          else walk_to_label_help target_label tail
      end
    | _::tail ->
      walk_to_label_help target_label tail in 
  match (decode_eip target_label) with
  | X86eip(_) -> raise UnexpectedEIP
  | Internal(ilabel) -> walk_to_label_help ilabel tail

let complete_node (raw_node : veritesting_node) (child : veritesting_node)
    (sl : V.stmt list) (dl: V.decl list) =
 (match raw_node with
 | Raw progn -> 
   begin
     (* stitch the completed child into the code segment. *)
     let closed_variant = Completed (Segment {eip = progn.eip;
					      stmts = sl;
					      decls = dl;
					      next = Some child;}) in
     progn.next <- Some closed_variant
   end
 | _ -> failwith "Should only be able to call complete node on a raw_node");
  child

let consider_statements node = 
  let progn = match node with
    |Raw progn -> progn
    | _ -> failwith "only for raw nodes" in
  let rec wsl accum = function
    | [] -> Completed (Segment progn)
    | stmt::rest ->
      begin
	match stmt with
	| V.Special("int 0x80") -> Completed (SysCall progn.eip)
	| V.Special _ ->           Completed (Special progn.eip)
	| V.Return _ ->            Completed (Return progn.eip)
	| V.Call _ ->              Completed (FunCall progn.eip)
	| V.Halt _  ->             Completed (Halt progn.eip)
	| V.Label(l) ->
	  begin
	    match (decode_eip l) with
	    (* update this node to finished.  It's stmt list is just the current accum.
	       then, generate the raw child, which is an undecoded of the target eip *)  
	    | X86eip eip -> complete_node node (Undecoded eip) accum progn.decls
	    | Internal _ -> wsl accum rest (* drop internal labels on the floor *)
	  end
        | V.Jmp(V.Name(l)) ->
	  begin
	    match (decode_eip l) with
	    | X86eip eip -> complete_node node (Undecoded eip) accum progn.decls
	    | Internal _ ->
	      match walk_to_label l rest with
	      | None -> Completed (InternalLoop progn.eip)
	      | Some rest' -> wsl accum rest'
	  end 
        | V.CJmp(test, V.Name(true_lab), V.Name(false_lab)) ->
	  begin
	    match (decode_eip true_lab, decode_eip false_lab) with
	    | X86eip teip, X86eip feip -> 
	      let branch = Completed (Branch { test = test;
					       source_eip = progn.eip;
					       true_child = Undecoded teip;
					       false_child = Undecoded feip; }) in
	      complete_node node branch accum progn.decls
	    | X86eip teip, Internal flabel ->
	      begin
		match walk_to_label false_lab rest with
		| None -> Completed (InternalLoop progn.eip)
		| Some rest' ->
		  begin
		    let branch = Completed (Branch { test = test;
						     source_eip = progn.eip;
						     true_child = Undecoded teip;
						     false_child = Raw {eip = progn.eip; (* dangerous!  should be the label. *)
									decls = [];
									stmts = rest';
									next = None;}}) in
		    complete_node node branch accum progn.decls
		  end
	      end
	    | Internal tlabel, X86eip feip ->
	      begin
		match walk_to_label true_lab rest with
		| None -> Completed (InternalLoop progn.eip)
		| Some rest' ->
		  begin
		    let branch = Completed (Branch { test = test;
						     source_eip = progn.eip;
						     true_child = Raw {eip = progn.eip; (* dangerous!  should be the label. *)
								       decls = [];
								       stmts = rest';
								       next = None;};
						     false_child = Undecoded feip;}) in
		    complete_node node branch accum progn.decls
		  end
	      end
	    | Internal tlabel, Internal flabel ->
	      begin
		match (walk_to_label true_lab rest), (walk_to_label false_lab rest) with
		| Some traw, Some fraw ->
		  let branch = Completed (Branch { test = test;
						   source_eip = progn.eip;
						   true_child = Raw {eip = progn.eip;
								     decls = [];
								     stmts = traw;
								     next = None; };
						   false_child = Raw {eip = progn.eip;
								      decls = [];
								      stmts = fraw;
								      next = None;};
						 }) in
		  complete_node node branch accum progn.decls
		| _ -> Completed (InternalLoop progn.eip)
	      end
	  end
	| _ -> wsl (stmt::accum) rest
	  end
  in
  wsl [] progn.stmts


let generate_children fm gamma = function
  | Undecoded eip -> let decls, stmts = decode_insn_at fm gamma eip in
		     [Raw { eip = eip; decls = decls; stmts = stmts; next = None;}]
  | Raw progn as node -> [consider_statements node] (* this has to replace the current node with the appropriate structure? *)
  | Completed ft ->
    begin
      match ft with
      | ExternalLoop _
      | InternalLoop _ 
      | Return _
      | Halt _
      | FunCall _
      | SysCall _
      | Special _
      | SearchLimit _ -> []
      | Branch b -> [b.true_child; b.false_child]
      | Segment progn -> []
    end

let expand fm gamma (node : veritesting_node) = generate_children fm gamma node

let make_root eip = Undecoded eip
