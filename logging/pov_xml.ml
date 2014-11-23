(*
  XML Logger scratch pad
  Implements the POV reporting XML described for CGC
 *)

type pcre = string
type var = string

type data_typ =
| Hex
| Ascii

type echo =
| Yes
| No

type trace_type =
| NominalBehavior
| Error of int64

type delim = {
  deliminator : int;
  delim_typ : data_typ; 
}

type delay = {
  d_ms : int;
}

type timeout = {
  to_ms : int;
}

type cbid = { (* !opt_program_name (string option) from exec_set_options *)
  cb_name : string;
}

type decl = {
  var_name : var;
  value : int; (* int64? *)
}

type slice = {
  start : int option; (* negative values from rear.  Default 0 *)
  stop : int option;  (* negative values from rear. Default len data *)
}

type data = {
  data_typ : data_typ;
  contents : char array; (* maybe I want int64 here *)
}

type assign_value =
| Slice of slice
| Matching of pcre

type assign = {
  target : string;
  avalue : assign_value;
}

type match_el =
| Var of  var
| Data  of data
| Regexp of pcre

type cgc_match = {
  match_elements : match_el list;
}

type read_input =
| Length of int
| Delim of delim

type concrete_read = {
  echo : echo;
  input_marker : read_input;
  assign : assign option;
  mtch : cgc_match option;
  timeout : timeout option;
}

type write_target =
| WData of data
| WVar of var

type concrete_write = {
  datas : write_target list;
}

type symbolic_io = {
  start_addr : int64;
  end_addr : int64;
  constraints : Vine.exp array;
}


type action =
| ConcreteRead of concrete_read
| SymbolicRead of symbolic_io
| ConcreteWrite of concrete_write
| SymbolicWrite of symbolic_io
| Decl of decl
| Delay of delay

type replay = {
  actions : action list;
}

type pov = {
  cbid : cbid;
  replay : replay;
}

(* need to check times to make sure they're psoitive
   other structs are complete by construction *)
let valid (a : delay) = a.d_ms >= 0
let valid (a : timeout) = a.to_ms >= 0

(* XML Construction, output *)

(* if this is -1, the we're using a channel or sockets.  If it's >= 0,
   then we're using numbered files. *)
let pov_output_count = ref ~-1
let out_channel_name = ref "/dev/null"
let out_channel = ref (open_out "/dev/null")

let array_of_string str =
  Array.init (String.length str) (fun i -> str.[i])

let str_of_array char_array =
  (* String.init wasn't introduced until 4.0.2, have to do this in two passes.
  String.init (Array.length char_array) (fun i -> char_array.(i)) *)
  let str = String.make (Array.length char_array) ' ' in
  Array.iteri (fun i c -> str.[i] <- c) char_array;
  str


let file_perms name =
    (Unix.stat name).Unix.st_perm

let rec ensure_dir dirname =
    (** ensures that the given directory exists, creating any
	intermediate directories as needed *)
  if not (Sys.file_exists dirname)
  then let sub = Filename.dirname dirname in
       ensure_dir sub;
    (* check again to avoid failure of Filename.dirname on "/foo/bar/" *)
       if not (Sys.file_exists dirname)
       then Unix.mkdir dirname (file_perms sub)

let next_string_channel basename =
  (* we can change the filename to be incremented here.
     just, everytime we output xml, increment the counter.
     If we do that for the json as well, we won't need to
     synchronize anything. [JTT 11-13]*)
  (* make sure the directory for the base-name exists*)
  (* make pov-#.xml, ensure that they line up the right way by
     padding out with 0s *)
  if basename = "/dev/null"
  then open_out basename
  else
      (ensure_dir basename;
       (* ensure that basename exists *)
       open_out (Printf.sprintf
		   "%s/pov-%i.xml"
		   basename
		   !pov_output_count))


let set_logfile_channel filename =
  (* Stolen / adapted from logger_config.ml -- I'm going to prune out all of the json logging at some point,
     as no one is using it anymore -- [JTT / 10-28] *)
  match filename with
  | "stdout" -> out_channel := Pervasives.stdout
  | "stderr" -> out_channel := Pervasives.stderr
  | _ ->
    let regex = Str.regexp ".*:[0-9]+" in
    let chan = 
      if Str.string_match regex filename 0
      then
	begin
	  if (!pov_output_count = 0)
	  then
	    (let tokens = Str.split (Str.regexp ":") filename in
	     assert ((List.length tokens) == 2);
	     let addr = Unix.inet_addr_of_string (List.hd tokens)
	     and port = int_of_string (List.hd (List.tl tokens)) in
	     let _, outchan = Unix.open_connection (Unix.ADDR_INET(addr, port)) in
	     out_channel := outchan)
	end
      else out_channel := next_string_channel filename in
    chan


let set_out_channel fname =
  (* here's the actual tricky part.  This is the entry point for
     setting up the channels for the pov xml.  The channel is actually
     setup here by the arguments.  We need to replace the channel
     setup here with setting a base string from which the channel is
     going to be constructed. *)
  out_channel_name := fname


let debug_print v =
  Xml.print_list
    (fun s -> Printf.fprintf !out_channel "%s" s;
      flush !out_channel) [v];
  Printf.fprintf !out_channel "\n"


let echo_to_xml = function
  | Yes -> Xml.string_attrib "echo" "yes"
  | No ->  Xml.string_attrib "echo" "no"
(*  | Ascii -> Xml.string_attrib "echo" "ascii"*)

let data_type_to_xml = function
  | Hex -> Xml.string_attrib "format" "hex"
  | ascii  -> Xml.string_attrib "format" "asciic"

let data_to_xml (a : data) =
  let as_string = String.concat ""
    (Array.fold_right
       (fun el accum ->  (Printf.sprintf "%c" el) :: accum)
       a.contents []) in
  Xml.node ~a:[data_type_to_xml a.data_typ] "data"
    [Xml.pcdata as_string]

let delim_to_xml (a : delim) =
  Xml.node ~a:[data_type_to_xml a.delim_typ] 
    "delim"
    [Xml.pcdata (Printf.sprintf "%i" a.deliminator);]

let delay_to_xml (a : delay) =
  Xml.node "delay" [Xml.pcdata (Printf.sprintf "%i" a.d_ms)]

let timeout_to_xml (a : timeout) =
  Xml.node "timeout" [Xml.pcdata (Printf.sprintf "%i" a.to_ms)]

let cbid_to_xml (a : cbid) =
  Xml.node "cbid" [Xml.pcdata a.cb_name]

let decl_to_xml (a : decl) =
  Xml.node "decl"
    [Xml.node "var" [Xml.pcdata a.var_name];
     Xml.node "value" [Xml.pcdata (Printf.sprintf "%i" a.value)]]
	
let slice_to_xml (a : slice) =
  match (a.start, a.stop) with
  | Some a, Some b -> Xml.node ~a:[Xml.int_attrib "begin" a; Xml.int_attrib "end" b] "slice" []
  | Some a, None -> Xml.node ~a:[Xml.int_attrib "begin" a] "slice" []
  | None, Some b -> Xml.node ~a:[Xml.int_attrib "end" b] "slice" []
  | None, None -> failwith "Can't have a slice with neither a start nor a stop."

let pcre_to_xml (a : pcre) =
  Xml.node "pcre" [Xml.pcdata a]

let assignval_to_xml = function 
  | Slice s -> slice_to_xml s
  | Matching r -> pcre_to_xml r

let assign_to_xml (a : assign) =
  Xml.node "assign"
    [Xml.node "var" [Xml.pcdata a.target];
     assignval_to_xml a.avalue;]

let matchel_to_xml = function 
  | Var v -> Xml.node "var" [Xml.pcdata v]
  | Data d -> data_to_xml d
  | Regexp r -> (pcre_to_xml r)

let match_to_xml (a : cgc_match) =
  Xml.node "match"
    (List.map matchel_to_xml a.match_elements)

let read_input_to_xml = function
  | Length l -> Xml.node "length" [Xml.pcdata (Printf.sprintf "%i" l)]
  | Delim d -> delim_to_xml d

let concrete_read_to_xml a =
  let children = ref [] in
  children := (read_input_to_xml a.input_marker) :: !children;
  (match a.mtch with
  | Some s -> children := (match_to_xml s) :: !children 
  | None -> ());
  (match a.timeout with
  | Some s -> children := (timeout_to_xml s) :: !children 
  | None -> ());
  (match a.assign with
  | Some s -> children := (assign_to_xml s) :: !children 
  | None -> ());
  Xml.node ~a:[(echo_to_xml a.echo)] "read" !children

let symbolic_read_to_xml fm r =
  failwith "stub"
    

let write_target_to_xml = function
  | WData d -> data_to_xml d
  | WVar v -> Xml.node "var" [Xml.pcdata v]

let symbolic_write_to_xml fm w =
  (* solve the current path constrains, give the correct output *)
  let (_, ce) = fm#query_with_path_cond Vine.exp_true false in
  let as_int64 = Array.init (Array.length w.constraints) (fun i -> fm#eval_expr_from_ce ce w.constraints.(i)) in
  let num_chars = ref 0 in
  let as_string = Array.init (Array.length as_int64)
    (fun i -> let to_add = Printf.sprintf "%02LX" as_int64.(i) in
	      num_chars := !num_chars + (String.length to_add);
	      to_add) in
  let char_array = Array.create !num_chars ' ' in
  let i = ref 0 in
  Array.iter (fun e ->
    for j = 0 to ((String.length e) - 1)
    do
      char_array.(!i) <- e.[j];
      i := !i + 1
    done) as_string;
  let dt = {data_typ = Hex;
	    contents = char_array; } in
  let wt = WData dt in
  Xml.node "write" [(write_target_to_xml wt)]
  

let action_to_xml fm = function
  | ConcreteWrite w -> Xml.node "write" (List.map write_target_to_xml w.datas)
  | SymbolicWrite w -> symbolic_write_to_xml fm w
  | ConcreteRead r -> concrete_read_to_xml r
  | SymbolicRead r -> symbolic_read_to_xml fm r
  | Decl d -> decl_to_xml d
  | Delay d -> delay_to_xml d

let replay_to_xml a fm =
  Xml.node "replay"
    (List.map (action_to_xml fm) a.actions)

let pov_to_xml a fm =
  Xml.node "pov"
    [cbid_to_xml a.cbid;
     replay_to_xml a.replay fm]

(* incremental pov construction *)
type event_list = action list
let events = ref ([] : event_list)



let add_read (read_in : string) (start : int64) (length : int) =
  (* Add a read command to the list of actions to be put into the pov *)
  let me = { echo = Yes;
	     input_marker = Length length;
	     assign = None;
	     mtch = None;
	     timeout = None; } in
  events := (ConcreteRead me) :: !events

let add_read_car (read_in : char array) (start : int64) =
  add_read (str_of_array read_in) start (Array.length read_in)

let add_transmit contents length =
  (* add a write command to the list of actions to be put into the pov *)
  (* cgcos_transmit *)
  let dt = {data_typ = Hex;
	    contents = contents; } in
(*  let dt = {data_typ = Ascii;
	    contents = contents; } in *)
  let wt = WData dt in
  let write = {datas = [wt]} in
  events := (ConcreteWrite write) :: !events

  

let add_transmit_str string length =
  add_transmit (array_of_string string) length

let symb_reads = ref 0
and symb_writes = ref 0

let add_symbolic_read start_addr end_addr constraints =
  symb_reads := !symb_reads + 1;
  let to_add = {start_addr = start_addr;
		end_addr = end_addr;
		constraints = constraints; } in
  events := (SymbolicRead to_add) :: !events

let add_symbolic_transmit start_addr end_addr constraints =
  symb_writes := !symb_writes + 1;
  let to_add = {start_addr = start_addr;
		end_addr = end_addr;
		constraints = constraints; } in
  events := (SymbolicWrite to_add) :: !events
  


let add_decl () =
  (* cgc allocate? *)
  failwith "stub"

let add_delay duration =
  failwith "stub"

let add_timeout duration =
(* cgcos_fdwait *)
  failwith "stub"

let reset () =
  events := []

let write_pov name fragmach =
  (* build the xml you need to output *)
  (* [JTT 11/17/14 ] -- set the channel here, because
     you haven't opened it yet.  Don't set it at the end, because that results in empty
     files that are ugly and confusing. *)
  pov_output_count := !pov_output_count + 1;
  set_logfile_channel !out_channel_name;
  let cbid = { cb_name = name; } in
  let replay = {actions = List.rev !events} in
  let pov = {cbid = cbid; replay = replay} in
  (* put it out *)
  debug_print (pov_to_xml pov fragmach);
  (* clear your cache *)
  reset ()


let pov_xml_cmdline_opts =
  [("-pov-xml-output", Arg.String set_out_channel,
    "{directoryname,_host:ip,_stdout,_stderr} Sets output location for pov xml files. Either a directory name, stdout, or IP:PORT."); ]

