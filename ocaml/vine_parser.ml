module D = Debug.Make(struct let name = "Vine_parser" and default=`Debug end)
open D


let flag_cppFlags = ref [""];;
let flag_pp = ref false;;
let flag_typecheck = ref true;;
let flag_track_lines = ref false;;

let set_flag_cppFlags s = flag_cppFlags := s :: !flag_cppFlags;;

let defspecs = [
  ("-cpp", Arg.String(set_flag_cppFlags), "  Pass a string of flags to cpp");
  ("-pp",  Arg.Set(flag_pp), "  Pretty-print AST");
  ("-nocheck", Arg.Clear(flag_typecheck), "  Disable typechecking.");
  ("-linenums", Arg.Set(flag_track_lines),
			("Enable cpp line number tracking "^
			   "(inefficient and will slow you down)"));
  ("-nolinenums", Arg.Clear(flag_track_lines),
			("Disable cpp line number tracking (default)"));
  ("-stripnums", Arg.Set Vine_absyn.strip_nums,
   "Strip the _number from the end of parsed variable names");
   
];;

try (* set the default include path *)
  (* FIXME: handle : separated paths *)
  let s = Sys.getenv "VINE_INCLUDES" in
    set_flag_cppFlags("-I "^s)
with Not_found -> ()

let linux_cpp infile inflags = 
  "cpp -C -E -o -"
  ^ List.fold_left (fun r s -> " "^s^r) "" inflags
  ^ " " ^ infile
;;

let parse_exp decls lexbuf = 
  let scope = Vine_absyn.getscope () in 
  let () = List.iter (fun x -> scope#insert x) decls in 
  let e = (
    try 
      Vine_grammar.expr Vine_lexer.token lexbuf 
    with
	Parsing.Parse_error ->
	  raise (Vine.VineError (
	    Printf.sprintf "Syntax error %s\n" 
	      (Vine_lexer.lexbuf_error_to_string lexbuf)))
  ) in
    e


let parse_exp_from_string (decls:Vine.var list) (str:string) : Vine.exp = 
  let _ = Vine_absyn.startFile "" in 
  let () = Vine_lexer.set_line_tracking !flag_track_lines in 
  let lexbuf = Lexing.from_string str in 
    parse_exp decls lexbuf



let parse_lexbuf lexbuf = 
  let (dl,sl) = 
    Vine_grammar.program Vine_lexer.token lexbuf in 
    (dl,sl)


let parse_channel ch =
  let _ = Vine_absyn.startFile "" in 
  let () = Vine_lexer.set_line_tracking !flag_track_lines in 
  let lexbuf = Lexing.from_channel ch in
      parse_lexbuf lexbuf

let preprocess filename = 
  let cmd = linux_cpp filename !flag_cppFlags in 
    Unix.open_process_in cmd
(* (* just discovered close_process_in, so we don't need this *)
  let (infd,outfd) = Unix.pipe() in
  let pid = Unix.fork() in
    if pid > 0 then (* parent *)
      (Unix.in_channel_of_descr infd, fun () -> snd(Unix.wait pid))
    else if pid = 0 then (* child *)
      let () = Unix.dup2 outfd Unix.stdout in
    else (* err isn't an exception? *)
      failwith "fork returned negative pid!?!"
*)
;;


let parse_file filename : Vine.program = 
  let infd = preprocess filename in
  let scope = Vine_absyn.getscope () in
  let () = scope#clear () in 
  let _ = Vine_absyn.startFile filename in
  let () = Vine_lexer.set_line_tracking !flag_track_lines in 
  let lexbuf = Lexing.from_channel infd in
    (try  
       let (dl,sl) =  Vine_grammar.program Vine_lexer.token lexbuf in 
       let () = 
	 try (
	   match Unix.close_process_in infd with
	     | Unix.WEXITED 0 -> ()
	     | Unix.WEXITED n ->
		 failwith("parse_file: preprocessor exited "^string_of_int n)
	     | Unix.WSIGNALED n ->
		 failwith("parse_file: preprocessor was killed "^string_of_int n)
	     | Unix.WSTOPPED n ->
		 failwith("parse_file: preprocessor was stopped "^string_of_int n)
	 )
	 with 
	     Unix.Unix_error(Unix.ECHILD,_,_)  -> 
	       wprintf "Child preprocessing child died unexpectedly.";
	       wprintf "see bug #5 in bugzilla"
	   | Unix.Unix_error(e,_,_) -> failwith (Unix.error_message e)
       in
	 (dl,sl)
      with
	  Parsing.Parse_error ->
	    let str = 
	    Printf.sprintf "Syntax error %s\n" 
	      (Vine_lexer.lexbuf_error_to_string lexbuf) in 
	    Vine_absyn.error (Lexing.lexeme_start lexbuf,
			     Lexing.lexeme_end lexbuf)
	      str;  exit(-1)
    )
;;
