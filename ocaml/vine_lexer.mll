(* Lexer for our IR language *)
(* Author: David Brumley *)

{
 open Vine_grammar;;               (* open the grammar for the tokens *)
 open Lexing;;
 open Vine_absyn;;

 exception LexError of string

   (* if true, we keep single-line slashy-slashy comments *)
 let flag_keep_linecomments = ref true;;
 (* if true, we keep slashy star star slashy comments *)
 let flag_keep_blkcomments = ref true;;

 let track_line_numbers = ref false;;

 let disable_line_numbers () = 
   track_line_numbers = ref false
   
 let enable_line_numbers () = 
   track_line_numbers = ref true

 let set_line_tracking b = 
   track_line_numbers := b


     

 let get = Lexing.lexeme

 let lend = lexeme_end

 let lstart = Lexing.lexeme_start

 let incr_linenum lexbuf =
   let pos = lexbuf.Lexing.lex_curr_p in
     lexbuf.Lexing.lex_curr_p <- { pos with
				     Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
				     Lexing.pos_bol = pos.Lexing.pos_cnum;
				 }

 let fileoffset = ref 0;;


(* quoted string handling code taken from:
   http://caml.inria.fr/pub/ml-archives/caml-list/2005/10/d22d128ac2fd06df780201ccb4b49ead.en.html
*)

 let char_for_backslash = function   
   | 'a' -> '\007'
   | 'v' -> '\011'
   | 'f' -> '\012'
   | 'n' -> '\n'
   | 't' -> '\t'
   | 'b' -> '\b'
   | 'r' -> '\r'
   | c   -> c
       

  

 let string_buff = Buffer.create 256
 let reset_string_buffer () = Buffer.clear string_buff  
 let store_string_char c = Buffer.add_char string_buff c
 let store_string s = Buffer.add_string string_buff s
 let get_stored_string () = Buffer.contents string_buff


 let in_comment = ref 0;;
 let comment_pos = ref 0;;
 let comment_start yypos =
   assert(!in_comment = 0);
   in_comment := 1;
   comment_pos := yypos;
   reset_string_buffer()  
 let enter_comment () = 
   in_comment := !in_comment + 1
 let exit_comment () = 
   in_comment := !in_comment - 1
 let in_comment () = !in_comment > 0;;


 let eof ()  = 
  if (in_comment ())  then  (
     Vine_absyn.error (!comment_pos, !comment_pos)
       "Unterminated comment";
     raise (Vine.VineError("Lexing error"))
  ) else ();

   EOF
 

 let lexbuf_error_to_string  lexbuf = 
   let pos = lexbuf.Lexing.lex_curr_p in 
   let linepos = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in 
   let lexstr = (get lexbuf) in 
     Printf.sprintf "line %u pos %u: %s" pos.Lexing.pos_lnum
       linepos lexstr
}



let bs_escapes = [ '\032' - '\255' ]
let alpha = ['a'-'z' 'A'-'Z' '_']
let digit = ['0'-'9']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let hexinteger = ('0'('x' | 'X'))hexdigit*
let id = alpha(digit|alpha)*
let nl = ('\r' | '\n' | "\r\n")
let ws = [' ''\t']
let fname = ('"')[^'\r' '\n']*('"')[^'\r' '\n']* nl
let singlecomment = "//"[^'\n' '\n']*nl 

(***********************************************************************)
(* Parse the source-code \ comments \ cpp *)
(***********************************************************************)

rule token = parse
  | eof          { eof() }
  | (ws)+        { token lexbuf }
  | nl           { if !track_line_numbers then 
		     Vine_absyn.startNewline (lstart lexbuf);  
		   incr_linenum lexbuf; token lexbuf }
(* types *)
  | "reg1_t"     { TYP(Vine.REG_1) }
  | "reg8_t"     { TYP(Vine.REG_8) } 
  | "reg16_t"    { TYP(Vine.REG_16) }
  | "reg32_t"    { TYP(Vine.REG_32) } 
  | "reg64_t"    { TYP(Vine.REG_64) }
  | "string_t"   { TYP(Vine.TString) } 
  | "addr_t"     { TYP(Vine.addr_t) }
  | "mem32l_t"   { TYP(Vine.TMem(Vine.REG_32, Vine.Little)) }
  | "mem64l_t"   { TYP(Vine.TMem(Vine.REG_64, Vine.Little)) } 
  | "void"       { TVOID }
  | "_attr_"     { ATTR } 
(* control flow *)
  | "cjmp"       { CJMP }
  | "jmp"        { JMP } 
  | "call"       { CALL } 
  | "halt"       { HALT }
  | "return"     { RETURN } 
(* logical *)
  | "assert"     { ASSERT }
  | "true"       { TRUE }
  | "false"      { FALSE }
(* misc *)
  | "unknown"    { UNKNOWN }
  | "special"    { SPECIAL }
(* declarators *)
  | "name"       { NAME } 
  | "var"        { VAR } 
  | "let"        { LET }
  | "in"         { IN }
  | "label"      { LABEL } 
  | "extern"     { EXTERN }
  | "cast"       { CAST }
(* id must come after all keywords *)
  | '{'          { LCURLY }
  | '}'          { RCURLY }
  | '['          { LSQUARE }
  | ']'          { RSQUARE }
  | '+'          { PLUS }
  | '-'          { MINUS }
  | '*'          { TIMES }
  | '/'          { DIVIDE }
  | "/$"         { SDIVIDE }
  | '%'          { MOD }
  | "%$"         { SMOD }
  | "<<"         { LSHIFT }
  | ">>"         { RSHIFT }
  | "@>>"        { ARSHIFT }
  | '&'          { AND }
  | '|'          { OR }
  | '^'          { XOR } 
  | "=="         { EQUAL }
  | "<>"         { NEQ } 
  | '<'          { LT }
  | "<="         { LE } 
  | "<$"         { SLT } 
  | "<=$"        { SLE } 
  | '>'          { GT }
  | ">="         { GE } 
  | ">$"         { SGT } 
  | ">=$"        { SGE } 
  | '!'          { NOT }
  | "="          { ASSIGN }
  | ':'          { COLON } 
  | '?'          { QUESTION }
  | ';'          { SEMI }
  | '('          { LPAREN }
  | ')'          { RPAREN }
  | ','          { COMMA } 
  | '"'          { reset_string_buffer ();
	   	   scan_str lexbuf;
		   let s = get_stored_string () in
		      STRING(s)
	         }

  | "/*"         { comment_start(lstart lexbuf);
		   blkcomment lexbuf;
		   if !flag_keep_blkcomments then 
		     COMMENT(get_stored_string())
		   else
		     token lexbuf 
		   (* COMMENT("") *)
		     (* throw away this type of comment *)
		 }
  | "#"           { cpptoken lexbuf }
  | singlecomment { if !flag_keep_linecomments then
		      let s = get lexbuf in 
			(* -3 to remove the newline *)
			COMMENT(String.sub s 2 ((String.length s) -3)) 
		    else
		      token lexbuf 
		  } 
  | id           { ID(get lexbuf) }
  | digit+       { 
	try 
	  INT(Int64.of_string(get lexbuf))
	with 
	    int_of_string -> 
	      raise(LexError "Error converting integer");
      }
  | hexinteger       { 
	try 
	  INT(Int64.of_string(get lexbuf))
	with 
	    int_of_string -> 
	      raise(LexError "Error converting integer");
      }

  | _ as s    { raise(LexError("Unrecognized char '"^Char.escaped s^"'")) }

(* no longer needed *)
(* and comment = parse  *)
(* |  nl *)
(*     { incr_linenum lexbuf; *)
(*       if in_comment () then  *)
(* 	( exit_comment (); token lexbuf ) *)
(*       else  *)
(* 	token lexbuf *)
(*     } *)
(* |  eof       { eof () } *)
(* |  _ as c        { store_string_char c; comment lexbuf  } *)

(* probably not the best way to implement this, but the lack of block comments
   has been bugging me too much. --aij *)
and blkcomment = parse 

| "/*"         { store_string "/*"; enter_comment();  blkcomment lexbuf }
|  "*/"
    { if in_comment () && (exit_comment(); (*still_*)in_comment())
      then 
	(store_string "*/"; blkcomment lexbuf)
      else 
	()
    (* token lexbuf *)
    }
| nl           { incr_linenum lexbuf; store_string_char '\n'; blkcomment lexbuf }
|  eof       { ignore(eof ());() } 
| _ as c         { store_string_char c; blkcomment lexbuf  }

and scan_str = parse
  | ['"']   {  () }
  | '\\'  (bs_escapes as c)
            { store_string_char (char_for_backslash c);
	      scan_str lexbuf 
	    }
  | eof      { raise(LexError "Unterminated string") }
  | _  as c   {  store_string_char c; scan_str lexbuf }
			   
and cpptoken = parse
  | digit+ { if !track_line_numbers then (
	       fileoffset := (Pervasives.int_of_string (get lexbuf));
	     );
             cpptoken lexbuf
           }
  | fname { 
      if !track_line_numbers then (
	Vine_absyn.startNewFile (lstart lexbuf)
              (Vine_absyn.extractFileName (get lexbuf))
              !fileoffset;
      );
            token lexbuf
          }
  | nl  { token lexbuf }
  | ws+   { cpptoken lexbuf}
  | eof { eof () }
  | _ { Vine_absyn.error (lstart lexbuf, lend lexbuf -1)
          ("Illegal CPP instruction: " ^ (get lexbuf));
        raise (Vine.VineError("Lexing error"))
      }
