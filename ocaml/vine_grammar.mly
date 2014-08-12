%{
(* IR Grammar file *)
(* Orignal author: David Brumley *)

 open Vine_absyn;;
 open Vine;;

  (* utility function for creating position information *)
  let line_ctxt l r:abspos = (rhs_start l, rhs_end r);; 

  let scope = getscope ()




%}

%token <string> ID
%token <Int64.t> INT
%token <string> STRING
%token <string> COMMENT
%token <Vine.typ> TYP

%token LPAREN RPAREN SEMI EOF  LCURLY RCURLY COLON QUESTION
%token LSQUARE RSQUARE COMMA 
%token CJMP NAME JMP CAST INIT VAR LET IN TRUE FALSE LABEL
%token ATTR CALL ASSERT HALT
%token SPECIAL UNKNOWN STATE TVOID RETURN EXTERN 
%token PLUS MINUS  DIVIDE MOD SMOD TIMES 
%token SDIVIDE LSHIFT RSHIFT ARSHIFT XOR NEQ
%token SLT SLE AND OR 
%token EQUAL LT  LE NOT ASSIGN 
%token GT GE SGT SGE

%start program expr

%type <Vine.program> program
%type <Vine.stmt list> stmtlist 
%type <Vine.exp > expr
%nonassoc IN
%nonassoc LET
%nonassoc ASSIGN
/* If the precedence for any of these changes, vine.ml needs to be updated
   accordingly, so that it can parethesize things properly */
%right QUESTION COLON
%left OR XOR AND
%left EQUAL NEQ
%left LT SLT SLE LE   GT GE SGT SGE
%left LSHIFT RSHIFT ARSHIFT
%left PLUS MINUS
%left TIMES DIVIDE SDIVIDE MOD SMOD
%left UMINUS 
%left NOT
%%

program: 
| stmtlist EOF { 
    let dl = scope#decls () in
    (* let () = scope#dec () in  *)
      (List.rev dl, $1)
  }

stmtlist:
| revstmtlist  { List.rev $1 }

/* This is needed, because if we say stmtlist := stmt stmtlist, then the parser
   needs to put all the stmts on a stack, since it can't process them until
   it parses the last one. Said stack is limited to Sys.max_array_length, which
   means than on i386, we woulddn't be able to parse a stmtlist of more than
   about 4 million.
   This is confirmed at
   http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/sec-recursive-rules.html
 */
revstmtlist:
| revstmtlist stmt  {  match $2 with
			 | [x] -> x::$1
			 | [] -> $1
			 | _ -> failwith "stmt returned more than one stmt"
		    }
|                { [] }

stmt:
| jmpstmt SEMI        { [$1] }
| specialstmt SEMI    { [$1] } 
| labelstmt           { [$1] } 
| COMMENT             { [Comment($1)] }
| simplestmt SEMI     { [$1] } 
| func                { [$1] } 
| vardecls            { [] } 
| stmtblock           { [$1] }
| SEMI                { [] }


jmpstmt: 
| JMP LPAREN expr RPAREN 
    { Jmp($3) }
| CJMP LPAREN expr COMMA expr COMMA expr RPAREN
         { CJmp($3, $5, $7)  }


specialstmt:
| SPECIAL LPAREN STRING RPAREN 
    { Special($3)}

labelstmt:
|  LABEL ID COLON { Label($2) }

simplestmt: 
| assignment       { $1 }
| expr             { ExpStmt($1) }
| RETURN optexpr   { Return($2) }
| HALT expr        { Halt($2) }
| ASSERT expr      { Assert($2) } 
| callstmt         { $1 } 

assignment :
| lval ASSIGN expr { Move($1, $3) }
| lval ASSIGN callstmt {
    match $3 with
	Call(None, a, b) -> Call(Some($1), a, b)
      | _ -> err (line_ctxt 1 3) "Bad call statement"
  }


callstmt:
| CALL expr  LPAREN arguments RPAREN 
      { Call(None, $2, $4) }
| ID LPAREN arguments RPAREN 
  { (* old syntax *) Call(None, Vine.Name($1), $3) } 

optexpr:
|      { None }
| expr { Some($1) }

arguments :
|  { [] }
| args { $1 } 

args:
| expr   { [$1] }
| args COMMA expr  { $3 :: $1 } 


func:
| optextern funtyp ID LPAREN formals RPAREN optbody
    { 
      (
	match $7 with
	    Some _ -> if $1 then 
	      err (line_ctxt 1 3) 
		"Function declared external but defined here"
	  | _ -> () 
      );
      scope#dec (); (* this decrements the formals scope *)
      Function($3, $2, List.rev $5, $1, $7)
    }

funtyp :
| TVOID { (* we increment the scope here so that the
	     formals are in a new scope *) 
    scope#inc (); None }
| typ { scope#inc (); Some($1)}

optextern:
|         { false }
| EXTERN  { true }

optbody:
| SEMI      { None }
| stmtblock { Some($1) }

formals:
| formallist { $1 }
| { [] } 

formallist:
| formallist COMMA formal { $3 :: $1 }
| formal { [$1] }

formal: 
| ID COLON typ { scope#add $1 $3 } 


vardecls:
| VAR idents COLON typ SEMI
  { 
    List.iter (fun n -> ignore(scope#add n $4)) (List.rev $2)
  }

idents:
| revidents  { List.rev $1 }

revidents:
| ID              { [$1] }
| revidents COMMA ID { $3 :: $1 }

lcurlyrule:
LCURLY { ignore(scope#inc ()) } 

stmtblock:
| lcurlyrule stmtlist RCURLY { 
    let dl = scope#decls () in 
    let () = scope#dec () in 
      Block(dl, $2)
  }



typ:
| basetyp attrs  { match $2 with 
		       [] -> $1
		     | _ -> TAttr($1, $2) 
		 } 
| typ LSQUARE typ RSQUARE  { Array($1, array_idx_type_to_size $3) } 
| typ LSQUARE INT RSQUARE { Array($1, $3)  } 

basetyp:
| TYP { $1} 


attrs:
|    { [] }
| ATTR LPAREN idlist RPAREN { $3  }

idlist:
/* |   { [] } */
| ID { [$1] }
| ID COMMA idlist { $1 :: $3 } 

lval:
| ID optindex opttyp { 
    check_lval scope $1 $2 $3 (line_ctxt 1 1)
  } 

opttyp:
| { None } %prec IN /* low prec means in case of conflict, the typ
                       is not optional */
| COLON typ { Some($2) } 

optindex:
|  { None } 
| LSQUARE expr RSQUARE  { Some($2)  } 

letstart:
| LET ID optindex opttyp ASSIGN expr { 
    scope#inc(); 
    match $3,$4 with
	None,None ->
	  Vine_absyn.err (line_ctxt 1 3) 
	    "Let variable declared without type"
      | None,Some(t) -> (Temp(scope#add $2 t), $6)
      | Some(idx), None -> (
	  let v= scope#lookup $2 in 
	    match v with
		(_,_,Array(t1,_)) -> (Mem(v,idx,t1), $6)
	      | _ -> Vine_absyn.err (line_ctxt 1 3)
		  "Memory reference required type annotation"
	)
      | Some(idx), Some(t) ->
	  (Mem(scope#lookup $2, idx, t),$6)
  }

expr:
| LPAREN expr RPAREN { $2 }
| expr PLUS expr     { BinOp(PLUS, $1, $3) }
| expr MINUS expr    { BinOp(Vine.MINUS, $1, $3)}
| expr TIMES expr    { BinOp(Vine.TIMES, $1, $3) }
| expr DIVIDE expr   { BinOp(Vine.DIVIDE, $1, $3) }
| expr SDIVIDE expr  { BinOp(Vine.SDIVIDE, $1, $3) }
| expr MOD expr      { BinOp(Vine.MOD, $1, $3) }
| expr SMOD expr     { BinOp(Vine.SMOD, $1, $3) }
| expr LSHIFT expr   { BinOp(Vine.LSHIFT, $1, $3) }
| expr RSHIFT expr   { BinOp(Vine.RSHIFT, $1, $3) }
| expr ARSHIFT expr  { BinOp(Vine.ARSHIFT, $1, $3) }
| expr AND expr      { BinOp(Vine.BITAND, $1, $3) }
| expr OR expr       { BinOp(Vine.BITOR, $1, $3) }
| expr XOR expr      { BinOp(Vine.XOR,  $1, $3) }
| expr EQUAL expr    { BinOp(Vine.EQ, $1, $3) }
| expr NEQ expr      { BinOp(Vine.NEQ, $1, $3) }
| expr LT expr       { BinOp(Vine.LT, $1, $3) }
| expr LE expr       { BinOp(Vine.LE,  $1, $3) }
| expr SLT expr      { BinOp(Vine.SLT, $1, $3) }
| expr SLE expr      { BinOp(Vine.SLE, $1, $3) }
/* syntactic sugar for greater than */
| expr GT expr       { BinOp(Vine.LT,  $3, $1) }
| expr GE expr       { BinOp(Vine.LE,  $3, $1) }
| expr SGT expr      { BinOp(Vine.SLT, $3, $1) }
| expr SGE expr      { BinOp(Vine.SLE, $3, $1) }
| NOT expr           { UnOp(Vine.NOT, $2) }
| MINUS expr %prec UMINUS  { UnOp(Vine.NEG, $2) }
| constexp           { $1 } 
| UNKNOWN STRING     { Unknown($2) } 
| NAME LPAREN ID RPAREN { Name($3) } 
| lval               { Lval($1) } 
| letstart   IN expr { scope#dec (); 
		       let (x,y) = $1 in 
		       Let(x,y, $3) } 
| CAST LPAREN expr RPAREN ID  COLON typ  
    { Cast(casttype_of_string $5, $7, $3) }	  
| expr QUESTION expr COLON expr
                     { $3 }

constexp:
| TRUE               { Vine.exp_true } 
| FALSE              { Vine.exp_false }
| INT COLON typ      { Constant(Int($3, $1)) } 
| STRING             { Constant(Str($1)) }


%%

