(* this is for testing Libstp only. Regular programs should go through the
 * Stpvc module instead, since sets up all the hooks to have things
 * cleaned up properly during garbage collection. *)
open Libstp

let vc = vc_createValidityChecker()

let t = vc_trueExpr vc
let f = vc_falseExpr vc
;;

vc_assertFormula vc (vc_impliesExpr vc t f);
print_endline "asserts:";
vc_printAsserts vc;
print_endline "end asserts";

print_endline "querying false: ";
print_int (vc_query vc f);
print_endline "\nprinting query:";
vc_printQuery vc;
print_endline "\nprinting counterexample:";
vc_printCounterExample vc;

