let vc = Stpvc.create_validity_checker()
let bv64 = Stpvc.bitvector_t vc 64

let rec loop n f = match n with 0 -> () | _ -> f(); loop (n-1) f
;;
loop (-1) (fun()->Stpvc.e_eq vc (Stpvc.e_true vc) (Stpvc.e_false vc))
