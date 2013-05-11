let vc = Stpvc.create_validity_checker()

let rec loop n f = match n with 0 -> () | _ -> f(); loop (n-1) f
;;
loop (-1) (fun()->Stpvc.bitvector_t vc 64)
