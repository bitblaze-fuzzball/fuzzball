let rec loop n f = match n with 0 -> () | _ -> f(); loop (n-1) f
;;
loop 1000000 Stpvc.create_validity_checker
