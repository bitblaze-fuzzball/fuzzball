(* logs table of indirect jump targts *)

let assoc = Hashtbl.create 100
let output_loc = ref "./indirect_addrs.txt"


let add current_addr target_addr =
  Build_assoc.add assoc current_addr target_addr


let clear () =
  Build_assoc.clear assoc


let i64_and_space str i64 = 
  Printf.fprintf str "0x%08Lx " i64


let make_print_key str =
  i64_and_space str


let make_print_associates str =
  let printer _ i64 = i64_and_space str i64 in
  (fun ht ->
    Hashtbl.iter printer ht;
    Printf.fprintf str "\n")


let flush () =
  (* establish channel *)
  let str = open_out !output_loc in
  let print_key = make_print_key str
  and print_assoc = make_print_associates str in
  (* run printer *)
  Build_assoc.print assoc print_key print_assoc;
  close_out str
