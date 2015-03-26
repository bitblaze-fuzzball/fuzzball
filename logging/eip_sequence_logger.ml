(* logs sequence of EIPs encountered by the program for use in coverage computation *)

let output_loc = ref "./eip_sequence.txt"
let chan_opt = ref None

let add _ this_addr = 
  let c =
    match !chan_opt with
    | None -> (let c = open_out !output_loc in
	       chan_opt := Some c;
	       c)
    | Some c -> c in
  Printf.fprintf c " %#08Lx" this_addr (* 0x00000000 *)


let flush () =
  match !chan_opt with
  | None -> failwith "EIP sequence logger: Flushing no channel"
  | Some c ->
    begin
      Printf.fprintf c "\n";
      flush c
    end


let close () = 
  match !chan_opt with
  | None -> ()
  | Some c ->
    begin
      close_out c;
      chan_opt := None
    end
