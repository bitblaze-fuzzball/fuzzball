(** Keeps track of how many weird behaviors fuzzball has encountered during the run.
    Actually fails when too many have been added. *)
module EO = Exec_options

let weird_count = ref 0

let reset () =
  weird_count := 0

let g_assert assertion penalty saw_at =
(* g for guarded *)
  if assertion = false then
    begin
      weird_count := !weird_count + penalty;
      if (!weird_count > !EO.opt_max_weirdness) ||
	(penalty > !EO.opt_single_weirdness_threshold) then
	failwith (Printf.sprintf "Too many errors\n.Last Straw: %s" saw_at)
    end
