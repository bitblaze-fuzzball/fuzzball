type levels = [`Always | `Standard | `Debug | `Trace | `Never]
(* Semantics
   Always -- Always output information
   Standard -- Typically Display Information
   Debug -- Print when debugging
   Trace -- deep debugging
   Never -- Unless something really weird is happening, don't display *)

(* store logger level arguments in this bad boy *)
type log_channel =
| Fixed of out_channel
    (* COUNT, MAJOR NAME, MINOR NAME *)
| Incrementing of ((int * string * string) * out_channel)

let logger_level = Hashtbl.create 10
let logger_channels = Hashtbl.create 10

let level_to_int = function
  | `Always ->    5
  | `Standard ->  4
  | `Debug ->     3
  | `Trace ->     2
  | `Never ->     1

let level_to_string = function
  | `Always ->    "Always"
  | `Standard ->  "Standard"
  | `Debug ->     "Debug"
  | `Trace ->     "Trace"
  | `Never ->     "Never"


let get_level name =
  try
    let level, _ = Hashtbl.find logger_level name in
    level
  with _ -> `Standard


let file_perms name =
    (Unix.stat name).Unix.st_perm


let rec ensure_dir dirname =
    (** ensures that the given directory exists, creating any
	intermediate directories as needed *)
  if not (Sys.file_exists dirname)
  then let sub = Filename.dirname dirname in
       ensure_dir sub;
    (* check again to avoid failure of Filename.dirname on "/foo/bar/" *)
       if not (Sys.file_exists dirname)
       then Unix.mkdir dirname (file_perms sub)

let is_closed chan =
  try
    Printf.fprintf chan " ";
    false
  with _ -> true


let next_incrementing_channel = function
  | Fixed _ -> failwith
    "Expected an incrementing channel in next_incrementing_channel"
  | Incrementing ((index, loggername, filename), old_chan) ->
    let index' = index + 1 in
    let next_chan = Printf.sprintf "%s/%s-%i.json" filename loggername index' in
    (* Make sure the channel won't clobber an extant file *)
    if Sys.file_exists next_chan then 
      failwith (Printf.sprintf "%s already exists and it shouldn't yet." next_chan);
    (* so long as the old channel isn't stdout, check to see that it was closed! *)
    if index >= 0 && old_chan != stdout && old_chan != stderr then
      if not (is_closed old_chan)
      then failwith "Expected old incrementing channel to be closed.";
    if index' < 0 then
      failwith (Printf.sprintf
		  "Not a 0-indexed file! Index was %i, expected at least 0." index);
    ensure_dir filename; (* we want the binary name here for cgc *)
    let next = Incrementing ((index', loggername, filename), (open_out next_chan)) in
    Printf.eprintf "Opened %s up as next channel!\n" next_chan;
    Hashtbl.replace logger_channels filename next;
    next


let establish_socket filename =
  try
    let tokens = Str.split (Str.regexp ":") filename in
    if ((List.length tokens) != 2)
    then failwith (Printf.sprintf
		     "%s not a properly formatted socket string. Expected IP:PORT"
		     filename);
    let host_token = List.hd tokens
    and port_token = (List.hd (List.tl tokens)) in
    let addr = 
      (try Unix.inet_addr_of_string host_token
       with _ -> (let host_ent = Unix.gethostbyname host_token in
		  host_ent.Unix.h_addr_list.(0)))
    and port = int_of_string port_token in
    let _, outchan = Unix.open_connection (Unix.ADDR_INET(addr, port)) in
    Fixed outchan
  with
  | (Unix.Unix_error (enum, s1, s2)) ->
    (failwith (Printf.sprintf
		 "Failed to open socket to %s: %s %s %s\n" filename
		 (Unix.error_message enum) s1 s2))
  | Not_found -> failwith (Printf.sprintf
			     "Couldn't find server associated with %s" filename)


let get_logfile_channel (frequency, logger_name) =
  let name = frequency, logger_name in 
  let _, filename = Hashtbl.find logger_level name in
  match filename with
  | "stdout" -> (Fixed Pervasives.stdout)
  | "stderr" -> (Fixed Pervasives.stderr)
  | _ ->
      try
	match Hashtbl.find logger_channels filename with 
	| Fixed oc as foc -> foc
	| Incrementing ((index, loggername, filename), chan) as ic ->
	  (if (index < 0) || (is_closed chan)
	   then next_incrementing_channel ic
	   else ic)
      with Not_found ->
	(let regex = Str.regexp ".*:[0-9]+" in
	 let chan = 
	   if Str.string_match regex filename 0
	   then establish_socket filename
	   else (next_incrementing_channel
		   (Incrementing ((~-1, logger_name, filename), Pervasives.stdout)))
	 in
	 Hashtbl.replace logger_channels filename chan;
	 chan)


let sufficient name my_level =
  let min_level = level_to_int (get_level name)
  and my_level_int = level_to_int my_level in
  my_level_int >= min_level


module type LoggerConfig =
sig
  val major_name  : string
  val minor_name  : string
  val use_hr_time : bool
  val out_channel : (unit -> Pervasives.out_channel)
end

(* eg
   module AlwaysDecisionTree = struct
   let level = `Always
   and major_name = "DecisionTree"
   and minor_name = "BranchDecision"
   and use_hr_time = false
   and out_channel = Pervasives.stdout
   end
*)


let major = ref ""
let minor = ref ""

let verb_cmdline_opts =
  [
    ("--always",
     Arg.Tuple [Arg.Set_string major;
		Arg.Set_string minor;
                Arg.String (fun logfile ->
                  Hashtbl.add logger_level
		    (!major, !minor)
		    (`Always, logfile));],
     "general_specific_{filename,_host:ip,_stdout,_stderr} Prune logging messages beneath always threshold");
    ("--standard",
     Arg.Tuple [Arg.Set_string major;
		Arg.Set_string minor;
                Arg.String (fun logfile ->
                  Hashtbl.add logger_level (!major, !minor) (`Standard, logfile))],
     "general_specific_{filename,_host:ip,_stdout,_stderr} Prune logging messages beneath standard threshold. Standard is the default value");
    ("--debug",
     Arg.Tuple [Arg.Set_string major;
		Arg.Set_string minor;
                Arg.String (fun logfile ->
                  Hashtbl.add logger_level (!major, !minor) (`Debug, logfile))],
     "general_specific_{filename,_host:ip,_stdout,_stderr} Prune logging messages beneath the debug threshold.");
    ("--trace",
     Arg.Tuple [Arg.Set_string major;
		Arg.Set_string minor;
                Arg.String (fun logfile ->
                  Hashtbl.add logger_level (!major, !minor) (`Trace, logfile))],
     "general_specific_{filename,_host:ip,_stdout,_stderr} Prune logging messages beneath the trace threshold.");
    ("--never",
     Arg.Tuple [Arg.Set_string major;
		Arg.Set_string minor;
                Arg.String (fun logfile ->
                  Hashtbl.add logger_level (!major, !minor) (`Never, logfile))],
     "general_specific_{filename,_host:ip,_stdout,_stderr} Print everything.");
  ]

let usage_msg =
  "Example Usage\n"^
    "--trace DecisionTree BranchingDecision\n" ^
    "--always STP ReturnedConstraints\n" ^
    "\n" ^
    "Always (Major*Minor*Logfile) -- Prune Logs from here beneath the always threshold\n" ^
    "Standard -- Prune Logs from here beneath the standard threshold\n" ^
    "Debug -- Prune logs from here beneath the debug threshold\n" ^
    "Trace -- Prune logs from here beneath the trace threshold\n" ^
    "Never -- Print everything.\n"

let close_wrapped_channel _ = function
  | Fixed chan
  | Incrementing (_, chan) ->
    try
      close_out chan
    with _ -> ()

(* HERE BE DRAGONS.  Seriously though, this is going to iterate across all
   open channels and close them out.  This is supposed to happen as the program
   is tearing down and at no other time. *)
let close_all_channels () =
  Hashtbl.iter close_wrapped_channel logger_channels
  
