type levels = [`Always | `Standard | `Debug | `Trace | `Never]
(* Semantics
   Always -- Always output information
   Standard -- Typically Display Information
   Debug -- Print when debugging
   Trace -- deep debugging
   Never -- Unless something really weird is happening, don't display *)

(* store logger level arguments in this bad boy *)
let logger_level = Hashtbl.create 10
let logger_channels = Hashtbl.create 10
let json_logging = ref false


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

let get_logfile_channel name =
  try
    let _, filename = Hashtbl.find logger_level name in
    match filename with
    | "stdout" -> Pervasives.stdout
    | "stderr" -> Pervasives.stderr
    | _ -> try Hashtbl.find logger_channels filename
      with _ -> (
	let regex = Str.regexp ".*:[0-9]+" in
	let chan = 
	  if Str.string_match regex filename 0
	  then (
	    let tokens = Str.split (Str.regexp ":") filename in
	    assert ((List.length tokens) == 2);
	    let addr = Unix.inet_addr_of_string (List.hd tokens)
	    and port = int_of_string (List.hd (List.tl tokens)) in
	    
	    let _, outchan = Unix.open_connection (Unix.ADDR_INET(addr, port)) in
	    outchan
	  )
	  else
	    open_out filename in
	Hashtbl.add logger_channels filename chan;
	chan
      )
  with _ -> Pervasives.stdout

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
                  Hashtbl.add logger_level (!major, !minor) (`Always, logfile));],
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
    (* This is terrible.  I shouldn't be having these loggers work differently from the others. JTT 10/28 *)
    ("-pov-xml-output", Arg.String Pov_xml.set_out_channel,
     "Sets output location of pov xml file. Either a file name, stdout, or IP:PORT.");
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


