type levels = [`Always | `Standard | `Debug | `Trace | `Never]
(* Semantics
   Always -- Always output information
   Standard -- Typically Display Information
   Debug -- Print when debugging
   Trace -- deep debugging
   Never -- Unless something really weird is happening, don't display *)

(* store logger level arguments in this bad boy *)
let logger_level = Hashtbl.create 10
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
    Hashtbl.find logger_level name
  with _ -> `Standard

let sufficient name my_level =
  let min_level = level_to_int (get_level name)
  and my_level_int = level_to_int my_level in
  my_level_int >= min_level


module type LoggerConfig =
sig
  val major_name  : string
  val minor_name  : string
  val use_hr_time : bool
  val out_channel : Pervasives.out_channel
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

let verb_cmdline_opts =
  [
    ("--always",
     Arg.Tuple [Arg.Set_string major;
                Arg.String (fun minor ->
                  Hashtbl.add logger_level (!major, minor) `Always)],
     "general:specific Prune logging messages beneath always threshold");
    ("--standard",
     Arg.Tuple [Arg.Set_string major;
                Arg.String (fun minor ->
                  Hashtbl.add logger_level (!major, minor) `Standard)],
     "general:specific Prune logging messages beneath standard threshold. Standard is the default value");
    ("--debug",
     Arg.Tuple [Arg.Set_string major;
                Arg.String (fun minor ->
                  Hashtbl.add logger_level (!major, minor) `Debug)],
     "general:specific Prune logging messages beneath the debug threshold.");
    ("--trace",
     Arg.Tuple [Arg.Set_string major;
                Arg.String (fun minor ->
                  Hashtbl.add logger_level (!major, minor) `Trace)],
     "general:specific Prune logging messages beneath the trace threshold.");
    ("--never",
     Arg.Tuple [Arg.Set_string major;
                Arg.String (fun minor ->
                  Hashtbl.add logger_level (!major, minor) `Never)],
     "general:specific Print everything.");
  ]

let usage_msg =
  "Example Usage\n"^
    "--trace DecisionTree BranchingDecision\n" ^
    "--always STP ReturnedConstraints\n" ^
    "\n" ^
    "Always (Major*Minor) -- Prune Logs from here beneath the always threshold\n" ^
    "Standard -- Prune Logs from here beneath the standard threshold\n" ^
    "Debug -- Prune logs from here beneath the debug threshold\n" ^
    "Trace -- Prune logs from here beneath the trace threshold\n" ^
    "Never -- Print everything.\n"


