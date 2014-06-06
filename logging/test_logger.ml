(* Test program for the logging functionality *)

(* Tools for checking out command line arguments and what nots *)
let main () =
  Arg.parse
    Logger_config.verb_cmdline_opts
    (fun anon -> Printf.printf "%s" ("Unrecognized argument: " ^ anon ^ "\n"))
    Logger_config.usage_msg;
   (* this has to happen after arguments are parsed.  Not Before *)
  let module TestStdout = Text_logger.Logger(Log_configs.StandardTestLogger) in
  TestStdout.always (fun _ ->   "Always  : Hello World");
  TestStdout.standard (fun _ -> "Standard: Hello World");
  TestStdout.debug (fun _ ->    "Debug   : Hello World");
  TestStdout.trace (fun _ ->    "Trace   : Hello World");
  TestStdout.never (fun _ ->    "Never   : Hello World")


let _ =
  main ()
