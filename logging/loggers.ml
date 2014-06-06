(*  A stable of loggers *)

open Log_configs

let fuzzball_general = ref (Text_logger.make_logger
                              (module FuzzballGeneralCfg : Logger_config.LoggerConfig))

let update_fuzzball_general () =
  fuzzball_general := (Text_logger.make_logger
                         (module FuzzballGeneralCfg : Logger_config.LoggerConfig))

let update_all_loggers () =
  update_fuzzball_general ()
    
(* and then later you would bind it with
let module FBG = (!fuzzball_general : TextLog) in
   FBG.always (fun _ -> "Boo")
*)
