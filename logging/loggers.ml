(*  A stable of loggers *)

open Log_configs

let fuzzball_general = ref (Text_logger.make_logger
                              (module FuzzballGeneralCfg : Logger_config.LoggerConfig))

let update_fuzzball_general () =
  fuzzball_general := (Text_logger.make_logger
                         (module FuzzballGeneralCfg : Logger_config.LoggerConfig))

let fuzzball_general_json = ref (Yojson_logger.make_logger
				   (module FuzzballGeneralCfg : Logger_config.LoggerConfig))

let update_fuzzball_general_json () = 
  fuzzball_general_json := (Yojson_logger.make_logger
			      (module FuzzballGeneralCfg : Logger_config.LoggerConfig))

let fuzzball_bdt_json = ref (Yojson_logger.make_logger
			       (module FuzzballBDTCfg : Logger_config.LoggerConfig))

let update_fuzzball_bdt_json () = 
  fuzzball_bdt_json := (Yojson_logger.make_logger
			      (module FuzzballBDTCfg : Logger_config.LoggerConfig))

let fuzzball_timing_json = ref (Yojson_logger.make_logger
			       (module FuzzballTimingCfg : Logger_config.LoggerConfig))

let update_fuzzball_timing_json () = 
  fuzzball_timing_json := (Yojson_logger.make_logger
			     (module FuzzballTimingCfg : Logger_config.LoggerConfig))

let cgc_restart_json = ref (Yojson_logger.make_logger
			       (module RestartLogger : Logger_config.LoggerConfig))

let update_restart_json () = 
  fuzzball_timing_json := (Yojson_logger.make_logger
			     (module RestartLogger : Logger_config.LoggerConfig))

let cgc_sexp_logger = ref (Text_logger.make_logger
			       (module SexpLogger : Logger_config.LoggerConfig))

let update_cgc_sexp () = 
  cgc_sexp_logger := (Text_logger.make_logger
			     (module SexpLogger : Logger_config.LoggerConfig))

let update_all_loggers () =
  update_fuzzball_general ();
  update_fuzzball_general_json ();
  update_fuzzball_bdt_json ();
  update_fuzzball_timing_json ();
  update_restart_json ();
  update_cgc_sexp ()
    
(* and then later you would bind it with
let module FBG = (!fuzzball_general : TextLog) in
   FBG.always (fun _ -> "Boo")
*)
