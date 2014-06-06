open Yojson.Safe  (* For the json output *)
open Logger_config


module JSONLogger (Verb : LoggerConfig) =
struct
let timestamp use_highres =
  (* Returns a timestamp object in JSON *)
  let time = Unix.localtime (Unix.time ()) (* cluster is in one place, safe? *)
  and high_res_time = Unix.gettimeofday() in
  `Assoc
       [ "_type", `String "timestamp";
         "gross", `String (Printf.sprintf "%d-%02d-%02d:%02d:%02d:%02d"
                             (time.Unix.tm_year + 1900)
                             (time.Unix.tm_mon + 1)
                             (time.Unix.tm_mday)
                             (time.Unix.tm_hour)
                             (time.Unix.tm_min)
                             (time.Unix.tm_sec));
         "high_res", `Variant ("hr_time",
                               (if use_highres
                                then Some (`Float high_res_time)
                                else None))]


let process_identifier () =
  (* return some information that uniquely identifies the process
     Pete had some suggestions regarding a master-slave architechture, *)
  `Assoc
    [ "_type", `String "process_id";
      "hostname", `String (Unix.gethostname ());
      "pid", `Int (Unix.getpid());
      "name", `String Verb.major_name]


let json_command_arg = function
  (* Returns part of the json assoc representing a command line
     argument.  I think, unfortunately, we may just have to hardcode the
     settings output as there's no good way to get a mapping from flags
     to argument names (other than writing it down) *)
  | (flag, _, description) ->
    ["flag", `String flag;
     "description", `String description]


let fuzzball_config () =
  (* return the current configuration of this process as a json object *)
  failwith "stub"


let log message_thunk =
  pretty_to_channel
    Verb.out_channel
    (`Assoc
        [ "_type", `String "log";
          "time", timestamp Verb.use_hr_time;
          "component", `String Verb.major_name;
          "subcomponent", `String Verb.minor_name;
          "message", message_thunk ()])

let dummy_log _ = ()

let always =
  if Logger_config.sufficient (Verb.major_name,Verb.minor_name) `Always
  then log
  else dummy_log


let standard =
  if Logger_config.sufficient (Verb.major_name,Verb.minor_name) `Standard
  then log
  else dummy_log

let debug =
  if Logger_config.sufficient (Verb.major_name,Verb.minor_name) `Debug
  then log
  else dummy_log

let trace =
  if Logger_config.sufficient (Verb.major_name,Verb.minor_name) `Trace
  then log
  else dummy_log


let never =
  if Logger_config.sufficient (Verb.major_name,Verb.minor_name) `Never
  then log
  else dummy_log

end

(* eg
   module DTLog = JSONLogger(AlwaysDT)
*)

