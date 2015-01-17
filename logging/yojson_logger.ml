open Yojson.Safe  (* For the json output *)
open Logger_config

type lazy_type = LazyString of string Lazy.t
		 | LazyInt of int Lazy.t
		 | LazyInt64 of int64 Lazy.t
		 | LazyFloat of float Lazy.t
		 | LazyBool of bool Lazy.t
		 | LazyJson of json Lazy.t

let evaluateLazyTypeToJson lazyType =
  match lazyType with
  | LazyString s -> `String (Lazy.force s)
  | LazyInt i -> `Int (Lazy.force i)
  | LazyInt64 i -> `Intlit (Int64.to_string (Lazy.force i))
  | LazyFloat f -> `Float (Lazy.force f)
  | LazyBool b -> `Bool (Lazy.force b)
  | LazyJson j -> Lazy.force j

module type JSONLog = sig
  val always   : lazy_type -> unit
  val standard : lazy_type -> unit
  val debug    : lazy_type -> unit
  val trace    : lazy_type -> unit
  val never    : lazy_type -> unit
end

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

let log lazy_message =
  let chan = Verb.out_channel () in
  pretty_to_channel
    chan
    (`Assoc
        [ "_type", `String "log";
          "time", timestamp Verb.use_hr_time;
          "component", `String Verb.major_name;
          "subcomponent", `String Verb.minor_name;
          "message", (evaluateLazyTypeToJson lazy_message)
	]);
  if chan = stderr || chan = stdout
  then (Printf.eprintf "Channel wasn't a file, flushing and not closing\n";
	flush chan)
  else (Printf.eprintf "Channel WAS a file, closing.\n"; close_out chan);
  flush stderr

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


let make_logger verb =
  let module Verb = (val verb : Logger_config.LoggerConfig) in
  (module struct

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
(*          "high_res", `Variant ("hr_time",
				(if use_highres
                                 then Some (`Float high_res_time)
                                 else None))*)
	  "high_res", `Float (if use_highres then high_res_time else 0.)
	]


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
	  
let log lazy_message =
  pretty_to_channel
    (Verb.out_channel ())
    (`Assoc
        [ "_type", `String "log";
          "time", timestamp Verb.use_hr_time;
          "component", `String Verb.major_name;
          "subcomponent", `String Verb.minor_name;
          "message", (evaluateLazyTypeToJson lazy_message)
	])

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


  end : JSONLog );;
