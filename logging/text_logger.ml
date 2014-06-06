module type TextLog = sig
  val always   : (unit -> string) -> unit
  val standard : (unit -> string) -> unit
  val debug    : (unit -> string) -> unit
  val trace    : (unit -> string) -> unit
  val never    : (unit -> string) -> unit
end

(* an in place variant if you don't need the complexity of first-class modules (we do) *)
module Logger (Verb : Logger_config.LoggerConfig) =
struct
let timestamp use_highres =
    if use_highres
    then Printf.sprintf "%f" (Unix.gettimeofday())
    else let time = Unix.localtime (Unix.time ()) in
         Printf.sprintf "%d-%02d-%02d:%02d:%02d:%02d"
           (time.Unix.tm_year + 1900)
           (time.Unix.tm_mon + 1)
           (time.Unix.tm_mday)
           (time.Unix.tm_hour)
           (time.Unix.tm_min)
           (time.Unix.tm_sec)

let log message_thunk =
      Printf.fprintf
        Verb.out_channel
        "%s %s %s: %s\n"
        (timestamp Verb.use_hr_time)
        Verb.major_name
        Verb.minor_name
        (message_thunk ())

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


let make_logger verb =
  let module Verb = (val verb : Logger_config.LoggerConfig) in
  (module struct
    let timestamp use_highres =
      if use_highres
    then Printf.sprintf "%f" (Unix.gettimeofday())
    else let time = Unix.localtime (Unix.time ()) in
         Printf.sprintf "%d-%02d-%02d:%02d:%02d:%02d"
           (time.Unix.tm_year + 1900)
           (time.Unix.tm_mon + 1)
           (time.Unix.tm_mday)
           (time.Unix.tm_hour)
           (time.Unix.tm_min)
           (time.Unix.tm_sec)

let log message_thunk =
      Printf.fprintf
        Pervasives.stdout
        "%s %s %s: %s\n"
        (timestamp Verb.use_hr_time)
        Verb.major_name
        Verb.minor_name
        (message_thunk ())

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

end : TextLog);;

(* so, you can generate a module doing something like the following

make_logger (module StandardTestLogger : Logger_config.LoggerConfig)

*)
