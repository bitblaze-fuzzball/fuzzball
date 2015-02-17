type lazy_type =
| LazyString of string Lazy.t
| LazyInt of int Lazy.t
| LazyInt64 of int64 Lazy.t
| LazyFloat of float Lazy.t
| LazyBool of bool Lazy.t

let evaluateLazyTypeToString lazyType =
  match lazyType with
  | LazyString s -> Lazy.force s
  | LazyInt i -> Pervasives.string_of_int (Lazy.force i)
  | LazyInt64 i -> Int64.to_string (Lazy.force i)
  | LazyFloat f -> Pervasives.string_of_float (Lazy.force f)
  | LazyBool b -> Pervasives.string_of_bool (Lazy.force b)

module type TextLog = sig
  val always   : ?sign:bool -> lazy_type -> unit
  val standard : ?sign:bool ->lazy_type -> unit
  val debug    : ?sign:bool ->lazy_type -> unit
  val trace    : ?sign:bool ->lazy_type -> unit
  val never    : ?sign:bool ->lazy_type -> unit
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

let log ?(sign = true) lazy_message =
  let chan = Verb.out_channel () in
  if sign
  then
      Printf.fprintf
	chan
        "%s %s %s: %s\n"
        (timestamp Verb.use_hr_time)
        Verb.major_name
        Verb.minor_name
        (evaluateLazyTypeToString lazy_message)
  else
      Printf.fprintf
	chan
        "%s\n"
        (evaluateLazyTypeToString lazy_message);
  flush chan
	

let dummy_log ?(sign = true) _ = ()

let always ?(sign = true) args =
  if Logger_config.sufficient (Verb.major_name,Verb.minor_name) `Always
  then log ~sign args
  else dummy_log ~sign args

let standard ?(sign = true) args =
  if Logger_config.sufficient (Verb.major_name,Verb.minor_name) `Standard
  then log ~sign args
  else dummy_log ~sign args

let debug ?(sign = true) args =
  if Logger_config.sufficient (Verb.major_name,Verb.minor_name) `Debug
  then log ~sign args
  else dummy_log ~sign args

let trace ?(sign = true) args =
  if Logger_config.sufficient (Verb.major_name,Verb.minor_name) `Trace
  then log ~sign args
  else dummy_log ~sign args

let never ?(sign = true) args =
  if Logger_config.sufficient (Verb.major_name,Verb.minor_name) `Never
  then log ~sign args
  else dummy_log ~sign args

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

let log ?(sign = true) lazy_message =
  if sign
  then
      Printf.fprintf
        (Verb.out_channel ())
        "%s %s %s: %s\n"
        (timestamp Verb.use_hr_time)
        Verb.major_name
        Verb.minor_name
        (evaluateLazyTypeToString lazy_message)
  else
      Printf.fprintf
        (Verb.out_channel ())
        "%s\n"
        (evaluateLazyTypeToString lazy_message);
  flush (Verb.out_channel ())
	    

let dummy_log ?(sign = true) args = ()

let always ?(sign = true) args =
  if Logger_config.sufficient (Verb.major_name,Verb.minor_name) `Always
  then log ~sign args
  else dummy_log ~sign args


let standard ?(sign = true) args =
  if Logger_config.sufficient (Verb.major_name,Verb.minor_name) `Standard
  then log ~sign args
  else dummy_log ~sign args

let debug ?(sign = true) args =
  if Logger_config.sufficient (Verb.major_name,Verb.minor_name) `Debug
  then log ~sign args
  else dummy_log ~sign args

let trace ?(sign = true) args =
  if Logger_config.sufficient (Verb.major_name,Verb.minor_name) `Trace
  then log ~sign args
  else dummy_log ~sign args


let never ?(sign = true) args =
  if Logger_config.sufficient (Verb.major_name,Verb.minor_name) `Never
  then log ~sign args
  else dummy_log ~sign args

end : TextLog);;

(* so, you can generate a module doing something like the following

make_logger (module StandardTestLogger : Logger_config.LoggerConfig)

*)
