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
  pretty_to_channel
    Verb.out_channel
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
  pretty_to_channel
    Verb.out_channel
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


(*
  We'll revisit this once the time to create more complex
  objects arises....


let buffer : (string, Yojson.Safe.json) Hashtbl.t = Hashtbl.create 0

let jsonBufferError logFunc key =
  logFunc (fun _ -> 
    `String (Printf.sprintf "Could not find json object %s" key)
  )

type supported_json_primitives = JBool of bool
				 | JFloat of float
				 | JInt of int
				 | JString of string
				 | JJson of json

let constructJSONPair key value =
  match value with
  | JBool b -> (key, `Bool b)
  | JFloat f -> (key, `Float f)
  | JInt i -> (key, `Int i)
  | JString s -> (key, `String s)
  | JJson j -> (key, `Assoc j)

let addValueToJSON obj key value =
  match obj with
  | `Assoc jsonlist ->
    `Assoc ((constructJSONPair key value) :: jsonlist)
  | _ ->
    failwith "Can't add to non-json-list"

let replaceValueInJSON obj key value =
  match obj with
  | `Assoc jsonlist ->
    Yojson.Safe.from_string
      (Yojson.Basic.to_string
	 (Yojson.Basic.Util.map (fun j ->
	   let k, _ = List.hd (Yojson.Basic.Util.to_assoc j) in
	   if String.compare key k == 0 then
	      `Assoc [constructJSONPair key value]
	   else 
	     j
	  ) (to_basic obj)
	 )
      )
  | _ ->
    failwith "Can't update non-json-list"

let rec updateJson obj path value =
  let key = List.hd path in
  if List.length path == 1 then    
    match (Yojson.Basic.Util.member key (to_basic obj)) with
    | `Null -> addValueToJSON obj key value
    | _ -> replaceValueInJSON obj key value
  else
    match (Yojson.Basic.Util.member key (to_basic obj)) with
    | `Null -> 
      let newObj = updateJson (`Assoc []) (List.tl path) value in
      addValueToJSON
	obj
	key
	(`JJson newObj)
    | _ -> replaceValueInJSON obj key value

let bufferJson key path value =
  let json =
    if Hashtbl.mem buffer key then 
      let j = Hashtbl.find buffer key in
      Hashtbl.remove buffer key;
      j
    else
      `Assoc [] in
  Hashtbl.add
    buffer
    key
    (updateJson
       json
       path
       value)

let dummyBuffer _ _ _ = ()

let alwaysBufferJson =
  if Logger_config.sufficient (Verb.major_name,Verb.minor_name) `Always
  then bufferJson
  else dummyBuffer

let standardBufferJson =
  if Logger_config.sufficient (Verb.major_name,Verb.minor_name) `Standard
  then bufferJson 
  else dummyBuffer

let debugBufferJson =
  if Logger_config.sufficient (Verb.major_name,Verb.minor_name) `Debug
  then bufferJson 
  else dummyBuffer

let traceBufferJson =
  if Logger_config.sufficient (Verb.major_name,Verb.minor_name) `Trace
  then bufferJson 
  else dummyBuffer

let neverBufferJson =
  if Logger_config.sufficient (Verb.major_name,Verb.minor_name) `Never
  then bufferJson
  else dummyBuffer

let flushJson logFunc key =
  if Hashtbl.mem buffer key then (
    let json = Hashtbl.find buffer key in
    Hashtbl.remove buffer key;
    logFunc (fun _ -> json)
  )
  else
    jsonBufferError logFunc key

let alwaysFlushJson =
  if Logger_config.sufficient (Verb.major_name,Verb.minor_name) `Always
  then flushJson log
  else dummy_log

let standardFlushJson =
  if Logger_config.sufficient (Verb.major_name,Verb.minor_name) `Standard
  then flushJson log
  else dummy_log

let debugFlushJson =
  if Logger_config.sufficient (Verb.major_name,Verb.minor_name) `Debug
  then flushJson log
  else dummy_log

let traceFlushJson =
  if Logger_config.sufficient (Verb.major_name,Verb.minor_name) `Trace
  then flushJson log
  else dummy_log

let neverFlushJson =
  if Logger_config.sufficient (Verb.major_name,Verb.minor_name) `Never
  then flushJson log
  else dummy_log
*)
