(*
  XML Logger scratch pad
  Implements the POV reporting XML described for CGC
 *)

#use "topfind"
#require "tyxml"

type pcre = string
type var = string

type data_typ =
| Hex
| Ascii

type echo =
| Yes
| No
| Ascii

type delim = {
  deliminator : int;
  delim_typ : data_typ; 
}

type delay = {
  d_ms : int;
}

type timeout = {
  to_ms : int;
}

type cbid = {
  cb_name : string;
}

type decl = {
  var_name : var;
  value : int; (* int64? *)
}

type slice = {
  start : int option; (* negative values from rear.  Default 0 *)
  stop : int option;  (* negative values from rear. Default len data *)
}

type data = {
  data_typ : data_typ;
  contents : int; (* maybe I want int64 here *)
}

type assign_value =
| Slice of slice
| Matching of pcre

type assign = {
  target : string;
  avalue : assign_value;
}

type match_el =
| Var of  var
| Data  of data
| Regexp of pcre

type cgc_match = {
  match_elements : match_el list;
}

type read_input =
| Length of int
| Delim of delim

type read = {
  echo : echo;
  input_marker : read_input;
  assign : assign option;
  mtch : cgc_match option;
  timeout : timeout option;
}

type write_target =
| WData of data
| WVar of var

type write = {
  datas : write_target list;
}

type action =
| Write of write
| Read of read
| Decl of decl
| Delay of delay

type replay = {
  actions : action list;
}

type pov = {
  cbid : cbid;
  replay : replay;
}

(* need to check times to make sure they're psoitive
   other structs are complete by construction *)
let valid (a : delay) = a.d_ms >= 0
let valid (a : timeout) = a.to_ms >= 0

(* XML Construction, output *)

let debug_print v =
  Xml.print_list (Printf.printf "%s") [v]

let echo_to_xml = function
  | Yes -> Xml.string_attrib "echo" "yes"
  | No ->  Xml.string_attrib "echo" "no"
  | Ascii -> Xml.string_attrib "echo" "ascii"

let data_type_to_xml = function
  | Hex -> Xml.string_attrib "format" "hex"
  | ascii  -> Xml.string_attrib "format" "ascii"

let data_to_xml (a : data) =
  Xml.node ~a:[data_type_to_xml a.data_typ] "data" [Xml.pcdata (Printf.sprintf "%i" a.contents)]

let delim_to_xml (a : delim) =
  Xml.node ~a:[data_type_to_xml a.delim_typ] 
    "delim"
    [Xml.pcdata (Printf.sprintf "%i" a.deliminator);]

let delay_to_xml (a : delay) =
  Xml.node "delay" [Xml.pcdata (Printf.sprintf "%i" a.d_ms)]

let timeout_to_xml (a : timeout) =
  Xml.node "timeout" [Xml.pcdata (Printf.sprintf "%i" a.to_ms)]

let cbid_to_xml (a : cbid) =
  Xml.node "cbid" [Xml.pcdata a.cb_name]

let decl_to_xml (a : decl) =
  Xml.node "decl"
    [Xml.node "var" [Xml.pcdata a.var_name];
     Xml.node "value" [Xml.pcdata (Printf.sprintf "%i" a.value)]]
	
let slice_to_xml (a : slice) =
  match (a.start, a.stop) with
  | Some a, Some b -> Xml.node ~a:[Xml.int_attrib "begin" a; Xml.int_attrib "end" b] "slice" []
  | Some a, None -> Xml.node ~a:[Xml.int_attrib "begin" a] "slice" []
  | None, Some b -> Xml.node ~a:[Xml.int_attrib "end" b] "slice" []
  | None, None -> failwith "Can't have a slice with neither a start nor a stop."

let pcre_to_xml (a : pcre) =
  Xml.node "pcre" [Xml.pcdata a]

let assignval_to_xml = function 
  | Slice s -> slice_to_xml s
  | Matching r -> pcre_to_xml r

let assign_to_xml (a : assign) =
  Xml.node "assign"
    [Xml.node "var" [Xml.pcdata a.target];
     assignval_to_xml a.avalue;]

let matchel_to_xml = function 
  | Var v -> Xml.node "var" [Xml.pcdata v]
  | Data d -> data_to_xml d
  | Regexp r -> (pcre_to_xml r)

let match_to_xml (a : cgc_match) =
  Xml.node "match"
    (List.map matchel_to_xml a.match_elements)

let read_input_to_xml = function
  | Length l -> Xml.node "length" [Xml.pcdata (Printf.sprintf "%i" l)]
  | Delim d -> delim_to_xml d

let read_to_xml (a : read) =
  let children = ref [] in
  children := (read_input_to_xml a.input_marker) :: !children;
  (match a.mtch with
  | Some s -> children := (match_to_xml s) :: !children 
  | None -> ());
  (match a.timeout with
  | Some s -> children := (timeout_to_xml s) :: !children 
  | None -> ());
  (match a.assign with
  | Some s -> children := (assign_to_xml s) :: !children 
  | None -> ());
  Xml.node ~a:[(echo_to_xml a.echo)] "read" !children
    
let write_target_to_xml = function
  | WData d -> data_to_xml d
  | WVar v -> Xml.node "var" [Xml.pcdata v]


let write_to_xml (a : write) =
  Xml.node "write" (List.map write_target_to_xml a.datas)
  

let action_to_xml = function
  | Write w -> write_to_xml w
  | Read r -> read_to_xml r
  | Decl d -> decl_to_xml d
  | Delay d -> delay_to_xml d

let replay_to_xml (a : replay) =
  Xml.node "replay"
    (List.map action_to_xml a.actions)

let pov_to_xml (a : pov) =
  Xml.node "pov"
    [cbid_to_xml a.cbid;
     replay_to_xml a.replay]
