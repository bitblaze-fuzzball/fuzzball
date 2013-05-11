(** Generally useful things.

    This module contains functions that are used in VinE, but which are
    not at all Vine specific.

    @author Ivan Jager
*)

(** The identity function *)
let id = fun x -> x

(** Curry a tupled function *)
let curry f = fun x y -> f(x,y)

(** The opposite of [curry] *)
let uncurry f = fun (x,y) -> f x y

(** [foldn f i n] is f (... (f (f i n) (n-1)) ...) 0 *)
let rec foldn f i = function
    0 -> f i 0
  | n when n>0 -> foldn f (f i n) (n-1)
  | -1 -> i
  | _ -> raise (Invalid_argument "negative index number in foldn")


(** [mapn f n] is the same as [f 0; f 1; ...; f n] *)
let mapn f n =
  List.rev (foldn (fun l i -> f(n-i)::l) [] n)
  (* List.rev is needed to make side effects happen in the same order *)


(** Increment an int ref *)
let inc x = x := succ(!x)

(** Decrement an int ref *)
let dec x = x := pred(!x)



(** @return a union b, assuming a and b are sets *)
let list_union a b = 
  List.fold_left (fun acc x ->
		    if List.mem x b then acc else x::acc) b a
;;
  
(** @return a intersect b, assuming a and b are sets *)
let list_intersection a b =
  List.rev(List.fold_left (fun acc x ->
		    if List.mem x b then x::acc else acc) [] a)
;;

(** @return true if the intersection of two lists is not empty *)
let list_does_intersect a b =
  List.exists (fun x -> List.mem x a) b


(** Calls f with whichever of it's arguments is smaller first, and the longer
    one second. *)
let shortest_first f a b =
  let (la, lb) = (List.length a, List.length b) in
    if lb < la then f lb la else f la lb
    

(** @return a - b, assuming a and b are sets *)
let list_difference a b = 
    List.rev  (List.fold_left (fun acc x ->
			      if List.mem x b then
				acc
			      else
				x :: acc) [] a)
      
(** {list_subset a b} returns true when all elements in a are also in b *)
let list_subset a b =
  List.for_all (fun x -> List.mem x b) a

(** @return true when both sets contain the same elements *)
let list_set_eq a b = list_subset a b && list_subset b a


(** [union_find map items], where [map] is a mapping from items to
    their dependencies, finds independent elements in [items] *)
let union_find map items =
  let add_one res item =
    let set = map item in
    let (joined,indep) =
      List.partition (fun (s,is) -> list_does_intersect s set) res
    in
    let joined =
      List.fold_left
	(fun (s,is) (s2,is2) -> (list_union s2 s, List.rev_append is2 is))
	(set,[item]) joined
    in
      joined::indep
  in
  let res = List.fold_left add_one [] items in
    List.map snd res




(** Like [List.fold_left] but the arguments go in the same order as most fold functions.
    
    This exists, because I got fed up with fold_left being backwards.
*)
let list_foldl f l a = List.fold_left (fun i a -> f a i) a l

(** Pop the first element off a list ref. *)
let list_pop l =
  match !l with
    | x::xs -> l := xs; x
    | [] -> failwith "hd"

(** @return the last element of a list *)
let rec list_last = function
    [x] -> x
  | _::x -> list_last x
  | [] -> raise (Invalid_argument("list_last expects non-empty list"))

(** @return (lst,last) where [lst] is the input lst minus 
    the last element [last]. @raise Invalid_argument if [lst] is empty *)
let list_partition_last lst = 
  let lst = List.rev lst in 
    match lst with
      [x] -> ([],x)
      | x::ys -> (List.rev ys,x)
      | _ -> 
	 raise (Invalid_argument "list_partition_last expects non-empty list")

(** Like [list_last] but returns an option rather than failing *)
let list_last_option = function [] -> None | x -> Some(list_last x)

let list_filter_some f =
  let rec helper r l =
    match l with
	x::xs -> helper (match f x with Some s -> s::r | None -> r) xs
      | [] -> List.rev r
  in
    helper []

let rec list_find_some f =
  function
      x::xs -> (match f x with Some s -> s | None -> list_find_some f xs)
    | [] -> raise Not_found


(** [list_count f l] counts the number of items in [l] for which the
    predicate [f] is true. *)
let list_count f =
  List.fold_left (fun c x -> if f x then c+1 else c) 0

(** [list_unique l] returns a list of elements that occur in [l], without
    duplicates. (uses [=] and [Hashtbl.hash])  *)
let list_unique l =
  let h = Hashtbl.create (List.length l) in
  let () = List.iter (fun x -> Hashtbl.replace h x ()) l in
    Hashtbl.fold (fun k () ul -> k::ul) h [] 

let rec split_common_prefix la lb = 
  match la,lb with
      [], _ -> ([], la, lb)
    | _, [] -> ([], la, lb)
    | h1::t1, h2::t2 -> if h1 = h2 then
	match split_common_prefix t1 t2 with
	    (a,b,c) -> (h1::a, b, c)
      else ([], la, lb)

let split_common_suffix la lb =
  let (s,rla,rlb) = split_common_prefix (List.rev la) (List.rev lb) in
    (List.rev s, List.rev rla, List.rev rlb)

(** a composition operator. [(f <@ g) x] = [f(g x)] *)
let (<@) f g = (fun x -> f(g x))

(** Maps an ['a option] to a ['b option], given a function [f : 'a -> 'b] *)
let option_map f opt = match opt with
    None -> None
  | Some x -> Some(f x)

(** Map Some items and drop others from the list.
*)
let list_map_some f =
  let rec help res = 
    function [] -> List.rev res
      | x::xs -> help (match f x with Some i -> (i::res) | None -> res) xs
  in
    help []
  

let list_join f =
  function
      x::(_::_ as xs) -> List.fold_left f x xs
    | [x] -> x
    | [] -> raise(Invalid_argument "list_join on empty list")

module HashUtil (H:Hashtbl.S) =
struct

  let hashtbl_eq ?(eq=(=)) h1 h2 =
    let subtbl h1 h2 =
      H.fold
	(fun k v r ->
	   try r && eq v (H.find h2 k)
	   with Not_found -> false )
	h1 true
    in
      subtbl h1 h2 && subtbl h2 h1
end

(* GRR, Hashtbl doesn't ascribe to the Hashtbl.S signature *)
let hashtbl_eq ?(eq=(=)) h1 h2 =
  let subtbl h1 h2 =
    Hashtbl.fold
      (fun k v r ->
	 try r && eq v (Hashtbl.find h2 k)
	 with Not_found -> false )
      h1 true
  in
    subtbl h1 h2 && subtbl h2 h1


module StringSet = Set.Make(String) ;;

let trim_newline s = 
  if String.length s > 0 && String.get s ((String.length s) -1) = '\n'
  then	String.sub s 0 ((String.length s)-2)
  else	s


		    
  
let apply_option f k = 
  match f with
      None -> k
    | Some(f') -> f' k

let rec print_separated_list ps sep lst = 
  let rec doit acc = function
      [] -> acc^""
    | x::[] -> acc^(ps x)
    | x::y::zs -> let acc = (ps x)^sep in
	(doit acc (y::zs))
  in
    doit "" lst


(* stuff that should be in Int64 *)


(** Unsigned comparison of int64 *)
let int64_ucompare x y =
  if x < 0L && y >= 0L then 1
  else if x >= 0L && y < 0L then -1
  else Int64.compare x y

(** Unsigned int64 division *)
let int64_udiv x y =
  (* Reference: Hacker's Delight (Warren, 2002) Section 9.3 *)
  if y < 0L then
    (if (int64_ucompare x y < 0) then 0L else 1L)
  else if x < 0L
  then let all_but_last_bit =
    Int64.shift_left (Int64.div (Int64.shift_right_logical x 1) y) 1
  in
    if int64_ucompare (Int64.sub x (Int64.mul all_but_last_bit y)) y >= 0 then
      Int64.succ all_but_last_bit
    else
      all_but_last_bit
  else Int64.div x y


(** Unsigned int64 remainder *)
let int64_urem x y =
  Int64.sub x (Int64.mul y (int64_udiv x y))

(** Unsigned maxima of int64 *)
let int64_umax x y =
  if int64_ucompare x y > 0 then x else y

(** Unsigned minimum of int64 *)
let int64_umin x y =
  if int64_ucompare x y < 0 then x else y

(** Like Random.int64, but work for large unsigned bounds too *)
let int64_urandom bound =
  let rec loop () =
    let bits_low = Int64.of_int (Random.bits ()) and
	bits_med = Int64.shift_left (Int64.of_int (Random.bits ())) 30 and
	bits_high = Int64.shift_left (Int64.of_int (Random.bits () land 15)) 60
    in
    let bits = Int64.logor bits_low (Int64.logor bits_med bits_high) in
      if int64_ucompare bits bound < 0 then
	bits
      else
	loop ()
  in
    if bound >= 0L then
      Random.int64 bound
    else
      loop ()

(** Like Int64.to_float, but treat negative numbers as unsigned *)
let int64_u_to_float x =
  if x >= 0L then
    Int64.to_float x
  else
    (2.0 ** 63.0) +. (Int64.to_float (Int64.sub x 0x8000000000000000L))

(** Like Int64.of_float, but convert values greater than 2**63-1 to 
    unsigned (negative) int64s *)
let int64_u_of_float x =
  if x <= (2.0 ** 63.0) -. 1.0 then
    Int64.of_float x
  else
    Int64.add 0x8000000000000000L (Int64.of_float (x -. (2.0 ** 63.0)))

(* end stuff that should be in Int64 *)

(** execute f with fd_from remapped to fd_to.
    useful for redirecting output of external code;
    e.g., redirecting stdout when calling STP code. *)
let run_with_remapped_fd fd_from fd_to f =
  (* remap *)
  let fd_to_saved = Unix.dup fd_to in
  Unix.dup2 fd_from fd_to;

  (* execute *)
  let rv = f () in

  (* restore *)
  Unix.dup2 fd_to_saved fd_to;
  Unix.close fd_to_saved;

  rv

