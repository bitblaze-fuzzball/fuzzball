(* Can't index a standard array on int64s, but we really need that functionality.
   This class adds it.  Provides a subset of array operations, but eventually
   we'll probably want to do the whole set.  Doesn't have the same sort of
   convenience operators that you see in standard arrays.
*)

type 'a int64Array = {
  arrays : ('a array) array;
  length : int64;
}

let max_int_as_int64 = Int64.of_int Sys.max_array_length

let indexes (index : int64) =
  (* Hide this away *)
  let which_bucket = Int64.to_int (Int64.div index max_int_as_int64)
  and which_element = Int64.to_int (Int64.rem index max_int_as_int64) in
  which_bucket, which_element


let get (array: 'a int64Array) (index : int64) =
  let which_bucket, which_element = indexes index in
  (array.arrays.(which_bucket)).(which_element)


let set (array : ('a int64Array)) (index : int64) el =
  let which_bucket, which_element = indexes index in
  (array.arrays.(which_bucket)).(which_element) <- el


let make (size : int64) initial =
  let num_buckets = (Int64.to_int (Int64.div size max_int_as_int64)) in
  let bucket_maker i =
    if i <> num_buckets
    then Array.make Sys.max_array_length initial
    else (let size = Int64.to_int (Int64.rem size max_int_as_int64) in
	  Array.make size initial) in
  let arrays = Array.init (num_buckets + 1) bucket_maker in
    { arrays = arrays;
      length = size;}


let create (size : int64) initial =
  make size initial


let iter (array : ('a int64Array)) ifun =
  let index = ref Int64.zero in
  while array.length > !index
  do
    ifun (get array !index);
    index := Int64.add !index Int64.one;
  done


let iteri (array : ('a int64Array)) iifun =
  let index = ref Int64.zero in
  while array.length > !index
  do
    iifun (get array !index) !index;
    index := Int64.add !index Int64.one;
  done


let init (size : int64) ifun =
  let iel = ifun Int64.zero in
  let ar = make size iel in
  let my_init_fun index _ =
    set ar index (ifun index) in
  iteri ar my_init_fun


let copy (array : ('a int64Array)) =
  let iel = get array Int64.zero in
  let ar = make array.length iel in
  let copy_fun index element =
    set ar index element in
  iteri array copy_fun;
  ar


let mapi (array : ('a int64Array)) mfun =
  let array_init_fun index =
    mfun (get array index) in
  init array.length array_init_fun


let length (array : ('a int64Array)) =
  array.length


let of_array (source : ('a array)) =
  let dest = create (Int64.of_int (Array.length source)) source.(0) in
  Array.iteri (fun int el -> set dest (Int64.of_int int) el) source;
  dest
