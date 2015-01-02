(*
    Modified interval tree, taken from CLRS third edition -- pg 348-354
*)

type interval = {
  low  : int64;
  high : int64;
}

exception IntersectingRange of (interval * interval)
exception ConsumedRange of (interval * interval)
exception FailedRemove of interval
exception CouldntFind of interval

let make_interval low high =
  assert (high >= low);
  assert (low >= 0);
  { low = Int64.of_int low;
    high = Int64.of_int high;}

let make_interval_wcheck low high =
  assert (high >= low);
  { low = low;
    high = high;}

let compare i1 i2 =
  let l1 = i1.low
  and l2 = i2.low in
  if l1 = l2
  then 0
  else if l1 < l2
  then ~-1
  else 1

let intersects i1 i2 =
  (i1.low >= i2.low && i1.low <= i2.high) (* left endpoint i1 in i2 *)
  || (i1.high <= i2.high && i1.high >= i2.low) (* right of i1 in i2 *)
  || (i2.low >= i1.low && i2.low <= i1.high)      (* left of l2 in l1 *)
  || (i2.high <= i1.high && i2.high >= i1.low) (* right of l2 in l1 *)

let abbuts i1 i2 =
  i2.low = i1.high ||
  i2.high = i1.low ||
  i1.low = i2.high ||
  i1.high = i2.low

let is_in i1 i2 =
    i2.low >= i1.low &&
    i2.high <= i1.high

let safe_merge i1 i2 =
  assert (intersects i1 i2);
  make_interval_wcheck 
    (Pervasives.min i1.low i2.low)
    (Pervasives.max i1.high i2.high)

let fast_merge i1 i2 =
  make_interval_wcheck
    (Pervasives.min i1.low i2.low)
    (Pervasives.max i1.high i2.high)


let merge = safe_merge


let check_interval i =
  assert (i.low <= i.high)


let remove i1 i2 = (** order matters -- remove i2 from i1 *)
  (* cases --
     a overlapping, i2 to left
     b overlapping, i2 to right
     c overlapping, i2 consumes (empty range return)
     d overlapping, i1 consumes (two ranges come back
     e no intersection *)
  let save_high_1 = i1.high <= i2.high
  and save_high_2 = i2.high <= i1.high in
  let i1_in_i2 = i1.low >= i2.low && save_high_1
  and i2_in_i1 = i2.low >= i1.low && save_high_2
  and no_intersect = i1.high < i2.low || i2.high < i1.low
  and i1_left = i1.low < i2.low && i1.high >= i2.low && save_high_1
  and i2_left = i2.low < i1.low && i2.high >= i2.low && save_high_2 in
  if no_intersect
  then [i1]
  else if i1_in_i2
  then []
  else if i2_in_i1
  then
    (if i2.high = i1.high
     then [(make_interval_wcheck i1.low (Int64.sub i2.low Int64.one))]
     else if i2.low = i1.low
     then [(make_interval_wcheck (Int64.add i2.high Int64.one) i1.high)]
     else [(make_interval_wcheck i1.low (Int64.sub i2.low Int64.one));
	   (make_interval_wcheck (Int64.add i2.high Int64.one) i1.high);])
  else if i1_left
  then [(make_interval_wcheck i1.low (Int64.sub i2.low Int64.one))]
  else if i2_left
  then [(make_interval_wcheck (Int64.add i2.high Int64.one) i1.high)]
  else failwith "I don't think this code is reachable."

(* And now for the nodes *)

type interval_node = {
  data : interval;
  mutable left : interval_node;
  mutable right : interval_node;
  mutable parent : interval_node;
  mutable red : bool;
  is_nil : bool;
}

(* and the tree definition *)

type tree = {
  mutable root : interval_node;
  nil : interval_node;
  mutable count : int;
  
}

let init_interval_node interval parent sentinel = 
  { data = interval;
    left = sentinel;
    right = sentinel;
    parent = parent;
    red = false;
    is_nil = false; }
  

(********** utilities (used internally) **********)
let equals node1 node2 =
  node1.data.low = node2.data.low &&
  node1.data.high = node2.data.high

let less_than node1 node2 =
  (compare node1.data node2.data) = -1
  

let is_nil node =
  node.is_nil


let replace_data t oldn new_data = (* for replacing a node with it's merge *)
  assert (not oldn.is_nil);
  let n = { data = new_data;
	    left = oldn.left;
	    right = oldn.right;
	    parent = oldn.parent;
	    red = oldn.red;
	    is_nil = false; (*never replace nil's data*)} in
    if equals oldn.parent.left oldn
    then oldn.parent.left <- n
    else oldn.parent.right <- n;
    if not (is_nil n.left) then n.left.parent <- n;
    if not (is_nil n.right) then n.right.parent <- n


let sibling t node =
  (* snags the other child of your parent *)
  if equals node.parent.left node
  then node.parent.right
  else node.parent.left

let get_parent node =
  node.parent

let get_right_child node =
  node.right

let get_left_child node =
  node.left

let get_nil t =
  t.nil

let is_left_child t node =
  equals node node.parent.left

let is_right_child t node =
  equals node node.parent.right

let rec min_below t n =
  (** minimum node at or below [n] in [t] *)
  if not (is_nil n.left)
  then min_below t n.left
  else n

let min t =
(*  if t.root = t.nil then raise Not_found;*)
  min_below t t.root

let next t n =
  (** node ordered immediately after [n] in [t] or t.nil if none exists. *)
    if not (is_nil n.right)       (* look down *)
    then min_below t n.right
    else
      (* must go up *)
      let rec ascend n parent =
	if (not (is_nil parent) && (equals n parent.right)) then
	  ascend parent parent.parent
	else parent
	  (* if parent == nil, then came to root from the right, so no more
	     nodes and we should return nil (== parent).  if n !=
	     parent.right, we came to parent from left, so parent itself is
	     the next node. *)
      in
      ascend n n.parent


let adjust_parent t a b =
  (** changes [a] to have same parent as [b] and adjusts parent of [b] to
    point to [a] instead.  useful for having [a] take [b]'s place in the
    tree.  *)
  a.parent <- b.parent;
  if is_nil b.parent
  then t.root <- a
  else if (equals b b.parent.left)
  then b.parent.left <- a
  else b.parent.right <- a


let adjust_parent_new t a b =
  assert (is_left_child t b || is_right_child t b || equals b t.root);
  a.parent <- b.parent;
  if is_nil  b.parent  then t.root <- a
  else if is_left_child t b then b.parent.left <- a
  else if is_right_child t b then b.parent.right <- a

(*************** basics ************)

let make_tree () =
  let neg1 = Int64.sub Int64.zero Int64.one in
  let rec nil = { data = {low = neg1; high = neg1;};
		  left = nil;
		  right = nil;
		  parent = nil;
		  red = false;
		  is_nil = true;} in
  let tree = { root = nil;
	       nil = nil;
	       count = 0; } in tree


let empty_p t =
  is_nil t.root


let count t =
  t.count


let count_nodes t =
  let rec v_count n =
    if is_nil n then
      0
    else
      1 + (v_count n.left) + (v_count n.right)
  in
    v_count t.root


let data n =
    n.data


let find t x =
  let rec find_below node =
    if is_nil node
    then raise (CouldntFind x)
    else if intersects x node.data
    then node.data
    else if x.low < node.data.low
    then find_below node.left
    else if x.low > node.data.low
    then find_below node.right
    else raise (CouldntFind x)Not_found
  in
    find_below t.root

(* iterators, maps, and folds *)


let fold_left f i t =
  let rec foo so_far n =
    if is_nil n then
      so_far
    else
      foo (f so_far n.data) (next t n)
  in
    foo i (min t)


let map f t =
  List.rev (fold_left (fun a x -> (f x)::a) [] t)


let iter f t =
  (** copy elements to array first *)
  let a = Array.init (count t) (let n = ref (min t) in
				  (fun _ ->
				     let d = (!n).data in
				       n := next t !n;
				       d)) in
    Array.iter f a


let raw_iter f t =
  (** copy elements to array first *)
  let a = Array.init (count t) (let n = ref (min t) in
				  (fun _ ->
				     let d = !n in
				       n := next t !n;
				       d)) in
    Array.iter f a



let unsafe_iter f t =
  let rec foo n =
    if is_nil n then
      ()
    else
      (* tolerate addition after n *)
      (f n.data;
       foo (next t n))
  in
    foo (min t)


let unsafe_iter2 f t =
  let rec foo n =
    if is_nil n then
      ()
    else
      (* tolerate removal of n *)
      let next = next t n in
	f n.data;
	foo next
  in
    foo (min t)

let visit_interval sats_lower_p sats_upper_p f t =
  (** satisfies_lower_bound_p = high enough. *)
  let rec v_int n =
    if not (is_nil n) then
      (if sats_lower_p n.data then
	 (v_int n.left;
	  if sats_upper_p n.data then
	    (f n;
	     v_int n.right))
       else
	 v_int n.right)
  in
    v_int t.root

(** Test functions **)
let random t =
  (** returns a random node *)
  let max = count t in
    if max = 0 then
      failwith "Doset.random: can't take random node from empty tree"
    else
      let n = ref (min t)
      and guard = Random.int max in
      for i = 0 to guard
      do let ne = next t !n in
	 if not (is_nil ne)
	 then n := ne
      done;
      !n

let print_interval ch i =
  Printf.fprintf ch "%Lx to %Lx" i.low i.high

let print_using ch t =
  let rec print n d =
    if is_nil n then
      ()
    else
      (print n.left (d+1);
       output_string ch (String.make (d * 3) ' ');
       print_interval ch n.data;
       if n.red then output_char ch '*';
       output_char ch '\n';
       flush ch;
       print n.right (d+1))
  in
    print t.root 0


(************ rotation subroutines ***********)


let rotate_left t n =
  (** moves [n]'s right child above n *)
  let rising = n.right in
    n.right <- rising.left;
    if not (is_nil rising.left) then rising.left.parent <- n;
    adjust_parent t rising n;
    rising.left <- n;
    n.parent <- rising


let rotate_right t n =
  (** moves [n]'s left child above n *)
  let rising = n.left in
    n.left <- rising.right;
    if not (is_nil rising.right) then rising.right.parent <- n;
    adjust_parent t rising n;
    rising.right <- n;
    n.parent <- rising


(****************** search *********************)
let find_interval_node tree interval =
  let rec walk_tree node =
    if is_nil node
    then None
    else
      (if (intersects interval node.data)
       then Some node
       else if ((not (is_nil node.left))
		&& node.data.low > interval.low)
       then walk_tree node.left
       else walk_tree node.right) in
  walk_tree tree.root


let find_interval tree interval =
  match find_interval_node tree interval with
  | None -> None
  | Some n -> Some n.data


(**************** insertion **************)

let fix_after_insert t n =
  (** restore red/black properties in [t], starting from [n].  See CLR. *)
  let z = ref n in
    while !z.parent.red do
      if equals !z.parent !z.parent.parent.left
      then let uncle = !z.parent.parent.right in
	   if uncle.red
	   then (!z.parent.red <- false;
		 uncle.red <- false;
		 !z.parent.parent.red <- true;
		 z := !z.parent.parent)
	   else
	     (if equals !z !z.parent.right then
		 (z := !z.parent;
		  rotate_left t !z);
	      !z.parent.red <- false;
	      !z.parent.parent.red <- true;
	      rotate_right t !z.parent.parent)
      else
	(* symmetric with above *)
	let uncle = !z.parent.parent.left in
	  if uncle.red then
	    (!z.parent.red <- false;
	     uncle.red <- false;
	     !z.parent.parent.red <- true;
	     z := !z.parent.parent)
	  else
	    (if equals !z !z.parent.left then
		(z := !z.parent;
		 rotate_right t !z);
	     !z.parent.red <- false;
	     !z.parent.parent.red <- true;
	     rotate_left t !z.parent.parent)
    done;
    t.root.red <- false


let insert_node t e =
  (** store datum [e] in [t].  return new node *)
  let rec find_parent parent node =
    if is_nil node
    then parent
    else if e.low < node.data.low
    then find_parent node node.left
    else find_parent node node.right in
  let parent = find_parent t.nil t.root in
    (* make and insert node under leaf parent *)
  let n = {data = e;
	   left = t.nil;
	   right = t.nil;
	   parent = parent;
	   red = true;
	   is_nil = false;} in
  (if is_nil parent
   then (t.count <- t.count + 1;
	 t.root <- n;
	 fix_after_insert t n;
	 n)
   else 
      (if (intersects parent.data e)
       then ((*Printf.eprintf "Interval being inserted overlaps with existing interval?\nExtant ";
	     print_interval stderr parent.data;
	     Printf.eprintf "\nNew ";
	     print_interval stderr e;
	     Printf.eprintf "\n";
	     print_using stderr t;*)
	     failwith "Overlapping interval being inserted. Violates invariant.");
       t.count <- t.count + 1;
       (if (compare e parent.data) = ~-1
	then parent.left <- n
	else parent.right <- n);
       fix_after_insert t n;
       n))
	

(****************** deletion *******************)


let rec fix_after_delete t n =
  (** restore red/black properties in [t], starting from [n].  See CLR. *)
  if equals n t.root (* ensure black *)
  then n.red <- false
  else
    (* can assume black *)
    if equals n n.parent.left
    then let sib = ref n.parent.right in
	 if !sib.red (* transform case 1 (sib is red) to 2, 3, or 4 *)
	 then (!sib.red <- false;
	       n.parent.red <- true;
	       rotate_left t n.parent;
	       sib := n.parent.right);
	 if ((not !sib.left.red) && (not !sib.right.red)) (* case 2: both black *)
	 then (!sib.red <- true;
	       if n.parent.red
	       then n.parent.red <- false
	       else fix_after_delete t n.parent)
	 else
	   (if (not !sib.right.red) (* transform case 3 (left red) to case 4 *)
	    then (!sib.left.red <- false;
		  !sib.red <- true;
		  rotate_right t !sib;
		  sib := n.parent.right);
	   (* case 4: right red: fix and return *)
	    !sib.red <- n.parent.red;
	    n.parent.red <- false;
	    !sib.right.red <- false;
	    rotate_left t n.parent)
    else
      (* symmetric, with sib the left sibling *)
      let sib = ref n.parent.left in
	if !sib.red
	then (!sib.red <- false;
	      n.parent.red <- true;
	      rotate_right t n.parent;
	      sib := n.parent.left);
	if ((not !sib.left.red) && (not !sib.right.red))
	then (!sib.red <- true;
	      if n.parent.red
	      then n.parent.red <- false
	      else fix_after_delete t n.parent)
	else
	  (if (not !sib.left.red)
	   then (!sib.right.red <- false;
		 !sib.red <- true;
		 rotate_left t !sib;
		 sib := n.parent.left);
	   !sib.red <- n.parent.red;
	   n.parent.red <- false;
	   !sib.left.red <- false;
	   rotate_right t n.parent)

let delete_wheeler t n =
  (** remove node [n] from [t] *)
  (* may not be setting tree empty when removing last node *)
  t.count <- t.count - 1;
  assert (t.count >= 0);
  (* node to get spliced out or moved around. must be missing at least
     one child. *)
  let fated =
    (if ((is_nil n.left) || (is_nil n.right))
     then n
     else min_below t n.right)
  in
    (* child of fated to save, if any *)
    let child = if not (is_nil fated.left)
      then fated.left
      else fated.right in
      (* route around fated to child *)
      adjust_parent t child fated;
      let fated_red = fated.red in
	if not (equals fated n)
	then
	  (* need to replace n with fated in tree *)
	  (fated.left <- n.left;
	   n.left.parent <- fated;
	   fated.right <- n.right;
	   n.right.parent <- fated;
	   adjust_parent t fated n;
	   fated.red <- n.red);
	if not fated_red then
	  fix_after_delete t child


let delete t n =
  delete_wheeler t n;
  match find_interval_node t n.data with
  | None -> ()
  | Some i -> ((*Printf.eprintf "Removed interval still overlaps with existing interval in tree!\n";
	       print_interval stderr n.data;
	       Printf.eprintf "\n";
	       print_interval stderr i.data;
	       Printf.eprintf "\n";*)
	       assert false)


let rec insert t e =
  match find_interval_node t e with
  | None -> ((*Printf.eprintf "No overlapping interval, inserting raw ";
	     print_interval stderr e;
	     Printf.eprintf "\n";*)
	     ignore (insert_node t e))
  | Some n ->
    (if is_in n.data e
     then raise (ConsumedRange (n.data, e))
     else
	(if abbuts e n.data (* abbutting ranges are fine, 1-2, 2-3 produces 1-3 *)
	 then (let new_range = merge n.data e in
	       ignore (delete t n);
	       insert t new_range)
	 else raise (IntersectingRange (n.data, e))))


let remove_range t e =
  let rec helper range =
      match find_interval_node t range with
      | None -> raise (FailedRemove e)
      | Some n ->
	(let new_ranges = remove n.data range in
	 ignore(delete t n);
	 List.iter (fun el -> insert t el) new_ranges;
	 helper range) in
  helper e


let resort tree =
  failwith "Doset.resort not implemented!"


let clear tree =
  tree.root <- tree.nil

(********** copy *******)
let copy_interval i =
  {low = i.low;
   high = i.high;}

let copy tree =
  let new_tree = make_tree () in
  let rec copy_tree parent node =
    if is_nil node
    then new_tree.nil
    else (let this = {data = copy_interval node.data;
			  parent = parent;
			  left  = tree.nil;
			  right = tree.nil;
			  red = node.red;
			  is_nil = node.is_nil;} in
	  this.left <- copy_tree this node.left;
	  this.right <- copy_tree this node.right;
	  this) in
  let new_root = copy_tree new_tree.nil tree.root in
  new_tree.root <- new_root;
  new_tree.count <- tree.count;
  new_tree



(*************** consistency checks ************)

let check_links t =
  let rec check n =
    if is_nil n then
      (* nil's parent and children are arbitrary *)
      ()
    else if not ((equals n t.root) ||
		 (equals n.parent.left n) ||
		 (equals n.parent.right n)) then
      failwith "Doset.check_links: child not a child of its parent"
    else
      (if not (is_nil n.left) then
	 if (not (equals n.left.parent n)) then
	   failwith "Doset.check_links: left child doesn't point to parent"
	 else
	   check n.left;
       if not (is_nil n.right) then
	 if (not (equals n.right.parent n)) then
	   failwith "Doset.check_links: right child doesn't point to parent"
	 else
	   check n.right)
  in
    check t.root


let check_order t =
  let rec check_children n =
    if not (is_nil n) then
      (if not (is_nil n.left) then
	 if not (less_than n.left n) then
	   failwith "Doset.check_order: left > node"
	 else
	   check_children n.left;
       if not (is_nil n.right) then
	 if not (less_than n n.right) then
	   failwith "Doset.check_order: node > right"
	 else
	   check_children n.right)
  in
    check_children t.root


let check_color t =
  (** root is black, no two red nodes in a row, dummy node is black *)
  if t.root.red then failwith "Doset.check_color: root is red";
  if t.nil.red then failwith "Doset.check_color: dummy node is red";
  let rec check_children n =
    if is_nil n then
      ()
    else
      (if n.red then
	 (if n.left.red then
	    failwith "Doset.check_color: red node with red left child";
	  if n.right.red then
	    failwith "Doset.check_color: red node with red right child");
       check_children n.left;
       check_children n.right)
  in
    check_children t.root


let check_height t =
  (** all leaves should have same black depth *)
  let max = ref None in
  let rec descend n depth =
    if is_nil n then
      (match !max with
	 None -> max := Some depth
       | Some d ->
	   if d != depth then
	     failwith
	       (Printf.sprintf
		  "Doset.check_height: depth %d != expected %d" depth d))
    else
      let depth = if n.red then depth else depth + 1 in
	descend n.left depth;
	descend n.right depth
  in
    descend t.root 0


let check_count t n =
  let n2 = count t in
    if (n2 <> (count_nodes t)) then
      failwith (Printf.sprintf "Doset.check_count: count not right");
    if n2 <> n then
      failwith (Printf.sprintf "Doset.check_count got %d nodes instead of %n" n2 n)


let check t n =
  check_links t;
  check_order t;
  check_color t;
  check_height t;
  check_count t n

let check_no_count t =
  check_links t;
  check_order t;
  check_color t;
  check_height t

(** interval testing code **)

let rec random_interval max_point =
  let p1 = Int64.of_int (Random.int max_point)
  and p2 = Int64.of_int (Random.int max_point) in
  if p1 > p2
  then make_interval_wcheck p2 p1
  else make_interval_wcheck p1 p2 (* if they're equal, this is also ok. *)


let check_intersects samples =
  for i = 1 to samples
  do
    let i1 = random_interval 100
    and i2 = random_interval 100 in
    let dir1 = intersects i1 i2
    and dir2 = intersects i2 i1 in
    if not (dir1 = dir2)
    then
      (Printf.printf "Intervals don't align properly:\n";
       print_interval stdout i1;
       Printf.printf "\n";
       print_interval stdout i2;
       Printf.printf "\n%b %b\n" dir1 dir2;
       assert false)
  done

let check_merge samples =
  for i = 1 to samples
  do
    let i1 = random_interval 100 in
    let range = Int64.sub i1.high i1.low in
    if range > Int64.zero
    then
      let p1 = Int64.add i1.low (Int64.of_int (Random.int (Int64.to_int range)))
      and p2 = Int64.add i1.low (Int64.of_int (Random.int (Int64.to_int range))) in
      let i2 = if p1 > p2 then { low = p2; high = p1} else { low = p1; high = p2} in
      let i3 = merge i1 i2 in
    (* the ranges being merged intersect *)
      assert (intersects i1 i2);
    (* the merged region intersects its constituents *)
    assert (intersects i1 i3);
    assert (intersects i2 i3);
    assert (intersects i3 i1);
    assert (intersects i3 i2);
  done


let check_remove samples_per_case =
  (* case 1 -- no overlap *)
  for i = 1 to samples_per_case
  do
    let i1 = random_interval 100 in
    if i1.low > Int64.one then
    let i2 = random_interval (Int64.to_int (Int64.sub i1.low Int64.one)) in
    assert (not (intersects i1 i2));
    let i3 = remove i1 i2 in
    assert ((List.length i3) = 1);
    assert ((compare i1 (List.hd i3)) = 0);
  done;
  (* case 2 -- total overlap *)
  for i = 1 to samples_per_case
  do
    let i1 = random_interval 100 in
    assert ((remove i1 i1) = [])
  done;
  (* case 3 -- some overlap, new min element *)
  for i = 1 to samples_per_case
  do
    let i1 = random_interval 100 in
    let range = Int64.div (Int64.sub i1.high i1.low) (Int64.of_int 2) in
    if range > Int64.zero && i1.low > Int64.one
    then
      (let p1 = Int64.of_int (Random.int (Int64.to_int (Int64.sub i1.low Int64.one)))
      and p2 = Int64.add i1.low (Int64.of_int (Random.int (Int64.to_int range))) in
       let i2 = {low = p1; high = p2} in
       let return = remove i1 i2 in
       assert ((List.length return) = 1);)
  (* other tests? *)
  done;
  (* case 4 -- some overlap, new max element *)
  for i = 1 to samples_per_case
  do
    let i1 = random_interval 100 in
    let range = Int64.div (Int64.sub i1.high i1.low) (Int64.of_int 2) in
    if range > Int64.zero
    then
      let p1 = Int64.sub i1.high (Int64.of_int (Random.int (Int64.to_int range)))
      and p2 = Int64.add i1.high (Int64.of_int (Random.int (Int64.to_int range))) in
      let i2 = {low = p1; high = p2} in
      let return = remove i1 i2 in
      match return with
      | [] -> (Printf.printf "r2 consumes r1.\n";
	       print_interval stdout i1;
	       Printf.printf "\n";
	       print_interval stdout i2;
	       Printf.printf "\n";
	       assert false)
      | [el] -> ()
      | [e1; e2] -> (Printf.printf "r2 split r1.\n";
		     print_interval stdout i1;
		     Printf.printf "\n";
		     print_interval stdout i2;
		     Printf.printf "\n";
		     assert false)
      | _ -> failwith "remove returned more than 3 elements."
    (* other tests? *)
  done;
  (* case 5 -- some overlap, totally subsumed *)
  for i = 1 to samples_per_case
  do
    let i1 = random_interval 100 in
    let range = Int64.div (Int64.sub i1.high i1.low) (Int64.of_int 3) in
    if range > Int64.zero
    then
      (let two = Int64.of_int 2 in
       let p1 = (Int64.add (Int64.add i1.low two) (Int64.of_int (Random.int (Int64.to_int range))))
       and p2 = (Int64.sub (Int64.sub i1.high two) (Int64.of_int (Random.int (Int64.to_int range)))) in
      let i2 = if p1 > p2 then { low = p2; high = p1} else { low = p1; high = p2} in
      let return = remove i1 i2 in
      match return with
      | [] -> (Printf.printf "r2 consumes r1.\n";
	       print_interval stdout i1;
	       Printf.printf "\n";
	       print_interval stdout i2;
	       Printf.printf "\n";
	       assert false)
      | [el] -> (Printf.printf "one remaining region?\n";
	       print_interval stdout i1;
	       Printf.printf "\n";
	       print_interval stdout i2;
	       Printf.printf "\n";
	       assert false)
      | [e1; e2] -> ()
      | _ -> failwith "remove returned more than 3 elements."
      )
  done

(** tree testing code **)




let rec remove_elt list ind accum =
  if ind = 0
  then
    (let this = List.hd list
    and rest = List.tl list in
     this, (accum @ rest))
  else
    remove_elt (List.tl list) (ind - 1) ((List.hd list)::accum)

let random_element list =
  let ind = Random.int (List.length list) in
  remove_elt list ind []

let rec check_via_removal tree = function
  | [] -> empty_p tree
  | lst ->
    (let to_remove, remainder = random_element lst in
     remove_range tree to_remove;
     check_via_removal tree remainder)
  

let test = ref (make_tree ())
and ranges = ref []

let construct_random_tree ?(max_val = 1000) size =
  ranges := [];
  test := make_tree ();
  for i = 1 to size
  do
    let p1 = Random.int max_val
    and p2 = Random.int max_val in
    let interval = if p1 > p2
      then make_interval p2 p1
      else make_interval p1 p2 in
    try
      insert !test interval;
      ranges := interval :: !ranges;
      ignore (find !test interval)
    with
    | ConsumedRange (original, attempted) -> () (* wholly consumed *)
    | IntersectingRange (original, attempted) ->
      Printf.eprintf "Desired range intersected extant range:\t";
      print_interval stderr attempted;
      Printf.eprintf " ";
      print_interval stderr original;
      Printf.eprintf "\n";
      let to_insert = remove attempted original in
      List.iter
	(fun e ->
	  Printf.eprintf "Inserting sub-range ";
	  print_interval stderr e;
	  Printf.eprintf "\n";
	  flush stderr;
	  insert !test e;
	  ranges := e::!ranges) to_insert
    (* and here is where we validate things *)
  done

let random_test ?(max_val = 1000) size =
  construct_random_tree ~max_val size;
  List.iter (fun el -> ignore (find !test el)) !ranges; (* make sure you can find every inserted element *)
  Printf.printf "everything is ok!\n";
  flush stderr;
  check_via_removal !test !ranges
