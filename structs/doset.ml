(* $Id: doset.ml,v 1.3 2005/12/10 22:37:50 ruml Exp ruml $

   ordered set with destructive modification.

   implemented using balanced binary red-black trees with parent
   pointers. See Cormen, Leiserson, Rivest (2e recommended).  Sedgewick (2e
   or 3e) doesn't discuss deletion.  Arne Andersson (1993)'s simple
   balanced trees are another option, but would probably be a tiny bit
   slower.
*)


type 'a node = {
  data : 'a;
  mutable left : 'a node;
  mutable right : 'a node;
  mutable parent : 'a node;
  (* color of link to this node *)
  mutable red : bool;
  is_nil : bool;
}


type 'a t = {
  mutable root : 'a node;
  less_than : 'a -> 'a -> bool;
  (* dummy sentinel node for use by manipulation functions.  one use is as
     the terminal leaf for all nodes. *)
  nil : 'a node;
  mutable count : int;
  equals : 'a node -> 'a node -> bool;
}


(********** utilities (used internally) **********)
let is_nil node =
  node.is_nil

let replace_data t oldn new_data =
  assert (not oldn.is_nil);
  let n = { data = new_data;
	    left = oldn.left;
	    right = oldn.right;
	    parent = oldn.parent;
	    red = oldn.red;
	    is_nil = false; (*never replace nil's data*)} in
    if t.equals oldn.parent.left oldn
    then oldn.parent.left <- n
    else oldn.parent.right <- n;
    if not (is_nil n.left) then n.left.parent <- n;
    if not (is_nil n.right) then n.right.parent <- n

let sibling t node =
  (* snags the other child of your parent *)
  if t.equals node.parent.left.data node.data
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
  t.equals node node.parent.left

let is_right_child t node =
  t.equals node node.parent.right


let rec min_below t n =
  (** minimum node at or below [n] in [t] *)
  if not (is_nil n.left) then
    min_below t n.left
  else
    n


let min t =
(*  if t.root = t.nil then raise Not_found;*)
  min_below t t.root


let next t n =
  (** node ordered immediately after [n] in [t] or t.nil if none exists. *)
    if not (is_nil n.right) then
      (* look down *)
      min_below t n.right
    else
      (* must go up *)
      let rec ascend n parent =
	if (not (is_nil parent) && (t.equals n parent.right)) then
	  ascend parent parent.parent
	else
	  (* if parent == nil, then came to root from the right, so no more
	     nodes and we should return nil (== parent).  if n !=
	     parent.right, we came to parent from left, so parent itself is
	     the next node. *)
	  parent
      in
	ascend n n.parent


let adjust_parent t a b =
  (** changes [a] to have same parent as [b] and adjusts parent of [b] to
    point to [a] instead.  useful for having [a] take [b]'s place in the
    tree.  *)
  a.parent <- b.parent;
  if is_nil b.parent
  then t.root <- a
  else if (t.equals b b.parent.left)
  then b.parent.left <- a
  else b.parent.right <- a


let adjust_parent_new t a b =
  assert (is_left_child t b || is_right_child t b || t.equals b t.root);
  a.parent <- b.parent;
  if is_nil  b.parent  then t.root <- a
  else if is_left_child t b then b.parent.left <- a
  else if is_right_child t b then b.parent.right <- a

(*************** basics ************)


let make_with ?(equals = None) predicate dummy_val =
  let rec nil = { data = dummy_val;
		  left = nil;
		  right = nil;
		  parent = nil;
		  red = false;
		  is_nil = true;} in

  let eq = (match equals with
		None -> (==)
	      | Some f ->
		  let rec to_ret a b =
		  f a.data b.data && (* this isn't enough. *)
		    a.red = b.red && (* need to make sure the nodes are identical. *)
		    (if is_nil a.left then is_nil b.left
		     else to_ret a.left b.left) &&
		    (if is_nil a.right then is_nil b.right
		     else to_ret a.right b.right) in to_ret) in
  { root = nil;
    less_than = predicate;
    nil = nil;
    equals = eq;
    count = 0; }


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
    if is_nil node then
      raise Not_found
    else if t.less_than x node.data then
      find_below node.left
    else if t.less_than node.data x then
      find_below node.right
    else
      node.data
  in
    find_below t.root


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


let print_using ch func t =
  let rec print n d =
    if is_nil n then
      ()
    else
      (print n.left (d+1);
       output_string ch (String.make (d * 3) ' ');
       func ch n.data;
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


(**************** insertion **************)


let fix_after_insert t n =
  (** restore red/black properties in [t], starting from [n].  See CLR. *)
  let z = ref n in
    while !z.parent.red do
      if t.equals !z.parent !z.parent.parent.left then
	let uncle = !z.parent.parent.right in
	  if uncle.red then
	    (!z.parent.red <- false;
	     uncle.red <- false;
	     !z.parent.parent.red <- true;
	     z := !z.parent.parent)
	  else
	    (if t.equals !z !z.parent.right then
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
	    (if t.equals !z !z.parent.left then
	       (z := !z.parent;
		rotate_right t !z);
	     !z.parent.red <- false;
	     !z.parent.parent.red <- true;
	     rotate_left t !z.parent.parent)
    done;
    t.root.red <- false


let insert_node t e =
  (** store datum [e] in [t].  return new node *)
  t.count <- t.count + 1;
  let rec find_parent parent node =
    if is_nil node then
      parent
    else if t.less_than e node.data then
      find_parent node node.left
    else
      find_parent node node.right in
  let parent = find_parent t.nil t.root in
    (* make and insert node under leaf parent *)
  let n = {data = e;
	   left = t.nil;
	   right = t.nil;
	   parent = parent;
	   red = true;
	   is_nil = false;} in
    (if is_nil parent then
       t.root <- n
     else if t.less_than e parent.data then
       parent.left <- n
     else
       parent.right <- n);
    fix_after_insert t n;
    n


let insert t e =
  ignore (insert_node t e)


(****************** deletion *******************)


let rec fix_after_delete t n =
  (** restore red/black properties in [t], starting from [n].  See CLR. *)
  if t.equals n t.root then
    (* ensure black *)
    n.red <- false
  else
    (* can assume black *)
    if t.equals n n.parent.left then
      let sib = ref n.parent.right in
	if !sib.red then
	  (* transform case 1 (sib is red) to 2, 3, or 4 *)
	  (!sib.red <- false;
	   n.parent.red <- true;
	   rotate_left t n.parent;
	   sib := n.parent.right);
	if ((not !sib.left.red) && (not !sib.right.red)) then
	  (* case 2: both black *)
	  (!sib.red <- true;
	   if n.parent.red then
	     n.parent.red <- false
	   else
	     fix_after_delete t n.parent)
	else
	  (if (not !sib.right.red) then
	     (* transform case 3 (left red) to case 4 *)
	     (!sib.left.red <- false;
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
	if !sib.red then
	  (!sib.red <- false;
	   n.parent.red <- true;
	   rotate_right t n.parent;
	   sib := n.parent.left);
	if ((not !sib.left.red) && (not !sib.right.red)) then
	  (!sib.red <- true;
	   if n.parent.red then
	     n.parent.red <- false
	   else
	     fix_after_delete t n.parent)
	else
	  (if (not !sib.left.red) then
	     (!sib.right.red <- false;
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
	if not (t.equals fated n)
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


let delete = delete_wheeler


let resort tree =
  failwith "Doset.resort not implemented!"


let clear tree =
  tree.root <- tree.nil

(*************** consistency checks ************)


let check_links t =
  let rec check n =
    if is_nil n then
      (* nil's parent and children are arbitrary *)
      ()
    else if not ((t.equals n t.root) ||
		 (t.equals n.parent.left n) ||
		 (t.equals n.parent.right n)) then
      failwith "Doset.check_links: child not a child of its parent"
    else
      (if not (is_nil n.left) then
	 if (not (t.equals n.left.parent n)) then
	   failwith "Doset.check_links: left child doesn't point to parent"
	 else
	   check n.left;
       if not (is_nil n.right) then
	 if (not (t.equals n.right.parent n)) then
	   failwith "Doset.check_links: right child doesn't point to parent"
	 else
	   check n.right)
  in
    check t.root


let check_order t =
  let rec check_children n =
    if not (is_nil n) then
      (if not (is_nil n.left) then
	 if not (t.less_than n.left.data n.data) then
	   failwith "Doset.check_order: left > node"
	 else
	   check_children n.left;
       if not (is_nil n.right) then
	 if not (t.less_than n.data n.right.data) then
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

(************** debugging benchmarks ***********)


let print_debug_using t =
  let indent d =
    Printf.printf "%s" (String.make (d * 3) ' ') in
  let rec print n d =
    if is_nil n then
      ()
    else
      (print n.left (d+1);
       indent d;
       Printf.printf "%d (p=%d)" n.data n.parent.data;
       if n.red then Printf.printf " *red";
       Printf.printf "\n";
       print n.right (d+1))
  in
    Printf.printf "nil's parent is %d\n" t.nil.parent.data;
    print t.root 0


let print_int_tree t =
  Printf.printf "Result:\n";
  (* print_debug_using t; *)
  print_using stdout (fun ch i -> Printf.fprintf ch "%i" i)  t;
  Printf.printf "-------\n"


let print_tree t fn =
  print_using stderr fn t


let add_random t m v =
  let e = Random.int m
  and n = count t in
    if v then Printf.printf "Inserting %d.\n" e;
    insert t e;
    if v then print_int_tree t;
    check t (n+1)


let delete_random t v =
  let n = count t
  and node = random t in
    if v then Printf.printf "Deleting %d.\n" node.data;
    delete t node;
    if v then print_int_tree t;
    check t (n-1)


let test1 n m v =
  let t = make_with (<=) (-1) in
    for i =1 to n do
      add_random t m v
    done;
    Printf.printf "\n\nInsertion complete, testing removes\n\n";
    for i = 1 to n do
      delete_random t v
    done


let test2 n m v =
  let t = make_with ~equals:(Some (=)) (<=) (-1) in
  for i = 1 to n do
    add_random t m v
  done;
  Printf.printf "\n\nInsertion complete, testing removes\n\n";
  for i = 1 to n do
    delete_random t v
  done


let test3 n m v = (* this test should fail *)
  let t = make_with (<) (-1) in
  for i = 1 to n do
    add_random t m v
  done;
  Printf.printf "\n\nInsertion complete, testing removes\n\n";
  for i = 1 to n do
    delete_random t v
  done



(* EOF *)
