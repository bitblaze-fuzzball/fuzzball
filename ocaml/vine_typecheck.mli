(**
   Type inference and typechecker for the VinE IR.
*)

(** The type used for the typechecking context. *)
type ctx

(** The type for optional contexts *)
type gamma = ctx option

(** [tint t] true iff [t] is an integer type *)
val tint : Vine.typ -> bool

(** [tcompat t1 t2] true if t1 compatible with t2, e.g., only differ
  on attributes. *)
val tcompat : Vine.typ -> Vine.typ -> bool

(** create a new persistent typing context *)
val gamma_create : unit -> gamma

(** Extend the typing context and returns a new
    persistent context. *)
val gamma_extend :
  gamma -> Vine.var -> Vine.typ -> gamma

(** [gamma_find gamma v] returns Some(t) where [t] is the type of [v]
    if [v] is in the context, else None *)
val gamma_find : gamma -> Vine.var -> Vine.typ option

(** [gamma_check gamma v t] returns t if v is of type [t] in gamma,
    error if [v] is not of type [t], AND returns [t] if [v] is not in
    gamma. Note this last part is what makes it not just a boolean
    test.  *)
val gamma_check : gamma -> Vine.var -> Vine.typ -> Vine.typ

(** typecheck a value. no context needed. *)
val typecheck_value : Vine.value -> Vine.typ

(** typecheck a program. *)
val typecheck : Vine.program -> unit

(** [infer_type gamma e] infers the type of [e] under [gamma]. [gamma]
    can be None *)
val infer_type : gamma -> Vine.exp -> Vine.typ

(** [infer_type_fast e] produces the same results as
    [infer_type None e] when e is type-correct, but much faster.
    In particular, its runtime depends on the average depth of a path in
    the expression, as opposed to the total number of nodes in the
    expression. However, it won't catch most type errors, and if the
    expression is not type correct the results may even be
    non-deterministic. *)
val infer_type_fast : Vine.exp -> Vine.typ
