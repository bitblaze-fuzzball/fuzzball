type ctx
type revctx

val typ_to_stp : Stpvc.vc -> Vine.typ -> Stpvc.typ
val empty_ctx : unit -> ctx
val new_ctx : Stpvc.vc -> Vine.decl list -> ctx
val rev_ctx : ctx -> revctx
val vine_to_stp : Stpvc.vc -> ctx -> Vine.exp -> Stpvc.exp
val stp_to_type : Stpvc.exp -> Vine.typ
val stp_to_vine : ?strip_nums:bool -> ?ctx:revctx -> Stpvc.exp -> Vine.exp

val vc_simplify : Stpvc.vc -> ctx -> Vine.exp -> Vine.exp
