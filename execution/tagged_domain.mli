(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

open Exec_domain;;

module TaggedDomainFunctor : functor (D : DOMAIN) -> DOMAIN
