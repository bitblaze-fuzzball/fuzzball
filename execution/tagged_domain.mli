(*
  Copyright (C) BitBlaze, 2009-2010. All rights reserved.
*)

open Exec_domain;;

module TaggedDomainFunctor : functor (D : DOMAIN) -> DOMAIN
