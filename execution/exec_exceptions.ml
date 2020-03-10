(*
  Copyright (C) BitBlaze, 2009-2012, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

type null_info = {
  eip_of_deref : int64;
  last_set_to_null : int64;
  addr_derefed : int64;
}

exception BranchLimit
exception DeepPath
exception DisqualifiedPath
exception DivideByZero
exception FinishNow
exception IllegalInstruction
exception JumpToNull
exception KnownPath
exception LastIteration
exception NotConcrete of Vine.exp
exception NullDereference of null_info
exception ReachedInfluenceBound
exception ReachedMeasurePoint
exception SentErrorMessage of string
exception Signal of string
exception Simplify_failure of string
exception SimulatedAbort
exception SimulatedExit of int64
exception SimulatedSegfault of int64 * bool
exception SolverFailure
exception StartSymbolic of int64 * (unit -> unit)
exception SymbolicJump
exception SymbolicSyscall
exception TooManyIterations
exception UnhandledSysCall of string
exception UnhandledTrap
exception UnproductivePath
exception Double_Free
exception Dealloc_Not_Alloc
exception Alloc_Dealloc_Length_Mismatch
exception Unsafe_Memory_Access
exception Uninitialized_Memory
exception WeirdSymbolicAddress
