(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

exception BranchLimit
exception DeepPath
exception DisqualifiedPath
exception DivideByZero
exception IllegalInstruction
exception JumpToNull
exception KnownPath
exception LastIteration
exception NotConcrete of Vine.exp
exception NullDereference
exception ReachedInfluenceBound
exception ReachedMeasurePoint
exception Signal of string
exception Simplify_failure of string
exception SimulatedExit of int64
exception SolverFailure
exception SymbolicJump
exception SymbolicSyscall
exception TooManyIterations
exception UnhandledSysCall of string
exception UnhandledTrap
