(*
  Copyright (C) BitBlaze, 2009-2012. All rights reserved.
*)

val load_x87_emulator : Fragment_machine.fragment_machine -> string -> int64

val load_dynamic_program : Fragment_machine.fragment_machine
  -> string -> int64 -> bool -> bool ->
  (int64 * int64) list -> string list -> (int64 * int64)

val load_core : Fragment_machine.fragment_machine -> string -> int64

val setup_tls_segment : Fragment_machine.fragment_machine 
  -> int64 -> int64 -> unit

val proc_identities : (int * int * int * int) option ref

val addr_to_io : string -> int64 -> IO.input

(* Stuff below here is exposed to reduce duplication between
   Linux_loader and Decree_loader. *)

type elf_header = {
  eh_type : int;
  machine : int;
  version : int64;
  entry : int64;
  phoff : int64;
  shoff : int64;
  eh_flags : int64;
  ehsize : int;
  phentsize : int;
  phnum : int;
  shentsize : int;
  shnum : int;
  shstrndx : int
}

(* "Program header" is the standard ELF terminology, but it would be
   more natural to call this a "segment header". (However the
   abbreviation "SH" in ELF always stands for "section header", which
   is something else.) *)
type program_header = {
  ph_type : int64;
  offset : int64;
  vaddr : int64;
  paddr : int64;
  filesz : int64;
  memsz : int64;
  ph_flags : int64;
  align : int64
}

val read_program_headers : in_channel -> elf_header -> program_header list

val load_segment : Fragment_machine.fragment_machine -> in_channel ->
  program_header -> int64 -> bool -> unit

val load_partial_segment : Fragment_machine.fragment_machine -> in_channel ->
  program_header -> int64 -> int64 -> unit
