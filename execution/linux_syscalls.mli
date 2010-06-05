(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

val linux_initial_break : int64 option ref

val linux_setup_tcb_seg :
  < set_short_var : Fragment_machine.register_name -> int -> unit;
    set_word_var : Fragment_machine.register_name -> int64 -> unit;
    store_byte_conc : int64 -> int -> unit; .. >
    -> int -> int64 -> int64 -> int64 -> unit

class linux_special_handler :
  < get_word_var : Fragment_machine.register_name -> int64;
    get_word_var_concretize :
      Fragment_machine.register_name -> bool -> string -> Int64.t;
    load_byte_concretize : int64 -> bool -> string -> int;
    load_short_concretize : Int64.t -> bool -> string -> int;
    load_word_concretize : Int64.t -> bool -> string -> Int64.t;
    read_buf : Int64.t -> int -> char array;
    read_cstr : Int64.t -> string;
    set_short_var : Fragment_machine.register_name -> int -> unit;
    set_word_var : Fragment_machine.register_name -> Int64.t -> unit;
    store_byte_conc : int64 -> int -> unit;
    store_byte_idx : Int64.t -> int -> int -> unit;
    store_cstr : Int64.t -> int64 -> string -> unit;
    store_long_conc : Int64.t -> int64 -> unit;
    store_page_conc : Int64.t -> string -> unit;
    store_str : Int64.t -> int64 -> string -> unit;
    store_word_conc : int64 -> int64 -> unit;
    zero_fill : Int64.t -> int -> unit; .. >
      ->
object
  method handle_special : string -> Vine.stmt list option

  method fresh_fd : unit -> int
  method get_fd : int -> Unix.file_descr

  method errno : Unix.error -> int
  method put_errno : Unix.error -> unit

  method fresh_addr : int64 -> int64

  method string_create : int -> string

  method do_unix_read : Unix.file_descr -> int64 -> int -> int
  method do_write : int -> char array -> int -> unit

  method read_sockaddr : int64 -> int -> Unix.sockaddr
  method write_sockaddr : Unix.sockaddr -> int64 -> int64 -> unit

  method oc_kind_to_mode : Unix.file_kind -> int
  method flags_to_oc_flags : int -> Unix.open_flag list
  method write_oc_statbuf : int64 -> Unix.stats -> unit
  method write_fake_statfs64buf : int64 -> unit
  method write_ftime_as_words : float -> int64 -> float -> unit

  method set_proc_identities : (int * int * int * int) option -> unit

  method get_pid : int
  method get_ppid : int
  method get_pgrp : int
  method get_sid : int

  method sys_access : string -> int -> unit
  method sys_brk : int64 -> unit
  method sys_clock_gettime : int -> int64 -> unit
  method sys_close : int -> unit
  method sys_connect : int -> int64 -> int -> unit
  method sys_exit_group : int64 -> unit
  method sys_futex : int64 -> int -> int64 -> int64 -> int64 -> int64 -> unit
  method sys_ugetrlimit : int -> int64 -> unit
  method sys_getgid32 : unit -> unit
  method sys_getegid32 : unit -> unit
  method sys_getuid32 : unit -> unit
  method sys_geteuid32 : unit -> unit
  method sys_getpid : unit -> unit
  method sys_getpgid : int -> unit
  method sys_getpgrp : unit -> unit
  method sys_getppid : unit -> unit
  method sys_getsid : unit -> unit
  method sys_gettid : unit
  method sys_getpeername : int -> int64 -> int64 -> unit
  method sys_getsockname : int -> int64 -> int64 -> unit
  method sys_gettimeofday : int64 -> int64 -> unit
  method sys_ioctl : int -> int64 -> int64 -> unit
  method sys_lseek : int -> int64 -> int -> unit
  method sys__llseek : int -> int64 -> int64 -> int -> unit
  method sys_mkdir : string -> Unix.file_perm -> unit
  method sys_mmap2 : int64 -> int64 -> int64 -> int64 -> int -> int -> unit
  method sys_mprotect : int64 -> int64 -> int64 -> unit
  method sys_munmap : int64 -> int64 -> unit
  method sys_open : string -> int -> Unix.file_perm -> unit
  method sys_read : int -> int64 -> int -> unit
  method sys_readlink : string -> int64 -> int -> unit
  method sys_select : int -> int64 -> int64 -> int64 -> int64 -> unit
  method sys_setgid32 : int -> unit
  method sys_setuid32 : int -> unit
  method sys_set_robust_list : int64 -> int64 -> unit
  method sys_set_thread_area : int64 -> unit
  method sys_set_tid_address : int64 -> unit
  method sys_rt_sigaction : int -> int64 -> int64 -> int -> unit
  method sys_rt_sigprocmask : int -> int64 -> int64 -> int -> unit
  method sys_socket : int -> int -> int -> unit
  method sys_stat64 : string -> int64 -> unit
  method sys_fstat64 : int -> int64 -> unit
  method sys_statfs64 : string -> int -> int64 -> unit
  method sys_time : int64 -> unit
  method sys_times : int64 -> unit
  method sys_uname : int64 -> unit
  method sys_write : int -> char array -> int -> unit
  method sys_writev : int -> int64 -> int -> unit

  method handle_linux_syscall : unit -> unit
end
