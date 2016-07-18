(*
  Copyright (C) BitBlaze, 2009-2013. All rights reserved.
*)

val linux_initial_break : int64 option ref

val linux_setup_tcb_seg : Fragment_machine.fragment_machine
    -> int -> int64 -> int64 -> int64 -> unit

val linux_set_up_arm_kuser_page : Fragment_machine.fragment_machine -> unit

val chroot : string -> string

class linux_special_handler : Fragment_machine.fragment_machine ->
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
  method write_fake_statfs_buf : int64 -> unit
  method write_fake_statfs64buf : int64 -> unit
  method write_ftime_as_words : float -> int64 -> float -> unit

  method add_symbolic_file : string -> bool -> unit
  method make_snap : unit
  method reset : unit

  method set_proc_identities : (int * int * int * int) option -> unit

  method get_pid : int
  method get_ppid : int
  method get_pgrp : int
  method get_sid : int

  method sys_access : string -> int -> unit
  method sys_bind : int -> int64 -> int -> unit
  method sys_accept : int -> int64 -> int64 -> unit
  method sys_brk : int64 -> unit
  method sys_capget : int64 -> int64 -> unit
  method sys_chdir : string -> unit
  method sys_chmod : string -> int -> unit
  method sys_fadvise64_64 : int -> int64 -> int64 -> int -> unit
  method sys_fchdir : int -> unit
  method sys_fchmod : int -> int -> unit
  method sys_chown : string -> int -> int -> unit
  method sys_fchown32 : int -> int -> int -> unit
  method sys_clock_getres : int -> int64 -> unit
  method sys_clock_gettime : int -> int64 -> unit
  method sys_close : int -> unit
  method sys_connect : int -> int64 -> int -> unit
  method sys_dup : int -> unit
  method sys_dup2 : int -> int -> unit
  method sys_eventfd2 : int64 -> int -> unit
  method sys_exit : int64 -> unit
  method sys_exit_group : int64 -> unit
  method sys_fcntl : int -> int -> int64 -> unit
  method sys_fcntl64 : int -> int -> int64 -> unit
  method sys_futex : int64 -> int -> int64 -> int64 -> int64 -> int64 -> unit
  method sys_getcwd : int64 -> int -> unit
  method sys_getdents : int -> int64 -> int -> unit
  method sys_getdents64 : int -> int64 -> int -> unit
  method sys_getrlimit : int -> int64 -> unit
  method sys_setrlimit : int -> int64 -> unit
  method sys_getgid : unit -> unit
  method sys_getgid32 : unit -> unit
  method sys_getegid : unit -> unit
  method sys_getegid32 : unit -> unit
  method sys_getuid : unit -> unit
  method sys_getuid32 : unit -> unit
  method sys_geteuid : unit -> unit
  method sys_geteuid32 : unit -> unit
  method sys_getresgid32 : int64 -> int64 -> int64 -> unit
  method sys_getresuid32 : int64 -> int64 -> int64 -> unit
  method sys_getgroups32 : int -> int64 -> unit
  method sys_setgroups32 : int -> int64 -> unit
  method sys_getpid : unit -> unit
  method sys_getpgid : int -> unit
  method sys_getpgrp : unit -> unit
  method sys_getppid : unit -> unit
  method sys_getsid : unit -> unit
  method sys_gettid : unit
  method sys_getrusage : int -> int64 -> unit
  method sys_getpeername : int -> int64 -> int64 -> unit
  method sys_socketpair : int -> int -> int -> int64 -> unit
  method sys_getsockname : int -> int64 -> int64 -> unit
  method sys_gettimeofday : int64 -> int64 -> unit
  method sys_getxattr : string -> string -> int64 -> int -> unit
  method sys_lgetxattr : string -> string -> int64 -> int -> unit
  method sys_ioctl : int -> int64 -> int64 -> unit
  method sys_link: string -> string -> unit
  method sys_listen : int -> int -> unit
  method sys_lseek : int -> int64 -> int -> unit
  method sys__llseek : int -> int64 -> int64 -> int -> unit
  method sys_mincore : int64 -> int -> int64 -> unit
  method sys_mkdir : string -> Unix.file_perm -> unit
  method sys_mmap : int64 -> int64 -> int -> int -> int -> int64 -> unit
  method sys_mmap2 : int64 -> int64 -> int -> int -> int -> int64 -> unit
  method sys_mprotect : int64 -> int64 -> int64 -> unit
  method sys_munmap : int64 -> int64 -> unit
  method sys_open : string -> int -> Unix.file_perm -> unit
  method sys_openat : int -> string -> int -> Unix.file_perm -> unit
  method sys_pipe : int64 -> unit
  method sys_pipe2 : int64 -> int -> unit
  method sys_poll : int64 -> int -> int64 -> unit
  method sys_read : int -> int64 -> int -> unit
  method sys_readv : int -> int64 -> int -> unit
  method sys_pread64 : int -> int64 -> int -> int64 -> unit
  method sys_readlink : string -> int64 -> int -> unit
  method sys_symlink : string -> string -> unit
  method sys_recv : int -> int64 -> int -> int -> unit
  method sys_recvfrom : int -> int64 -> int -> int -> int64 -> int64 -> unit
  method sys_shutdown: int -> int -> unit
  method sys_recvmsg : int -> int64 -> int -> unit
  method sys_rename : string -> string -> unit
  method sys_sched_getparam : int -> int64 -> unit
  method sys_sched_get_priority_max : int -> unit
  method sys_sched_get_priority_min : int -> unit
  method sys_sched_getscheduler : int -> unit
  method sys_select : int -> int64 -> int64 -> int64 -> int64 -> unit
  method sys_send : int -> int64 -> int -> int -> unit
  method sys_sendto : int -> int64 -> int -> int -> int64 -> int -> unit
  method sys_sendmsg : int -> int64 -> int -> unit
  method sys_sendmmsg : int -> int64 -> int -> int -> unit
  method sys_setgid32 : int -> unit
  method sys_setuid32 : int -> unit
  method sys_setreuid : int -> int -> unit
  method sys_setresuid32 : int -> int -> int -> unit
  method sys_setresgid32: int -> int -> int -> unit
  method sys_setsockopt : int -> int -> int -> int64 -> int -> unit
  method sys_getsockopt : int -> int -> int -> int64 -> int64 -> unit
  method sys_set_robust_list : int64 -> int64 -> unit
  method sys_set_thread_area : int64 -> unit
  method sys_arch_prctl : int -> int64 -> unit
  method sys_set_tid_address : int64 -> unit
  method sys_set_tls : int64 -> unit
  method sys_rt_sigaction : int -> int64 -> int64 -> int -> unit
  method sys_sigaltstack : int64 -> int64 -> unit
  method sys_rt_sigprocmask : int -> int64 -> int64 -> int -> unit
  method sys_socket : int -> int -> int -> unit
  method sys_stat : string -> int64 -> unit
  method sys_lstat : string -> int64 -> unit
  method sys_fstat : int -> int64 -> unit
  method sys_stat64 : string -> int64 -> unit
  method sys_lstat64 : string -> int64 -> unit
  method sys_fstat64 : int -> int64 -> unit
  method sys_statfs : string -> int64 -> unit
  method sys_fstatfs : int -> int64 -> unit
  method sys_statfs64 : string -> int -> int64 -> unit
  method sys_fsync : int -> unit
  method sys_time : int64 -> unit
  method sys_alarm : int -> unit
  method sys_times : int64 -> unit
  method sys_tgkill : int -> int -> int -> unit
  method sys_umask : int -> unit
  method sys_uname : int64 -> unit
  method sys_unlink : string -> unit
  method sys_utime : string -> int64 -> unit
  method sys_utimensat : int -> int64 -> int64 -> int -> unit
  method sys_write : int -> char array -> int -> unit
  method sys_writev : int -> int64 -> int -> unit
end
