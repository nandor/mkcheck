// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

#include "syscall.h"

#include <cstdlib>
#include <iostream>
#include <stdexcept>
#include <string>

#include <fcntl.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "trace.h"
#include "util.h"



// -----------------------------------------------------------------------------
static void sys_open(Trace *trace, const Args &args)
{
  auto proc = trace->GetTrace(args.PID);
  const std::string path = ReadString(args.PID, args[0]);
  const uint64_t flags = args[1];
  const int fd = args.Return;

  if (args.Return >= 0) {
    proc->MapFd(args.Return, path);
    if ((flags & O_WRONLY) || (flags & O_RDWR) || (flags & O_CREAT)) {
      proc->AddOutput(path);
    } else {
      proc->AddInput(path);
    }
  }
}

// -----------------------------------------------------------------------------
static void sys_stat(Trace *trace, const Args &args)
{
  if (args.Return >= 0) {
    trace->GetTrace(args.PID)->AddInput(ReadString(args.PID, args[0]));
  }
}

// -----------------------------------------------------------------------------
static void sys_lstat(Trace *trace, const Args &args)
{
  if (args.Return >= 0) {
    trace->GetTrace(args.PID)->AddInput(ReadString(args.PID, args[0]));
  }
}

// -----------------------------------------------------------------------------
static void sys_dup(Trace *trace, const Args &args)
{
  auto proc = trace->GetTrace(args.PID);
  if (args.Return >= 0) {
    proc->MapFd(args.Return, proc->GetFd(args[0]));
  }
}

// -----------------------------------------------------------------------------
static void sys_dup2(Trace *trace, const Args &args)
{
  auto proc = trace->GetTrace(args.PID);
  if (args.Return >= 0) {
    proc->MapFd(args.Return, proc->GetFd(args[0]));
  }
}

// -----------------------------------------------------------------------------
static void sys_access(Trace *trace, const Args &args)
{
  if (args.Return >= 0) {
    trace->GetTrace(args.PID)->AddInput(ReadString(args.PID, args[0]));
  }
}

// -----------------------------------------------------------------------------
static void sys_chdir(Trace *trace, const Args &args)
{
  if (args.Return >= 0) {
    trace->GetTrace(args.PID)->SetCwd(ReadString(args.PID, args[0]));
  }
}

// -----------------------------------------------------------------------------
static void sys_fchdir(Trace *trace, const Args &args)
{
  auto proc = trace->GetTrace(args.PID);
  if (args.Return >= 0) {
    proc->SetCwd(proc->GetFd(args[0]));
  }
}

// -----------------------------------------------------------------------------
static void sys_rename(Trace *trace, const Args &args)
{
  if (args.Return >= 0) {
    trace->GetTrace(args.PID)->Rename(
        ReadString(args.PID, args[0]),
        ReadString(args.PID, args[1])
    );
  }
}

// -----------------------------------------------------------------------------
static void sys_mkdir(Trace *trace, const Args &args)
{
  if (args.Return >= 0) {
    trace->GetTrace(args.PID)->AddOutput(ReadString(args.PID, args[0]));
  }
}

// -----------------------------------------------------------------------------
static void sys_rmdir(Trace *trace, const Args &args)
{
  if (args.Return >= 0) {
    trace->GetTrace(args.PID)->Remove(ReadString(args.PID, args[0]));
  }
}

// -----------------------------------------------------------------------------
static void sys_unlink(Trace *trace, const Args &args)
{
  if (args.Return >= 0) {
    trace->GetTrace(args.PID)->Remove(ReadString(args.PID, args[0]));
  }
}

// -----------------------------------------------------------------------------
static void sys_symlink(Trace *trace, const Args &args)
{
  //abort();
}

// -----------------------------------------------------------------------------
static void sys_readlink(Trace *trace, const Args &args)
{
  if (args.Return >= 0) {
    trace->GetTrace(args.PID)->AddInput(ReadString(args.PID, args[0]));
  }
}

// -----------------------------------------------------------------------------
static void sys_openat(Trace *trace, const Args &args)
{
  const int fd = args[0];
  auto proc = trace->GetTrace(args.PID);
  const fs::path path = ReadString(args.PID, args[1]);

  if (args.Return >= 0) {
    if (path.is_relative()) {
      if (fd == AT_FDCWD) {
        proc->MapFd(fd, (proc->GetCwd() / path).normalize());
      } else {
        throw std::runtime_error("Not implemented: openat");
      }
    } else {
      proc->MapFd(args[0], path);
    }
  }
}

// -----------------------------------------------------------------------------
static void sys_faccessat(Trace *trace, const Args &args)
{
  auto proc = trace->GetTrace(args.PID);
  const fs::path path = ReadString(args.PID, args[1]);

  if (args.Return >= 0) {
    if (path.is_relative()) {
      throw std::runtime_error("Not implemented: faccessat");
    } else {
      proc->AddInput(path);
    }
  }
}

// -----------------------------------------------------------------------------
static void sys_ignore(Trace *trace, const Args &args)
{
}

typedef void (*HandlerFn) (Trace *trace, const Args &args);

static const HandlerFn kHandlers[] =
{
  /* 0x000 */ [SYS_read              ] = sys_ignore,
  /* 0x001 */ [SYS_write             ] = sys_ignore,
  /* 0x002 */ [SYS_open              ] = sys_open,
  /* 0x003 */ [SYS_close             ] = sys_ignore,
  /* 0x004 */ [SYS_stat              ] = sys_stat,
  /* 0x005 */ [SYS_fstat             ] = sys_ignore,
  /* 0x006 */ [SYS_lstat             ] = sys_lstat,
  /* 0x008 */ [SYS_lseek             ] = sys_ignore,
  /* 0x009 */ [SYS_mmap              ] = sys_ignore,
  /* 0x00A */ [SYS_mprotect          ] = sys_ignore,
  /* 0x00B */ [SYS_munmap            ] = sys_ignore,
  /* 0x00C */ [SYS_brk               ] = sys_ignore,
  /* 0x00D */ [SYS_rt_sigaction      ] = sys_ignore,
  /* 0x00E */ [SYS_rt_sigprocmask    ] = sys_ignore,
  /* 0x00F */ [SYS_rt_sigreturn      ] = sys_ignore,
  /* 0x010 */ [SYS_ioctl             ] = sys_ignore,
  /* 0x011 */ [SYS_pread64           ] = sys_ignore,
  /* 0x013 */ [SYS_readv             ] = sys_ignore,
  /* 0x015 */ [SYS_access            ] = sys_access,
  /* 0x016 */ [SYS_pipe              ] = sys_ignore,
  /* 0x017 */ [SYS_select            ] = sys_ignore,
  /* 0x018 */ [SYS_sched_yield       ] = sys_ignore,
  /* 0x019 */ [SYS_mremap            ] = sys_ignore,
  /* 0x01a */ [SYS_msync             ] = sys_ignore,
  /* 0x01b */ [SYS_mincore           ] = sys_ignore,
  /* 0x01c */ [SYS_madvise           ] = sys_ignore,
  /* 0x020 */ [SYS_dup               ] = sys_dup,
  /* 0x021 */ [SYS_dup2              ] = sys_dup2,
  /* 0x023 */ [SYS_nanosleep         ] = sys_ignore,
  /* 0x027 */ [SYS_getpid            ] = sys_ignore,
  /* 0x029 */ [SYS_socket            ] = sys_ignore,
  /* 0x02A */ [SYS_connect           ] = sys_ignore,
  /* 0x033 */ [SYS_getsockname       ] = sys_ignore,
  /* 0x034 */ [SYS_getpeername       ] = sys_ignore,
  /* 0x035 */ [SYS_socketpair        ] = sys_ignore,
  /* 0x036 */ [SYS_setsockopt        ] = sys_ignore,
  /* 0x037 */ [SYS_getsockopt        ] = sys_ignore,
  /* 0x038 */ [SYS_clone             ] = sys_ignore,
  /* 0x03A */ [SYS_vfork             ] = sys_ignore,
  /* 0x03B */ [SYS_execve            ] = sys_ignore,
  /* 0x03D */ [SYS_wait4             ] = sys_ignore,
  /* 0x03F */ [SYS_uname             ] = sys_ignore,
  /* 0x048 */ [SYS_fcntl             ] = sys_ignore,
  /* 0x04D */ [SYS_ftruncate         ] = sys_ignore,
  /* 0x04E */ [SYS_getdents          ] = sys_ignore,
  /* 0x04F */ [SYS_getcwd            ] = sys_ignore,
  /* 0x050 */ [SYS_chdir             ] = sys_chdir,
  /* 0x051 */ [SYS_fchdir            ] = sys_fchdir,
  /* 0x052 */ [SYS_rename            ] = sys_rename,
  /* 0x053 */ [SYS_mkdir             ] = sys_mkdir,
  /* 0x054 */ [SYS_rmdir             ] = sys_rmdir,
  /* 0x057 */ [SYS_unlink            ] = sys_unlink,
  /* 0x058 */ [SYS_symlink           ] = sys_symlink,
  /* 0x059 */ [SYS_readlink          ] = sys_readlink,
  /* 0x05A */ [SYS_chmod             ] = sys_ignore,
  /* 0x05F */ [SYS_umask             ] = sys_ignore,
  /* 0x061 */ [SYS_getrlimit         ] = sys_ignore,
  /* 0x062 */ [SYS_getrusage         ] = sys_ignore,
  /* 0x063 */ [SYS_sysinfo           ] = sys_ignore,
  /* 0x066 */ [SYS_getuid            ] = sys_ignore,
  /* 0x068 */ [SYS_getgid            ] = sys_ignore,
  /* 0x06B */ [SYS_geteuid           ] = sys_ignore,
  /* 0x06C */ [SYS_getegid           ] = sys_ignore,
  /* 0x06E */ [SYS_getppid           ] = sys_ignore,
  /* 0x083 */ [SYS_sigaltstack       ] = sys_ignore,
  /* 0x089 */ [SYS_statfs            ] = sys_ignore,
  /* 0x09D */ [SYS_prctl             ] = sys_ignore,
  /* 0x09E */ [SYS_arch_prctl        ] = sys_ignore,
  /* 0x0A0 */ [SYS_setrlimit         ] = sys_ignore,
  /* 0x0BA */ [SYS_gettid            ] = sys_ignore,
  /* 0x0C0 */ [SYS_lgetxattr         ] = sys_ignore,
  /* 0x0CA */ [SYS_futex             ] = sys_ignore,
  /* 0x0CB */ [SYS_sched_setaffinity ] = sys_ignore,
  /* 0x0CC */ [SYS_sched_getaffinity ] = sys_ignore,
  /* 0x0DA */ [SYS_set_tid_address   ] = sys_ignore,
  /* 0x0E5 */ [SYS_clock_getres      ] = sys_ignore,
  /* 0x0E7 */ [SYS_exit_group        ] = sys_ignore,
  /* 0x101 */ [SYS_openat            ] = sys_openat,
  /* 0x106 */ [SYS_newfstatat        ] = sys_ignore,
  /* 0x10D */ [SYS_faccessat         ] = sys_faccessat,
  /* 0x111 */ [SYS_set_robust_list   ] = sys_ignore,
  /* 0x118 */ [SYS_utimensat         ] = sys_ignore,
  /* 0x122 */ [SYS_eventfd2          ] = sys_ignore,
  /* 0x123 */ [SYS_epoll_create1     ] = sys_ignore,
  /* 0x125 */ [SYS_pipe2             ] = sys_ignore,
  /* 0x12E */ [SYS_prlimit64         ] = sys_ignore,
};

// -----------------------------------------------------------------------------
void Handle(Trace *trace, int64_t sno, const Args &args)
{
  if (sno < 0) {
    return;
  }

  if (sno > sizeof(kHandlers) / sizeof(kHandlers[0]) || !kHandlers[sno]) {
    throw std::runtime_error(
        "Unknown syscall " + std::to_string(sno) + " in " +
        trace->GetFileName(trace->GetTrace(args.PID)->GetImage())
    );
  }

  kHandlers[sno](trace, args);
}
