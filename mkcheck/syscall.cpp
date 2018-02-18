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
static void sys_open(Process *proc, const Args &args)
{
  const fs::path path = proc->Normalise(ReadString(args.PID, args[0]));
  const uint64_t flags = args[1];
  const int fd = args.Return;

  if (args.Return >= 0) {
    proc->MapFd(args.Return, path);

    if ((flags & O_WRONLY) || (flags & O_RDWR) || (flags & O_CREAT)) {
      proc->AddOutput(path);
    } else {
      proc->AddInput(path);
    }

    if (flags & O_CLOEXEC) {
      proc->ClearCloseExec(fd);
    } else {
      proc->SetCloseExec(fd);
    }
  }
}

// -----------------------------------------------------------------------------
static void sys_close(Process *proc, const Args &args)
{
}

// -----------------------------------------------------------------------------
static void sys_stat(Process *proc, const Args &args)
{
  const fs::path path = proc->Normalise(ReadString(args.PID, args[0]));

  if (args.Return >= 0) {
    proc->AddInput(path);
  }
}

// -----------------------------------------------------------------------------
static void sys_lstat(Process *proc, const Args &args)
{
  const fs::path path = proc->Normalise(ReadString(args.PID, args[0]));

  if (args.Return >= 0) {
    proc->AddInput(path);
  }
}

// -----------------------------------------------------------------------------
static void sys_dup(Process *proc, const Args &args)
{
  if (args.Return >= 0) {
    proc->DupFd(args[0], args.Return);
  }
}

// -----------------------------------------------------------------------------
static void sys_dup2(Process *proc, const Args &args)
{
  if (args.Return >= 0) {
    proc->DupFd(args[0], args.Return);
  }
}

// -----------------------------------------------------------------------------
static void sys_access(Process *proc, const Args &args)
{
  const fs::path path = proc->Normalise(ReadString(args.PID, args[0]));

  if (args.Return >= 0) {
    proc->AddInput(path);
  }
}

// -----------------------------------------------------------------------------
static void sys_pipe(Process *proc, const Args &args)
{
  int fds[2];
  ReadBuffer(args.PID, fds, args[0], 2 * sizeof(int));

  if (args.Return >= 0) {
    const fs::path path = "/proc/" + std::to_string(args.PID) + "/fd/";
    proc->MapFd(fds[0], path / std::to_string(fds[0]));
    proc->MapFd(fds[1], path / std::to_string(fds[1]));
  }
}

// -----------------------------------------------------------------------------
static void sys_fcntl(Process *proc, const Args &args)
{
  const int fd = args[0];
  const int cmd = args[1];

  if (args.Return >= 0) {
    switch (cmd) {
      case F_DUPFD: {
        proc->DupFd(args[0], args.Return);
        break;
      }
      case F_DUPFD_CLOEXEC: {
        proc->DupFd(args[0], args.Return);
        proc->SetCloseExec(args.Return);
        break;
      }
      case F_SETFD: {
        const int arg = args[2];
        if (arg & O_CLOEXEC) {
          proc->ClearCloseExec(fd);
        } else {
          proc->SetCloseExec(fd);
        }
        break;
      }
      case F_GETFD:
      case F_GETFL:
      case F_SETFL: {
        break;
      }
      default: {
        throw std::runtime_error(
            "Unknown fnctl (cmd = " + std::to_string(cmd) + ")"
        );
      }
    }
  }
}

// -----------------------------------------------------------------------------
static void sys_chdir(Process *proc, const Args &args)
{
  const fs::path path = proc->Normalise(ReadString(args.PID, args[0]));

  if (args.Return >= 0) {
    proc->SetCwd(path);
  }
}

// -----------------------------------------------------------------------------
static void sys_fchdir(Process *proc, const Args &args)
{
  const int fd = args[0];
  if (args.Return >= 0) {
    proc->SetCwd(proc->GetFd(fd));
  }
}

// -----------------------------------------------------------------------------
static void sys_rename(Process *proc, const Args &args)
{
  const fs::path src = proc->Normalise(ReadString(args.PID, args[0]));
  const fs::path dst = proc->Normalise(ReadString(args.PID, args[1]));

  if (args.Return >= 0) {
    proc->Rename(src, dst);
  }
}

// -----------------------------------------------------------------------------
static void sys_mkdir(Process *proc, const Args &args)
{
  const fs::path path = proc->Normalise(ReadString(args.PID, args[0]));

  if (args.Return >= 0) {
    proc->AddOutput(path);
  }
}

// -----------------------------------------------------------------------------
static void sys_rmdir(Process *proc, const Args &args)
{
  const fs::path path = proc->Normalise(ReadString(args.PID, args[0]));

  if (args.Return >= 0) {
    proc->Remove(path);
  }
}

// -----------------------------------------------------------------------------
static void sys_unlink(Process *proc, const Args &args)
{
  const fs::path path = proc->Normalise(ReadString(args.PID, args[0]));

  if (args.Return >= 0) {
    proc->Remove(path);
  }
}

// -----------------------------------------------------------------------------
static void sys_symlink(Process *proc, const Args &args)
{
  const fs::path src = proc->Normalise(ReadString(args.PID, args[0]));
  const fs::path dst = proc->Normalise(ReadString(args.PID, args[1]));

  if (args.Return >= 0) {
    proc->Symlink(src, dst);
  }
}

// -----------------------------------------------------------------------------
static void sys_readlink(Process *proc, const Args &args)
{
  const fs::path path = proc->Normalise(ReadString(args.PID, args[0]));
  if (args.Return >= 0) {
    proc->AddInput(path);
  }
}

// -----------------------------------------------------------------------------
static void sys_unlinkat(Process *proc, const Args &args)
{
  const int fd = args[0];
  const fs::path path = proc->Normalise(fd, ReadString(args.PID, args[1]));

  if (args.Return >= 0) {
    proc->Remove(path);
  }
}

// -----------------------------------------------------------------------------
static void sys_openat(Process *proc, const Args &args)
{
  const int dirfd = args[0];
  const fs::path path = proc->Normalise(dirfd, ReadString(args.PID, args[1]));
  const uint64_t flags = args[2];

  if (args.Return >= 0) {
    const int fd = args.Return;

    proc->MapFd(fd, path);

    if ((flags & O_WRONLY) || (flags & O_RDWR) || (flags & O_CREAT)) {
      proc->AddOutput(path);
    } else {
      proc->AddInput(path);
    }

    if (flags & O_CLOEXEC) {
      proc->ClearCloseExec(fd);
    } else {
      proc->SetCloseExec(fd);
    }
  }
}

// -----------------------------------------------------------------------------
static void sys_faccessat(Process *proc, const Args &args)
{
  const int fd = args[0];
  const fs::path path = proc->Normalise(fd, ReadString(args.PID, args[1]));

  if (args.Return >= 0) {
    proc->AddInput(path);
  }
}

// -----------------------------------------------------------------------------
static void sys_pipe2(Process *proc, const Args &args)
{
  // TODO: implment this
  abort();
}

// -----------------------------------------------------------------------------
static void sys_ignore(Process *proc, const Args &args)
{
}

typedef void (*HandlerFn) (Process *proc, const Args &args);

static const HandlerFn kHandlers[] =
{
  /* 0x000 */ [SYS_read              ] = sys_ignore,
  /* 0x001 */ [SYS_write             ] = sys_ignore,
  /* 0x002 */ [SYS_open              ] = sys_open,
  /* 0x003 */ [SYS_close             ] = sys_close,
  /* 0x004 */ [SYS_stat              ] = sys_stat,
  /* 0x005 */ [SYS_fstat             ] = sys_ignore,
  /* 0x006 */ [SYS_lstat             ] = sys_lstat,
  /* 0x007 */ [SYS_poll              ] = sys_ignore,
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
  /* 0x016 */ [SYS_pipe              ] = sys_pipe,
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
  /* 0x048 */ [SYS_fcntl             ] = sys_fcntl,
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
  /* 0x05C */ [SYS_chown             ] = sys_ignore,
  /* 0x05F */ [SYS_umask             ] = sys_ignore,
  /* 0x060 */ [SYS_gettimeofday      ] = sys_ignore,
  /* 0x061 */ [SYS_getrlimit         ] = sys_ignore,
  /* 0x062 */ [SYS_getrusage         ] = sys_ignore,
  /* 0x063 */ [SYS_sysinfo           ] = sys_ignore,
  /* 0x066 */ [SYS_getuid            ] = sys_ignore,
  /* 0x068 */ [SYS_getgid            ] = sys_ignore,
  /* 0x06B */ [SYS_geteuid           ] = sys_ignore,
  /* 0x06C */ [SYS_getegid           ] = sys_ignore,
  /* 0x06E */ [SYS_getppid           ] = sys_ignore,
  /* 0x06F */ [SYS_getpgrp           ] = sys_ignore,
  /* 0x073 */ [SYS_getgroups         ] = sys_ignore,
  /* 0x083 */ [SYS_sigaltstack       ] = sys_ignore,
  /* 0x089 */ [SYS_statfs            ] = sys_ignore,
  /* 0x08A */ [SYS_fstatfs           ] = sys_ignore,
  /* 0x09D */ [SYS_prctl             ] = sys_ignore,
  /* 0x09E */ [SYS_arch_prctl        ] = sys_ignore,
  /* 0x0A0 */ [SYS_setrlimit         ] = sys_ignore,
  /* 0x0BA */ [SYS_gettid            ] = sys_ignore,
  /* 0x0C0 */ [SYS_lgetxattr         ] = sys_ignore,
  /* 0x0CA */ [SYS_futex             ] = sys_ignore,
  /* 0x0CB */ [SYS_sched_setaffinity ] = sys_ignore,
  /* 0x0CC */ [SYS_sched_getaffinity ] = sys_ignore,
  /* 0x0DA */ [SYS_set_tid_address   ] = sys_ignore,
  /* 0x0DD */ [SYS_fadvise64         ] = sys_ignore,
  /* 0x0E5 */ [SYS_clock_getres      ] = sys_ignore,
  /* 0x0E7 */ [SYS_exit_group        ] = sys_ignore,
  /* 0x101 */ [SYS_openat            ] = sys_openat,
  /* 0x106 */ [SYS_newfstatat        ] = sys_ignore,
  /* 0x107 */ [SYS_unlinkat          ] = sys_unlinkat,
  /* 0x10D */ [SYS_faccessat         ] = sys_faccessat,
  /* 0x111 */ [SYS_set_robust_list   ] = sys_ignore,
  /* 0x118 */ [SYS_utimensat         ] = sys_ignore,
  /* 0x122 */ [SYS_eventfd2          ] = sys_ignore,
  /* 0x123 */ [SYS_epoll_create1     ] = sys_ignore,
  /* 0x125 */ [SYS_pipe2             ] = sys_pipe2,
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

  auto *proc = trace->GetTrace(args.PID);

  try {
    kHandlers[sno](proc, args);
  } catch (std::exception &ex) {
    throw std::runtime_error(
        "Exception while handling syscall " + std::to_string(sno) +
        " in process " + std::to_string(proc->GetUID()) + ": " +
        ex.what()
    );
  }
}
