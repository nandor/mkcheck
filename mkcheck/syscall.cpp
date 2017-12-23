// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

#include <cstdlib>
#include <stdexcept>
#include <string>

#include <sys/syscall.h>

#include "syscall.h"
#include "trace.h"


// -----------------------------------------------------------------------------
static void sys_read(Trace *trace, const Args &args)
{
}

// -----------------------------------------------------------------------------
static void sys_write(Trace *trace, const Args &args)
{
}

// -----------------------------------------------------------------------------
static void sys_open(Trace *trace, const Args &args)
{
}

// -----------------------------------------------------------------------------
static void sys_close(Trace *trace, const Args &args)
{
}

// -----------------------------------------------------------------------------
static void sys_stat(Trace *trace, const Args &args)
{
}

// -----------------------------------------------------------------------------
static void sys_fstat(Trace *trace, const Args &args)
{
}

// -----------------------------------------------------------------------------
static void sys_lstat(Trace *trace, const Args &args)
{
}

// -----------------------------------------------------------------------------
static void sys_lseek(Trace *trace, const Args &args)
{
}

// -----------------------------------------------------------------------------
static void sys_ioctl(Trace *trace, const Args &args)
{
}

// -----------------------------------------------------------------------------
static void sys_pread64(Trace *trace, const Args &args)
{
}

// -----------------------------------------------------------------------------
static void sys_readv(Trace *trace, const Args &args)
{
}

// -----------------------------------------------------------------------------
static void sys_access(Trace *trace, const Args &args)
{
}

// -----------------------------------------------------------------------------
static void sys_pipe(Trace *trace, const Args &args)
{
}

// -----------------------------------------------------------------------------
static void sys_clone(Trace *trace, const Args &args)
{
}

// -----------------------------------------------------------------------------
static void sys_execve(Trace *trace, const Args &args)
{
}

// -----------------------------------------------------------------------------
static void sys_fcntl(Trace *trace, const Args &args)
{
}

// -----------------------------------------------------------------------------
static void sys_getdents(Trace *trace, const Args &args)
{
}

// -----------------------------------------------------------------------------
static void sys_getcwd(Trace *trace, const Args &args)
{
}

// -----------------------------------------------------------------------------
static void sys_chdir(Trace *trace, const Args &args)
{
}

// -----------------------------------------------------------------------------
static void sys_rename(Trace *trace, const Args &args)
{
}

// -----------------------------------------------------------------------------
static void sys_readlink(Trace *trace, const Args &args)
{
}

// -----------------------------------------------------------------------------
static void sys_chmod(Trace *trace, const Args &args)
{
}

// -----------------------------------------------------------------------------
static void sys_umask(Trace *trace, const Args &args)
{
}

// -----------------------------------------------------------------------------
static void sys_pipe2(Trace *trace, const Args &args)
{
}

// -----------------------------------------------------------------------------
static void sys_ignore(Trace *trace, const Args &args)
{
}


// -----------------------------------------------------------------------------
typedef void (*HandlerFn) (Trace *trace, const Args &args);

static const HandlerFn kHandlers[] =
{
  [SYS_read           ] = sys_read,
  [SYS_write          ] = sys_write,
  [SYS_open           ] = sys_open,
  [SYS_close          ] = sys_close,
  [SYS_stat           ] = sys_stat,
  [SYS_fstat          ] = sys_fstat,
  [SYS_lstat          ] = sys_lstat,
  [SYS_lseek          ] = sys_lseek,
  [SYS_mmap           ] = sys_ignore,
  [SYS_mprotect       ] = sys_ignore,
  [SYS_munmap         ] = sys_ignore,
  [SYS_brk            ] = sys_ignore,
  [SYS_rt_sigaction   ] = sys_ignore,
  [SYS_rt_sigprocmask ] = sys_ignore,
  [SYS_rt_sigreturn   ] = sys_ignore,
  [SYS_ioctl          ] = sys_ioctl,
  [SYS_pread64        ] = sys_pread64,
  [SYS_readv          ] = sys_readv,
  [SYS_access         ] = sys_access,
  [SYS_pipe           ] = sys_pipe,
  [SYS_clone          ] = sys_clone,
  [SYS_execve         ] = sys_execve,
  [SYS_wait4          ] = sys_ignore,
  [SYS_fcntl          ] = sys_fcntl,
  [SYS_getdents       ] = sys_getdents,
  [SYS_getcwd         ] = sys_getcwd,
  [SYS_chdir          ] = sys_chdir,
  [SYS_rename         ] = sys_rename,
  [SYS_readlink       ] = sys_readlink,
  [SYS_chmod          ] = sys_chmod,
  [SYS_umask          ] = sys_umask,
  [SYS_getrlimit      ] = sys_ignore,
  [SYS_getrusage      ] = sys_ignore,
  [SYS_sigaltstack    ] = sys_ignore,
  [SYS_arch_prctl     ] = sys_ignore,
  [SYS_setrlimit      ] = sys_ignore,
  [SYS_futex          ] = sys_ignore,
  [SYS_set_tid_address] = sys_ignore,
  [SYS_exit_group     ] = sys_ignore,
  [SYS_set_robust_list] = sys_ignore,
  [SYS_pipe2          ] = sys_pipe2,
};

// -----------------------------------------------------------------------------
void Handle(Trace *trace, int64_t sno, const Args &args)
{
  if (sno < 0) {
    return;
  }

  if (sno > sizeof(kHandlers) / sizeof(kHandlers[0]) || !kHandlers[sno]) {
    throw std::runtime_error("Unhandled syscall: " + std::to_string(sno));
  }
}
