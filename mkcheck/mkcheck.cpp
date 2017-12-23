// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

#include <cstdlib>
#include <cstring>

#include <memory>
#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>

#include <limits.h>
#include <signal.h>
#include <unistd.h>
#include <sys/ptrace.h>
#include <sys/reg.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/user.h>
#include <sys/wait.h>

#include "trace.h"
#include "syscall.h"



// -----------------------------------------------------------------------------
class ProcessState final {
public:
  /// Initialises the process state.
  ProcessState(pid_t pid)
    : pid_(pid)
    , exiting_(false)
  {
  }

  /// Indicates if we're exiting/exiting the syscall.
  bool IsExiting() const { return exiting_; }

  /// Returns the syscall number.
  int64_t GetSyscall() const { return syscall_; }
  /// Returns the arguments.
  Args GetArgs() const
  {
    Args args;
    args.PID = pid_;
    args.Return = return_;
    for (size_t i = 0; i < kSyscallArgs; ++i) {
      args.Arg[i] = args_[i];
    }
    return args;
  }

  /// Extracts information from a register set.
  void Read(const struct user_regs_struct *regs)
  {
    if (exiting_) {
      return_ = regs->rax;
      exiting_ = false;
    } else {
      syscall_ = regs->orig_rax;
      args_[0] = regs->rdi;
      args_[1] = regs->rsi;
      args_[2] = regs->rdx;
      args_[3] = regs->r10;
      args_[4] = regs->r8;
      args_[5] = regs->r9;
      exiting_ = true;
    }
  }

private:
  /// PID of the traced process.
  pid_t pid_;
  /// Flag to indicate if the syscall is entered/exited.
  bool exiting_;
  /// List of arguments.
  uint64_t args_[kSyscallArgs];
  /// Return value.
  int64_t return_;
  /// Syscall number.
  int64_t syscall_;
};


// -----------------------------------------------------------------------------
std::string FindExecutable(const std::string &exec)
{
  size_t m, n, len;
  struct stat64 st;
  char pathname[PATH_MAX];
  for (const char *path = getenv("PATH"); path && *path; path += m) {
    if (const char *colon = strchr(path, ':')) {
      n = colon - path;
      m = n + 1;
    } else {
      m = n = strlen(path);
    }

    if (n == 0) {
      if (!getcwd(pathname, PATH_MAX)) {
        continue;
      }
      len = strlen(pathname);
    } else if (n > sizeof(pathname) - 1) {
      continue;
    } else {
      strncpy(pathname, path, n);
      len = n;
    }

    if (len && pathname[len - 1] != '/') {
      pathname[len++] = '/';
    }

    if (exec.size() + len > sizeof(pathname) - 1) {
      continue;
    }

    strcpy(pathname + len, exec.c_str());
    if (stat64(pathname, &st) != 0) {
      continue;
    }

    if (S_ISREG(st.st_mode) && st.st_mode & S_IXUSR) {
      break;
    }
  }
  return pathname;
}

// -----------------------------------------------------------------------------
int RunChild(const std::string &exec, const std::vector<char *> &args)
{
  ptrace(PTRACE_TRACEME);
  raise(SIGSTOP);
  return execvp(FindExecutable(exec).c_str(), args.data());
}

// -----------------------------------------------------------------------------
static constexpr int kTraceOptions
  = PTRACE_O_TRACESYSGOOD
  | PTRACE_O_TRACEEXEC
  | PTRACE_O_TRACEEXIT
  | PTRACE_O_TRACECLONE
  | PTRACE_O_TRACEFORK
  | PTRACE_O_TRACEVFORK
  ;

// -----------------------------------------------------------------------------
int RunTracer(pid_t pid)
{
  // Skip the first signal, which is SIGSTOP.
  int status;
  waitpid(pid, &status, 0);
  ptrace(PTRACE_SETOPTIONS, pid, nullptr, kTraceOptions);

  // State we are tracing.
  auto trace = std::make_unique<Trace>();

  // Set of tracked processses.
  std::unordered_map<pid_t, std::shared_ptr<ProcessState>> tracked;
  tracked[pid] = std::make_shared<ProcessState>(pid);

  // Keep tracking syscalls while any process in the hierarchy is running.
  int restart_sig = 0;
  while (!tracked.empty()) {
    if (pid > 0) {
      if (ptrace(PTRACE_SYSCALL, pid, 0, restart_sig) < 0) {
        throw std::runtime_error("ptrace failed");
      }
    }

    if ((pid = wait3(&status, __WALL, 0)) < 0) {
      throw std::runtime_error("wait3 failed");
    }

    if (WIFEXITED(status) || WIFSIGNALED(status)) {
      tracked.erase(pid);
      pid = -1;
      continue;
    }

    switch (int sig = WSTOPSIG(status)) {
      case SIGTRAP | 0x80: {
        // By setting PTRACE_O_TRACESYSGOOD, bit 7 of the system call
        // number is set in order to distinguish system call traps
        // from other traps.
        restart_sig = 0;
        break;
      }
      case SIGTRAP: {
        // SIGTRAP is sent with an event number in certain scenarios.
        // Simply restart the process with signal number 0.
        restart_sig = 0;
        continue;
      }
      case SIGSTOP: {
        // Ignore the first SIGSTOP in each process since it is dispatched
        // after the new process is started, deliver it otherwise.
        auto it = tracked.find(pid);
        if (it == tracked.end()) {
          tracked[pid] = std::make_shared<ProcessState>(pid);
          restart_sig = 0;
        } else {
          restart_sig = SIGSTOP;
        }
        continue;
      }
      default: {
        // Deliver other signals to the process.
        restart_sig = sig;
        continue;
      }
    }

    // Fetch the state desribing the process.
    std::shared_ptr<ProcessState> state;
    {
      auto it = tracked.find(pid);
      if (it == tracked.end()) {
        throw std::runtime_error("Invalid PID.");
      }
      state = it->second;
    }

    // Read syscall arguments on entry & the return value on exit.
    struct user_regs_struct regs;
    ptrace(PTRACE_GETREGS, pid, 0, &regs);
    state->Read(&regs);

    // On exit, process the system call.
    if (state->IsExiting()) {
      Handle(trace.get(), state->GetSyscall(), state->GetArgs());
    }
  }

  return 0;
}

// -----------------------------------------------------------------------------
int main(int argc, char **argv)
{
  if (argc < 2) {
    std::cerr << "Missing arguments." << std::endl;
    return EXIT_FAILURE;
  }

  switch (pid_t pid = fork()) {
    case -1: {
      return EXIT_FAILURE;
    }
    case 0: {
      std::vector<char *> args;
      for (int i = 1; i < argc; ++i) {
        args.push_back(argv[i]);
      }
      args.push_back(nullptr);
      return RunChild(argv[1], args);
    }
    default: {
      return RunTracer(pid);
    }
  }

  return EXIT_SUCCESS;
}
