// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

#include <cassert>
#include <cstdlib>
#include <cstring>

#include <memory>
#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>

#include <getopt.h>
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

#include "syscall.h"
#include "trace.h"
#include "util.h"



// -----------------------------------------------------------------------------
class ProcessState final {
public:
  /// Initialises the process state.
  ProcessState(pid_t pid)
    : pid_(pid)
    , entering_(false)
  {
  }

  /// Indicates if we're exiting/exiting the syscall.
  bool IsExiting() const { return !entering_; }

  /// Returns the syscall number.
  int64_t GetSyscall() const { return syscall_; }

  /// Returns the return value.
  int64_t GetReturn() const { return return_; }

  /// Returns a specific argument.
  uint64_t GetArg(size_t idx) const
  {
    assert(idx < kSyscallArgs);
    return args_[idx];
  }

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
    if (entering_) {
      return_ = regs->rax;
      entering_ = false;
    } else {
      syscall_ = regs->orig_rax;
      args_[0] = regs->rdi;
      args_[1] = regs->rsi;
      args_[2] = regs->rdx;
      args_[3] = regs->r10;
      args_[4] = regs->r8;
      args_[5] = regs->r9;
      entering_ = true;
    }
  }

  /// Sets the name of the executable.
  void SetExecutable(const std::string &exec)
  {
    exec_ = exec;
  }

  /// Returns the name of the executable.
  std::string GetExecutable() const
  {
    return exec_;
  }

private:
  /// PID of the traced process.
  pid_t pid_;
  /// Flag to indicate if the syscall is entered/exited.
  bool entering_;
  /// Name of the executable.
  std::string exec_;
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
      return pathname;
    }
  }
  throw std::runtime_error("Cannot find executable.");
}

// -----------------------------------------------------------------------------
int RunChild(const std::string &exec, const std::vector<char *> &args)
{
  ptrace(PTRACE_TRACEME);
  raise(SIGSTOP);
  return execvp(exec.c_str(), args.data());
}

// -----------------------------------------------------------------------------
static constexpr int kTraceOptions
  = PTRACE_O_TRACESYSGOOD
  | PTRACE_O_TRACECLONE
  | PTRACE_O_TRACEFORK
  | PTRACE_O_TRACEVFORK
  ;

// -----------------------------------------------------------------------------
int RunTracer(Trace *trace, pid_t pid)
{
  // Skip the first signal, which is SIGSTOP.
  int status;
  waitpid(pid, &status, 0);
  ptrace(PTRACE_SETOPTIONS, pid, nullptr, kTraceOptions);
  trace->SpawnTrace(0, pid);

  // Set of tracked processses.
  std::unordered_map<pid_t, std::shared_ptr<ProcessState>> tracked;
  tracked[pid] = std::make_shared<ProcessState>(pid);

  // Candidate processses to be started.
  std::unordered_map<pid_t, pid_t> candidates;

  // Process to wait for - after clone/vfork/exec, the callee is given
  // priority in order to trap and set up the child's data structure.
  // Otherwise, it is -1, stopping the first available child.
  pid_t waitFor = -1;

  // Keep tracking syscalls while any process in the hierarchy is running.
  int restart_sig = 0;
  while (!tracked.empty()) {
    // Trap a child on the next syscall.
    if (pid > 0) {
      if (ptrace(PTRACE_SYSCALL, pid, 0, restart_sig) < 0) {
        throw std::runtime_error("ptrace failed");
      }
    }

    // Wait for a child or any children to stop.
    if ((pid = waitpid(waitFor, &status, 0)) < 0) {
      throw std::runtime_error("waitpid failed");
    }
    waitFor = -1;

    // Remove the process from the tracked ones on exit.
    if (WIFEXITED(status) || WIFSIGNALED(status)) {
      trace->EndTrace(pid);
      tracked.erase(pid);
      pid = -1;
      continue;
    }

    /// Handle signals dispatched to children.
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
        switch (status >> 16) {
          case PTRACE_EVENT_FORK:
          case PTRACE_EVENT_VFORK:
          case PTRACE_EVENT_CLONE: {
            // Get the ID of the child process.
            pid_t child;
            ptrace(PTRACE_GETEVENTMSG, pid, 0, &child);
            // Start tracking it.
            candidates[child] = pid;
            restart_sig = 0;
            break;
          }
          default: {
            break;
          }
        }
        restart_sig = 0;
        continue;
      }
      case SIGSTOP: {
        // Ignore the first SIGSTOP in each process since it is dispatched
        // after the new process is started, deliver it otherwise.
        auto it = candidates.find(pid);
        if (it != candidates.end()) {
          restart_sig = 0;
          tracked.emplace(pid, std::make_shared<ProcessState>(pid));
          trace->SpawnTrace(it->second, pid);
          candidates.erase(it);
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
        throw std::runtime_error("Invalid PID: " + std::to_string(pid));
      }
      state = it->second;
    }

    // Read syscall arguments on entry & the return value on exit.
    struct user_regs_struct regs;
    ptrace(PTRACE_GETREGS, pid, 0, &regs);
    state->Read(&regs);

    // Handle the system call.
    auto sno = state->GetSyscall();
    switch (sno) {
      case SYS_execve: {
        // Execve is special since its arguments can't be read once the
        // process image is replaced, thus the argument is read on entry.
        if (state->IsExiting()) {
          if (state->GetReturn() >= 0) {
            trace->StartTrace(pid, state->GetExecutable());
          }
        } else {
          state->SetExecutable(ReadString(pid, state->GetArg(0)));
        }
        break;
      }
      case SYS_clone:
      case SYS_vfork:
      case SYS_fork: {
        waitFor = pid;
        break;
      }
    }

    // All other system calls are handled on exit.
    if (state->IsExiting()) {
      Handle(trace, sno, state->GetArgs());
    }
  }

  return EXIT_SUCCESS;
}

// -----------------------------------------------------------------------------
static struct option kOptions[] =
{
  { "output",  required_argument, 0, 'o' },
};

// -----------------------------------------------------------------------------
int main(int argc, char **argv)
{
  // Parse arguments.
  std::string output;
  std::string exec;
  std::vector<char *> args;
  {
    int c = 0, idx = 0;
    while (c >= 0) {
      switch (c = getopt_long(argc, argv, "o:", kOptions, &idx)) {
        case -1: {
          break;
        }
        case 'o': {
          output = optarg;
          continue;
        }
        default: {
          std::cerr << "Unknown option." << std::endl;
          return EXIT_FAILURE;
        }
      }
    }

    if (output.empty()) {
      std::cerr << "Missing output directory." << std::endl;
      return EXIT_FAILURE;
    }
    if (optind == argc) {
      std::cerr << "Missing executable." << std::endl;
      return EXIT_FAILURE;
    }

    for (int i = optind; i < argc; ++i) {
      args.push_back(argv[i]);
    }
    args.push_back(nullptr);
    exec = FindExecutable(args[0]);
  }

  // Create the tracer.
  auto trace = std::make_unique<Trace>(output);

  // Fork & start tracing.
  switch (pid_t pid = fork()) {
    case -1: {
      return EXIT_FAILURE;
    }
    case 0: {
      return RunChild(exec, args);
    }
    default: {
      try {
        return RunTracer(trace.get(), pid);
      } catch (const std::exception &ex) {
        kill(pid, SIGKILL);
        std::cerr << "[Exception] " << ex.what() << std::endl;
        return EXIT_FAILURE;
      }
    }
  }

  return EXIT_SUCCESS;
}
