// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

#pragma once

#include <cassert>
#include <cstddef>
#include <cstdint>

class Trace;



/**
 * Number of system call arguments.
 */
constexpr size_t kSyscallArgs = 6;


/**
 * State passed to the system call.
 */
struct Args {
  /// Identifier of the process.
  int64_t PID;
  /// Return value.
  int64_t Return;
  /// List of arguments.
  uint64_t Arg[kSyscallArgs];

  uint64_t operator[] (size_t idx) const
  {
    assert(idx <= kSyscallArgs);
    return Arg[idx];
  }
};


/**
 * Handles a system call.
 */
void Handle(Trace *trace, int64_t sno, const Args &args);
