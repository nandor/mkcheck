// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

#pragma once

#include <memory>
#include <string>
#include <unordered_map>



/**
 * Process in a trace.
 */
class Process final {
public:


private:
  /// Unique process identifier.
  uint64_t uid_;
};



/**
 * The trace we are constructing.
 */
class Trace final {
public:
  /**
   * Initiates a new trace, storing output in the specified directory.
   */
  Trace(const std::string &output);

  /**
   * Cleanup.
   */
  ~Trace();

  /// Spawns a new process.
  void SpawnTrace(pid_t parent, pid_t pid);
  /// Starts a new trace.
  void StartTrace(pid_t pid, const std::string &exec);
  /// Closes trace.
  void EndTrace(pid_t pid);

private:
  /// List of processes.
  std::unordered_map<pid_t, std::unique_ptr<Process>> procs_;
};
