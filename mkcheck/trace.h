// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

#pragma once

#include <memory>
#include <string>
#include <unordered_map>

class Trace;



/**
 * Process in a trace.
 */
class Process final {
public:
  /// Creates a new trace with a given image.
  Process(
      Trace *trace,
      pid_t parent,
      pid_t pid,
      uint64_t uid,
      const std::string &image,
      bool isCOW)
    : trace_(trace)
    , parent_(parent)
    , pid_(pid)
    , uid_(uid)
    , image_(image)
    , isCOW_(isCOW)
  {
  }

  /// Returns the parent.
  pid_t GetParent() const { return parent_; }
  /// Returns the name of the image.
  std::string GetImage() const { return image_; }

private:
  /// Pointer to the trace.
  Trace *trace_;
  /// Parent identifier.
  pid_t parent_;
  /// Process identifier.
  pid_t pid_;
  /// Unique instance identifier.
  uint64_t uid_;
  /// Name of the image.
  std::string image_;
  /// If image is copy-on-write.
  bool isCOW_;
  /// Open files.
  std::unordered_map<int64_t, std::string> files_;
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
  void StartTrace(pid_t pid, const std::string &image);
  /// Closes trace.
  void EndTrace(pid_t pid);
  /// Returns the process for a PID.
  Process *GetTrace(pid_t pid);

private:
  /// Output directory.
  const std::string output_;
  /// Next available UID.
  uint64_t nextUID_;
  /// List of processes.
  std::unordered_map<pid_t, std::shared_ptr<Process>> procs_;
};
