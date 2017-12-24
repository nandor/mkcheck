// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

#pragma once

#include <memory>
#include <string>
#include <unordered_map>

#include <boost/filesystem.hpp>

namespace fs = boost::filesystem;

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
      const fs::path &image,
      const fs::path &cwd,
      bool isCOW)
    : trace_(trace)
    , parent_(parent)
    , pid_(pid)
    , uid_(uid)
    , image_(image)
    , cwd_(cwd)
    , isCOW_(isCOW)
  {
  }

  /// Returns the parent.
  pid_t GetParent() const { return parent_; }
  /// Returns the name of the image.
  fs::path GetImage() const { return image_; }
  /// Returns the working directory.
  fs::path GetCwd() const { return cwd_; }

  /// Adds an input without associating it with a descriptor.
  void AddInput(const fs::path &path);
  /// Adds an input attached to a descriptor.
  void AddInput(const fs::path &path, int fd);
  /// Adds an output attached to a descriptor.
  void AddOutput(const fs::path &path, int fd);
  /// Closes a descriptor so it can be reused.
  void Close(int fd);
  /// Duplicates a file descriptor.
  void Duplicate(int oldFd, int newFd);
  /// Sets the working directory.
  void SetCwd(const fs::path &cwd) { cwd_ = cwd; }
  /// Unlinks a file.
  void Unlink(const fs::path &path);
  /// Renames a file.
  void Rename(const fs::path &from, const fs::path &to);

private:
  /// Resolves a file to a unique identifier.
  uint64_t Resolve(const fs::path &path);

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
  fs::path image_;
  /// Working directory.
  fs::path cwd_;
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
  Trace(const fs::path &output);

  /**
   * Cleanup.
   */
  ~Trace();

  /// Spawns a new process.
  void SpawnTrace(pid_t parent, pid_t pid);
  /// Starts a new trace.
  void StartTrace(pid_t pid, const fs::path &image);
  /// Closes trace.
  void EndTrace(pid_t pid);
  /// Returns the process for a PID.
  Process *GetTrace(pid_t pid);

private:
  /// Output directory.
  const fs::path output_;
  /// Next available UID.
  uint64_t nextUID_;
  /// Next available file ID.
  uint64_t nextFID_;
  /// Map of processes.
  std::unordered_map<pid_t, std::shared_ptr<Process>> procs_;
  /// Map of files.
  std::unordered_map<std::string, uint64_t> files_;
};
