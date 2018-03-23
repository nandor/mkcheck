// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

#pragma once

#include <memory>
#include <string>
#include <set>
#include <unordered_map>
#include <vector>

#include "fd.h"

class Process;



/**
 * The trace we are constructing.
 */
class Trace final {
public:
  /**
   * Initiates a new trace, storing output in the specified directory.
   */
  Trace();

  /**
   * Cleanup.
   */
  ~Trace();

  /// Dumps the trace to a file.
  void Dump(const fs::path &output);

  /// Spawns a new process.
  void SpawnTrace(pid_t parent, pid_t pid);
  /// Starts a new trace.
  void StartTrace(pid_t pid, const fs::path &image);
  /// Closes trace.
  void EndTrace(pid_t pid);
  /// Returns the process for a PID.
  Process *GetTrace(pid_t pid);
  /// Unlinks a file.
  void Unlink(const fs::path &path);
  /// Finds a file.
  uint64_t Find(const fs::path &path);
  /// Clears delete/exists flags when a syscall creates a file.
  void Create(const fs::path &target);
  /// Returns the name of a file.
  std::string GetFileName(uint64_t fid) const;
  /// Adds a symlink/rename dependency between two files.
  void AddDependency(const fs::path &src, const fs::path &dst);

private:
  /// Next available UID.
  uint64_t nextUID_;
  /// Next available file ID.
  uint64_t nextFID_;
  /// Map of processes.
  std::unordered_map<pid_t, std::shared_ptr<Process>> procs_;
  /// Map of files to current IDs.
  std::unordered_map<std::string, uint64_t> fileIDs_;

  /// Information about a file.
  struct FileInfo {
    /// Name of the file.
    std::string Name;
    /// Flag indicating if this one exists by the end of the build.
    bool Deleted;
    /// Flag indicating if the file exists or not.
    bool Exists;
    /// List of other files this depends on.
    std::set<uint64_t> Deps;

    /// Constructs a new info object.
    FileInfo(const std::string &Name, bool exists)
      : Name(Name)
      , Deleted(false)
      , Exists(exists)
    {
    }
  };

  /// Set of names attached to an ID.
  std::unordered_map<uint64_t, FileInfo> fileInfos_;
};
