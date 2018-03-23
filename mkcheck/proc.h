// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

#pragma once

#include <iostream>
#include <set>
#include <unordered_map>

#include "fd.h"

class Trace;


/**
 * Process in a trace.
 */
class Process final {
public:
  /// Creates a new trace with a given image.
  Process(
      Trace *trace,
      pid_t pid,
      uint64_t parent,
      uint64_t uid,
      uint64_t image,
      const FDSet &fdSet,
      const fs::path &cwd,
      bool isCOW);

  /// Cleanup.
  ~Process();

  /// Dumps a process to a stream.
  void Dump(std::ostream &os);

  /// Returns the parent UID.
  uint64_t GetParent() const { return parent_; }
  /// Returns the process UID.
  uint64_t GetUID() const { return uid_; }
  /// Returns the name of the image.
  uint64_t GetImage() const { return image_; }
  /// Returns the working directory.
  fs::path GetCwd() const { return cwd_; }

  /// Checks if the process is Copy-On-Write.
  bool IsCOW() const { return isCOW_; }

  /// Resolves a path, relative to the cwd.
  fs::path Normalise(const fs::path &path);
  /// Resolves a path, given cwd.
  fs::path Normalise(const fs::path &path, const fs::path &cwd);
  /// Resolves a path, relative to any directory.
  fs::path Normalise(int fd, const fs::path &path);
  /// Resolves a path, relative to any directory, given a cwd.
  fs::path Normalise(int fd, const fs::path &path, const fs::path &cwd);

  /// Adds a touched (stat'd) file to a process.
  void AddTouched(const fs::path &path) { AddInput(path); }
  /// Adds an input file to a process.
  void AddInput(const fs::path &path);
  /// Adds an output file to a process.
  void AddOutput(const fs::path &path);

  /// Adds a touched file to a process.
  void AddTouched(int fd) { AddInput(fd); }
  /// Adds an input fd to a process.
  void AddInput(int fd) { AddInput(GetFd(fd)); }
  /// Adds an output fd to a process.
  void AddOutput(int fd) { AddOutput(GetFd(fd)); }

  /// Adds a destination path as an input.
  void AddDestination(const fs::path &path);
  /// Sets the working directory.
  void SetCwd(const fs::path &cwd) { cwd_ = cwd; }
  /// Unlinks a file or directory.
  void Remove(const fs::path &path);
  /// Renames a file.
  void Rename(const fs::path &from, const fs::path &to);
  /// Creates a symlink.
  void Link(const fs::path &target, const fs::path &linkpath);

  /// Maps a file descriptor to a path.
  void MapFd(int fd, const fs::path &path);
  /// Returns the path to a file opened by a descriptor.
  fs::path GetFd(int fd);
  /// Duplicates a file descriptor.
  void DupFd(int from, int to);
  /// Sets up a pipe.
  void Pipe(int rd, int wr);
  /// Closes a file descriptor.
  void CloseFd(int fd);
  /// Adds a file descriptor to the cloexec set.
  void SetCloseExec(int fd, bool closeExec);

  /// Returns all file descriptors.
  FDSet GetAllFDs();
  /// Returns the set of non-cloexec fds.
  FDSet GetInheritedFDs();

private:
  /// Pointer to the trace.
  Trace *trace_;
  /// Process identifier.
  const pid_t pid_;
  /// Parent identifier.
  const uint64_t parent_;
  /// Unique instance identifier.
  const uint64_t uid_;
  /// Name of the image.
  const uint64_t image_;
  /// Working directory.
  fs::path cwd_;
  /// If image is copy-on-write.
  bool isCOW_;

  /// Open files.
  std::unordered_map<int, FDInfo> files_;
  /// Input files.
  std::set<uint64_t> inputs_;
  /// Output files.
  std::set<uint64_t> outputs_;
};
