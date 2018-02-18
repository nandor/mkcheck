// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

#pragma once

#include <memory>
#include <string>
#include <set>
#include <unordered_map>
#include <vector>

#include <boost/filesystem.hpp>

namespace fs = boost::filesystem;

class Trace;



/**
 * File descriptor information.
 */
struct FDInfo {
  /// Descriptor number.
  int Fd;
  /// Path to file.
  fs::path Path;
  /// Close-On-Exec flag.
  bool CloseExec;

  /// Constructs a file descriptor info object.
  FDInfo(int fd, bool closeExec)
    : Fd(fd)
    , CloseExec(closeExec)
  {
  }

  /// Constructs a file descriptor info object.
  FDInfo(int fd, const fs::path &path, bool closeExec)
    : Fd(fd)
    , Path(path)
    , CloseExec(closeExec)
  {
  }
};



/**
 * Map of inherited file descriptors.
 */
typedef std::vector<FDInfo> FDSet;

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

  /// Destroys the process & writes data to a file.
  ~Process();

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
  /// Resolves a path, relative to any directory.
  fs::path Normalise(int fd, const fs::path &path);

  /// Adds an input file to a process.
  void AddInput(const fs::path &path);
  /// Adds an output file to a process.
  void AddOutput(const fs::path &path);
  /// Adds a destination path as an input.
  void AddDestination(const fs::path &path);
  /// Sets the working directory.
  void SetCwd(const fs::path &cwd) { cwd_ = cwd; }
  /// Unlinks a file or directory.
  void Remove(const fs::path &path);
  /// Renames a file.
  void Rename(const fs::path &from, const fs::path &to);
  /// Creates a symlink.
  void Symlink(const fs::path &target, const fs::path &linkpath);

  /// Maps a file descriptor to a path.
  void MapFd(int fd, const fs::path &path);
  /// Returns the path to a file opened by a descriptor.
  fs::path GetFd(int fd);
  /// Duplicates a file descriptor.
  void DupFd(int from, int to);

  /// Adds a file descriptor to the cloexec set.
  void SetCloseExec(int fd);
  /// Removes a file from the cloexec set.
  void ClearCloseExec(int fd);

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

  /// Returns the path to the output directory.
  fs::path GetOutput() const { return output_; }

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
  /// Returns the name of a file.
  std::string GetFileName(uint64_t fid) const;
  /// Adds a symlink/rename dependency between two files.
  void AddDependency(const fs::path &src, const fs::path &dst);

private:
  /// Output directory.
  const fs::path output_;
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
    /// List of other files this depends on.
    std::vector<uint64_t> Deps;

    /// Constructs a new info object.
    FileInfo(const std::string &Name)
      : Name(Name)
      , Deleted(false)
    {
    }
  };

  /// Set of names attached to an ID.
  std::unordered_map<uint64_t, FileInfo> fileInfos_;
};
