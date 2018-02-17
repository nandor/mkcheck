// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

#include "trace.h"

#include <cassert>
#include <climits>
#include <iostream>
#include <fstream>

#include <unistd.h>
#include <fcntl.h>



// -----------------------------------------------------------------------------
Process::~Process()
{
  if (isCOW_) {
    return;
  }

  std::ofstream os((trace_->GetOutput() / std::to_string(uid_)).string());
  os << uid_ << " " << parent_ << " " << image_ << std::endl;

  for (const auto output : outputs_) {
    os << output << " ";
  }
  os << std::endl;

  for (const auto input : inputs_) {
    if (outputs_.find(input) == outputs_.end()) {
      os << input << " ";
    }
  }
  os << std::endl;
}

// -----------------------------------------------------------------------------
fs::path Process::Realpath(const fs::path &path)
{
  if (path.is_relative()) {
    return (cwd_ / path).normalize();
  } else {
    return path;
  }
}

// -----------------------------------------------------------------------------
fs::path Process::Realpath(int fd, const fs::path &path)
{
  if (path.is_relative()) {
    if (fd == AT_FDCWD) {
      return (cwd_ / path).normalize();
    } else {
      throw std::runtime_error("Not implemented: realpath");
    }
  } else {
    return path;
  }
}

// -----------------------------------------------------------------------------
void Process::AddInput(const fs::path &path)
{
  inputs_.insert(trace_->Find(Normalise(path)));
}

// -----------------------------------------------------------------------------
void Process::AddOutput(const fs::path &path)
{
  const auto &fullPath = Normalise(path);
  outputs_.insert(trace_->Find(fullPath));
  AddDestination(fullPath);
}

// -----------------------------------------------------------------------------
void Process::AddDestination(const fs::path &path)
{
  uint64_t parent = trace_->Find(path.parent_path());
  if (outputs_.find(parent) == outputs_.end()) {
    inputs_.insert(parent);
  }
}

// -----------------------------------------------------------------------------
void Process::Remove(const fs::path &path)
{
  trace_->Unlink(Normalise(path));
}

// -----------------------------------------------------------------------------
void Process::Rename(const fs::path &from, const fs::path &to)
{
  trace_->Rename(Normalise(from), Normalise(to));
  AddDestination(to);
}

// -----------------------------------------------------------------------------
void Process::MapFd(int fd, const fs::path &path)
{
  files_[fd] = path;
}

// -----------------------------------------------------------------------------
fs::path Process::GetFd(int fd)
{
  return files_[fd];
}

// -----------------------------------------------------------------------------
fs::path Process::Normalise(const fs::path &path)
{
  return fs::absolute(path, cwd_).normalize();
}



// -----------------------------------------------------------------------------
Trace::Trace(const fs::path &output)
  : output_(fs::absolute(output).normalize())
  , nextUID_(1)
  , nextFID_(1)
{
  if (fs::exists(output)) {
    fs::remove_all(output);
  }
  if (!fs::create_directories(output)) {
    throw std::runtime_error("Cannot craete directory.");
  }
}

// -----------------------------------------------------------------------------
Trace::~Trace()
{
  std::ofstream os((output_ / "files").string());
  for (const auto &file : fileNames_) {
    os << file.first << " ";
    for (const auto &name : file.second) {
      os << name << " ";
    }
    os << std::endl;
  }
}

// -----------------------------------------------------------------------------
void Trace::SpawnTrace(pid_t parent, pid_t pid)
{
  // Find the working directory.
  fs::path cwd;
  uint64_t parentUID;
  uint64_t image;
  {
    auto it = procs_.find(parent);
    if (it == procs_.end()) {
      char buffer[PATH_MAX];
      getcwd(buffer, PATH_MAX);
      cwd = buffer;
      image = 0;
      parentUID = 0;
    } else {
      auto proc = it->second;
      cwd = proc->GetCwd();
      image = proc->GetImage();
      parentUID = proc->GetUID();
    }
  }

  // Create the COW trace.
  procs_.emplace(pid, std::make_shared<Process>(
      this,
      pid,
      parentUID,
      nextUID_++,
      image,
      cwd,
      true
  ));
}

// -----------------------------------------------------------------------------
void Trace::StartTrace(pid_t pid, const fs::path &image)
{
  // Find the previous copy - it must exist.
  auto it = procs_.find(pid);
  assert(it != procs_.end());
  auto proc = it->second;

  // Replace with a non-COW trace which has a new image.
  it->second = std::make_shared<Process>(
      this,
      pid,
      proc->GetParent(),
      nextUID_++,
      Find(image),
      proc->GetCwd(),
      false
  );
}

// -----------------------------------------------------------------------------
void Trace::EndTrace(pid_t pid)
{
}

// -----------------------------------------------------------------------------
Process *Trace::GetTrace(pid_t pid)
{
  auto it = procs_.find(pid);
  assert(it != procs_.end());
  return it->second.get();
}

// -----------------------------------------------------------------------------
void Trace::Unlink(const fs::path &path)
{
  fileIDs_.erase(path.string());
}

// -----------------------------------------------------------------------------
void Trace::Rename(const fs::path &from, const fs::path &to)
{
  auto fromID = Find(from);
  fileIDs_.erase(from.string());
  fileIDs_[to.string()] = fromID;
  fileNames_[fromID].push_back(to.string());
}

// -----------------------------------------------------------------------------
uint64_t Trace::Find(const fs::path &path)
{
  const std::string name = path.string();
  auto it = fileIDs_.find(name);
  if (it == fileIDs_.end()) {
    uint64_t id = nextFID_++;
    fileIDs_.emplace(name, id);
    fileNames_.emplace(id, std::vector<std::string>{ name });
    return id;
  } else {
    return it->second;
  }
}

// -----------------------------------------------------------------------------
std::string Trace::GetFileName(uint64_t fid) const
{
  auto it = fileNames_.find(fid);
  assert(it != fileNames_.end());
  return *it->second.rbegin();
}
