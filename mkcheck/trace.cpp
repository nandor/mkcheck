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
#include <sys/stat.h>


// -----------------------------------------------------------------------------
Process::Process(
    Trace *trace,
    pid_t pid,
    uint64_t parent,
    uint64_t uid,
    uint64_t image,
    const FDSet &fdSet,
    const fs::path &cwd,
    bool isCOW)
  : trace_(trace)
  , pid_(pid)
  , parent_(parent)
  , uid_(uid)
  , image_(image)
  , cwd_(cwd)
  , isCOW_(isCOW)
{
  for (const auto &fd : fdSet) {
    files_.emplace(fd.Fd, fd);
  }
}

// -----------------------------------------------------------------------------
Process::~Process()
{
}

// -----------------------------------------------------------------------------
void Process::Dump(std::ostream &os)
{
  os << "{" << std::endl;
  os << "  \"uid\": " << uid_ << "," << std::endl;
  os << "  \"parent\": " << parent_ << "," << std::endl;
  os << "  \"image\": " << image_ << "," << std::endl;

  if (isCOW_) {
    os << "  \"cow\": true,";
  }

  // Dump output files.
  os << "  \"output\": [";
  for (auto it = outputs_.begin(); it != outputs_.end();) {
    os << *it;
    if (++it != outputs_.end()) {
      os << ",";
    }
  }
  os << "]," << std::endl;

  // Dump input files.
  os << "  \"input\": [";
  for (auto it = inputs_.begin(); it != inputs_.end();) {
    os << *it;
    if (++it != inputs_.end()) {
      os << ",";
    }
  }
  os << "]" << std::endl;

  os << "}";
}

// -----------------------------------------------------------------------------
fs::path Process::Normalise(const fs::path &path)
{
  return Normalise(AT_FDCWD, path, cwd_);
}

// -----------------------------------------------------------------------------
fs::path Process::Normalise(const fs::path &path, const fs::path &cwd)
{
  return Normalise(AT_FDCWD, path, cwd);
}

// -----------------------------------------------------------------------------
fs::path Process::Normalise(int fd, const fs::path &path)
{
  return Normalise(fd, path, cwd_);
}

// -----------------------------------------------------------------------------
fs::path Process::Normalise(int fd, const fs::path &path, const fs::path &cwd)
{
  boost::system::error_code ec;

  // Get rid of the relative path.
  fs::path fullPath;
  if (path.is_relative()) {
    if (fd == AT_FDCWD) {
      fullPath = (cwd / path).normalize();
    } else {
      return (GetFd(fd) / path).normalize();
    }
  } else {
    fullPath = path;
  }

  // If the file exists, return the canonical path.
  const fs::path canonical = fs::canonical(fullPath, ec);
  if (!ec) {
    return canonical;
  }

  // If the file was deleted, try to canonicalise the parent.
  const fs::path parent = fullPath.parent_path();
  const fs::path file = fullPath.filename();

  const fs::path canonicalParent = fs::canonical(parent, ec);
  if (!ec) {
    return canonicalParent / file;
  }

  return fullPath;
}

// -----------------------------------------------------------------------------
void Process::AddInput(const fs::path &path)
{
  inputs_.insert(trace_->Find(path));
}

// -----------------------------------------------------------------------------
void Process::AddOutput(const fs::path &path)
{
  outputs_.insert(trace_->Find(path));
  AddDestination(path);
  trace_->Create(path);
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
  trace_->Unlink(from);
  trace_->AddDependency(from, to);
  AddOutput(to);
}

// -----------------------------------------------------------------------------
void Process::Link(const fs::path &target, const fs::path &linkpath)
{
  trace_->AddDependency(target, linkpath);
  trace_->Create(linkpath);
  AddOutput(linkpath);
}

// -----------------------------------------------------------------------------
void Process::MapFd(int fd, const fs::path &path)
{
  FDInfo info(fd, path, false);

  auto it = files_.find(fd);
  if (it == files_.end()) {
    files_.emplace(fd, info);
  } else {
    it->second = info;
  }
}

// -----------------------------------------------------------------------------
fs::path Process::GetFd(int fd)
{
  auto it = files_.find(fd);
  if (it == files_.end()) {
    throw std::runtime_error(
        "Unknown file descriptor: " + std::to_string(fd)
    );
  }
  return it->second.Path;
}

// -----------------------------------------------------------------------------
void Process::DupFd(int from, int to)
{
  auto it = files_.find(from);
  if (it == files_.end()) {
    throw std::runtime_error(
        "Unknown file descriptor: " + std::to_string(from)
    );
  }

  const auto &oldInfo = it->second;
  FDInfo info(to, oldInfo.Path, false);

  auto jt = files_.find(to);
  if (jt == files_.end()) {
    files_.emplace(to, info);
  } else {
    jt->second = info;
  }
}

// -----------------------------------------------------------------------------
void Process::Pipe(int rd, int wr)
{
  const fs::path path = "/proc/" + std::to_string(pid_) + "/fd/";
  const auto pipeRd = path / std::to_string(rd);
  const auto pipeWr = path / std::to_string(wr);

  trace_->AddDependency(pipeWr, pipeRd);
  MapFd(rd, pipeRd);
  MapFd(wr, pipeWr);
}

// -----------------------------------------------------------------------------
void Process::CloseFd(int fd)
{
  files_.erase(fd);
}

// -----------------------------------------------------------------------------
void Process::SetCloseExec(int fd, bool closeExec)
{
  auto it = files_.find(fd);
  if (it == files_.end()) {
    throw std::runtime_error(
        "Unknown file descriptor: " + std::to_string(fd)
    );
  }
  it->second.CloseExec = closeExec;
}

// -----------------------------------------------------------------------------
FDSet Process::GetAllFDs()
{
  FDSet fdSet;
  for (const auto &file : files_) {
    fdSet.push_back(file.second);
  }
  return fdSet;
}

// -----------------------------------------------------------------------------
FDSet Process::GetInheritedFDs()
{
  FDSet fdSet;
  for (const auto &file : files_) {
    const auto &info = file.second;
    if (!info.CloseExec) {
      fdSet.push_back(info);
    }
  }
  return fdSet;
}

// -----------------------------------------------------------------------------
Trace::Trace()
  : nextUID_(1)
  , nextFID_(1)
{
}

// -----------------------------------------------------------------------------
Trace::~Trace()
{
}

// -----------------------------------------------------------------------------
void Trace::Dump(const fs::path &output)
{
  std::ofstream os(output.string());

  // Save the list of files.
  os << "{" << std::endl;
  {
    os << "\"files\": [" << std::endl;
    for (auto it = fileInfos_.begin(); it != fileInfos_.end();) {
      const auto &info = it->second;
      os << "{";
      os << "\"id\": " << it->first << ",";
      os << "\"name\": \"" << info.Name << "\"";
      if (info.Deleted) {
        os << ",\"deleted\": true";
      }
      if (info.Exists) {
        os << ",\"exists\": true";
      }
      if (!info.Deps.empty()) {
        os << ",\"deps\": [";
        for (auto jt = info.Deps.begin(); jt != info.Deps.end();) {
          os << *jt;
          if (++jt != info.Deps.end()) {
            os << ",";
          }
        }
        os << "]";
      }
      os << "}";
      if (++it != fileInfos_.end()) {
        os << "," << std::endl;
      }
    }
    os << "]," << std::endl;
  }

  // Save the list of processes.
  {
    std::vector<Process *> procs;
    for (const auto &proc : procs_) {
      if (!proc.second->IsCOW()) {
        procs.push_back(proc.second.get());
      }
    }

    os << "\"procs\": [" << std::endl;
    for (auto it = procs.begin(); it != procs.end();) {
      (*it)->Dump(os);
      if (++it != procs.end()) {
        os << ",";
      }
    }
    os << "]" << std::endl;
  }
  os << "}" << std::endl;
}

// -----------------------------------------------------------------------------
void Trace::SpawnTrace(pid_t parent, pid_t pid)
{
  // Find the working directory.
  fs::path cwd;
  uint64_t parentUID;
  uint64_t image;
  FDSet fdSet;
  {
    auto it = procs_.find(parent);
    if (it == procs_.end()) {
      char buffer[PATH_MAX];
      getcwd(buffer, PATH_MAX);
      cwd = buffer;
      image = 0;
      parentUID = 0;

      fdSet.emplace_back(0, "/dev/stdin", false);
      fdSet.emplace_back(1, "/dev/stdout", false);
      fdSet.emplace_back(2, "/dev/stderr", false);
    } else {
      auto proc = it->second;
      cwd = proc->GetCwd();
      image = proc->GetImage();
      parentUID = proc->GetUID();
      fdSet = proc->GetAllFDs();
    }
  }

  // Create the COW trace.
  procs_.emplace(pid, std::make_shared<Process>(
      this,
      pid,
      parentUID,
      nextUID_++,
      image,
      fdSet,
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
      Find(proc->Normalise(image)),
      proc->GetInheritedFDs(),
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
  auto fileID = Find(path);
  auto &info = fileInfos_.find(fileID)->second;
  info.Deleted = true;
  info.Exists = false;
}

// -----------------------------------------------------------------------------
uint64_t Trace::Find(const fs::path &path)
{
  if (!path.is_absolute()) {
    throw std::runtime_error("Path not absolute: " + path.string());
  }

  const std::string name = path.string();
  auto it = fileIDs_.find(name);
  if (it == fileIDs_.end()) {
    uint64_t id = nextFID_++;
    fileIDs_.emplace(name, id);
    fileInfos_.emplace(id, FileInfo(name, fs::exists(path)));
    return id;
  } else {
    return it->second;
  }
}

// -----------------------------------------------------------------------------
void Trace::Create(const fs::path &path)
{
  const std::string name = path.string();
  auto it = fileIDs_.find(name);
  if (it == fileIDs_.end()) {
    throw std::runtime_error("Unknown file: " + path.string());
  }

  FileInfo &info = fileInfos_.find(it->second)->second;
  info.Deleted = false;
  info.Exists = true;
}

// -----------------------------------------------------------------------------
std::string Trace::GetFileName(uint64_t fid) const
{
  auto it = fileInfos_.find(fid);
  assert(it != fileInfos_.end());
  return it->second.Name;
}

// -----------------------------------------------------------------------------
void Trace::AddDependency(const fs::path &src, const fs::path &dst)
{
  const auto sID = Find(src);
  const auto dID = Find(dst);
  auto &info = fileInfos_.find(dID)->second;
  info.Deps.push_back(sID);
}
