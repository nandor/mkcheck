// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

#include <fcntl.h>

#include "proc.h"
#include "trace.h"



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
  inputs_.insert(image);
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
