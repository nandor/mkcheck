// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

#include "trace.h"

#include <cassert>
#include <climits>
#include <iostream>

#include <unistd.h>



// -----------------------------------------------------------------------------
Process::~Process()
{
  if (inputs_.empty() && outputs_.empty()) {
    return;
  }

  std::ofstream os((trace_->GetOutput() / std::to_string(uid_)).string());

  os << uid_ << " " << image_ << std::endl;

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
void Process::AddInput(const fs::path &path)
{
  inputs_.insert(trace_->Find(Normalise(path)));
}

// -----------------------------------------------------------------------------
void Process::AddOutput(const fs::path &path)
{
  outputs_.insert(trace_->Find(Normalise(path)));
}

// -----------------------------------------------------------------------------
void Process::Unlink(const fs::path &path)
{
  trace_->Unlink(Normalise(path));
}

// -----------------------------------------------------------------------------
void Process::Rename(const fs::path &from, const fs::path &to)
{
  trace_->Rename(Normalise(from), Normalise(to));
}

// -----------------------------------------------------------------------------
fs::path Process::Normalise(const fs::path &path)
{
  return fs::absolute(path, cwd_).normalize();
}



// -----------------------------------------------------------------------------
Trace::Trace(const fs::path &output)
  : output_(output)
  , nextUID_(1)
  , nextFID_(1)
{
  if (fs::exists(output)) {
    fs::remove_all(output);
  }
  if (!fs::create_directory(output)) {
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
  uint64_t image;
  {
    auto it = procs_.find(parent);
    if (it == procs_.end()) {
      char buffer[PATH_MAX];
      getcwd(buffer, PATH_MAX);
      cwd = buffer;
      image = 0;
    } else {
      auto proc = it->second;
      cwd = proc->GetCwd();
      image = proc->GetImage();
    }
  }

  // Create the COW trace.
  procs_.emplace(pid, std::make_shared<Process>(
      this,
      parent,
      pid,
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
      proc->GetParent(),
      pid,
      nextUID_++,
      Find(image),
      proc->GetCwd(),
      false
  );
}

// -----------------------------------------------------------------------------
void Trace::EndTrace(pid_t pid)
{
  procs_.erase(pid);
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
  throw std::runtime_error("Not implemented.");
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
