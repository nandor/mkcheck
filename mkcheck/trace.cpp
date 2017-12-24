// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

#include "trace.h"

#include <cassert>
#include <climits>
#include <iostream>

#include <unistd.h>



// -----------------------------------------------------------------------------
void Process::AddInput(const std::string &path)
{
}

// -----------------------------------------------------------------------------
void Process::AddInput(const std::string &path, int fd)
{
}

// -----------------------------------------------------------------------------
void Process::AddOutput(const std::string &path, int fd)
{
}

// -----------------------------------------------------------------------------
void Process::Close(int fd)
{
}

// -----------------------------------------------------------------------------
void Process::Duplicate(int oldFd, int newFd)
{
}

// -----------------------------------------------------------------------------
Trace::Trace(const std::string &output)
  : output_(output)
  , nextUID_(0)
{
}

// -----------------------------------------------------------------------------
Trace::~Trace()
{
}

// -----------------------------------------------------------------------------
void Trace::SpawnTrace(pid_t parent, pid_t pid)
{
  // Find the working directory.
  std::string cwd;
  std::string image;
  {
    auto it = procs_.find(parent);
    if (it == procs_.end()) {
      char buffer[PATH_MAX];
      getcwd(buffer, PATH_MAX);
      cwd = buffer;
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
void Trace::StartTrace(pid_t pid, const std::string &image)
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
      image,
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
void Trace::Unlink(const std::string &path)
{

}

// -----------------------------------------------------------------------------
void Trace::Rename(const std::string &from, const std::string &to)
{
}
