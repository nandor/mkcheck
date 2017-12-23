// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

#include <iostream>
#include "trace.h"



// -----------------------------------------------------------------------------
Trace::Trace(const std::string &output)
{
}

// -----------------------------------------------------------------------------
Trace::~Trace()
{
}

// -----------------------------------------------------------------------------
void Trace::SpawnTrace(pid_t parent, pid_t pid)
{
  std::cerr << parent << " -> " << pid << std::endl;
}

// -----------------------------------------------------------------------------
void Trace::StartTrace(pid_t pid, const std::string &exec)
{
  std::cerr << pid << ": " << exec << std::endl;
}

// -----------------------------------------------------------------------------
void Trace::EndTrace(pid_t pid)
{
  std::cerr << pid << ": exit" << std::endl;
}
