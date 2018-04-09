// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

#include "fd.h"



// -----------------------------------------------------------------------------
FDInfo::FDInfo()
  : FDInfo(0, "/dev/null", false)
{
}

// -----------------------------------------------------------------------------
FDInfo::FDInfo(int fd, const fs::path &path, bool closeExec)
  : Fd(fd)
  , Path(path)
  , CloseExec(closeExec)
  , Closed(false)
{
}

// -----------------------------------------------------------------------------
FDInfo::~FDInfo()
{
}
