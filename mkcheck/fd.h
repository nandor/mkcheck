// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

#pragma once

#include <vector>
#include <boost/filesystem.hpp>

namespace fs = boost::filesystem;



/**
 * File descriptor information.
 */
struct FDInfo final {
  /// Descriptor number.
  int Fd;
  /// Path to file.
  fs::path Path;
  /// Close-On-Exec flag.
  bool CloseExec;

  /// Constructs an empty file descriptor.
  FDInfo();

  /// Constructs a file descriptor info object.
  FDInfo(int fd, const fs::path &path, bool closeExec);

  /// Destroys the structure.
  ~FDInfo();
};



/**
 * Map of inherited file descriptors.
 */
typedef std::vector<FDInfo> FDSet;
