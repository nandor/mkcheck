// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

#include "util.h"

#include <limits>
#include <stdexcept>

#include <sys/uio.h>


// -----------------------------------------------------------------------------
constexpr size_t kPageSize = 4096;

// -----------------------------------------------------------------------------
ssize_t ReadBuffer(pid_t pid, void *dst, uint64_t src, size_t len)
{
  const struct iovec local =
  {
    .iov_base = dst,
    .iov_len = len
  };

  const struct iovec remote =
  {
    .iov_base = reinterpret_cast<void *>(src),
    .iov_len = len
  };

  return process_vm_readv(pid, &local, 1, &remote, 1, 0);
}

// -----------------------------------------------------------------------------
std::string ReadString(pid_t pid, uint64_t addr)
{
  return ReadString(pid, addr, std::numeric_limits<size_t>::max());
}

// -----------------------------------------------------------------------------
std::string ReadString(pid_t pid, uint64_t addr, size_t len)
{
  std::string result;
  char buffer[kPageSize];
  uint64_t read = 0;

  for (size_t i = 0; i < len; ++i) {
    const uint64_t end = (addr + kPageSize) & (kPageSize - 1);
    const uint64_t len = kPageSize - end;

    ssize_t count = ReadBuffer(pid, buffer, addr, len);
    if (count < 0) {
      throw std::runtime_error("Cannot read from child memory.");
    }

    for (size_t i = 0; i < count; ++i) {
      if (buffer[i] == '\0') {
        result.append(buffer, i);
        return result;
      }
    }

    result.append(buffer, count);
    addr += count;
  }

  return result;
}
