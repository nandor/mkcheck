// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

#pragma once

#include <cstdint>
#include <string>



/**
 * Reads a buffer from a child's address space.
 */
ssize_t ReadBuffer(pid_t pid, void *dst, uint64_t src, size_t len);

/**
 * Reads a string from the child's address space.
 */
std::string ReadString(pid_t pid, uint64_t addr);

/**
 * Reads a string, up to a given length.
 */
std::string ReadString(pid_t pid, uint64_t addr, size_t len);
