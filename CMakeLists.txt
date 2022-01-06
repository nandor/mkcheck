# This file is part of the mkcheck project.
# Licensing information can be found in the LICENSE file.
# (C) 2017 Nandor Licker. All rights reserved.

cmake_minimum_required(VERSION 3.5)
project(mkcheck CXX)

find_package(Boost COMPONENTS system filesystem program_options REQUIRED)

add_compile_options(-std=c++17)

add_executable(mkcheck
    mkcheck/fd.cpp
    mkcheck/mkcheck.cpp
    mkcheck/syscall.cpp
    mkcheck/proc.cpp
    mkcheck/trace.cpp
    mkcheck/util.cpp
)
target_link_libraries(mkcheck
  ${Boost_LIBRARIES}
)
