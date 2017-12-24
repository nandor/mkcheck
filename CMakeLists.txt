# This file is part of the mkcheck project.
# Licensing information can be found in the LICENSE file.
# (C) 2017 Nandor Licker. All rights reserved.

cmake_minimum_required(VERSION 3.9)
project(mkcheck CXX)

find_package(Boost COMPONENTS system filesystem program_options REQUIRED)

add_compile_options(-std=c++1y)
add_compile_options(-ferror-limit=1000)
add_compile_options(-Wno-expansion-to-defined)

add_executable(mkcheck
    mkcheck/mkcheck.cpp
    mkcheck/syscall.cpp
    mkcheck/trace.cpp
    mkcheck/util.cpp
)
target_link_libraries(mkcheck
  ${Boost_LIBRARIES}
)
