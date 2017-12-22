# This file is part of the mkcheck project.
# Licensing information can be found in the LICENSE file.
# (C) 2017 Nandor Licker. All rights reserved.

cmake_minimum_required(VERSION 3.9)
project(mkcheck)

add_compile_options(-std=c++1y)
add_compile_options(-ferror-limit=1000)
add_compile_options(-Wno-expansion-to-defined)

add_executable(mkcheck
    mkcheck/mkcheck.cpp
)
