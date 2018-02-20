#!/usr/bin/env python

import os
import subprocess
import stat
import sys
import time

from graph import parse_graph, parse_files
from proc import run_proc
from mtime import read_mtimes

# Project directory.
scriptPath = os.path.dirname(os.path.abspath(__file__))
projectPath = os.path.abspath(os.path.join(scriptPath, os.pardir, os.pardir))
toolPath = os.path.join(projectPath, 'build/mkcheck')
buildPath = os.getcwd()

BUILD = [ "make", "-j1" ]
CLEAN = [ "make", "clean" ]

#BUILD = [ "python", "setup.py", "build" ]
#CLEAN = [ "git", "clean", "-xdf" ]

# TODO: make this an actual temporary folder
tmpPath = '/tmp/mkcheck'

# Build mkcheck.
run_proc([ 'ninja' ], cwd=os.path.join(projectPath, 'build'))

# Run a clean build.
run_proc(CLEAN, cwd=buildPath)

# Run the build with mkcheck.
run_proc(
  [ toolPath, "--output={0}".format(tmpPath), "--" ] + BUILD,
  cwd=buildPath
)

# Find the set of inputs and outputs, as well as the graph.
inputs, outputs = parse_files(tmpPath)
graph = parse_graph(tmpPath)
t0 = read_mtimes(outputs)

fuzzed = [f for f in inputs - outputs if os.access(f, os.W_OK)]
count = len(fuzzed)

for idx, input in zip(range(count), fuzzed):
    # Only care about the file if the user has write access to it.
    if not os.access(input, os.W_OK):
        continue

    print '[{0}/{1}] {2}:'.format(idx + 1, count, input)

    # Touch the file.
    os.utime(input, None)
    # Run the incremental build.
    run_proc(BUILD, cwd=buildPath)

    t1 = read_mtimes(outputs)

    # Find the set of changed files.
    modified = set()
    for k, v in t0.iteritems():
        if v != t1[k]:
            modified.add(k)

    # Find expected changes.
    expected = graph.find_deps(input) & outputs

    # Report differences.
    if modified != expected:
        over = False
        under = False
        for f in modified:
            if f not in expected:
                over = True
                print '  +', f

        for f in expected:
            if f not in modified:
                under = True
                print '  -', f

        if under:
            run_proc(CLEAN, cwd=buildPath)
            run_proc(BUILD, cwd=buildPath)
            t1 = read_mtimes(outputs)

    t0 = t1
