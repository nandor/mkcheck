#!/usr/bin/env python

import os
import stat
import subprocess
import sys
import time

from graph import parse_graph
from parser import find_outputs

# Project directory.
project = '/home/nand/Projects/mkcheck'
toolPath = os.path.join(project, 'build/mkcheck')
rootPath = os.path.join(project, 'test/make')
tmpPath = os.path.join(project, 'tmp')
graphPath = os.path.join(tmpPath, 'out_clean')

# Run a clean build.
subprocess.check_call(
  [
    "make",
    "clean"
  ],
  cwd=rootPath
)

# Find the initial timestamps of the tracked files.
tracked = set()
for dirName, subdirList, fileList in os.walk(rootPath, topdown=False):
  for fileName in fileList:
    tracked.add(os.path.join(dirName, fileName))

timestamps = {}
for entry in tracked:
  timestamps[entry] = os.stat(entry).st_mtime

# Run the build with mkcheck.
subprocess.check_call(
  [
    toolPath,
    "--output={0}".format(graphPath),
    "make"
  ],
  cwd=rootPath
)

graph = parse_graph(rootPath, graphPath)

# Run the build after touching each file.
for idx, file in zip(range(len(tracked)), tracked):
  print '\n\nTouching ', idx, ' ', file

  # Touch the file.
  os.utime(file, None)

  # Run the incremental build.
  metaDir = os.path.join(tmpPath, 'out_{0}'.format(idx))
  subprocess.check_call(
    [
      toolPath,
      "--output={0}".format(metaDir),
      "make"
    ],
    cwd=rootPath
  )

  deps = graph.find_deps(file[len(rootPath) + 1:])
  outs = find_outputs(rootPath, metaDir)
  if deps != outs:
      print 'Mismatch ', deps, outs

  with open(os.path.join(metaDir, 'file'), 'w') as out:
    out.write(file)
