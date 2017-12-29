#!/usr/bin/env python

import os
import subprocess

# Project directory.
project = '/home/nand/Projects/mkcheck'
toolPath = os.path.join(project, 'build/mkcheck')
rootPath = os.path.join(project, 'test/make')
tmpPath = os.path.join(project, 'tmp')
outPath = os.path.join(project, 'tmp/out_clean')

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
  tracked.add(dirName)
  for fileName in fileList:
    tracked.add(os.path.join(dirName, fileName))

timestamps = {}
for entry in tracked:
  timestamps[entry] = os.stat(entry).st_mtime

# Run the build with mkcheck.
subprocess.check_call(
  [
    toolPath,
    "--output={0}".format(outPath),
    "make"
  ],
  cwd=rootPath
)

# Run the build after touching each file.
for idx, file in zip(range(len(tracked)), tracked):
  print 'Touching ', file
  os.utime(file, None)

  metaDir = '{0}_{1}'.format(outPath, idx)
  subprocess.check_call(
    [
      toolPath,
      "--output={0}".format(metaDir),
      "make"
    ],
    cwd=rootPath
  )

  with open(os.path.join(metaDir, 'file'), 'w') as out:
    out.write(file)


