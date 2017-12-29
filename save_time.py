#!/usr/bin/env python

import os
import stat


PREFIX = '/home/nand/Projects/mkcheck/test/make'

with open('test/out/files', 'r') as f:
  for line in f.readlines():
    tokens = line.strip().split(' ')
    if len(tokens) < 2:
      continue
    uit = int(tokens[0])
    path = tokens[-1]

    if not path.startswith(PREFIX):
      continue

    name = path[len(PREFIX):]

    st = os.stat(path)

    if stat.S_ISREG(st.st_mode):
      print path
