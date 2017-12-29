#!/usr/bin/env python

import os
import sys
from graphviz import Digraph


def process(lines):
  return [
    [] if l.strip() == '' else [int(x) for x in l.strip().split(' ')]
    for l in lines[:3]
  ]

g = Digraph(comment='Build Graph')
g.attr(rankdir='LR')

PREFIX = '/home/nand/Projects/mkcheck/test/make'
PATH = sys.argv[1]

names = {}
known_files = {}
with open(os.path.join(PATH, 'files'), 'r') as f:
  for line in f.readlines():
    tokens = line.strip().split(' ')
    if len(tokens) < 2:
      continue
    uid = int(tokens[0])
    names[uid] = tokens[1:]
    name = tokens[-1]
    if name.startswith(PREFIX):
      file = name[len(PREFIX):]
      if file != '' and file != '/.':
        known_files[uid] = file[1:]

procs = [int(proc) for proc in os.listdir(PATH) if proc != 'files' and proc != 'file']
rendered = set()
for proc in procs:
  with open(os.path.join(PATH, str(proc)), 'r') as f:
    [uid, parent, image], outputs, inputs = process(f.readlines())

    if image != 0:
      g.node('p' + str(uid), str(uid) + ': ' + names[image][0], color='red')
    else:
      g.node('p' + str(uid), str(uid), color='red')

    if len(outputs) > 0:
      for f in inputs:
        if f in known_files:
          g.edge('f' + str(f), 'p' + str(uid))
          rendered.add(f)

      for f in outputs:
        if f in known_files:
          g.edge('p' + str(uid), 'f' + str(f))
          rendered.add(f)

    if parent != 0:
      g.edge('p' + str(parent), 'p' + str(uid), color='red')

for fid in rendered:
  g.node('f' + str(fid), str(fid) + ': ' + known_files[fid])

g.render('graph/test', view=False)
