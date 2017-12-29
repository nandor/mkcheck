#!/usr/bin/env python

import os
from graphviz import Digraph


def process(lines):
  return [
    [] if l.strip() == '' else [int(x) for x in l.strip().split(' ')]
    for l in lines[:3]
  ]

g = Digraph(comment='Build Graph')
g.attr(rankdir='LR')

PREFIX = '/home/nand/Projects/mkcheck/test/make'
PATH = 'test/out'

names = {}
rendered = set()
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
        rendered.add(uid)
        g.node('f' + str(uid), str(uid) + ': ' + file[1:])

procs = [int(proc) for proc in os.listdir(PATH) if proc != 'files' and proc != 'file']
for proc in procs:
  with open(os.path.join(PATH, str(proc)), 'r') as f:
    [uid, parent, image], outputs, inputs = process(f.readlines())

    if image != 0:
      g.node('p' + str(uid), str(uid) + ': ' + names[image][0], color='red')
    else:
      g.node('p' + str(uid), str(uid), color='red')

    if len(outputs) > 0:
      for f in inputs:
        if f in rendered:
          g.edge('f' + str(f), 'p' + str(uid))

      for f in outputs:
        if f in rendered:
          g.edge('p' + str(uid), 'f' + str(f))

    if parent != 0:
      g.edge('p' + str(parent), 'p' + str(uid), color='red')

g.render('graph/test', view=False)
