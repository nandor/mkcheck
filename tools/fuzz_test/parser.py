# This file is part of the mkcheck project.
# Licensing information can be found in the LICENSE file.
# (C) 2018 Nandor Licker. ALl rights reserved.

import os


def is_number(str):
    return all('0' <= ch and ch <= '9' for ch in str)

def read(lines):
  return [
    [] if l.strip() == '' else [int(x) for x in l.strip().split(' ')]
    for l in lines[:3]
  ]

def strip(prefix, path):
    return path[len(prefix) + 1:]

def get_procs(path):
    return sorted([int(f) for f in os.listdir(path) if is_number(f)])

def read_proc(path):
    with open(path, 'r') as f:
        [uid, parent, image], outputs, inputs = read(f.readlines())
        return (uid, parent, image), outputs, inputs

def find_names(prefix, path):
    names = {}
    with open(os.path.join(path, 'files'), 'r') as f:
      for line in f.readlines():
        tokens = line.strip().split(' ')
        if len(tokens) < 2:
          continue
        uid = int(tokens[0])
        fnames = [strip(prefix, n) for n in tokens[1:] if n.startswith(prefix)]
        if len(fnames) > 0:
            names[uid] = fnames[-1]
    return names

def find_outputs(prefix, path):
    """Collects all outputs written by a build."""

    touched = set()

    names = find_names(prefix, path)

    for proc in get_procs(path):
        _, outputs, _ = read_proc(os.path.join(path, str(proc)))
        for output in outputs:
            if output in names:
                touched.add(names[output])

    return touched
