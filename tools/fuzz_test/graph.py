# This file is part of the mkcheck project.
# Licensing information can be found in the LICENSE file.
# (C) 2018 Nandor Licker. ALl rights reserved.

import os
import json
from collections import defaultdict



class DependencyGraph(object):
    """Graph describing dependencies between file paths."""

    class Node(object):
        def __init__(self, path):
            self.path = path
            self.edges = set()

    def __init__(self):
        self.nodes = {}

    def add_dependency(self, src, dst):
        if src not in self.nodes:
            self.nodes[src] = self.Node(src)
        if dst not in self.nodes:
            self.nodes[dst] = self.Node(dst)
        self.nodes[src].edges.add(dst)

    def find_deps(self, src):
        deps = set()
        def traverse(name):
            if name in deps:
                return
            deps.add(name)
            if name in self.nodes:
                for edge in self.nodes[name].edges:
                    traverse(edge)
        traverse(src)
        return deps


def parse_graph(path):
    """Constructs the dependency graph based on files."""

    # Find all files.
    with open(path, 'r') as f:
        data = json.loads(f.read())

    files = {}
    for file in data["files"]:
        files[file['id']] = file

    graph = DependencyGraph()

    for uid, file in files.iteritems():
        for dep in file.get('deps', []):
            graph.add_dependency(files[dep]['name'], files[uid]['name'])
    
    gid = {}
    for proc in sorted(data["procs"], key=lambda p: p["uid"]):
      uid = proc["uid"]
      if proc.get('cow', False):
        gid[uid] = gid[proc["parent"]]
      else:
        gid[uid] = uid
  
    groups = defaultdict(lambda: (set(), set()))
    for proc in data["procs"]:
      group_id = gid[proc["uid"]]
      
      ins, outs = groups[group_id]
      ins.update(proc.get('input', []))
      outs.update(proc.get('output', []))
  
    for _, (ins, outs) in groups.iteritems():
        for input in ins:
            for output in outs:
                graph.add_dependency(
                    files[input]['name'],
                    files[output]['name']
                )

    return graph


def parse_files(path):
    """Finds files written and read during a clean build."""

    # Find all files and processes.
    files = {}
    inputs = set()
    outputs = set()
    with open(path, 'r') as f:
        data = json.loads(f.read())
        for file in data["files"]:
            files[file['id']] = file
        for proc in data["procs"]:
            inputs = inputs | set(proc.get('input', []))
            outputs = outputs | set(proc.get('output', []))

    def persisted(uid):
        if files[uid].get('deleted', False):
            return False
        if not files[uid].get('exists', False):
            return False
        name = files[uid]['name']
        if name.startswith('/dev') or name.startswith('/proc'):
            return False
        return os.path.exists(name) and not os.path.isdir(name)

    inputs = {files[uid]['name'] for uid in inputs if persisted(uid)}
    outputs = {files[uid]['name'] for uid in outputs if persisted(uid)}

    return inputs, outputs
