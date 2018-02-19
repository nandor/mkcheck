# This file is part of the mkcheck project.
# Licensing information can be found in the LICENSE file.
# (C) 2018 Nandor Licker. ALl rights reserved.

import os
import json



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
    with open(os.path.join(path, 'files'), 'r') as f:
        files = {}
        for file in json.loads(f.read()):
            files[file['id']] = file

    graph = DependencyGraph()

    for uid, file in files.iteritems():
        for dep in file.get('deps', []):
            graph.add_dependency(files[dep]['name'], files[uid]['name'])

    with open(os.path.join(path, 'procs'), 'r') as p:
        for proc in json.loads(p.read()):
            for input in proc.get('input', []):
                for output in proc.get('output', []):
                    graph.add_dependency(
                        files[input]['name'],
                        files[output]['name']
                    )

    return graph


def parse_files(path):
    """Finds files written and read during a clean build."""

    # Find all files.
    with open(os.path.join(path, 'files'), 'r') as f:
        files = {}
        for file in json.loads(f.read()):
            files[file['id']] = file

    # For each process, find the outputs.
    inputs = set()
    outputs = set()
    with open(os.path.join(path, 'procs'), 'r') as p:
        for proc in json.loads(p.read()):
            inputs = inputs | set(proc.get('input', []))
            outputs = outputs | set(proc.get('output', []))

    def persisted(uid):
        if files[uid].get('deleted', False):
            return False
        if not files[uid].get('exists', False):
            return False
        name = files[uid]['name']
        return os.path.exists(name) and not os.path.isdir(name)

    inputs = {files[uid]['name'] for uid in inputs if persisted(uid)}
    outputs = {files[uid]['name'] for uid in outputs if persisted(uid)}

    return inputs, outputs
