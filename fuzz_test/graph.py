# This file is part of the mkcheck project.
# Licensing information can be found in the LICENSE file.
# (C) 2018 Nandor Licker. ALl rights reserved.

import os

from parser import get_procs, read_proc, find_names

class DependencyGraph(object):
    """Graph describing dependencies between file paths."""

    class Node(object):
        def __init__(self, path):
            self.path = path
            self.edges = []

    def __init__(self):
        self.nodes = {}

    def add_dependency(self, src, dst):
        if src not in self.nodes:
            self.nodes[src] = self.Node(src)
        if dst not in self.nodes:
            self.nodes[dst] = self.Node(dst)
        self.nodes[src].edges.append(dst)

    def find_deps(self, src):
        deps = set()
        def traverse(name):
            if name in self.nodes:
                for edge in self.nodes[name].edges:
                    deps.add(edge)
                    traverse(edge)
        traverse(src)
        return deps


def parse_graph(prefix, path):
    graph = DependencyGraph()

    names = find_names(prefix, path)

    for proc in get_procs(path):
        _, outputs, inputs = read_proc(os.path.join(path, str(proc)))
        for input in inputs:
            for output in outputs:
                if input in names and output in names:
                    graph.add_dependency(names[input], names[output])

    return graph
