#!/usr/bin/env python

from __future__ import print_function

import argparse
import os
import yaml
import re
import resource
import subprocess
import stat
import sys
import tempfile
import time

from collections import defaultdict
from graph import parse_graph
from proc import run_proc
from mtime import read_mtimes



SCRIPT_PATH = os.path.dirname(os.path.abspath(__file__))
PROJECT_PATH = os.path.abspath(os.path.join(SCRIPT_PATH, os.pardir, os.pardir))
TOOL_PATH = os.path.join(PROJECT_PATH, 'build', 'mkcheck')

DELAY=1



class HashTouchContext(object):
    """Context touching a file by adding a dummy extension."""

    TEXT_EXT = [
        '.asm', '.asm-generic', '.awk', '.S', '.bc', '.ld',
        '.conf', '.l', 'VERSION', 'imgdesc', '.py', '.txt',
        '.config', '.s', '.h'
    ]

    def __init__(self, path):
        self.path = path
        self.tmp = tempfile.TemporaryFile()
    
    def __enter__(self):
        time.sleep(DELAY)
        with open(self.path, 'rb') as f:
            data = f.read()
            is_text = data and data[0] == '#'
            self.tmp.write(data)

        with open(self.path, 'ab') as f:
            if not is_text:
                for ext in self.TEXT_EXT:
                    if self.path.endswith(ext):
                        is_text = True
                        break
            f.write('\n' if is_text else '\0')
        time.sleep(DELAY)
    
    def __exit__(self, type, value, tb):
        self.tmp.seek(0)
        with open(self.path, 'wb') as f:
            f.write(self.tmp.read())
        self.tmp.close()


class TimeTouchContext(object):
    def __init__(self, path):
        self.path = path
    
    def __enter__(self): 
        time.sleep(DELAY)
        if os.path.exists(self.path): os.utime(self.path, None)
        time.sleep(DELAY)
    
    def __exit__(self, type, value, tb): 
        pass



class Project(object):
    """Generic project: automake, cmake, make etc."""
    
    # Based on past information, quite a few files are very 
    # likely to be correct, thus they are excluded from
    # fuzzing in order to save on execution time.
    FILTER_IN  = []
    FILTER_TMP = []
    FILTER_OUT = []
    
    def __init__(self, rule_file, use_hash, args):
        self._use_hash = use_hash
        self._args = args

        if rule_file:
            with open(rule_file, 'r') as f:
                data = yaml.load(f.read())
            filter_in = data.get('filter_in', [])
            filter_tmp = data.get('filter_tmp', [])
            filter_out = data.get('filter_out', [])
        else:
            filter_in = self.FILTER_IN
            filter_tmp = self.FILTER_TMP
            filter_out = self.FILTER_OUT

        self._filter_in = [re.compile('^' + f + '$') for f in filter_in]
        self._filter_tmp = [re.compile('^' + f + '$') for f in filter_tmp]
        self._filter_out = [re.compile('^' + f + '$') for f in filter_out]

    def filter_in(self, f):
        """Decides if the file is relevant to the project."""

        if not os.access(f, os.W_OK):
            return False
        if f == TOOL_PATH:
            return False
        name = os.path.basename(f)
        if name.startswith('.'):
          return False

        for pattern in self._filter_in:
            if pattern.match(f):
                return False

        return True

    def filter_tmp(self, f):
        """Filters out uninteresting temporaries."""
        
        if f == TOOL_PATH:
            return False

        name = os.path.basename(f)
        if name.startswith('.'):
            return False
    
        for pattern in self._filter_tmp:
            if pattern.match(f):
                return False

        return True

    def is_output(self, f):
        """Decides if a file should be considered an output."""

        name = os.path.basename(f)
        if name.startswith('.'):
            return False
        
        for pattern in self._filter_out:
            if pattern.match(f):
                return False
        
        return True

    def touch(self, path):
        """Adjusts the content hash/timestamp of a file."""
        if self._use_hash:
            return HashTouchContext(path)
        else:
            return TimeTouchContext(path)


class Make(Project):
    
    FILTER_IN  = [
        '.*\.pyc',
        '.*\Makefile',
        '.*\.d',
    ]
    FILTER_TMP = [
        '.*\.d',
    ]
    FILTER_OUT = [
        '.*\.d',
    ]
    
    def __init__(self, root, graph, rule_file, use_hash, args):
        super(Make, self).__init__(rule_file, use_hash, args)
        self.projectPath = root
        self.buildPath = root
        self.graph = graph

        with open(os.devnull, 'w') as devnull:
          code = subprocess.Popen(
            ['make', '--dry-run', 'clean'],
            stdout=devnull,
            stderr=devnull,
            cwd=root
          ).wait()
        self.has_clean = code == 0

    def clean_build(self):
        """Performs a clean build of the project."""
    
        # Run the build with mkcheck.
        run_proc(
          [ TOOL_PATH, "--output={0}".format(self.graph), "--", "make" ],
          cwd=self.buildPath
        )

    def clean(self):
        """Cleans the project."""

        if self.has_clean:
          run_proc([ "make", "clean" ], cwd=self.buildPath)
        else:
          run_proc([ "git", "clean", "-fdx" ], cwd=self.buildPath)

    def build(self):
        """Performs an incremental build."""

        run_proc([ "make"] + self._args, cwd=self.buildPath)

    def in_project(self, f):
        """Checks if a file is in the project."""

        return f.startswith(self.projectPath)


class SCons(Project):

    FILTER_IN = [
        '.*\.c', 
        '.*\.cc', 
        '.*\.cpp', 
        '.*\.h', 
        '.*\.hpp', 
        '.*\.i', 
        '.*\.ipp', 
        '.*\.o', 
        '.*\.pyc', 
        '.*\.sconf_temp',
        '.*/SConscript', 
        '.*/SConstruct',
        '.*scons.*',
    ]
    FILTER_TMP = [
        '.*\.o', 
        '.*\.dblite', 
        '.*\.a'
    ]
    FILTER_OUT = [
        '.*\.internal', 
        '.*\.includecache'
    ]

    def __init__(self, root, graph, rule_file, use_hash, args):
        super(SCons, self).__init__(rule_file, use_hash, args)
        self.projectPath = root
        self.buildPath = root
        self.graph = graph

    def clean_build(self):
        """Performs a clean build of the project."""

        # Build once - needed for some projects.
        run_proc(['scons'])

        # Clean the project.
        self.clean()

        # Run the build with mkcheck.
        run_proc(
          [ TOOL_PATH, "--output={0}".format(self.graph), "--", "scons" ],
          cwd=self.buildPath
        )

    def clean(self):
        """Cleans the project."""
        
        run_proc([ "scons", "--clean" ], cwd=self.buildPath)

    def build(self):
        """Performs an incremental build."""

        run_proc([ "scons", "-Q" ], cwd=self.buildPath)
    
    def filter_in(self, f):
        """Decides if the file is relevant to the project."""

        if not super(SCons, self).filter_in(f):
            return False
        if not f.startswith(self.projectPath):
            return False
        return True
    
    def in_project(self, f):
        """Checks if a file is in the project."""

        return f.startswith(self.projectPath)
  

class CMakeProject(Project):
    """Project relying on CMake."""
    
    FILTER_IN = [
        '.*\.cpp', 
        '.*\.cmake', 
        '.*\.cmake.in', 
        '.*\.c', 
        '.*\.h',
        '.*\.hpp',
        '.*\.cc', 
        '.*\.C',
        '.*\.make', 
        '.*\.mk',
        '.*\.marks', 
        '.*\.includecache', 
        '.*\.check_cache',
        '.*\.pyc',
        '.*/Doxyfile\.in',
        '.*/CMakeLists.txt', 
        '.*/flgas.make', 
        '.*/depend.internal', 
        '.*/link.txt',
        '.*/Makefile2', 
        '.*/Makefile', 
        '.*/CMakeCache.txt', 
        '.*/feature_tests.cxx',
        '.*/.ninja_deps', 
        '.*/.ninja_log',
    ]

    FILTER_TMP = [
        '.*\.output', 
        '.*\.includecache', 
        '.*\.internal', 
        '.*\.make', 
        '.*\.a', 
        '.*\.o', 
        '.*\.so',
    ]

    FILTER_OUT = [
        '.*\.internal', 
        '.*\.includecache', 
        '.*\.make',
        '.*swig.*',
        '.*doxygen.*',
        '.*INFO.*',
    ]

    def __init__(self, projectPath, buildPath, graph, rule_file, use_hash, args):
        super(CMakeProject, self).__init__(rule_file, use_hash, args)
        self.projectPath = projectPath
        self.graph = graph
        self.buildPath = buildPath

        if not os.path.isdir(self.buildPath):
            raise RuntimeError('Missing build directory')

    def clean_build(self):
        """Performs a clean build of the project."""

        # Build once - needed for some projects.
        run_proc(self.BUILD, cwd=self.buildPath)
    
        # Clean the project.
        self.clean()

        # Run the build with mkcheck.
        run_proc(
          [ TOOL_PATH, "--output={0}".format(self.graph), "--" ] + self.BUILD,
          cwd=self.buildPath
        )

    def clean(self):
        """Cleans the project."""

        run_proc(self.CLEAN, cwd=self.buildPath)

    def build(self):
        """Performs an incremental build."""

        run_proc(self.BUILD, cwd=self.buildPath)

    def filter_in(self, f):
        """Decides if the file is relevant to the project."""
        
        if not super(CMakeProject, self).filter_in(f):
            return False
        if self.buildPath != self.projectPath and f.startswith(self.buildPath):
            return False
        if not f.startswith(self.projectPath):
            return False
        return True
    
    def filter_tmp(self, f):
        """Decides if an internal file is relevant for race detection."""

        if not super(CMakeProject, self).filter_tmp(f):
            return False 
        if not f.startswith(self.projectPath):
            return False
        return True
    
    def in_project(self, f):
        """Checks if a file is in the project."""

        return f.startswith(self.projectPath) or f.startswith(self.buildPath)


class CMakeMake(CMakeProject):
    """CMake project built using make."""

    BUILD = [ 'make', '-j1' ]
    CLEAN = [ 'make', 'clean' ]

class CMakeNinja(CMakeProject):
    """CMake project built using ninja."""

    BUILD = [ 'ninja', '-j1' ]
    CLEAN = [ 'ninja', 'clean' ]


def build_tool():
    """Builds mkcheck."""

    if os.path.isfile(os.path.join(PROJECT_PATH, 'build', 'build.ninja')):
        run_proc([ 'ninja' ], cwd=os.path.join(PROJECT_PATH, 'build'))
        return

    if os.path.isfile(os.path.join(PROJECT_PATH, 'build', 'Makefile')):
        run_proc([ 'make' ], cwd=os.path.join(PROJECT_PATH, 'build'))
        return

    raise RuntimeError('Cannot rebuild mkcheck')


def fuzz_test(project, files):
    """Find the set of inputs and outputs, as well as the graph."""

    project.clean()
    project.build()

    inputs, outputs, built_by, graph = parse_graph(project.graph)

    if len(files) == 0:
        fuzzed = sorted([f for f in inputs - outputs if project.filter_in(f)])
    else:
        fuzzed = [os.path.abspath(f) for f in files]
   
    count = len(fuzzed)
    for idx, input in zip(range(count), fuzzed):
        print('[{0}/{1}] {2}:'.format(idx + 1, count, input))

        # Touch the file, run the incremental build and read timestamps.
        t0 = read_mtimes(outputs)
        with project.touch(input): project.build()
        t1 = read_mtimes(outputs)
        
        # Find the set of changed files.
        modified = set()
        for k, v in t0.items():
            if v < t1[k] and project.is_output(k):
                modified.add(k)
        
        # Reset the project.
        for f in outputs:
            if os.path.exists(f):
                os.utime(f, None)

        # Find expected changes.
        deps = graph.find_deps(input)
        expected = {f for f in deps & outputs if project.is_output(f)}

        # Report differences.
        if modified != expected:
            redundant = graph.prune_transitive(modified - expected)
            for f in sorted(redundant):
                print('  + {} ({})'.format(f, built_by[f]))
            
            missing = graph.prune_transitive(expected - modified)
            for f in sorted(missing):
                print('  - {} ({})'.format(f, built_by[f]))



def query(project, files):
    """Queries the dependencies of a set of files."""

    _, _, built_by, graph = parse_graph(project.graph)

    for f in files:
        path = os.path.abspath(f)
        print(f, ':')
        for dep in sorted(graph.find_deps(path)):
            skip = False
            for dir in ['/proc/', '/tmp/', '/dev/']:
                if dep.startswith(dir):
                    skip = True
                    break
            if dep == path or skip or not project.is_output(dep):
                continue
            if dep.startswith(project.projectPath):
                dep = dep[len(project.projectPath) + 1:]
            print('  ', dep)


def list_files(project, files):
    """Lists the files in the project to be fuzzed."""

    inputs, outputs, built_by, graph = parse_graph(project.graph)
    if len(files) == 0:
        fuzzed = sorted([f for f in inputs - outputs if project.filter_in(f)])
    else:
        fuzzed = [os.path.abspath(f) for f in files]
    
    count = len(fuzzed)
    for idx, input in zip(range(count), fuzzed):
        print(input)


def parse_test(project, path):
  """Compares the dynamic graph to the parsed one."""

  inputs, outputs, built_by, graph = parse_graph(project.graph)

  fuzzed = sorted([f for f in inputs - outputs if project.filter_in(f)])
  count = len(fuzzed)

  root = project.buildPath

  G = defaultdict(list)
  with open(path, 'r') as f:
    for line in f.readlines():
      src, deps = line.strip().split(':')
      src = os.path.normpath(os.path.join(root, src))
      for dep in (w.strip() for w in deps.split(', ')):
        G[os.path.normpath(os.path.join(root, dep))].append(src)

  def traverse_graph(node, viz):
    if node in viz:
      return viz

    for next in G[node]:
      viz.add(node)
      traverse_graph(next, viz)
    return viz

  for idx, input in zip(range(count), fuzzed):
      print('[{0}/{1}] {2}:'.format(idx + 1, count, input))

      expected = graph.find_deps(input) & outputs
      actual = traverse_graph(input, set())
      if actual != expected:
        for f in sorted(actual):
          if f not in expected:
            print('  +', f)

        for f in sorted(expected):
          if f not in actual:
            print('  -', f)

def race_test(project):
    """Test for race conditions."""
    
    inputs, outputs, built_by, graph = parse_graph(project.graph)
    fuzzed = {f for f in outputs & inputs if project.filter_tmp(f)}
    
    # Create a copy of the graph from which missing edges will be removed.
    build_graph = defaultdict(set)
    for f, node in graph.nodes.iteritems():
        for next in node.edges:
            build_graph[next].add(f)
    
    project.clean()
    project.build()
    
    missing_edges = []
    for input in sorted(fuzzed):
        deps = graph.find_deps(input)
        if len(deps) == 1 and input in deps:
            continue

        # Touch the file, run the incremental build and read timestamps.
        t0 = read_mtimes(outputs)
        with project.touch(input): project.build()
        t1 = read_mtimes(outputs)
        
        # Find the set of changed files.
        modified = set()
        for k, v in t0.iteritems():
            if v != t1[k] and project.is_output(k):
                modified.add(k)
        
        # Reset the project.
        for f in outputs:
            if os.path.exists(f):
                os.utime(f, None)
        
        # Find expected changes.
        deps = graph.find_deps(input)
        expected = {f for f in deps & outputs if project.is_output(f)}
        
        if modified != expected:
            missing = expected - modified

            # Report differences.
            for f in {f for f in missing if graph.is_direct(input, f)}:
                if project.filter_tmp(f):
                    missing_edges.append((input, f))
                    build_graph[f].remove(input)
    
    # Find the best and worst time a node can be scheduled in the build graph.
    graphs = {}
    build_graphs = {}
    race_files = set()
    for node in graph.topo_order():
        self = {node} if node in outputs else set()
        
        graphs[node] = self.union(
            *[graphs[p] for p in graph.rev_nodes[node].edges]
        )
        build_graphs[node] = self.union(
            *[build_graphs[p] for p in build_graph[node]]
        )
            
        if graphs[node] > build_graphs[node]:
            race_files.add(node)
    
    print('Races:')
    for f in graph.prune_transitive(race_files):
        print(f, built_by[f])

    print('Missing edges:')
    for src, dst in missing_edges:
        print(src, ' -> ', dst)



def get_project(root, args):
    """Identifies the type of the project."""
    
    graph = args.graph_path
    rule_path = args.rule_path
    use_hash = args.use_hash
    argv = [args.argv] if args.argv else []

    # CMake builds.
    if os.path.isfile(os.path.join(root, 'CMakeCache.txt')):
        projectDir = os.path.normpath(os.path.join(root, os.pardir))
        if os.path.isfile(os.path.join(projectDir, 'CMakeLists.txt')):
            # Out of source.
            if os.path.isfile(os.path.join(root, 'Makefile')):
                return CMakeMake(projectDir, root, graph, rule_path, use_hash, argv)
            if os.path.isfile(os.path.join(root, 'build.ninja')):
                return CMakeNinja(projectDir, root, graph, rule_path, use_hash, argv)
        else:
            # In-source.
            if os.path.isfile(os.path.join(root, 'Makefile')):
                return CMakeMake(root, root, graph, rule_path, use_hash, argv)
            if os.path.isfile(os.path.join(root, 'build.ninja')):
                return CMakeNinja(root, root, graph, rule_path, use_hash, argv)
    
    # Manual GNU Make build.
    if os.path.isfile(os.path.join(root, 'Makefile')):
        return Make(root, graph, rule_path, use_hash, argv)

    # SCons build.
    if os.path.isfile(os.path.join(root, 'SConstruct')):
        return SCons(root, graph, rule_path, use_hash, argv)

    raise RuntimeError('Unknown project type')


def main():
    parser = argparse.ArgumentParser(description='Build Fuzzer')

    parser.add_argument(
        '--graph-path',
        type=str,
        default='/tmp/mkcheck',
        help='Path to the graph file'
    )
    parser.add_argument(
        'cmd',
        metavar='COMMAND',
        type=str,
        help='Command (build/fuzz/query/list/parse/race)'
    )
    parser.add_argument(
        'files',
        metavar='FILES',
        type=str,
        nargs='*',
        help='Input files'
    )
    parser.add_argument(
        '--rule-path',
        type=str,
        help='Path to the rule file'
    )
    parser.add_argument(
        '--use-hash',
        action='store_true',
        help='Change content hashes instead of timestamps'
    )
    parser.add_argument(
        '--argv',
        type=str,
        default='',
        help='Additional arguments to make'
    )

    args = parser.parse_args()

    buildDir = os.getcwd()
    project = get_project(buildDir, args)

    build_tool()

    if args.cmd == 'build':
        project.clean_build()
        return
    if args.cmd == 'fuzz':
        fuzz_test(project, args.files)
        return
    if args.cmd == 'query':
        query(project, args.files)
        return
    if args.cmd == 'list':
        list_files(project, args.files)
        return
    if args.cmd == 'parse':
        parse_test(project, args.files[0])
        return
    if args.cmd == 'race':
        race_test(project)
        return

    raise RuntimeError('Unknown command: ' + args.cmd)



if __name__ == '__main__':
    try:
        # Graphs traversals use dfs - raise stack limit here.
        resource.setrlimit(resource.RLIMIT_STACK, (2 ** 29, -1))
        sys.setrecursionlimit(10 ** 6)
    except:
        print('WARNING: cannot set rlimit')
    main()
