#!/usr/bin/env python

import argparse
import os
import subprocess
import stat
import sys
import time

from collections import defaultdict
from graph import parse_graph, parse_files
from proc import run_proc
from mtime import read_mtimes



SCRIPT_PATH = os.path.dirname(os.path.abspath(__file__))
PROJECT_PATH = os.path.abspath(os.path.join(SCRIPT_PATH, os.pardir, os.pardir))
TOOL_PATH = os.path.join(PROJECT_PATH, 'build', 'mkcheck')



class Project(object):
    """Generic project: automake, cmake, make etc."""

    def filter(self, f):
        """Decides if the file is relevant to the project."""

        if not os.access(f, os.W_OK):
            return False
        if f == TOOL_PATH:
            return False
        for ending in ['.pyc']:
            if f.endswith(ending):
                return False
        return True

    def is_output(self, f):
      """Decides if a file should be considered an output."""

      for ending in ['.pyc', '.pyo']:
          if f.endswith(ending):
              return False

      return True


class Make(Project):

    def __init__(self, root, tmpPath):
        self.projectPath = root
        self.buildPath = root
        self.tmpPath = tmpPath

        with open(os.devnull, 'w') as devnull:
          code = subprocess.Popen(
            ['make', '--dry-run', 'clean'],
            stdout=devnull,
            cwd=root
          ).wait()
        self.has_clean = code == 0

    def clean_build(self):
        """Performs a clean build of the project."""

        # Clean the project.
        self.clean()

        # Run the build with mkcheck.
        run_proc(
          [ TOOL_PATH, "--output={0}".format(self.tmpPath), "--", "make" ],
          cwd=self.buildPath
        )

    def clean(self):
        """Cleans the project."""

        if self.has_clean:
          run_proc([ "make", "clean" ], cwd=self.buildPath)
        else:
          run_proc([ "git", "clean", "-f" ], cwd=self.buildPath)

    def build(self):
        """Performs an incremental build."""

        run_proc([ "make", "MALLOC=libc"], cwd=self.buildPath)
    
    def filter(self, f):
        """Decides if the file is relevant to the project."""

        if not super(Make, self).filter(f):
            return False

        for ending in ['Makefile']:
            if f.endswith(ending):
                return False

        return True


class CMakeProject(Project):
    """Project relying on CMake."""

    def __init__(self, projectPath, buildPath, tmpPath):
        self.projectPath = projectPath
        self.tmpPath = tmpPath
        self.buildPath = buildPath

        if not os.path.isdir(self.buildPath):
            raise RuntimeError('Missing build directory')

    def clean_build(self):
        """Performs a clean build of the project."""

        # Clean the project.
        self.clean()

        # Run the build with mkcheck.
        run_proc(
          [ TOOL_PATH, "--output={0}".format(self.tmpPath), "--" ] + self.BUILD,
          cwd=self.buildPath
        )

    def clean(self):
        """Cleans the project."""

        run_proc(self.CLEAN, cwd=self.buildPath)

    def build(self):
        """Performs an incremental build."""

        run_proc(self.BUILD, cwd=self.buildPath)

    FILTER_EXT = [
      '.h', '.cpp', '.cmake', '.cmake.in', '.c', '.cc', '.C',
      '.make', '.marks', '.includecache', '.check_cache', '.hpp',
    ]

    FILTER_FILE = [
       'CMakeLists.txt', 'flgas.make', 'depend.internal', 'link.txt',
       'Makefile2', 'Makefile', 'CMakeCache.txt', 'feature_tests.cxx',
       '.ninja_deps', '.ninja_log'
    ]

    def filter(self, f):
        """Decides if the file is relevant to the project."""

        if not super(CMakeProject, self).filter(f):
            return False

        if self.buildPath != self.projectPath and f.startswith(self.buildPath):
            return False
        if not f.startswith(self.projectPath):
            return False
        for ending in self.FILTER_EXT:
            if f.endswith(ending):
                return False

        name = os.path.basename(f)
        if name in self.FILTER_FILE:
            return False
        return True

    def is_output(self, f):
        if not super(CMakeProject, self).is_output(f):
            return False

        for ending in ['.internal', '.includecache']:
            if f.endswith(ending):
                return False

        name = os.path.basename(f)
        if name in self.FILTER_FILE:
            return False

        return True



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

    inputs, outputs, built_by = parse_files(project.tmpPath)
    graph = parse_graph(project.tmpPath)
    t0 = read_mtimes(outputs)


    if len(files) == 0:
        fuzzed = sorted([f for f in inputs - outputs if project.filter(f)])
    else:
        fuzzed = [os.path.abspath(f) for f in files]
    
    count = len(fuzzed)
    for idx, input in zip(range(count), fuzzed):
        print '[{0}/{1}] {2}:'.format(idx + 1, count, input)

        # Touch the file.
        os.utime(input, None)
        # Run the incremental build.
        project.build()

        t1 = read_mtimes(outputs)

        # Find the set of changed files.
        modified = set()
        for k, v in t0.iteritems():
            if v != t1[k] and project.is_output(k):
                modified.add(k)

        # Find expected changes.
        deps = graph.find_deps(input)
        expected = [f for f in deps & outputs if project.is_output(f)]

        # Report differences.
        if modified != expected:
            over = False
            under = False
            for f in sorted(modified):
                if f not in expected:
                    over = True
                    print '  + {} ({})'.format(f, built_by[f])

            for f in sorted(expected):
                if f not in modified:
                    under = True
                    print '  + {} ({})'.format(f, built_by[f])

            if under:
                project.clean()
                project.build()
                t1 = read_mtimes(outputs)

        t0 = t1


def query(project, files):
    """Queries the dependencies of a set of files."""

    graph = parse_graph(project.tmpPath)

    for f in files:
        path = os.path.abspath(f)
        print f, ':'
        for dep in sorted(graph.find_deps(path)):
            skip = False
            for dir in ['/proc/', '/tmp/', '/dev/']:
                if dep.startswith(dir):
                    skip = True
                    break
            if dep == path or skip:
                continue
            if dep.startswith(project.projectPath):
                dep = dep[len(project.projectPath) + 1:]
            print '  ', dep


def list_files(project, files):
    """Lists the files in the project to be fuzzed."""

    inputs, outputs, built_by = parse_files(project.tmpPath)
    graph = parse_graph(project.tmpPath)
    
    if len(files) == 0:
        fuzzed = sorted([f for f in inputs - outputs if project.filter(f)])
    else:
        fuzzed = [os.path.abspath(f) for f in files]

    count = len(fuzzed)
    for idx, input in zip(range(count), fuzzed):
        print input


def test_parse(project, path):
  """Compares the dynamic graph to the parsed one."""

  inputs, outputs, built_by = parse_files(project.tmpPath)
  graph = parse_graph(project.tmpPath)

  fuzzed = sorted([f for f in inputs - outputs if project.filter(f)])
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
      print '[{0}/{1}] {2}:'.format(idx + 1, count, input)

      expected = graph.find_deps(input) & outputs
      actual = traverse_graph(input, set())
      if actual != expected:
        for f in sorted(actual):
          if f not in expected:
            print '  +', f

        for f in sorted(expected):
          if f not in actual:
            print '  -', f

def get_project(root, args):
    """Identifies the type of the project."""

    if os.path.isfile(os.path.join(root, 'CMakeLists.txt')):
        if os.path.isfile(os.path.join(root, 'build', 'CMakeCache.txt')):
            return CMakeMake(root, os.path.join(root, 'build'), args.tmp_path)
        if os.path.isfile(os.path.join(root, 'CMakeCache')):
            return CMakeMake(root, root, args.tmp_path)

        if os.path.isfile(os.path.join(root, 'build', 'CMakeCache.txt')):
            return CMakeNinja(root, os.path.join(root, 'build'), args.tmp_path)
        if os.path.isfile(os.path.join(root, 'CMakeCache.txt')):
            return CMakeNinja(root, root, args.tmp_path)

    if os.path.isfile(os.path.join(root, 'Makefile')):
        return Make(root, args.tmp_path)

    raise RuntimeError('Unknown project type')


def main():
    parser = argparse.ArgumentParser(description='Build Fuzzer')

    parser.add_argument(
        '--tmp-path',
        type=str,
        default='/tmp/mkcheck',
        help='Path to the temporary output file'
    )
    parser.add_argument(
        'cmd',
        metavar='COMMAND',
        type=str,
        help='Command (query/fuzz)'
    )
    parser.add_argument(
        'files',
        metavar='FILES',
        type=str,
        nargs='*',
        help='Input files'
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
        test_parse(project, args.files[0])
        return

    raise RuntimeError('Unknown command: ' + args.cmd)



if __name__ == '__main__':
    main()
