# mkcheck

Incremental Build Verification

# Supported Platforms

Due to use of the ```ptrace``` system call, mkcheck only supports Linux-based operating systems at this time.
Python 2 is required for fuzz testing. ```mkcheck``` only compiles with clang at this time.

# Building

```
mkdir -C Release
cd Release
cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=clang++
make
```

# Running tests

```
make -C tests/make test
make -C tests/parallel test
```

## Dependency Graph Inference

The dependency graph inference tool runs a child process under ptrace and dumps information about all files and processes involved in a build to a database file, specified using the ```--output``` flag. The file is in JSON format and is read by the fuzz testing tool. More information about the format is available in ```tools/fuzz_test/graph.py```.

```
mkcheck --output=<path-to-graph> -- <build command>
```

## Build Fuzzing

Fuzz testing is performed using a Python script, executed out of the directory of the project under test. The dependency graph must be inferred beforehand and stored at a path accesible to the fuzzing tool.

```
cd <project-dir>
python fuzz_test.py --graph-path=<path-to-graph> fuzz|list|query|build
```

Other options to the fuzzing script are:
* ```--rule-path``` specifies the path to a YAML file containing regexes to filter out files. The document should be a map from the keys ```"filter_in"```, ```"filter_out"```, ```"filter_tmp"``` to lists of regexes which blacklist irrelevant inputs, outputs and files considered for race testing.
* ```--use-hash``` forces the use of content hashing.
* ```--argv``` specifies additional arguments to GNU Make projects.

# License

The source code of this project is available under the MIT License.
