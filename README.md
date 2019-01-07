# mkcheck

Incremental Build Verification

# Supported Platforms

Due to use of the ```ptrace``` system call, mkcheck only supports Linux-based operating systems at this time.

# Building

```
mkdir -C Release
cd Release
cmake .. -DCMAKE_BUILD_TYPE=Release
make
```

# Running tests

```
make -C tests/make test
make -C tests/parallel test
```


## Dependency Graph Inference

```
mkcheck --output=<path-to-graph> -- <build command>
```

## Build Fuzzing

```
cd <project-dir>
python fuzz_test.py --tmp-path=<path-to-graph> fuzz|list|query|build
```

# License

The source code of this project is available under the MIT License.
