# mkcheck

Incremental Build Verification


# Building

```
mkdir -C Release
cd Release
cmake .. -DCMAKE_BUILD_TYPE=Release
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

