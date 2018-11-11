# mkcheck

Incremental Build Verification

## Dependency Graph Inference

```
mkcheck --output=<path-to-graph> -- <build command>
```

## Build Fuzzing

```
cd <project-dir>
python fuzz_test.py --tmp-path=<path-to-graph> fuzz|list|query|build
```
