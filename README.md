To run: `sbt jmh:run`

## ChunkWidth: 16, ChunkHeight: 16, structureWidth: 5, structureHeight: 5

### maxX: 10, maxY: 10
```bash
[info] Benchmark                                   Mode  Cnt    Score    Error  Units
[info] Benchmarks.benchmarkBaseFilter             thrpt    5   31.166 ±  1.345  ops/s
[info] Benchmarks.benchmarkBaseFind               thrpt    5   42.386 ±  0.391  ops/s
[info] Benchmarks.benchmarkSgTreeFindAndContains  thrpt    5   80.501 ±  2.539  ops/s
[info] Benchmarks.benchmarkSgTreeForall           thrpt    5  675.408 ± 13.023  ops/s
[info] Benchmarks.benchmarkSgTreeGetAll           thrpt    5  201.539 ±  5.105  ops/s
```

### maxX: 20, maxY: 20
```bash
[info] Benchmark                                   Mode  Cnt    Score    Error  Units
[info] Benchmarks.benchmarkBaseFilter             thrpt    5    1.948 ±  0.201  ops/s
[info] Benchmarks.benchmarkBaseFind               thrpt    5    2.503 ±  0.084  ops/s
[info] Benchmarks.benchmarkSgTreeFindAndContains  thrpt    5    3.561 ±  1.370  ops/s
[info] Benchmarks.benchmarkSgTreeForall           thrpt    5  140.263 ± 40.585  ops/s
[info] Benchmarks.benchmarkSgTreeGetAll           thrpt    5   49.409 ±  1.094  ops/s
```

### maxX: 30, maxY: 30
```bash
[info] Benchmark                                   Mode  Cnt   Score    Error  Units
[info] Benchmarks.benchmarkBaseFilter             thrpt    5   0.358 ±  0.025  ops/s
[info] Benchmarks.benchmarkBaseFind               thrpt    5   0.468 ±  0.010  ops/s
[info] Benchmarks.benchmarkSgTreeFindAndContains  thrpt    5   0.554 ±  0.017  ops/s
[info] Benchmarks.benchmarkSgTreeForall           thrpt    5  62.815 ± 18.904  ops/s
[info] Benchmarks.benchmarkSgTreeGetAll           thrpt    5  22.570 ±  1.042  ops/s
```

References: https://github.com/ghostdogpr/mtl-benchmarks
