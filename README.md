To run: `sbt jmh:run`

### maxX: 10, maxY: 10
```bash
[info] Benchmark                        Mode  Cnt    Score    Error  Units
[info] Benchmarks.benchmarkBase        thrpt    5  355.245 ± 19.388  ops/s
[info] Benchmarks.benchmarkBaseFilter  thrpt    5  272.447 ± 42.122  ops/s
[info] Benchmarks.benchmarkSgTree      thrpt    5  659.536 ± 11.433  ops/s
```

### maxX: 20, maxY: 20
```bash
[info] Benchmark                        Mode  Cnt   Score   Error  Units
[info] Benchmarks.benchmarkBase        thrpt    5  20.736 ± 2.192  ops/s
[info] Benchmarks.benchmarkBaseFilter  thrpt    5  15.561 ± 8.943  ops/s
[info] Benchmarks.benchmarkSgTree      thrpt    5  34.548 ± 4.912  ops/s
```

### maxX: 30, maxY: 30
```bash
[info] Benchmark                        Mode  Cnt  Score   Error  Units
[info] Benchmarks.benchmarkBase        thrpt    5  3.738 ± 2.059  ops/s
[info] Benchmarks.benchmarkBaseFilter  thrpt    5  3.347 ± 1.073  ops/s
[info] Benchmarks.benchmarkSgTree      thrpt    5  4.662 ± 3.477  ops/s
```

References: https://github.com/ghostdogpr/mtl-benchmarks
