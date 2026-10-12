# Benchmarks

The `yolk-bench` suite uses `tasty-bench` and runs one benchmark at a time.
Every vector runs on both mutable and persistent backends. Inputs are
deterministic and forced before timing. Each timed invocation creates a
fresh graph, performs the operation, and forces a strict result containing
the cost, certificate, and a traversal of the entire extracted witness.
Graph construction, rebuilding, and result traversal are included. These
are end-to-end workloads, not isolated timings of the search loop.

The source of the fixtures is `bench-support/BenchVectors.hs`. The same
fixtures have correctness regressions in the normal test suite and are
checked before a timing run, preventing an error or missing witness from
appearing as a speedup. No result is cached across measured invocations.

| Workload | Vectors | Independently expected result |
| --- | --- | --- |
| Interning | 256 distinct nodes; 4,096 insertions over 64 payloads | 256 and 64 retained nodes |
| Congruence | Two depth-64 chains, followed by a leaf union | Equivalent roots; 64 compound nodes plus two distinct leaf members |
| Rewriting | 32 nested identity operators | One-node finite tree |
| Tree extraction | Shared diamonds of depth 8 and 12 | 511 and 8,191 tree occurrences |
| DAG extraction | Shared diamonds of depth 12 and 128 | 13 and 129 distinct nodes |
| Weighted search | Star families with 6 and 10 choices | Cost `1 + 5*k`, size `1 + k` |
| Bounded search | Star with 16 choices and 64 expansions | Valid bounds around optimum 81; expansion cap respected |
| Cycles | 24-class cycle, with/without finite exits | Optimal cost 1 with 24 nodes; no finite DAG |

The star fixture has one root of weight 1 and `k` independent child
classes. Each class chooses a leaf of weight 5 or a weight-zero operator
whose child has weight 10. The optimum is immediate by inspection, while
the cheapest-member search bound initially ignores those expensive children.
This exercises branching rather than only single-choice graphs.

## Run and compare

```sh
cabal build all --offline --ghc-options=-Werror
cabal test all --offline --test-show-details=direct

# Run the complete suite and retain its actual timings and allocation data.
cabal bench yolk-bench --offline \
  --benchmark-options='--csv bench/current.csv --stdev 2 --timeout 3s'

# Use a comparable local baseline and fail on a measurable slowdown over 10%.
cabal bench yolk-bench --offline \
  --benchmark-options='--baseline bench/baseline.csv --fail-if-slower 10 --csv bench/current.csv --stdev 2 --timeout 3s'

# Validate matching benchmark names and summarize runtime ratios.
cabal run yolk-bench-report --offline -- bench/baseline.csv bench/current.csv

# Verify that the native timing gate actually rejects a synthetic regression.
sh test/check-bench-gate.sh
```

The native gate compares each benchmark and accounts for the reported timing
dispersion. The separate report requires identical benchmark-name sets,
rejects duplicates and malformed/nonfinite timings, and reports raw mean
ratios. Run both when checking regressions: `tasty-bench` alone skips names
that have no matching baseline. A filtered run needs a matching filtered
baseline for the report; do not silently compare a partial suite to a full one.

`statistics` is used only in the comparison tool and its tests. It computes
the geometric mean of per-workload runtime ratios using log means, and the
sample standard deviation of those log ratios. These describe variation
across different workloads; they are not timing confidence intervals or
significance tests. Absolute runtimes of unrelated workloads are not pooled.
The aggregate cannot override a failing individual timing regression.

The checked-in [baseline metadata](baseline.md) records how `baseline.csv`
was measured. It is a local reference, not a portable performance promise.
Generate a new baseline on the machine/compiler/build configuration used
for comparisons, and inspect changes before accepting a baseline update:

```sh
cabal bench yolk-bench --offline \
  --benchmark-options='--csv bench/new-baseline.csv --stdev 2 --timeout 3s'
```

Keep the machine otherwise idle, use the same compiler/dependencies and RTS
settings, and run builds and timing suites sequentially. The local Cabal
project enables `-O2` and 64-byte procedure alignment for the library and
harness. The benchmark executable enables GC statistics and a fixed 32 MiB
nursery (`-T -A32m`). CPU time is the default. Timeout bounds a measurement
attempt and can leave it short of the requested precision; inspect the CSV
dispersion before treating a noisy result as a regression. Allocation and
copying figures are reported by `tasty-bench`; they do not establish the
original zero-allocation acceptance criterion.

This is an initial local suite. Upstream extraction-gym coverage, the full
v1 benchmark matrix, and hegg comparisons remain outstanding.

API references: [tasty-bench](https://github.com/Bodigrim/tasty-bench) and
[Statistics.Sample](https://hackage-content.haskell.org/package/statistics-0.16.5.0/docs/Statistics-Sample.html).
