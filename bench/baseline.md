# Local baseline

`baseline.csv` contains the actual output of a complete 26-case run on
2026-10-12. It is the first recorded baseline for this suite, not a speedup
claim or a reference suitable for every machine.

## Environment

- Platform: macOS 26.6, build 25G5065a, arm64.
- CPU model: unavailable from the sandbox; compare on this same host or
  establish a baseline for the target machine.
- GHC 9.14.1, base 4.22.0.0, cabal-install 3.18.1.0; GHC2024, `-O2`.
- Local project procedure alignment: `-fproc-alignment=64`.
- Benchmark RTS: `-T -A32m`; default CPU time; one benchmark at a time.
- Packages: tasty-bench 0.5.1, tasty 1.5.4, statistics 0.16.5.0,
  vector 0.13.2.0, hkd 0.2.1, containers 0.8, primitive 0.9.1.0.
- Library revision: `d1a7a43501c73018e08edc0aa4f61de5837c1022`.
  The benchmark harness/configuration were uncommitted additions at capture.
- All helper agents and compilation jobs were finished before timing.
  Other desktop/OS load was not controlled.

## Capture

The command started at approximately 21:02:27 UTC. The benchmark runner
reported 46.01 seconds for the 26 timing cases; all completed successfully.

```sh
cabal bench yolk-bench --offline \
  --benchmark-options='--csv bench/baseline.csv --stdev 2 --timeout 3s'
```

The CSV contains picoseconds for mean CPU time and twice the estimated
standard deviation, plus allocated/copied bytes and peak memory. All 26
estimates exceeded the requested 2% relative standard deviation within the
time cap; observed relative standard deviations ranged from approximately
2.63% to 10.99%. This is a bounded local reference, not a precision-certified
baseline. Inspect dispersion and use longer measurements on a quiet host
for performance decisions. Do not widen the 10% gate merely to accept noise.

## Harness identity

SHA-256 at capture:

```text
56e187f6e99561f95f518bd13df4507f4ef40d70c4809f2452e2e6497853d3fe  bench/Main.hs
fce0d3d6fd5e3b412e25fe518c1fb7a2922f74bcf28792c50da7bb4e6656c86c  bench-support/BenchVectors.hs
f33d51136fd11e69fae1c6ee13045da677171b8d1fad0e61ddf22f010a3d0ea4  cabal.project
38b38e727498c24eb6302499e51f23a155558ea9114f51dfe6fa05d5cc9496cb  yolk.cabal
```

Keep the original measurements when a comparison fails. Investigate the
workload/environment and record any new baseline as an explicit update,
with its own command and environment metadata.
