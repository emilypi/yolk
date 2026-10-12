#!/bin/sh
set -eu
cd "$(dirname "$0")/.."
bench_gate_dir=$(mktemp -d "${TMPDIR:-/tmp}/yolk-bench-gate.XXXXXX")
trap 'rm -rf "$bench_gate_dir"' EXIT HUP INT TERM

cabal build yolk-bench --offline
bench_gate_exe=$(cabal list-bin yolk-bench --offline)

# These deliberately impossible reference times test the gate's wiring, not
# performance. Never use them as real baselines. The name must match exactly.
cat > "$bench_gate_dir/slow.csv" <<'EOF'
Name,Mean (ps),2*Stdev (ps)
All.mutable.intern/fresh-256,1000000000000000,0
EOF
cat > "$bench_gate_dir/fast.csv" <<'EOF'
Name,Mean (ps),2*Stdev (ps)
All.mutable.intern/fresh-256,1,0
EOF

if ! "$bench_gate_exe" --pattern '$0 == "All.mutable.intern/fresh-256"' \
    --baseline "$bench_gate_dir/slow.csv" --fail-if-slower 10 --stdev 2 --timeout 3s \
    > "$bench_gate_dir/pass.log" 2>&1; then
  cat "$bench_gate_dir/pass.log"
  printf 'FAIL: the control benchmark did not complete successfully\n'
  exit 1
fi
if ! grep -q 'less than baseline' "$bench_gate_dir/pass.log"; then
  cat "$bench_gate_dir/pass.log"
  printf 'FAIL: the baseline did not match the benchmark\n'
  exit 1
fi

if "$bench_gate_exe" --pattern '$0 == "All.mutable.intern/fresh-256"' \
    --baseline "$bench_gate_dir/fast.csv" --fail-if-slower 10 --stdev 2 --timeout 3s \
    > "$bench_gate_dir/fail.log" 2>&1; then
  cat "$bench_gate_dir/fail.log"
  printf 'FAIL: an intentional timing regression was accepted\n'
  exit 1
fi
if ! grep -q 'more than baseline' "$bench_gate_dir/fail.log"; then
  cat "$bench_gate_dir/fail.log"
  printf 'FAIL: the benchmark failed for an unexpected reason\n'
  exit 1
fi
printf 'PASS: native 10%% timing gate accepts the control and rejects a regression\n'
