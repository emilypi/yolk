#!/bin/sh
set -eu
cd "$(dirname "$0")/.."
type_check_dir=$(mktemp -d "${TMPDIR:-/tmp}/yolk-types.XXXXXX")
trap 'rm -rf "$type_check_dir"' EXIT HUP INT TERM
for fixture in SortMismatch CoerceSort GraphRegion EscapeRegion TermOpticSort PatternOpticSort DagCoerceSort DagOpticSort; do
  if cabal exec --offline -- ghc -fno-code -XGHC2024 -package yolk \
      "test/compile-fail/$fixture.hs" > "$type_check_dir/$fixture.log" 2>&1; then
    printf 'FAIL: %s unexpectedly compiled\n' "$fixture"
    exit 1
  fi
  if ! grep -q "Couldn't match type" "$type_check_dir/$fixture.log"; then
    cat "$type_check_dir/$fixture.log"
    printf 'FAIL: %s failed for an unexpected reason\n' "$fixture"
    exit 1
  fi
  printf 'PASS: %s rejected at the type boundary\n' "$fixture"
done
