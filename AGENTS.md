# yolk

Deliver implementation in `src/`, public examples in `examples/`, and tests
in `test/`. `logs/` contains historical research, not the library.

- GHC 9.14.1, GHC2024, hkd 0.2.1; keep both backends and ST/PrimMonad.
- At most three co-working agents including descendants; default to two.
  Run builds and benchmarks sequentially. Cabal is configured with one job.
- Read the testing-patterns skill before adding tests. Follow the existing
  intake in `logs/2026-10-08-1152-yolk-intake/` for requirements.
- Do not weaken accepted tests or claim zero allocation from static checks.
- Keep type constructors for graph identities and snapshots abstract.
- Use local atomic commits; do not push. Do not add attribution trailers.
- Validate with `cabal build all --offline --ghc-options=-Werror` and
  `cabal test all --offline --test-show-details=direct`.
