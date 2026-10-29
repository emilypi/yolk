# Core implementation

I use the tested many-sorted boundary as the starting point for the library.
The public API is in `src/`; research logs remain supporting material.
This document describes the code that exists and the requirements it has
not yet met.

| Module | Implemented responsibility | Source |
| --- | --- | --- |
| `Yolk.Language` | HKD language law, typed finite terms, opaque graph identities | `hkd-0.2.1`, `Data.HKD` |
| `Yolk.Backend` | Unboxed mutable and immutable persistent union-find, union by size, read-only find | Sedgewick and Wayne, weighted quick-union |
| `Yolk.EGraph` | Region-scoped runners, add/lookup, union, rebuilding, snapshots and membership | Willsey et al., egg, POPL 2021 |
| `Yolk.Rewrite` | Typed pattern registration, visible-member search, bounded search/apply iteration | Willsey et al., egg, POPL 2021 |
| `Yolk.Extract` | Configurable ordered tree-cost relaxation, finite-tree extraction through cyclic graphs | egg extraction; Knuth, 1977 |
| `Yolk.Extract.Dag` | Typed shared output, joint roots, additive Word64 objective and optimality certificates | Solver-free branch-and-bound in `Yolk.Internal.Dag` |
| `Yolk.Optics` | Typed term/pattern optics, record lenses, error and stop-reason prisms | `lens` |

## Identity and sorts

`ClassId r s` is opaque, with nominal roles for both its graph region and
sort. Rank-2 runners generate `r`. `runMutable` additionally quantifies the
`ST` token. A finite `Term l s` can leave the runner; an identity cannot
escape as an identity for a caller-chosen region. Compile-negative tests
cover mixed sorts, sort coercion, region mixing, and region escape.

Every allocated node retains its original syntax and sort witness. The
interned key contains the result `SomeTypeRep`, a language-supplied
constructor/payload key, and ordered canonical child IDs. A language must
supply an injective operator key within a result sort, including the sorts
of child positions. Violating that law invalidates interning semantics;
it cannot cause the core to perform an unchecked cast.

The current representation stores sort witnesses with identities and boxed
typed nodes in maps. Unboxed per-operator rows are pending. Their decoder
must preserve the same sort and region boundary before replacing this
representation.

## Backends and rebuilding

The backend class supplies allocation, find, union, and allocated-ID count.
Mutable parents and sizes occupy growable `MutablePrimArray`s under
`PrimMonad`. Persistent parents and sizes are immutable `IntMap`s, so old
backend values can be branched. Both choose the larger class as parent,
breaking equal-size ties by the smaller root ID. Find is read-only; no
search query path compresses mutable parents. Raw backend methods require
valid IDs; the graph validates IDs before invoking them.

Union marks the graph dirty. Rebuilding canonicalizes all child IDs and
merges duplicate keys, then repeats if congruence discovered another union.
Each such union reduces the number of classes, so the finite store reaches
a fixpoint. Retired node handles remain valid through the union-find and
original-node table. Collision membership is the disjunction of the two
visibility flags.

This whole-store algorithm is a correctness baseline. Replace it with
occurrence queues while retaining the behavioral and independent-model
tests. Performance work must measure both backends and cannot infer zero
allocation from `INLINABLE` alone.

## Search and extraction

A snapshot contains immutable typed rows and a complete map from allocated
IDs to the representatives at capture time. It contains no mutable array
references. Rewrites search a common snapshot before applying any result.
RHS variables must be bound by the LHS at the same sort; registration checks
this before the rule can be run.

Patterns enumerate visible members. They do not currently use the pinned
egg compiler's bound non-leaf memo optimization. After pruning, memo lookup
and member enumeration are distinct operations. The planned relational
compiler must retain an explicit policy here and maintain visibility in
its indexes and deltas. No whole-matcher equivalence to egg is claimed.

The runner exposes iteration, node, and match limits. It rejects a
truncated search without applying any of that iteration. A rewrite applies
atomically before the node-limit check. The reported graph is rebuilt even
on a budget stop. The current scheduler is deterministic and unthrottled;
backoff scheduling remains to be implemented.

Tree extraction relaxes costs over visible members. `extractWith` accepts
`TreeCost l c`, an algebra over an HKD node with `Const c` children. It
keeps one least-cost finite witness per class, preserving operand order
and multiplicity. The cost order must be total, reachable costs must have
no infinite descending chain, and the algebra must be monotone and
superior (a parent costs at least each child). Under these laws, repeated
strict improvements terminate at the least finite tree cost. Only strict
improvements replace a witness, including when zero-cost cycles exist.

A cycle with no finite visible derivation yields `Nothing`; cycles with
finite alternatives are supported. Every extraction computes fresh costs
over an immutable snapshot, so querying with a different model cannot
reuse costs from another objective. The existing whole-store relaxation
is retained; a priority-queue implementation remains a performance task.

The built-in size, depth and weighted-size algebras use `Word64`, with
saturating addition to preserve ordering on overflow. They optimize in
that saturated domain, so unrepresentable costs tie at `maxBound`.
`extract` wraps `extractWith astSize`, converting to `Int` or reporting
`CostOverflow`. Callers can choose an unbounded domain or a lexicographic
tuple through `extractWith`. This is not an optimal DAG extractor or a
claim that saturation has finished. The separate `Yolk.Extract.Dag` path
handles shared-node minimization.

## Solver-free DAG extraction

`extractDagOptimal` and `extractDagMany` work over a rebuilt snapshot.
Visible members become an internal table of canonical owners, row IDs,
distinct child classes, and nonnegative Word64 weights. The output retains
the original typed node layers, including child order and multiplicity.
Every demanded canonical e-class gets exactly one selected member; selected
edges must form a finite DAG. Multiple roots share this selection and cost.

The solver removes direct self-loops, computes productive classes to a
fixpoint, removes members with nonproductive children, and restricts the
problem to classes reachable from the requested roots. A discovery-order
seed supplies a finite incumbent: every seed edge points to an earlier
discovery, even with zero weights. If a root is nonproductive, extraction
returns `Nothing`; a search budget never causes `Nothing`.

Branch-and-bound maintains a selected map, distinct demanded-but-unchosen
classes, and the cost already spent. It chooses the class with the fewest
cycle-safe alternatives, breaking ties by descending minimum member cost
then class ID. Selecting a member demands its children. An edge is rejected
if it closes a path back to the class being assigned. Children of unchosen
classes have no selected edges yet, so any new cycle must pass through
the newly assigned class. Successors are explored by ascending lower bound.

The lower bound is the cost already spent plus the sum of minimum member
weights over distinct unchosen demands. Every completion must select one
member for each of those classes. Ignoring all other dependencies and
cycles can only underestimate its cost. The search prunes a branch when
that bound reaches the incumbent cost. Every feasible selection follows
one branch, so exhausting the remaining branches proves the incumbent
optimal. At the expansion cap, the minimum bound over pending branches
and the incumbent is still a valid global lower bound.

All sums saturate at `maxBound :: Word64`. Nonnegative saturating addition
is monotone, so the same proof holds in that domain, including zero costs
and saturated ties. Arithmetic cannot wrap into a cheaper result. This API
does not accept negative weights, arbitrary tree algebras, or use an external
solver. Search can take exponential work; the default 100,000-expansion cap
is a work policy, not calibrated throughput or a wall-clock bound.

Typed reconstruction memoizes one `Dag l s` per selected class, with dense
postorder IDs across all roots. Witness equality checks guard heterogeneous
lookup; no unchecked casts are used. Nodes can escape the graph runner but
cannot be forged or edited. `foldDagM` also memoizes by node ID and calls its
algebra once per reachable node. IDs are meaningful only within one
extraction result. A typed reconstruction inconsistency reports
`InvalidExtraction`, distinct from the normal absence of a finite witness.

`DagResult` uses named record fields for its feasible output, incumbent cost,
lower bound, proof status and expansion count, with `lens` lenses.
Edits are ordinary user changes and do not revalidate the certificate. For
an extractor-produced result, `Optimal`
means lower bound equals cost; `Unproven` retains the incumbent without
claiming a proof. The entire result concerns the current visible e-graph,
not equalities a future saturation run might discover.

This is the B&B-first increment from the accepted extraction design. The
treewidth-DP portfolio, stronger mandatory-set bounds, remaining reduction
passes, and upstream extraction-gym validation and benchmarks remain work.
No timings or coverage numbers from the earlier Python research are claims
about this Haskell implementation.

## Optics

`Yolk.Optics` is imported separately because its record lens names match
existing field accessors. An iso exposes a term's typed constructor layer;
prisms distinguish pattern variables and nodes at the same sort. Only a
validated rewrite's name is editable: changing either body still requires
the `rewrite` validator. There are no optics exposing graph identity indices,
region witnesses, or snapshot internals.

## Tests and acceptance

The source tests exercise both interpreters through the same public graph
programs, including stale identities, typed interning, congruence chains,
pruning/reinterning, snapshots, rewrite registration, budget stops, and
cyclic extraction. Property tests compare generated equalities with an
independent finite closure model and configurable minimum costs with
exhaustive enumeration of small finite grammars. Arithmetic boundary tests
check Word64 saturation against unbounded arithmetic in the test oracle.
Optic laws and compositions are exercised using actual `lens` operators.
The example combines numeric and boolean rules and checks several cost
models at runtime with both backends. DAG tests compare exact results and
budget certificates against independent exhaustive selections on small
cyclic/shared graphs, and cover typed sharing, visibility, stale identities,
multiple roots, and arithmetic saturation. Compile-negative DAG fixtures
reject sort coercion and cross-sort optic access. The DAG example emits
each shared operation once and checks its exact and budget-limited results.

Build and test with GHC 9.14.1:

```sh
cabal build all --ghc-options=-Werror
cabal test all --test-show-details=direct
sh test/check-types.sh
cabal run yolk-example
```

## Benchmarking

`yolk-bench` uses tasty-bench with 13 deterministic vectors on both backends.
The inputs and their closed-form expectations are shared with the regression
suite in `bench-support/BenchVectors.hs`. Measured calls build a fresh graph,
perform the operation, and fully force a strict observation including a
traversal of every extracted node. Vector setup and correctness checks run
before the timing loop. This boundary includes construction and observation;
it does not isolate extraction from graph building.

The initial workloads cover fresh/repeated interning, congruence propagation,
identity rewriting, tree versus DAG sharing, weighted branching, a bounded
search, and productive/nonproductive cycles. Synthetic node payloads and
costs use Word64. No production API or solver algorithm is changed for timing.

Native tasty-bench CSV baselines provide the existing 10% slowdown gate.
`yolk-bench-report` uses statistics for descriptive geometric means and
log-ratio dispersion across workloads, with strict matching-name validation.
The report does not infer statistical significance or replace individual
regression failures. A script checks that the native gate accepts a control
and rejects a deliberately synthetic regression baseline. The recorded local
baseline includes environment and command metadata in `bench/baseline.md`.
See `bench/README.md` for reproducible commands and measurement limits.

The full v1 benchmark matrix, upstream corpora, hegg comparisons, and
allocation acceptance gates are still outstanding.

## Next implementation increments

1. Add the generic analysis contract with value/witness pairing, parent
   remaking, and retention plans. Ground-witness equality must precede
   pruning; a non-ground root-to-constant edge is not a ground proof.
2. Add explanation production and its independent sorted checker, carrying
   original terms and saved match evidence across rebuild/pruning.
3. Replace boxed storage and whole-store work with per-operator arrays,
   occurrence queues and relational matching, retaining both backends.
4. Integrate inspection tests, upstream suites, and hegg comparisons before
   making allocation, parity, or speed claims.
5. Complete the DAG extraction portfolio (treewidth DP, reductions, corpus
   benchmarks) and the remaining accepted v1 packages: egglog-style rules,
   slotted e-graphs, and deriving support.

These are outstanding requirements, not removed scope. The earlier
allocation-criterion question remains unanswered; this implementation
does not amend it.

## References

- [egg: Fast and Extensible Equality Saturation](https://doi.org/10.1145/3434304),
  Willsey et al., POPL 2021.
- [A generalization of Dijkstra's algorithm](https://doi.org/10.1016/0020-0190(77)90002-3),
  Knuth, 1977.
- [Weighted quick-union](https://algs4.cs.princeton.edu/15uf/WeightedQuickUnionUF.java.html),
  Sedgewick and Wayne.

## Pareto extraction

The additive-vector search keeps complete global class selections, preserving
cross-root sharing. Its lower bound charges accumulated cost plus coordinatewise
minimum weights for each distinct demanded but unselected class. These minima
need not occur in the same representative: each is still a lower bound, and
omitting further descendants only weakens it. Strict dominance by a feasible
archive entry can therefore discard a branch without losing nondominated
vectors or equal-vector witnesses. Inclusive upper bounds prune only when the
lower bound already violates a constraint.

`Yolk.Internal.Pareto` operates on immutable sort-erased choices.
`Yolk.Extract.Pareto` parses dimensions and limits into an opaque bounded
configuration, freezes the live graph,
and reconstructs each candidate through `Yolk.Dag`'s checked constructor.
The constructor checks visibility, selected-class membership, sorts and cycles.
Public candidates and results are named records with field lenses. Their
evidence applies to the extractor-produced values; callers can edit these
records, but doing so does not revalidate costs or completeness claims.
The solver uses boxed `Vector Natural` coordinates for repeated arithmetic
and dominance checks, while the public boundary keeps list-based costs.
Componentwise reductions use strict folds, including minima and pending bounds.
A snapshot selection asserts no semantic fact beyond the graph's equalities;
proof-producing rewrite explanations remain a separate outstanding feature.

The archive retains at most the requested witnesses per vector. Dropping tied
witnesses does not lose vector coverage. When adding an incomparable vector
exceeds the archive limit, the extractor keeps a deterministic bounded subset
and stops as `Approximate/ArchiveBudget`. It does not continue with unsound
claims that discarded witnesses still participate in dominance tests.
An expansion stop returns at most one componentwise lower envelope of pending
branches. If every pending lower bound is weakly dominated, vector coverage
is complete despite incomplete witness enumeration.

Tests compare the full vector set with an independent exhaustive oracle that
selects representatives for every class before DFS checking only the reachable
closure. Random cases include cyclic choices, sharing, duplicate roots,
zero costs and hard bounds. Returned witnesses are independently rescored and
checked for root mapping, membership, acyclicity and one selected node per
class. Both backends run the same cases. This supports extraction-model claims;
it does not establish physical circuit quality or complete Yolk v1 delivery.
