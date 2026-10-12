# yolk

I am building a many-sorted e-graph library for Haskell. A language is a
family of higher-kinded datatypes: its indices distinguish numbers from
booleans, bit-vectors of different widths, or the sorts of another object
language. E-class identities preserve those indices.

The first implementation is in `src/`. It provides a mutable backend in
`ST` or another `PrimMonad`, a persistent backend, canonical interning,
deferred congruence rebuilding, typed rewrites, bounded equality saturation,
immutable search snapshots, pruning, configurable tree minimization,
solver-free optimal DAG extraction, and optics compatible with `lens`.
The same graph program runs against either backend.

## Build and run

Use GHC 9.14.1 and cabal-install 3.18.1. The project file selects that
compiler, enables the test suite, and limits compilation to one job.
`hkd-0.2.1` needs the project-local `allow-newer: hkd:base` allowance.

```sh
cabal build all --ghc-options=-Werror
cabal test all --test-show-details=direct
sh test/check-types.sh
cabal run yolk-example
```

The example simplifies a numeric conditional and its boolean condition
using both backends. The expected results are `7` and `true`, each with
AST size 1. It then demonstrates size, depth, operator weights, and a
lexicographic objective. Size chooses `(2 * 3)`; making multiplication
expensive chooses `(2 + (2 + 2))`. It exits unsuccessfully if either backend
disagrees with the expected results. Its DAG example then minimizes shared
nodes, changes their weights, reports a budget-limited result, and emits
each shared operation once.

## Define a language

A language has kind `k -> (k -> Type) -> Type`. In the
[complete example](examples/Main.hs), the two indices are `'Number` and
`'Boolean`:

```haskell
data Sort = Number | Boolean

type Math :: Sort -> (Sort -> Type) -> Type
data Math s f where
  Lit     :: Integer -> Math 'Number f
  BoolLit :: Bool -> Math 'Boolean f
  Add     :: f 'Number -> f 'Number -> Math 'Number f
  Mul     :: f 'Number -> f 'Number -> Math 'Number f
  Equal   :: f 'Number -> f 'Number -> Math 'Boolean f
  If      :: f 'Boolean -> f 'Number -> f 'Number -> Math 'Number f
```

Supply `FFunctor`, `FFoldable`, and `FTraversable` from `Data.HKD`. The
example writes the traversal once and uses `ffmapDefault` and
`ffoldMapDefault` for the other instances.

`Language.operatorKey` identifies a constructor and all of its non-child
fields. Distinct literal payloads must have distinct keys. The graph adds
the result sort and ordered canonical child identities to that key. Sorts
are compared using `TypeRep`; heterogeneous retrieval uses equality
witnesses rather than unchecked casts. The key law also requires that
corresponding child positions have the same sorts.

## Rewrite and extract

`term` constructs a finite tree; `node` and `var` construct a pattern.
Here is an identity rule over the example language:

```haskell
x :: Pattern Math 'Number
x = var "x"

addZero :: Either RuleError (Rewrite Math)
addZero = rewrite "add-zero" (node (Add x (node (Lit 0)))) x
```

`rewrite` rejects an unbound RHS variable or a name used at inconsistent
sorts. Both sides have the same result sort in the Haskell type.

We add the input, saturate under validated rules, then extract a finite
tree. The graph's region parameter prevents its identities from escaping
or being used in a different graph:

```haskell
simplify :: Backend b m
         => [Rewrite Math]
         -> EGraphT b r Math m (Maybe (Int, Term Math 'Number))
simplify rules = do
  root <- addTerm (term (Add (term (Lit 7)) (term (Lit 0))))
  _ <- run defaultLimits rules
  extract root
```

Use `runMutable (simplify rules)` or `runPersistent (simplify rules)`.
Both return `Either EGraphError ...`. The generic `runEGraphT` also supports
a mutable graph in `IO` through `Proxy @Mutable`.

`run` reports why it stopped. `IterationLimit`, `NodeLimit`, and `MatchLimit`
are distinct from `Saturated`. A match-cap failure applies none of the
truncated iteration. Node limits are checked after each atomic rewrite,
so one RHS can overshoot the limit. Extracting an incumbent after a budget
stop does not mean the rewrite system has saturated.

## Choose what to minimize

`extract` retains its original `Maybe (Int, Term l s)` result and minimizes
AST size. `extractWith` takes a cost algebra and returns its cost type:

```haskell
type TreeCost l c = forall s. l s (Const c) -> c

extractWith :: (Backend b m, Language l, Ord c)
            => TreeCost l c
            -> ClassId r s
            -> EGraphT b r l m (Maybe (c, Term l s))
```

Each child position contains a `Const c` with that child's best cost. The
node still has its constructor, payload and result sort. The model can
inspect all of these, combine child costs, and return any lawful ordered
cost type; it does not need a `Num` instance.

The built-in models use `Word64`:

```haskell
smallest <- extractWith astSize root
shallowest <- extractWith astDepth root
cheapest <- extractWith (weightedSize operationWeight) root

-- For the example Math language:
operationWeight :: Math s f -> Word64
operationWeight Mul {} = 10
operationWeight _ = 1
```

`astSize` charges one per constructor, including leaves. `astDepth` charges
one plus the deepest child, with leaves at depth one. `weightedSize` sums
a caller-supplied constructor/payload weight and all child occurrences.
The example's `preferAddition` uses `(Word64, Word64)` to minimize the
number of multiplications first, then AST size.
When that primary component saturates, it absorbs the secondary component
too, preserving lexicographic monotonicity at the numeric boundary.

All three built-ins use saturating arithmetic: costs above `maxBound ::
Word64` tie at that bound instead of wrapping into cheap values.
`saturatingAdd` is public for custom models. Supply an unbounded cost
algebra if distinguishing costs beyond that range matters. The legacy
`extract` reports `CostOverflow` above `maxBound :: Int`.

A custom algebra must be monotone in its children, return a cost at least
as large as every child, use a lawful total order, and admit no infinite
descending chain of reachable costs. These are caller obligations. Zero
weights are supported: equal-cost alternatives keep the first finite
witness, so zero-cost cycles cannot replace it indefinitely. Specify a
secondary objective explicitly if ties matter.

Each call minimizes over the current visible e-graph and returns `Nothing`
when no finite visible tree exists. Repeated children count per occurrence;
this is tree minimization.

## Minimize a shared DAG

`Yolk.Extract.Dag` supplies a separate solver-free branch-and-bound path,
also reexported by `Yolk`. It selects one visible member per reachable
e-class, preserves sharing, and rejects cyclic selections. Its objective
is a sum of nonnegative `Word64` constructor/payload weights, charged once
per distinct selected node. `dagSizeCost` assigns one to every node.

```haskell
-- Prove the optimum, with no expansion cap:
best <- extractDagOptimal (DagLimits Nothing) dagSizeCost root

-- Bound the search and change the objective:
weighted <- extractDagOptimal defaultDagLimits operationWeight root

case best of
  Nothing -> ... -- no finite visible derivation
  Just result -> ... -- dag, dagCost, lowerBound, optimality, expansions
```

The example asserts the equality of `(1 + (2 + 1))` and
`((1 + 1) * (1 + 1))`. Tree minimization chooses the first, with five AST
nodes. DAG minimization chooses the second, with three distinct nodes:

```text
n0 = 1
n1 = n0 + n0
n2 = n1 * n1
result = n2
```

Giving multiplication weight 10 changes the DAG choice back to the first
expression, at cost 4. Minimizing a tree then deduplicating it would miss
the minimum DAG in the size example.

`DagLimits (Just n)` caps branch expansions; `defaultDagLimits` uses
100,000. A zero budget still constructs a feasible incumbent if one exists.
Every result satisfies `lowerBound <= optimum <= dagCost`; `Optimal`
means the bounds coincide. When work remains at the cap the status is
`Unproven`, not a claim of optimality. Costs and bounds saturate at
`maxBound :: Word64`, so larger mathematical sums deliberately tie.
The cap is not a time or memory limit: preprocessing, constructing the
successors of a state, and typed reconstruction also take work. Exact
search can take exponential time.

Use `extractDagMany limits weight roots` to optimize an HKD collection of
roots jointly, including different sorts. Sharing across those roots is
charged once; duplicate roots retain their positions. Empty roots give
an optimal cost-zero result.

`Dag l s` is immutable and may leave the graph runner. `dagNode` exposes a
typed layer with `Dag` children. `dagNodeId` is a dense postorder ID local
to one extraction, shared across its roots; IDs from separate calls are
unrelated. `dagSize` counts unique nodes. `foldDagM` runs an algebra once
per reachable node, children first, which supports shared code emission.
Separate folds have separate memo tables. `foldDag` is the pure version;
`dagToTerm` returns the tree representation, whose consumers may count or
evaluate shared subterms repeatedly.

This path currently supports additive node weights. Depth and general
lexicographic algebras remain available through the tree API; they are not
silently reinterpreted as DAG objectives.

## Optics

Import `Yolk.Optics` qualified; its record lenses use the same names as the
existing accessors, which remain available from `Yolk`:

```haskell
import Control.Lens ((&), (.~), (^.), (^?), _Just, _1, review)
import qualified Yolk.Optics as O

limits = defaultLimits & O.iterationLimit .~ 100
                       & O.nodeLimit .~ 200000
reason = result ^. O.stopReason
finished = result ^? O.stopReason . O._Saturated
bestCost = extracted ^? _Just . _1

x :: Pattern Math 'Number
x = review O._Var "x"
```

The module provides lenses for every `Limits` and `RunResult` field and
`rewriteName`, an `_Term` iso for one typed term layer, `_Var` and `_Node`
pattern prisms, and prisms for every stop reason, rule error, and graph
error. Compose `_Term` with prisms for your own language constructors.
Term and pattern optics preserve sorts. Graph identities and snapshots
stay opaque; changed rewrite bodies must pass through `rewrite` again.
For DAG extraction, `O.expansionLimit` is a lens and `O._Optimal` /
`O._Unproven` are prisms. `O.dag`, `O.dagCost`, `O.lowerBound`,
`O.optimality`, `O.expansions`, `O.dagNode` and `O.dagNodeId` are getters:
results and nodes cannot be edited in ways that invalidate their cost
certificates or sharing. For example, `result ^? O.optimality . O._Optimal`
checks the proof status, and `result ^. O.dag . O.dagNode` reads the root.

## Rebuilding, snapshots, and pruning

`union` records an equality and leaves the graph dirty. `rebuild` restores
congruence and canonical memo keys. `add` repairs pending congruence before
interning; `classNodes`, `lookupNode`, and `hideNonLeaves` instead report
`DirtyGraph` until an explicit rebuild. `equivalent` follows current
representatives even while the graph is dirty.

`freeze` rebuilds and captures an immutable search snapshot. Search reads
only that snapshot, then applies its saved substitutions to the live graph.
`snapshotNodes` keeps its original membership when the live graph changes.
An identity allocated after the snapshot has no members in that snapshot.

`hideNonLeaves` removes compound nodes from visible membership. Their
intern entries and congruence dependencies remain: re-adding one does not
restore it. When a hidden and visible occurrence collide, visibility is
retained. An analysis may prune again after merging. The matcher and
extractor enumerate visible nodes; `lookupNode` probes retained identity.

## Benchmarks and regressions

`cabal bench yolk-bench` runs deterministic interning, rebuilding, rewriting,
tree extraction, and solver-free DAG workloads on both backends using
`tasty-bench`. Shared test vectors check closed-form answers and budget
certificates in the normal test suite and before timing. Timed results force
the extracted witnesses; every invocation starts from a fresh graph.

```sh
cabal bench yolk-bench --offline \
  --benchmark-options='--baseline bench/baseline.csv --fail-if-slower 10 --csv bench/current.csv --stdev 2 --timeout 3s'
cabal run yolk-bench-report --offline -- bench/baseline.csv bench/current.csv
```

The comparison tool uses `statistics` to summarize matched runtime ratios
and rejects mismatched or invalid CSV inputs. The checked-in baseline is
machine-specific. See [workloads, measurement boundaries, and baseline
instructions](bench/README.md) before interpreting a comparison.

## Implementation status

This is an initial core, not the completed v1 described in the intake.
The mutable union-find uses unboxed arrays. Heterogeneous nodes, memo keys,
and graph metadata currently use boxed values and persistent maps;
rebuilding uses whole-store sweeps and matching uses backtracking. There
is no claim of allocation-free hot paths or a speedup over hegg.

The remaining v1 work includes per-operator unboxed node storage,
specialization/inspection gates, a relational matcher and backoff scheduler,
the generic analysis API, independently checked explanations, the DAG
treewidth-DP portfolio and corpus benchmarks, egglog-style rules, slotted
e-graphs, deriving support, and the
full upstream test/benchmark corpus. The existing acceptance criteria are
unchanged. [Design and next steps](docs/design.md) separates these from the
working source.
