# yolk

I am building a many-sorted e-graph library for Haskell. A language is a
family of higher-kinded datatypes: its indices distinguish numbers from
booleans, bit-vectors of different widths, or the sorts of another object
language. E-class identities preserve those indices.

The first implementation is in `src/`. It provides a mutable backend in
`ST` or another `PrimMonad`, a persistent backend, canonical interning,
deferred congruence rebuilding, typed rewrites, bounded equality saturation,
immutable search snapshots, pruning, and minimum AST-size tree extraction.
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
AST size 1. It exits unsuccessfully if either backend disagrees.

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

## Implementation status

This is an initial core, not the completed v1 described in the intake.
The mutable union-find uses unboxed arrays. Heterogeneous nodes, memo keys,
and graph metadata currently use boxed values and persistent maps;
rebuilding uses whole-store sweeps and matching uses backtracking. There
is no claim of allocation-free hot paths or a speedup over hegg.

The remaining v1 work includes per-operator unboxed node storage,
specialization/inspection gates, a relational matcher and backoff scheduler,
the generic analysis API, independently checked explanations, optimal DAG
extraction, egglog-style rules, slotted e-graphs, deriving support, and the
full upstream test/benchmark corpus. The existing acceptance criteria are
unchanged. [Design and next steps](docs/design.md) separates these from the
working source.
