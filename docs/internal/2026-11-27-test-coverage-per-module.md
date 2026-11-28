# Coverage only for the code under test: hpc off per test module

Date: 2026-11-27. Status: accepted; applied only where needed (so far
`test/Repository/ShareSpec.hs`, from
[PR #110](https://github.com/iustin/corydalis/pull/110)). Written on
2026-11-28, from the investigation notes.

## Context

CI builds the tests with `stack test --coverage`, which builds every
module of the package, the test suite's included, with hpc. hpc adds a
tick to each expression it counts, and wraps values in counting thunks.
Coverage of the test modules themselves is of no use, and the wrapping
changes what some tests see:

- a value passed to a function, or stored in a lazy field after a bang
  pattern, is a fresh thunk, so tests of evaluation see thunks the
  normal build doesn't have (see
  [Strictness tests observe behaviour](2026-11-24-behaviour-tests-not-heap-inspection.md));
- `makeStableName v` names a fresh tick thunk at each use of `v`, so a
  value is never the same object as itself, and identity checks fail.

The question was whether the test suite as a whole could be built
without hpc, keeping coverage for the library and the executables.

## Decision

hpc is turned off with a pragma, `{-# OPTIONS_GHC -fno-hpc #-}`, only
in the test modules that need it, i.e. that check identity or
evaluation. ShareSpec is the first.

## Alternatives

**`ghc-options: -fno-hpc` on the test component in `package.yaml`.**
Doesn't work: Cabal (3.12) puts `-fhpc -hpcdir ...` after the
component's options on GHC's command line, so `-fhpc` wins (GHC didn't
even recompile). Cabal has no per-component coverage switch
(`--enable-library-coverage` is a deprecated alias of
`--enable-coverage`).

**`-fno-hpc` through Stack's `--ghc-options`.** These come after
`-fhpc`, but apply to the whole package, so they would drop the
library's coverage too.

**The pragma in all test modules.** Works: tried on all 48 test
modules, the library's and executables' coverage was identical line by
line and branch by branch (6,358 lines, 5,961 hit), and CI's
`hpc-codecov stack:test` and `make coverage-hs` still worked. But the
test time didn't change (about 17 s), so a 48-file change bought
nothing. Rejected.

**`makeStableName $! v`** in identity tests instead of the pragma.
Noted as an option at the time; each use must remember it, while the
pragma covers the whole module.

## Consequences

- A new test of identity or evaluation needs the pragma in its module,
  and should be run in both builds (`make test` and the coverage
  build) before relying on it.
- The modules with the pragma report no coverage of themselves, which
  is not used anyway.
