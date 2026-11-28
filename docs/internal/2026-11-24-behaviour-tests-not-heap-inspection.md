# Strictness tests observe behaviour, not the heap

Date: 2026-11-24. Status: accepted; the tests themselves are not written
yet (deferred on 2026-11-27). Context: [PR
#94](https://github.com/iustin/corydalis/pull/94). Written on
2026-11-28, from the pull request and the discussion around it.

## Context

The rescan memory work (see [The memory and cost of a
rescan](2026-11-24-rescan-memory.md)) made the scan build its results
strictly, and stop keeping the previous repository. The first version of
PR #94 also tested this by walking the heap: a test helper
(`test/TestHeap.hs`, using `ghc-heap`) followed the published
repository, the progress values and the search cache, counting thunks,
and a weak pointer test checked that the previous repository was
collected.

Two problems showed up:

- CI builds the tests with coverage (hpc). hpc wraps values in counting
  thunks: every value passed to a function, or stored in a lazy field
  after a bang pattern. The walker then finds thunks that the normal
  build doesn't have, and fails.
- To make the walker pass, production code got shaped for the tests
  (`consEvaluated`, to build lists whose elements were already evaluated
  where the walker would look): code written for a test's view of the
  heap, not for its job.

## Decision

Tests of strictness and retention observe behaviour, from outside,
instead of inspecting closures. The walker, the `ghc-heap` test
dependency and `consEvaluated` were dropped before anything merged: the
branch's first eight commits went to main as they were (adbb05b), and
the review follow-ups (strict pair folds, progress fixes, tests of the
scan's logic) as PR #94 itself.

Three kinds of tests were agreed for a later round:

1. **Poisoned inputs** (already used since adbb05b): give a function a
   value that throws when evaluated, and expect the exception at once
   (e.g. "evaluates the errors it is given", "evaluates the results
   before caching them"). Only works for a single function that receives
   the value.
2. **Reading published data costs nothing**: after a scan, read the
   repository (or a progress, or the search cache) in full twice, and
   compare the allocation of each read with the per-thread counter
   (`getAllocationCounter`, not disturbed by tests running in parallel).
   Pending work makes the first read cost more. Weaker: it doesn't say
   where, and misses thunks that are cheap to evaluate but hold much
   data. Must be tried in both builds, with and without coverage.
3. **Old data goes away**: put a large known amount (about 50 MB) in the
   previous repository, let a new scan publish, force a major
   collection, and expect the live bytes (from `GHC.Stats`, which needs
   `-T`) to drop by most of it. Coarse on purpose, so that the noise of
   parallel tests doesn't matter.

The round implementing 2 and 3 was put on ice on 2026-11-27, after an
audit found no laziness or retention issue left in the rescan path.

## Alternatives

**Keeping the heap walker, and skipping its checks under coverage**
(detecting hpc at run time, e.g. with `Trace.Hpc.Reflect.examineTix`). A
patch was written but not proposed: the checks would only run in the
plain build, which CI doesn't run, and the production changes made for
the walker would stay.

**Keeping the walker and the production changes.** Rejected: code shaped
for a test's view of the heap, not for its job.

## Measurements

None for this decision; the coverage problem was seen as the walker's
tests failing in CI's coverage build of PR #94.

## Consequences

- Only single functions are checked, with poisoned inputs (e.g. the
  progress errors and the search cache). No test fails if a later change
  leaves the published repository partly unevaluated, or keeps the
  previous one; memory measurements (the benchmarks described in the
  rescan record) are the check for now.
- Tests that compare objects by identity (e.g. `StableName`) are
  affected by hpc the same way; see [Coverage only for the code under
  test](2026-11-27-test-coverage-per-module.md).
