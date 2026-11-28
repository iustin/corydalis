# Pinning a fixed vector-hashtables

Date: 2026-11-24. Status: accepted, in [PR
#95](https://github.com/iustin/corydalis/pull/95); to be dropped once a
fixed release is in the Stackage LTS. Written on 2026-11-28, from the
pull request and the investigation notes.

## Context

Names in the exif data (cameras, lenses, places...) are interned with
`symbolize`: equal texts give the same symbol, compared by pointer.
`symbolize` keeps a global table of the live symbols in a
`vector-hashtables` hash table, and removes a symbol from it, with
`alter`, in the symbol's finalizer.

A test ("Pics, build file from inode") started failing now and then
(roughly once in a dozen full test runs, as observed while working on
it), after a major collection was forced at the end of each scan (see
[The memory and cost of a rescan](2026-11-24-rescan-memory.md)): two
equal names compared as different.

The cause is in vector-hashtables 0.1.2.1. When `alter` (or `alterM`)
deletes a key, it assumes the entry is the head of its bucket's chain.
When it isn't, every entry in front of it in the chain is unlinked:
still in the table, but not found by `lookup`. So:

1. symbol X is interned, then symbol Y, whose hash lands in the same
   bucket (Y is now the head);
2. X dies, and its finalizer deletes it through `alter`, unlinking Y,
   which is alive;
3. interning Y's text again misses the table, creates a new symbol, and
   it compares unequal to Y.

It is flaky because `symbolize` seeds its hash key randomly per process
(which texts share a bucket changes each run), and depends on when
finalizers run; the forced collection made them run in the middle of
tests. It doesn't depend on the runtime (threaded at `-N1`, `-N4`,
non-threaded: similar rates). A long-running server is exposed too:
equal names can stop comparing equal.

Upstream: reported as
[vector-hashtables#43](https://github.com/klapaucius/vector-hashtables/issues/43),
with draft fixes in #44 and #46; nothing released.

## Decision

`stack.yaml` pins vector-hashtables as a `git:` extra-dep: 0.1.2.1 plus
the fix of upstream PR #46, as commit 84a8879 on a fork
(iustin/vector-hashtables), with the tag `corydalis-pin-alter-fix` so
that the commit stays reachable. The changelog and the todo list say so,
and that builds without Stack need the fix too.

The forced collection at the end of a scan was held back until the pin
was in, then added as its own change (PR #98).

## Alternatives

**Waiting for an upstream release.** Rejected: the bug can hit
production, and blocked the forced collection.

**Pinning the upstream pull request's commit directly.** Rejected: a
pull request's commits can disappear when it is rebased or closed; the
tagged fork keeps it.

**Leaving out the forced collection.** That only makes the bug rarer,
not absent, as finalizers run anyway.

## Measurements

From the reproduction programs (local tools, not in the repository;
lts-24.68, GHC 9.10.3, symbolize 1.0.3.1):

- a deterministic test of vector-hashtables alone (insert two keys of
  one bucket with `alter`, delete the first, look up the second) fails
  on 0.1.2.1 for 15 of the key pairs in 1 to 2,000, and passes with the
  fix;
- an end-to-end test (intern a text, force a major collection, intern it
  again and compare) failed in 5 to 9 of 20 runs on 0.1.2.1, and in 0 of
  20 runs at `-N1` and 0 of 20 at `-N4` with the fix.

## Consequences

- Builds depend on the fork being available.
- Drop the pin, and its notes in the changelog and the todo list, once a
  fixed vector-hashtables is in the Stackage LTS used by the project.
