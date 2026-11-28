# The memory and cost of a rescan

Date: 2026-11-24, with later rounds up to 2026-11-27. Status: done, in
PRs [#94](https://github.com/iustin/corydalis/pull/94) (and its first
eight commits, merged directly as adbb05b),
[#98](https://github.com/iustin/corydalis/pull/98),
[#100](https://github.com/iustin/corydalis/pull/100),
[#102](https://github.com/iustin/corydalis/pull/102),
[#103](https://github.com/iustin/corydalis/pull/103) and
[#109](https://github.com/iustin/corydalis/pull/109). Written on
2026-11-28, from the investigation report, the commit messages and the
pull requests.

## Context

After a rescan, the server's resident memory stayed much higher than
after a restart that loaded the same data from the cache: on a sample
library of 24,000 pictures, 579 MB against 263 MB. Two investigations
(one measuring an optimised build on that library, one walking the
heap with `ghc-heap` on an 832-picture library) were merged into one
report and plan, on main at 547b84e, and the plan was checked by a
correctness review and a security review before any code changed.

What they found:

- **No unevaluated exif data survived** a scan: the published
  repository held about 0.1 MB of thunks.
- **The peak was 2 to 3 times the steady state.** During a scan, the
  previous and the new repository are both live, on purpose, so pages
  keep working. But the previous one was also kept *after* publishing,
  until the end of rendering and cleanup, by the scan's error handler,
  which held it to write it back on failure (and so a failure in these
  phases brought back pictures the scan had found gone). The cache was
  written twice, each write adding a 36 MB buffer.
- **The resident size stayed at the peak** because GHC returns freed
  memory with `MADV_FREE`, which Linux keeps counting until it needs
  the memory.
- **Many folds and updates were lazy** (the preview classification,
  folder building, exif maps, date ranges, progress errors, the
  indexer's and handlers' lazy `Data.Map`, the search cache written by
  `stateTVar`): not a retention problem at the end, but chains of
  pending work during the scan, and evaluation inside the transaction
  that publishes the repository.
- **Correctness bugs** on the way: folder stats stale after the range
  steps, shadows of one directory dropped when merging folders (both
  later replaced by the composite images redesign, see
  [Composite images](2026-11-25-composite-images.md)), stale search
  results entering the cache, a launch race.

## Decision

In rounds, each its own pull request:

1. **Ownership by serial** (adbb05b). A scan is known only by its
   serial; status changes go through `updateRepoStatus`, which changes
   whatever repository is current if the serial still matches. The
   previous repository is no longer held; a failure before publishing
   keeps the previous data, one after keeps the new data.
2. **Build, then publish; write the cache once.** The new repository
   is fully evaluated before the transaction that publishes it. The
   first cache write (right after the scan) is replaced by removing
   the cache file: only a finished scan's cache is ever loaded, so the
   first write only served to make a restart during rendering scan
   again, which removing the file does without encoding everything
   twice.
3. **Strictness** where values are built: strict accumulators,
   `StrictPair`/`StrictTriple` for tuple folds, evaluated progress
   errors, `Data.Map.Strict` in all 14 modules that used the lazy one,
   the search cache written evaluated (the tuple folds' strict pairs
   are 71c0bc3, in PR #94).
4. **Return memory at once.** The executables run with
   `--disable-delayed-os-memory-return` (`MADV_DONTNEED`), and a major
   collection is forced at the end of a scan (PR #98), skipped when a
   newer scan replaced this one. The forced collection waited for the
   vector-hashtables fix (see
   [the pin](2026-11-24-vector-hashtables-pin.md)), as it made that bug
   show up in the tests.
5. **Less work per rescan** (PR #100): one light walk of the cache
   instead of two full ones, picture directories resolved once per
   preview check, fewer `stat` calls for the exif caches.
6. **Fixes found on the way** (PR #102): launching a scan takes the
   serial and registers the scanner in one transaction; scan failures
   are logged; a superseded scan stops with its own
   `OwnershipChanged` error.
7. **Search cache generations** (PR #103): see
   [its own record](2026-11-25-search-cache-generation.md).
8. **Sharing after a cache load** (PR #109): see
   [its own record](2026-11-27-restore-sharing-on-cache-load.md).

An audit of main at afa70e4 on 2026-11-27 found no laziness or
retention issue left in the rescan path. The tests of what is
evaluated were deferred: see
[Behaviour tests](2026-11-24-behaviour-tests-not-heap-inspection.md).

## Alternatives

**Other garbage collectors.** Measured (table below): the compacting
collector (`-c`) and the non-moving one were worse after a rescan.
Rejected.

**`-Fd1`** (return memory faster) gave a lower number than the flag
alone, but only changes how fast the resident size reaches its level,
not the level itself; `--disable-delayed-os-memory-return` is what
makes the resident size follow the live data. Not added.

**Clearing `ctxScanner` after a scan.** The finished `Async` holds the
same repository as the current one, so there is no memory to gain,
and clearing it adds a race with `waitForScan`. Rejected.

**Bounding the number of folders scanned in parallel.** No effect
seen on a warm rescan (the live heap grows linearly with the new
repository). Not done; a cold scan was not measured.

**Keeping the first cache write.** Rejected, see decision 2.

**Making a search read the repository inside its transaction** (the
plan's fix for stale search results, after both reviews rejected
tagging results with the repository serial). Replaced by generations,
which let a long search run outside any transaction.

## Measurements

Library: about 24,000 generated pictures (made with a local
generator script), 243,000 cache files, 48,600 media files.
Optimised non-dev build. Benchmark programs replacing the scanner's
`main` (local tools, not in the repository) ran a no-change rescan
and printed GHC's statistics and the process's resident size. The
machine was a cloud development machine; its model wasn't recorded.

Resident size, from the report (main at 547b84e):

| Build and runtime flags                          | After cache load | After rescan + 6 major GCs |
|--------------------------------------------------|------------------|----------------------------|
| Default flags                                    | 263 MB           | 579 MB                     |
| `--disable-delayed-os-memory-return`             | 263 MB           | 233 MB                     |
| Same, plus `-Fd1`                                | 263 MB           | 185 MB                     |
| `-c` (compacting), plus the flag                 | 165 MB           | 300 MB                     |
| `--nonmoving-gc`, plus the flag                  | 173 MB           | 487 MB                     |

Forcing a major collection at the end of a scan (PR #98, three
rescans each, compared with main just before it): resident size at
the end of a rescan 338 to 418 MB without, about 288 MB with; live
data at that point 99 to 134 MB without, 66.1 MB with.

Allocation and time of a no-change rescan (PR #100, from the commit
messages):

| Change                                   | Allocated          | Time               |
|------------------------------------------|--------------------|--------------------|
| Before                                   | 30.4 GB            | about 16 s         |
| One light walk of the cache              | 26.3 GB            | about 14.5 s       |
| Directories resolved once                | 25.3 GB            |                    |
| Fewer `stat` calls for the exif caches   | 25.2 GB            | no measurable change |

From the report: the second cache write cost 0.45 GB of allocation
and 0.19 s; evaluating the new repository inside the publishing
transaction took 0.17 s.

Launch race (PR #102): a test launching 20 scans at once, five times,
failed one round in 8 of 20 runs and five rounds in 20 of 20 with the
old code, and passed 70 of 70 runs with the new code.

## Consequences

- After a rescan, the resident size drops to about the live data at
  once, instead of staying at the peak until the kernel needs memory.
  The install guide documents the runtime flag.
- A failure during rendering or cleanup keeps the new data.
- A restart during rendering or cleanup scans again, as before, but
  without the second cache encoding.
- The rescan path's strictness is only checked by tests of single
  functions (poisoned inputs): a regression in the published
  repository as a whole would show as memory growth, not a test
  failure.
