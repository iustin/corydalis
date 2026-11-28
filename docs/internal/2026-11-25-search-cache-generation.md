# A generation number for the search cache

Date: 2026-11-25. Status: accepted, in [PR
#103](https://github.com/iustin/corydalis/pull/103) (commits 46f3046 and
5f547ec). Written on 2026-11-28, from the commits and the rescan
investigation report.

## Context

The server keeps the results of the last ten searches. A search read the
repository, computed its results, then cached them. If a rescan
published new data in between, results computed from the previous data
were cached after the publish had emptied the cache: later identical
searches listed removed pictures (as broken entries) and missed new
ones. To limit this, every status change of a rescan emptied the cache
again (four times per rescan), which threw out such results only by
chance: results cached after the last flush stayed until the next
rescan, or until ten other searches pushed them out.

On public-site builds, stale results can name pictures that were removed
on purpose, which the security review of the rescan plan called a
regression risk for the first fix proposed.

## Decision

The search cache has a generation number, increased in the transaction
that publishes new data, which also empties the cache. A search that
misses the cache reads the generation and the repository together,
computes its results outside any transaction, and caches them only if
the generation is unchanged. Otherwise it searches once more on the new
data, and if the generation changes again, returns these second results
without caching them.

The cache is emptied only when new data is published, not on the other
status changes.

## Alternatives

**Tagging the cached results with the repository's serial.** Rejected by
both reviews of the plan: a scan puts the previous data under its new
serial when it starts, and publishes the new data under the same serial,
so results computed from the previous data still pass the check.
Together with dropping the flushes, this would have been worse than
before: on a public-site build, anonymous visitors would keep seeing
names and exif of removed pictures.

**Reading the repository and computing the results inside the search's
transaction** (the plan's corrected design; the generation counter was
listed there as the other way). Not chosen: computing a search inside
STM means a long search runs again whenever the repository or the cache
changes under it. With the generation, the computation runs outside any
transaction.

**Keeping the flushes on each status change.** Rejected: they only
helped by chance, and cost the results of up to ten searches through the
rest of a rescan.

## Measurements

A search takes 130 to 170 ms on the sample library used for the rescan
work (from the commit message of 5f547ec); this is what each kept result
saves during a rescan.

## Consequences

- `getSearchResults` reads the repository itself, taking the search
  function rather than its results.
- Cached results always come from the current data.
- A search racing with two publishes in a row computes once more (two
  computations in all); its second results aren't cached.
