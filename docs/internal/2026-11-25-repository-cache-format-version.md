# A format version for the repository cache

Date: 2026-11-25. Status: accepted, in
[PR #104](https://github.com/iustin/corydalis/pull/104) (commit
94fa36c). Written on 2026-11-28, from the commit and the composite
images specification.

## Context

At the end of a scan, the whole repository (with the configuration it
was built from) is written to a cache file, and a restart loads it
instead of scanning. The cache was `Store.encode (config, repo)`, and
an old cache was only rejected when it failed to decode.

`store` doesn't describe the data it writes: it decodes by position,
trusting that the bytes have the layout of the type it is asked for.
So nothing guaranteed that a cache written for older types fails to
decode with newer ones; it could be read as something else, without an
error. The composite images redesign (see
[Composite images](2026-11-25-composite-images.md)) was about to change
the cached types, and its specification also needed old caches to be
rejected, so that their stale derived values (shadows, stats) are
never loaded.

## Decision

The cache is encoded with `Data.Store.Version`: a marker and a version
come before the data, and decoding fails (so the cache is ignored and
the application scans) when the version differs. The version is a hash
of the shape of the cached types (the names of the types, constructors
and fields, and the fields' types), computed at compile time from their
`Data` instances, and pinned in `repoCacheVersion`: a change to the
shape of a cached type fails the build until the pin is updated.

- The encoding is in the new `Repository.Encoding`; the pin is in
  `Repository`, as a value used at compile time can't come from the
  module that uses it.
- The cached types derive `Data`. `Regex` and `SymbolizedItem`, whose
  `Store` instances are hand-written, get hand-written `Data` instances
  describing what is stored (their text); `COff` gets an orphan one.
- The hash doesn't see encoding changes that keep the shape (the
  hand-written `Store` instances, `ShortText`'s, a library changing an
  encoding). For those, the cache holds a `CacheRevision` marker, which
  `store` writes as nothing, but whose constructor's name is part of
  the shape: renaming it (`CacheRevision1` to `CacheRevision2`) gives a
  new version.

## Alternatives

**Relying on failed decodes.** Rejected: not guaranteed, see above.

**A version number bumped by hand.** Rejected: forgetting to bump it is
exactly the failure to prevent. The hash changes by itself, and the
pin turns the change into a build failure.

**A pass over old caches on load that fixes their derived values**
(planned in the rescan work for stale stats). No longer needed once old
caches are rejected by version.

## Measurements

None taken for this change. The binary exif cache's record measures
the cost of the same scheme per file.

## Consequences

- The first start after upgrading scans again (existing caches have no
  version).
- A change to the shape of a cached type, or a resolver upgrade (the
  names of library types are part of the shape), fails the build until
  `repoCacheVersion` is updated; the development notes ("The repository
  cache's format version") say how.
- The binary exif cache later got the same scheme; see
  [A format version for the binary exif cache](2026-11-28-bexif-format-version.md).
