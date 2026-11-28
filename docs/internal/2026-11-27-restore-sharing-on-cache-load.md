# Restoring sharing after loading the repository cache

Date: 2026-11-27. Status: accepted, in [PR
#109](https://github.com/iustin/corydalis/pull/109) (commits 54ce678 and
d386337). Written on 2026-11-28, from the commits, the pull request and
its discussion.

## Context

A scan builds each image's exif from its files' exif, and the time
index, the folder keys and the images' parent names from values it
already has, so these are shared: one object, pointed to from several
places. The repository cache is a generic `store` encoding of the whole
repository, derived values included. Decoding makes a separate copy of
each value, so a repository read from the cache took noticeably more
memory than the same repository after a scan. The rescan investigation
had measured it at 95 MB against 70 MB on its sample library (see [The
memory and cost of a rescan](2026-11-24-rescan-memory.md)), and it was
the last item left of that work.

## Decision

On load (`restoreSharing` in `Repository.Build`), each image's exif is
computed again from its files', and each folder is finished again
(`finishFolder`), which rebuilds the links, the composite images' exif
and the derived values from the images. Folders are keyed by their own
names, and each image gets its folder's name as its parent, as after a
scan. The repository is then forced before it is published.

The result is equal to what was read: the composite image tests check it
for each folder they build, a test covers a folder with merged exif, and
a test writes a repository to the cache and reads it back.

## Alternatives

**Leaving it.** Rejected: the cache load is how every restart starts, so
the extra memory was paid for the whole life of the process, until the
next rescan.

**Caching only the source data** (the scanned files, without the derived
values), and building the rest on load as a scan does. This would also
shrink the cache, and stop derived-only type changes from invalidating
it. It needs a separate type for half-built folders, and its load time
wasn't measured. Kept as an idea for later, not decided.

## Measurements

Library: 25,200 generated pictures, from JPEG templates tagged with
exiftool. Optimised non-dev build, with a benchmark program replacing
the scanner's `main` (a local tool, not in the repository), reporting
GHC's live bytes after a major collection. Code: main at afa70e4, then
the commits of PR #109.

| Repository                          | Live heap |
|-------------------------------------|-----------|
| After a scan                        | 81.8 MB   |
| Read from the cache, before         | 105.1 MB  |
| Read from the cache, exif + finish  | 82.8 MB   |
| Same, plus folder and parent names  | 81.8 MB   |

The extra work on load takes about 0.17 s.

## Consequences

- A restart and a rescan give the same memory use.
- Everything `finishFolder` computes runs on every cache load, so a
  slower `finishFolder` slows down startup too. The exif sharing pass
  added later runs there (see [Sharing equal exif values per
  folder](2026-11-27-exif-sharing.md)).
