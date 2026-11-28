# A format version for the binary exif cache

Date: 2026-11-28. Status: accepted, in
[PR #111](https://github.com/iustin/corydalis/pull/111).

## Context

The scan reads each media file's exif data from one of three sources,
the first that works:

1. the binary exif cache (`<file>-bexif` in the cache directory): the
   decoded `Exif` value, encoded with `store`;
2. the raw exif cache (`<file>-exif`): exiftool's JSON output for the
   file, which is parsed and decoded into an `Exif` value, after which
   the binary cache is written;
3. exiftool itself, which then writes both caches.

A cache is only used if it isn't older than the file (and the binary
one, not older than the raw one).

The binary cache was a plain `store` encoding. `store` doesn't
describe the data it writes: it decodes by position, trusting that the
bytes have the layout of the type it is asked for. After a change to
the layout of `Exif` (a field added, removed, reordered, or of another
type), an older file can fail to decode, which is harmless (the raw
cache is read instead), or decode into wrong values, silently. Nothing
guaranteed which. Version 0.3.0 asked users to delete these files by
hand for this reason.

The repository cache had the same problem, and got a format version in
the same development cycle (see "The repository cache's format
version" in the development notes): a hash of the shape of the cached
types, computed at compile time by `Data.Store.Version`, written at the
start of the cache and checked when reading it. The planned redesign of
`Exif` (grouping its fields) changes its layout, so the binary exif
cache needed the same protection first.

## Decision

Each binary exif cache file starts with a format version, computed the
same way as the repository cache's: a hash of the shape of
`CachedExif` (a `BExifRevision` marker and the `EExif` value), pinned
in `bExifCacheVersion` in `Exif`, so that a change to the shape fails
the build until the pin is updated. The marker's constructor name is
part of the shape, and is renamed for encoding changes the shape
doesn't show (the hand-written `Store` instances).

A file with another version, or from before versions, fails to decode.
The scan then reads the raw exif cache, without running exiftool, and
writes the binary file again.

The encoding needs the `Data` instances of the cached types at compile
time, from an already compiled module, so it can't be in `Exif`, which
defines them: `getExif` and the cache handling moved to a new
`Exif.Load` module.

## Alternatives

**Keeping the binary cache unversioned, and asking users to delete it
when needed** (as in 0.3.0). Rejected: it relies on users reading the
release notes, and a missed step means wrong data, not an error.

**Dropping the binary cache, and always reading the raw one.** Rejected:
reading the raw cache is 12 to 22 times slower (see the measurements
below), which a rescan of a whole library would pay for every file.

**A plain version number instead of a hash.** Rejected: it has to be
bumped by hand, and forgetting to is exactly the failure being
prevented. The hash changes by itself, and the pin turns the change
into a build failure. It is also what the repository cache uses.

**One version check for all the files**, e.g. a marker file in the cache
directory, read once per scan. The binary cache files are many and
small, and their versions differ only after an upgrade, so checking
each file seemed wasteful. Since the repository cache holds `Exif` too,
a change to its layout also invalidates the repository cache, so every
folder is scanned again and every binary file read in the same scan;
with the marker written only once that scan completes, an interrupted
scan would leave the old marker, and the next one would redo the work.
But files of different versions can still be in the cache at the same
time: those of a source directory taken out of the configuration (the
cache cleanup doesn't visit its cache tree) and added back after an
upgrade, and those written by builds of different versions sharing a
cache directory (e.g. a downgrade, or the scanner and the server). A
single marker would accept such files as current, decoding them into
wrong values. Covering these cases needs more machinery (e.g. a marker
per source directory), to save about 0.1 µs per file. Rejected. (A
directory that is configured but unreadable during a scan isn't one of
these cases: the cleanup deletes the cache files of files the scan
didn't find.)

**The version in the file name** (e.g. `<file>-bexif-<hash>`). Files of
another version are then simply not found, at no cost per read, and the
cache cleanup already deletes the files whose suffix it doesn't know.
Rejected: the hash and its pin would have to be computed outside of
`Data.Store.Version`, which only provides the version inside the
encoded data, and the cost it saves is negligible.

## Measurements

Measured on main at 29e8374 (before the change) with GHC 9.10.3, `-O2`,
`store` 0.7.22, on a 4-core Intel Xeon (2.1 GHz) cloud machine with an
ext4 file system and a warm page cache. The benchmark programs were
local tools, not part of the repository.

**Cost of the version.** On a generated library of 25,500 files (300
folders of 42 raw and 42 JPEG files, plus a panorama each, made from
12 JPEG templates tagged with exiftool), after a scan:

| Per file                                 | Unversioned | Versioned     |
|------------------------------------------|-------------|---------------|
| Size of the binary cache                 | 470 bytes   | 502 bytes     |
| Decoding it (in memory)                  | 2.61 µs     | 2.69–2.74 µs  |
| What the scan does: 2 `stat` calls, read, decode | ~38 µs | ~38 µs     |

The version adds 32 bytes to each file: a 4-byte marker, the length of
the hash (8 bytes) and the hash (20 bytes). The difference in the full
read is below the noise between runs.

**Binary against raw cache.** With the raw exif caches of three real
pictures, from three cameras, each parsed and decoded 2,000 times in
memory, and read from 2,000 copies on disk (median of 9 runs):

| File               | Raw (JSON) | Binary | Parse JSON to `Exif` | Decode binary | Read + JSON | Read + binary |
|--------------------|------------|--------|----------------------|---------------|-------------|---------------|
| iPhone SE, HEIC    | 12.3 KB    | 406 B  | 152 µs               | 1.8 µs        | 179 µs      | 14 µs         |
| Nikon Z 9, NEF     | 27.2 KB    | 448 B  | 353 µs               | 2.3 µs        | 386 µs      | 17 µs         |
| Nikon Z50 II, NEF  | 18.1 KB    | 555 B  | 241 µs               | 2.5 µs        | 283 µs      | 16 µs         |

Parsing the JSON takes about 13 µs per KB; converting the parsed value
to `Exif` (`exifFromRaw`) adds little. For 25,500 files of the NEF
sizes, reading the raw caches takes about 7 s, the binary ones about
0.4 s.

## Consequences

- After upgrading, the first scan reads every file's exif data from the
  raw cache once, and rewrites the binary cache, so it takes somewhat
  longer. exiftool doesn't run.
- A change to the shape of `Exif`, or of a type it uses, fails the build
  until both `bExifCacheVersion` and `repoCacheVersion` are updated. A
  change to the hand-written `Store` instances of `ShortText` or
  `SymbolizedItem` needs both `BExifRevision` and `CacheRevision`
  renamed. The development notes describe both.
- The version detects files of another format, not damaged files: a
  truncated file fails to decode, but changed bytes within a file of
  the right length can decode into wrong values, as before.
