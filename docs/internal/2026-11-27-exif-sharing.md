# Sharing equal exif values per folder, and the exif redesign plan

Date: 2026-11-27. Status: step 1 accepted, in
[PR #110](https://github.com/iustin/corydalis/pull/110); step 2 in
[PR #111](https://github.com/iustin/corydalis/pull/111); steps 3 and 4
planned, not started. Written on 2026-11-28, from the investigation
report, the redesign plan, the commits and the pull request.

## Context

Each file's exif is read on its own, from exiftool's output or from a
cache, so equal values are separate copies: the same camera, lens,
place, keywords or aperture once per file and per picture, and the same
capture date for a raw and its JPEG. Names are interned
(`SymbolizedItem`), but interning only shares the characters: each
`mkSymbolizedItem` allocates its own box, and each `Maybe` its own
`Just`.

An investigation measured how much of the live heap these copies are,
by applying hash-consing passes to a loaded repository, one value type
at a time: about half (table below).

## Decision

**Step 1: a sharing pass, with no type changes** (PR #110). The new
`Repository.Share` module replaces, in a folder, each exif value by the
first equal one met, with one table per value type (optional texts,
doubles, integers, ints, sets, lenses, flashes, dates), local to the
folder: pure, nothing kept between folders or scans. `finishFolder`
calls it first, so the composite images' exif and the derived values
point to the kept values; the scan, the rescan and the cache load all
finish their folders, so all three get it. The cache format doesn't
change.

Values are compared by their exact representation, so that sharing
never changes what is shown: dates by local time and time zone, not by
instant (`ExifTime`'s `Eq` compares instants, which would merge equal
instants in different zones), doubles by their bits (so `0.0` and
`-0.0` stay apart), texts by `SymbolizedItem`'s exact ordering. The
tests follow the types: the fields checked come from `Exif`'s `Data`
instance, a walk through `Data` finds every exif in a folder, and
QuickCheck properties draw near-miss values (same instant in other
zones, case variants, composed and decomposed characters), so a new
field, or a looser ordering, fails them.

**The rest of the plan:**

- Step 2: version the binary exif cache before its layout changes (see
  [A format version for the binary exif cache](2026-11-28-bexif-format-version.md)).
- Step 3: group the exif fields (30 to 20): camera (make, model,
  serial), lens (with its make), place, format groups, each with a
  `Monoid` that keeps the left value unless it is empty. Exposure
  fields stay flat, shared per field, as they don't move together.
  Sharing tables are kept explicit per folder.
- Step 4: move the readers to the groups.
- Merge rule: grouping must not make the merge
  of a picture's files all-or-nothing, since processing tools enrich
  sidecars and outputs beyond the raw, and movies lack camera serials.
  So the merge of files stays field by field within each group; only
  composite images take a whole group from a component (as they do
  already).

Open: the lens merge rule, and a new name for today's derived
`exifCamera` ("Model (#serial)", 33 uses).

## Alternatives

**Unpacked, monomorphic strict maybes** for the number and text
fields, instead of sharing. Measured (below): each takes one more word
per record even when empty, and saves less than sharing; after sharing
they would cost memory. Rejected.

**Dropping `SymbolizedItem` once values are shared.** A sharing pass
makes it mostly redundant for memory, but interning at parse time
lowers the peak during a scan, and it has 157 uses in 19 files. Kept
for now.

**Grouping the fields first.** Grouping saves more (estimated 8 to 11
MB beyond sharing on the library below), but changes the types, the
cache format and every reader. Sharing on the flat types came first,
as it changes nothing visible; grouping is steps 3 and 4.

**A table shared across folders or kept across scans.** Not chosen:
the per-folder pass is pure, runs where the folder is finished, and
keeps nothing alive between scans. The extra saving of sharing across
folders wasn't measured.

## Measurements

Library: 25,800 generated pictures (in each folder, raws with their
JPEGs plus one panorama JPEG; 51,000 files), made from 12 JPEG templates tagged with exiftool, with unique
dates. Optimised non-dev build; a benchmark
program replacing the scanner's `main` (a local tool, not in the
repository) loads the repository, then applies the passes one after
the other and reports the live heap after each. Code: main at d386337.

In this investigation's run, the live heap was 91.6 MB after a cache
load (89.2 MB after a scan), and 43.5 MB after all passes. These are a
few MB above the PR's numbers below for the same library; the runs
differ in build options (e.g. the info tables used for heap profiling
add about 2.5 MB), so compare each table within itself. What each pass
saved (smaller diagnostic passes, e.g. over file names, are left
out):

| Values shared                                         | Saved   |
|-------------------------------------------------------|---------|
| `Just` and symbol boxes of the 13 optional text fields | 15.0 MB |
| Numbers                                               | 11.7 MB |
| People and keyword sets                               | 9.0 MB  |
| Dates (a raw's and its JPEG's)                        | 5.1 MB (3.9 MB after a scan) |
| Lenses                                                | 4.1 MB  |
| Flash                                                 | 1.8 MB  |

Unpacked strict maybes, measured with the same program: 5.7 MB saved
for the 10 number fields, 13.0 MB with the 13 text fields too, against
11.7 MB and 26.6 MB by sharing; on top of sharing they would add
13.5 MB.

With the pass in `finishFolder` (PR #110, same library):

| Repository                | Before  | After   |
|---------------------------|---------|---------|
| Live heap after cache load | 89.2 MB | 44.9 MB |
| Live heap after a rescan  | 89.4 MB | 45.0 MB |
| Cache load time           | 1.0–1.1 s | 1.3–1.5 s |

Still unshared after it: the `Exif` records themselves (about 19 MB),
files' modification times as big integers (estimated 4 MB), and the
search cache (3.5 MB per all-images search).

## Consequences

- Half the live heap for exif-heavy libraries, for about 0.3 s more
  per cache load.
- A new exif field, or a field added to `Image` or `PicDir` that holds
  files, is checked by the sharing tests without listing it.
- The sharing tests compare objects by `StableName`, which doesn't work
  under coverage, so their module is built without hpc (see
  [Coverage only for the code under test](2026-11-27-test-coverage-per-module.md)).
