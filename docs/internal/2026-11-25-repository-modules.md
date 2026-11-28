# Splitting Pics into Repository modules

Date: 2026-11-25. Status: accepted, in
[PR #104](https://github.com/iustin/corydalis/pull/104) (commits
73cbea7 and 9b705c1). Written on 2026-11-28, from the commits and the
instructions given for the composite images work.

## Context

`Pics` had grown to hold the repository's types, its live state, the
reading of file names, the building of images and folders, the scan
with its render and cleanup phases, and the previews. The composite
images redesign (see [Composite images](2026-11-25-composite-images.md))
was about to change the name reading and the building of folders.

## Decision

Split `Pics` by job, before the redesign, as a pure move (no function
changes, the same 1193 test examples):

- `Repository`: the types the repository is made of, and the pure
  functions on them;
- `Repository.State`: the live state (context, current repository,
  scan ownership, search cache);
- `Repository.Names`: reading file names (copies, ranges);
- `Repository.Build`: building images and folders from scanned files;
- `Repository.Scan`: walking the directories, the scan with its render
  and cleanup phases, the snapshot on disk;
- `Repository.Encoding`, added with the cache format version: encoding
  and decoding the repository cache.

`Pics` keeps the previews and the choice of file to view. The tests
were split the same way.

Module names must not clash with short names that already exist in
the code base (`Types`, `Stats`, `Cache`, `Model`, `Handler.Folder`,
`Handler.Search`, `Handler.Image` and the like), so that a short name
always means one module. That is why the new modules are under one
`Repository` namespace instead of e.g. `Types` or `Cache`.

## Alternatives

**Changing `Pics` in place for the redesign, and splitting later.**
Not done: the split was asked for first, which also keeps the moves
and the behaviour changes in separate commits.

**Generic names** (`Types`, `Cache`, `Scan` at the top level).
Rejected: several clash with existing modules or handlers.

## Measurements

None: a pure move, checked by the same 1193 test examples passing.

## Consequences

- Importers import the module that holds what they use; `Pics` can't
  re-export the scan without an import cycle.
- The split broke the release build through an unused import in the
  scanner, which no pull request check compiled; see
  [Executables that only build without the dev flag](2025-09-26-dev-flag-executables.md).
