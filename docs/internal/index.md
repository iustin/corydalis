# Internal notes

Records of design decisions: why the code is the way it is, which
alternatives were considered and why they weren't chosen, and what was
measured to decide. The development notes say how to work with the
code as it is; these say how it got there.

Each record is one file, named after the date of the decision and a
short name (`YYYY-MM-DD-short-name.md`), so that the files sort in
order and records written at the same time don't compete for a
number. A record stands on its own: it explains its context, so it
can be read without the discussion that led to it. It isn't rewritten
later, except to fix mistakes or to link to a newer record; a
decision that is revisited gets a new record, which links to the old
one.

A record has these parts:

- **Context**: the problem, and what the code did before.
- **Decision**: what was done.
- **Alternatives**: the other options, and why each wasn't chosen.
- **Measurements**: the numbers that informed the decision, with how
  they were measured (the code version, the data, the machine), so
  that they can be measured again.
- **Consequences**: what follows from the decision, for users and for
  development.

## Records

- 2025-09-26: [Executables that only build without the dev flag](2025-09-26-dev-flag-executables.md)
- 2026-11-24: [Strictness tests observe behaviour, not the heap](2026-11-24-behaviour-tests-not-heap-inspection.md)
- 2026-11-24: [The memory and cost of a rescan](2026-11-24-rescan-memory.md)
- 2026-11-24: [Static files bundled by esbuild and embedded in the binary](2026-11-24-static-files.md)
- 2026-11-24: [Pinning a fixed vector-hashtables](2026-11-24-vector-hashtables-pin.md)
- 2026-11-25: [Composite images: links between images instead of shadow copies](2026-11-25-composite-images.md)
- 2026-11-25: [A format version for the repository cache](2026-11-25-repository-cache-format-version.md)
- 2026-11-25: [Splitting Pics into Repository modules](2026-11-25-repository-modules.md)
- 2026-11-25: [A generation number for the search cache](2026-11-25-search-cache-generation.md)
- 2026-11-27: [Sharing equal exif values per folder, and the exif redesign plan](2026-11-27-exif-sharing.md)
- 2026-11-27: [Restoring sharing after loading the repository cache](2026-11-27-restore-sharing-on-cache-load.md)
- 2026-11-27: [Coverage only for the code under test: hpc off per test module](2026-11-27-test-coverage-per-module.md)
- 2026-11-28: [A format version for the binary exif cache](2026-11-28-bexif-format-version.md)

The records dated before 2026-11-28 were written on that day, after
the fact, from the pull requests, commit messages and investigation
notes of the time; each says so.
