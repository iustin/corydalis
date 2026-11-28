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

- 2026-11-28: [A format version for the binary exif
  cache](2026-11-28-bexif-format-version.md)
