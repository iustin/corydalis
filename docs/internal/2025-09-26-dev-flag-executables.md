# Executables that only build without the dev flag

Date: 2025-09-26, with a follow-up on 2026-11-26. Status: accepted, in
commit bf19af4 and [PR
#107](https://github.com/iustin/corydalis/pull/107). Written on
2026-11-28, from the commits and the pull request.

## Context

The `dev` Cabal flag turns on development settings. With it, the library
is built with `-DDEVELOPMENT -DTEST` and `-O0`. Under `DEVELOPMENT`,
`Foundation` enables `Yesod.Auth.Dummy`: anyone can log in under any
user name, and a user that doesn't exist is created on the spot. Under
`TEST`, modules can export functions only for the tests.

Until then, the flag defaulted to off, so that a release build couldn't
pick it up by accident. That made conditional exports for the tests
impossible: the test suite was built without the flag too.

## Decision

The flag defaults to on, and the executables (`corydalis`, `scanner`,
`db-util`) are not buildable when it is set (nor with `library-only`).
The test suite is only buildable with it. So:

- the library and the tests build with the flag, by default, and the
  tests can use the `TEST`-only exports;
- a binary can only be built with `--flag corydalis:-dev`, and never
  contains the dummy authentication. A development binary can't reach a
  production server, because it can't exist.

### Follow-up: warnings in the executables (2026-11-26)

After the split of `Pics` into the Repository modules ([PR
#104](https://github.com/iustin/corydalis/pull/104)), the release build
on main failed on an unused import in `app/scanner.hs`. No pull request
check had caught it. The build-and-test job uses the dev flag, so it
never compiles the executables. The e2e job builds them without the
flag, but the executables only got `-Wall` and `-Werror` from
`--pedantic`, which only `make release` passed.

PR #107 keeps the flag as it is, and gives the executables the library's
warning options, including `-Werror`. The e2e build also passes
`--pedantic`, so that it stays at least as strict as the release build.
The e2e job is now the check that the executables build, on every pull
request.

## Alternatives

**Keeping the flag off by default.** Rejected: no conditional exports
for the tests (Haskell has nothing like Rust's `cfg(test)`).

**Making the executables buildable with the flag, e.g. so that CI's main
job compiles them.** Rejected: a dev binary has the dummy
authentication, and must never be built, let alone deployed. The non-dev
e2e build checks them instead.

**Adding a separate non-dev build job only for the executables.** Not
done: the e2e job already makes that build, so adding the warning
options and `--pedantic` there was enough.

## Measurements

None: the decision is about what can be built, not about cost.

## Consequences

- `stack build` and `stack test` build the library and tests only.
  Binaries need `--flag corydalis:-dev` (`make release` passes it), and
  benchmarks and profiling builds of the scanner need it too.
- A warning in an executable fails every non-dev build: e2e on pull
  requests, `make dist` and the release.
- The e2e job is the only pull request check that compiles the
  executables; removing or weakening it would leave them unchecked until
  the release build.
