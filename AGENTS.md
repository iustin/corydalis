# Agent instructions

Corydalis is a Yesod web app with two separate codebases. Use the matching
toolchain for each; do not mix them.

## Layout

- Haskell (Yesod backend): `src/`, `app/`, `test/`, `templates/`,
  `package.yaml`, `stack.yaml`
- TypeScript (frontend): `js/`, `package.json`, bundled with esbuild into
  `static/`

## Haskell

Stack is the build tool. After Haskell changes:

- Run tests with `make test`; `exiftool`, ImageMagick (`convert`) and
  `ffmpeg` must be installed, as some tests run them (they fail, rather
  than being skipped, if they are missing)
- Obtain coverage with `make coverage-hs` (`make coverage` covers both
  codebases)
- Lint with `make lint`

When bumping the Stack resolver (`stack.yaml`) to an LTS with a new GHC
major version, also raise the `base` lower bound in `package.yaml` to the
`base` version of that GHC (`stack exec -- ghc-pkg field base version`):
only the snapshot is built and tested, so the bound should not claim
support for older compilers.

Add tests for any new Haskell code. Specs live under `test/` and are
discovered by hspec (`test/Spec.hs`). Mirror the module path (e.g.
`src/Pics.hs` → `test/PicsSpec.hs`).

In general, do not use time-based tests (via `threadDelay`) for checking
behaviour; try to use synchronisation primitives instead.

## TypeScript

npm is the frontend toolchain (`package.json`, `js/tsconfig.json`).

- Type-check: `npm run type-check-oneshot`
- Lint and unit tests: `npm test` (oxlint on `js/`, configured in
  `.oxlintrc.json` with type-aware rules and type-checking enabled, then
  `node:test` tests for `js/*.test.ts`, run directly by Node, which
  strips the TypeScript types; imports from tests need explicit `.ts`
  extensions)
- Node.js version: as in `.node-version` (also used by CI)
- Build: `npm run build`
- Coverage: `npm run coverage` (`c8`, configured in `.c8rc.json`; also
  lists the modules no test loads, at 0%), with an lcov report in
  `coverage/ts/` (the Haskell one is in `coverage/hs/`)

Add tests for any new TypeScript code as well.

## Static files

The server serves the files in `static/` (see "Static files" in
`docs/devel.md`). JavaScript libraries are imported by the TypeScript
code and bundled by esbuild: don't add links for them in `static/`. A
link into `node_modules` there is only for a file that the server sends
as it is, and must be removed, with its uses, when the library is
dropped or gets bundled. `make check-static` (run by CI and `make
release`, after `make bootstrap`) fails on unused files and broken
links.

## Documentation

User documentation lives in `docs/`. If a change is user-visible, remind the
user to update the documentation.

Installation and deployment instructions are in `docs/install.md`; update it
when changing the external tools the application needs at runtime (e.g.
ImageMagick, exiftool, ffmpeg), the settings related to serving (HTTPS, Unix
socket, reverse proxies), or the security headers the application sends.

The semi-user important change log is in `CHANGELOG.md` at the root of
the repository. Usually this has a `vNext` section, if the code change you
are generating is meaningful, update the appropriate section (see the file
for structure of older changelog entries).

## General

Add tests for any new code, regardless of language.

Every source file (Haskell under `src/`, `test/` and `app/`, TypeScript
under `js/`, including tests and small entry points) starts with the AGPL
copyright header, with `Copyright (C) 2013 Iustin Pop` as in all existing
files: the year is the project's start, not the file's creation. Copy it
from an existing file of the same language (e.g. `src/Pics.hs` or
`js/viewer.ts`), and keep it when rewriting a file. `test/CopyrightSpec.hs`
checks this.
