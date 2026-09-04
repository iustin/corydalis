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

- Run tests with `make test`
- Obtain coverage with `make coverage`
- Lint with `make lint`

Add tests for any new Haskell code. Specs live under `test/` and are
discovered by hspec (`test/Spec.hs`). Mirror the module path (e.g.
`src/Pics.hs` → `test/PicsSpec.hs`).

## TypeScript

npm is the frontend toolchain (`package.json`, `js/tsconfig.json`).

- Type-check: `npm run type-check-oneshot`
- Lint: `npm test` (eslint on `js/`)
- Build: `npm run build`

Add tests for any new TypeScript code as well.

## Documentation

User documentation lives in `docs/`. If a change is user-visible, remind the
user to update the documentation.

## General

Add tests for any new code, regardless of language.
