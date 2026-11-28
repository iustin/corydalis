# Static files bundled by esbuild and embedded in the binary

Date: 2026-11-24. Status: accepted, in PRs
[#87](https://github.com/iustin/corydalis/pull/87),
[#90](https://github.com/iustin/corydalis/pull/90),
[#92](https://github.com/iustin/corydalis/pull/92) and
[#93](https://github.com/iustin/corydalis/pull/93) (the former "Build
and static files" items of the todo list). Written on 2026-11-28, from the
pull requests and their commits.

## Context

Before these changes, `static/` was a mix of committed files, symbolic
links into `node_modules`, and generated files:

- the scripts were bundled by esbuild, but the style sheets (Bootstrap,
  Font Awesome and ours) were linked from `node_modules`, Font
  Awesome's after a patching step (`build/patch-font-awesome`), and
  combined at compile time by yesod-static (`combineStylesheets`,
  into `static/combined`);
- the server served `static/` from disk with `Yesod.Static`, from the
  `static-dir` setting, so a release had to install the binary and a
  copy of `static/` (followed links included), and the server wrote
  widget content into `static/tmp`;
- the URLs carry a hash of each file's contents (the `etag`
  parameter), computed when `Settings/StaticFiles.hs` is compiled.
  GHC didn't know that the module depends on the files
  (`staticFiles` doesn't call `addDependentFile`), so after changing
  only a bundle, a build kept the old hashes, and browsers kept their
  cached copies;
- text files went out uncompressed: wai's gzip middleware skips file
  responses.

## Decision

1. **Everything through esbuild** (#87, #93). `css/bundle.css` imports
   Bootstrap, Font Awesome and our `basic.css`; esbuild bundles it into
   one minified style sheet and copies the fonts it uses, with a
   content hash in their names, since the style sheet refers to them
   by relative URLs, which carry no `etag`. The web app manifest moves
   to the repository root, and esbuild copies it. esbuild empties
   `static/` before building (refusing if it isn't a real directory),
   so `static/` is entirely generated and ignored by git.
2. **A stamp file for the hashes** (#90). esbuild writes
   `.static-files.stamp`, a hash of the names and contents of the
   served files, only when it changes; `Settings/StaticFiles.hs`
   depends on it through `addDependentFile`, and the build fails with
   a clear message without it.
3. **Embedded in the binary** (#92). `Yesod.EmbeddedStatic` replaces
   `Yesod.Static`. Production builds embed the files (text ones
   gzip-compressed) when compiling the module, so the binary is the
   only file to install, and its URLs always match what it serves.
   Development builds read the files from disk on each request, so a
   rebuilt bundle needs only a page reload. URLs move to
   `/static/res/`, the `static-dir` setting and `static/tmp` go away
   (widget content is kept in memory).

## Alternatives

**Keeping yesod-static's compile-time combining of style sheets.**
Rejected: it needed the links into `node_modules` and the Font Awesome
patching, and esbuild was already in the build for the scripts.

**Serving `static/` from disk in production (`Yesod.Static`).**
Rejected: a release needed the binary and a matching copy of
`static/`, which could disagree with the hashes compiled into the
binary; the server needed a writable `static/tmp`; and the files went
out uncompressed.

**Only the stamp, without embedding.** This was the state after #90,
and fixed the stale hashes. Embedding was done on top for the single
file install and the compression. The stamp stays:
`mkEmbeddedStatic` doesn't call `addDependentFile` on the files
either.

## Measurements

From PR #92, measured on its branch (base 547b84e):

| What                                       | Before   | After           |
|--------------------------------------------|----------|-----------------|
| `bundle-plot.js` over the wire             | 1.6 MB   | 540 KB (gzip)   |
| `bundle.css` over the wire                 | 311 KB   | 50 KB (gzip)    |
| Size of the `corydalis` binary             | 66.2 MB  | 67.2 MB         |
| Compiling `Settings/StaticFiles.hs` (`-O`) | about 1 s | about 2.5 s, 183 MB of memory |

## Consequences

- The Haskell build needs `npm run build` (or `make bootstrap`) first,
  for `static/` and the stamp. After a change to `manifest.json`
  (e.g. after a pull), `npm run build` again.
- The embedded subsite answers without Yesod's handler machinery: no
  session cookie (a test checks it), and no `yesodMiddleware` or
  `isAuthorized` for static files.
- wai-app-static sends the gzipped body with `Content-Encoding: gzip`
  without checking `Accept-Encoding`. All browsers accept gzip; only a
  client that explicitly refuses it gets bytes it can't use.
- `make check-static` fails on files in `static/` that nothing serves
  (e.g. a development build's source maps), since they would be
  embedded.
- Deployments: drop `static/tmp` from the service's `ReadWritePaths`
  before deleting `static/`, as systemd refuses to start a service
  with a missing path there.
