# Development notes

A few notes, mostly for myself.

## Bootstrapping in a clean environment

To avoid the scenario of being stuck on a plane without in-flight wifi
and with only a partial setup :), make sure to install:

Operating system:

- on Debian: the usual build packages (`build-essential`, `git`,
  `patch`, `diff`, `rsync`, etc.).
- make sure you have a full-fledged editor (emacs, vim, but not the
  basic vi that Debian installs by default).
- editor support for Haskell, Typescript/JavaScript, YAML.
- for documentation: `mkdocs`, either from the distribution or via `pip
  install mkdocs`.
- make sure to clone from somewhere else the git configs for your usual
  aliases.
- install the image tools, see the install doc, e.g. `apt install
  imagemagick libimage-exiftool-perl ffmpeg`; the tests also need
  ImageMagick to read and write HEIC images, which on recent
  Debian/Ubuntu releases needs `libheif-plugin-libde265` and
  `libheif-plugin-x265`.
- have some images for a basic library (clone from another development
  machine, or fake ones, etc.).

Haskell:

- stack itself, from distribution or upstream.
- yesod binary, used for `make devel`, via `stack install yesod-bin`.
- hlint tool, via `stack install hlint`.
- todo tool, via `stack install lentil`.
- lcov coverage generator, via `stack install hpc-lcov`.
- for use with VS Code, the hspec-discover tool, `stack install
  hspec-discover`.
- run a `make git-version` if a clean source dir, otherwise next step
  will fail.
- run a `stack build`, `make test` and `make devel`, to ensure all
  dependencies are there.

JS:

- Node.js and npm, in the version given in `.node-version` (also used by
  CI); a version manager such as `fnm` or `nvm` picks it up from there.
  Distribution packages are usually too old.
- run `make bootstrap` to install the npm dependencies (with `npm ci`,
  i.e. exactly as in `package-lock.json`) and create the initial JS
  files. The committed `.npmrc` disables the install scripts of the
  dependencies (e.g. `postinstall`), which would otherwise run arbitrary
  code at install time; none are needed. If a new dependency does need
  one, run it explicitly, e.g. with `npm rebuild <package>
  --ignore-scripts=false`, and document it here.

App config:

- create `config/settings.yml` (start from `settings.yml.sample`).
- create the `db` directory - `mkdir -p db`.

Then continue with next section.

## Normal development

This is for normal, incremental work. See below for version upgrades.

You need up to three terminals, depending on which code you edit:

- one terminal for the Haskell part, in which you run `make devel`; this will
  rebuild Haskell sources and reload the development site.
- one terminal for the Typescript parts, in which you run `make ts-watch`; this
  will continuously type-check the TypeScript code.
- if you edit the TypeScript code or the style sheets (in `css/`), one more
  terminal in which you run `npm run watch`; this will rebuild the esbuild
  bundles in `static`, which are referred to by the server part.

## Static files

The server serves the files in `static/`, at `/static/res/` (e.g.
`/static/res/js/viewer.js`). The directory is entirely written by
esbuild (`npm run build`, also run by `make bootstrap`, or
`npm run watch`), which first empties it (refusing to if `static` is
not a real directory, e.g. a symbolic link, whose target it would
empty), and is not committed:

- the scripts in `static/js`, bundled from the TypeScript code in
  `js/`;
- the style sheet `static/css/bundle.css`, from `css/bundle.css`,
  which imports the Bootstrap and Font Awesome style sheets and ours
  (`css/basic.css`), with the Font Awesome fonts copied to
  `static/fonts` (their names include a hash of their contents, since
  the style sheet refers to them by relative URLs, which have no
  `etag`, see below);
- the files copied as they are (`copiedFiles` in `esbuild.js`): the
  web app manifest, `manifest.json` at the root of the repository.

`Settings/StaticFiles.hs` defines the static subsite (with
`Yesod.EmbeddedStatic`), and a route name for each file (e.g.
`js_viewer_js` for `static/js/viewer.js`), which the code and
templates use. Hidden files are skipped. How the files
are served depends on the build flags:

- in production builds (`-dev`, as made by `make release`), the files'
  contents are embedded in the binary when compiling the module, so
  the binary needs no `static/` directory at runtime; text files
  (scripts, style sheets, `manifest.json`) are embedded compressed
  with gzip, and sent as such (with `Content-Encoding: gzip`), even to
  clients that don't ask for it, which all browsers do anyway;
- in development builds (with the `dev` flag, the default, used by
  the tests, or the `library-only` one, used by `yesod devel`), the
  files are read from `static/` on each request, so after a change to
  them (e.g. by `npm run watch`), reloading the page is enough. Adding
  or removing a file still needs a rebuild, for its route name: e.g.
  the source maps that `npm run watch` writes (unlike `npm run build`)
  are only served once `Settings/StaticFiles.hs` is recompiled, which
  `yesod devel` does when the stamp file changes.

Older checkouts may have a `static/tmp` directory, which the server
used to write, and a `static/corydalis` one, where the bundles used to
be; `npm run build` deletes them, as it empties `static/`.

Style sheets and scripts generated by widgets (`addStaticContent`;
there are none at the moment) are kept in memory by the subsite, and
served at `/static/widget/`.

Libraries, both JavaScript and CSS, are imported by our code and
bundled by esbuild. `make check-static` (also run by `make release`,
`make dist` and CI) fails if a file in `static/` is not used by the
server (by its route name), e.g. a bundle that no page loads, or the
source maps of a development build, which would otherwise be embedded
too.

In production builds, the route names include a hash of each file's
contents, as the `etag` parameter of the URLs, so that browsers can
cache the files for long. This hash, and the embedded contents, are
computed when `Settings/StaticFiles.hs` is compiled, but GHC doesn't
know that the module depends on the static files, so it depends
instead on the stamp file `.static-files.stamp` (not committed): a
hash of the names and contents of the files in `static/` that are
served, written by `npm run build` (thus also by `make bootstrap`, and
by `npm run watch` after each rebuild), only when it changes. After a
change to the static files, the next build recompiles the module, with
the new contents and hashes; building without the stamp file fails,
asking to run `make bootstrap`. Note that `manifest.json` is copied,
so after it changes (e.g. after a `git pull` or a branch switch), run
`npm run build` (`npm run watch` copies it on each change).

## The repository cache's format version

The repository cache (`repo-cache` in the cache directory) holds the
configuration and the repository, encoded with `store`, behind a format
version: a hash of the shape of the cached types, computed at compile
time by `Data.Store.Version` from their `Data` instances. The shape is
the names of the types (with their modules), constructors and fields,
and the types of the fields. A cache with another version, or from
before versions, fails to decode, and the application scans instead.

The expected hash is pinned in `repoCacheVersion` (`src/Repository.hs`).
After a change to the shape of any type reachable from what the cache
holds (those in `Repository`, and the ones they use from `Types`,
`Exif`, `Stats` and `AtomTypes`), even only a renamed field or a type
moved to another module, the build fails. Check that the change is the
intended one, and copy the new hash into `repoCacheVersion`: in the
error, the new hash is the one called "expected", the pinned one is
called "specified". The build writes the new shape to
`dist/store-versioned/<hash>` (or under `.stack-work` if the `STACK_EXE`
environment variable is set when compiling, which `stack build` doesn't
do). `make clean` removes it, and `make dist` and `make release` don't
leave it next to the binaries. If the same directory built the previous
version, its shape is there too, and the error shows the `diff` command
to compare them.

A new type in the cache needs a `Data` instance: a `deriving Data` for
most, and an orphan instance for a library type without one (as for
`COff`). A type whose `Data` instance has no representation (`NoRep`)
fails the build, unless it is listed in `vcIgnore`.

The hash covers the shape, not the encodings, so a change to an
encoding that keeps the shape isn't noticed by itself. That is the case
for the hand-written `Store` instances: those of `Regex` and
`SymbolizedItem` (whose `Data` instances are hand-written too, so their
constructor's name can be changed), and that of `ShortText` in
`Compat.Orphans`. It is also the case for a library that changes how it
encodes a type whose name stays the same. For these, rename the
constructor of `CacheRevision` (in `Repository`) to the next number:
it is written in no byte of the cache, but it is part of the shape.
The binary exif cache holds `ShortText` and `SymbolizedItem` too, so
for these two rename `BExifRevision` (in `Exif`) as well (see below).

The names of library types are part of the shape too, so a resolver
upgrade can change the hash with no change to the code; update it then
as well.

## The binary exif cache's format version

Each media file's exif data is cached twice: as exiftool's JSON
output (`<file>-exif`), and, once decoded, encoded with `store`
(`<file>-bexif`), which a scan reads first. The binary files have a
format version too, computed the same way, from the shape of
`CachedExif` (in `Exif`). A file with another version, or from before
versions, isn't used: the scan reads the JSON file instead, without
running exiftool, and writes the binary file again. Each file starts
with the version (a 32-byte prefix: a 4-byte marker, the hash's length
and the 20-byte hash), as a file is only rewritten when a scan reads
it, so files with different versions can be in the cache at the same
time: for example those of a source directory taken out of the
configuration and added back later, or when builds of different
versions share a cache directory.

The expected hash is pinned in `bExifCacheVersion` (`src/Exif.hs`),
and updated as for the repository cache. As the repository cache
holds `Exif` too, a change to the shape of `Exif` (or of a type it
uses) changes both hashes; renaming `BExifRevision` only changes this
one. For an
encoding change that the shape doesn't show (the hand-written `Store`
instances of `SymbolizedItem` and `ShortText`, or a library's), rename
the constructor of `BExifRevision` (in `Exif`) to the next number, as
well as that of `CacheRevision`.

Why the binary cache has a version in each file, rather than one for
the whole cache, and what that costs, is in the internal note
[A format version for the binary exif
cache](internal/2026-11-28-bexif-format-version.md).

## Middlewares: where headers and checks go

Requests go through two layers of middleware, which see different
things:

- `appMiddlewares` (in `Application.hs`) wraps the whole WAI
  application, so it sees every response, however it was made: pages,
  redirects, errors, the authentication pages, and static files. It only
  knows HTTP (the request and the response, with their headers), and the
  settings it was built with; it can't use the session, the logged in
  user, or the routes.
- `yesodMiddleware` (in `Foundation.hs`) runs inside `Handler`, around
  each request's handler, so it can use the session, the user, the
  route and the settings, and can refuse or redirect the request (this
  is where the authorization and CSRF checks happen). But it doesn't
  run at all for the static files: the embedded static subsite answers
  them directly, with a WAI response of its own, so neither
  `isAuthorized` nor the headers added with `addHeader` apply to them
  (they are public, and don't touch the session).

So a header that every response must carry (the security headers,
`Cache-Control`) goes in `appMiddlewares`; anything that depends on the
user or the route, or must be able to stop a request, goes in
`yesodMiddleware`. A header meant for all responses, but added in
`yesodMiddleware`, silently misses the static files (as HSTS used to).

## Making a binary build/release

There are two targets for binaries:

- internal, private build; run `make release` for this, which will put
  everything into `dist/` that needs to be copied to the target machine.
- public/demo site, which only needs authentication for some parts of the site;
  run `make demo-release` for this, which again puts things into `dist`.

Both of these cleanup `dist/` before. The static files are embedded in
the `corydalis` binary, so they don't need to be copied; you need the
`config/` directory, though. For more details, see the
[install](install.md) document.

## Version upgrades

### Haskell code

- bump stack version in `stack.yaml`, run `make devel`, fix issues
  (this includes the format versions of the repository cache and of
  the binary exif cache, see "The repository cache's format version"
  and "The binary exif cache's format version")
- recommend to commit minimal diff to fix building with newer version, and run
  proper cleanup afterwards.

### Node/javascript code

Security problems:

- run `npm audit`, `npm audit fix --dry-run`, then without dry run.
- run `npm update` for minor version upgrades.
- run `npm outdated` and see what can be updated to newer major versions.

Then check behaviour, since there are no UI tests.

Note that `npm update` doesn't upgrade across major versions (of course). So
the process is:

- run `npm outdated` and choose one package.
- edit `package.json` and manually bump version.
- run `npm update`, check it gives no warnings.
- now build/run/test the code.

After each package change, run `make bootstrap` as a one-shot to rebuild the
JavaScript and CSS bundles, or run `npm run watch` for continuous rebuild
(although this might be tricky during typescript updates/etc.).

## Releasing a new version

Decide new version strings. For "CalVer", use:

```shell
date +'%Y.%V'
```

or alternatively, `make this-week`. This will generate the baseline calver
version number. Append a '.0' as patch level, or if already on the same week,
just bump the patch level (and congrats!).

Then:

- Update `package.yaml` with new version (again, pay attention to if it's the
  same).
- Build, test, etc.
- `git tag` the new version
- Push everything to github.
- Upload new version to [demo.corydalis.io](http://demo.corydalis.io).
- Party!
