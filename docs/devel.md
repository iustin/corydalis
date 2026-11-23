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
  bundles in `static/corydalis`, which are referred to by the server part.

## Static files

The server serves the files in `static/` (in production, from the
directory set by `static-dir`). Apart from our committed
`static/corydalis/manifest.json`, they are all generated, not committed:

- the esbuild bundles (`npm run build`, also run by `make bootstrap`):
  the scripts in `static/corydalis/js`, from the TypeScript code in
  `js/`; and the style sheet `static/corydalis/css/bundle.css`, from
  `css/bundle.css`, which imports the Bootstrap and Font Awesome style
  sheets and ours (`css/basic.css`), with the Font Awesome fonts copied
  to `static/corydalis/fonts` (their names include a hash of their
  contents, since the style sheet refers to them by relative URLs,
  which have no `etag`, see below);
- `static/tmp`, written at runtime.

When compiling `Settings/StaticFiles.hs`, `staticFiles` creates a route
name for each file (e.g. `corydalis_js_viewer_js` for
`static/corydalis/js/viewer.js`), which the code and templates use.

Libraries, both JavaScript and CSS, are imported by our code and
bundled by esbuild, so they need no link in `static/`. `make
check-static` (also run by `make release` and CI) fails if a file in
`static/` is not used, or is a symbolic link.

Note that the route names include a hash of each file's contents, as
the `etag` parameter of the URLs, so that browsers can cache the files
for long. This hash is computed when `Settings/StaticFiles.hs` is
compiled, but GHC doesn't know that the module depends on the static
files: after changing only them (e.g. the JavaScript or CSS bundles), neither
`stack build` nor touching the module recompiles it (GHC compares the
contents of the sources), and the URLs keep the old hash, so browsers
may keep using their cached copies. `make release`, which starts from a
clean build, is not affected; for other builds, change the module or
build from scratch. The server also needs a restart, as it reads the
files' hashes when it starts.

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
  is where the authorization and CSRF checks happen). But the headers it
  adds with `addHeader` don't reach the static files: yesod-static
  answers with a WAI response of its own, which doesn't include them.

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

Both of these cleanup `dist/` before. Note that on the target machine, the
`static/tmp` directory must exist. And of course, you need the `config/`
directory as well. For more details, see the [install](install.md) document.

## Version upgrades

### Haskell code

- bump stack version in `stack.yaml`, run `make devel`, fix issues
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
