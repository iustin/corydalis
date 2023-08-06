# Development notes

A few notes, mostly for myself.

## Bootstrapping in a clean environment

To avoid the scenario of being stuck on a plane without in-flight wifi
and with only a partial setup :), make sure to install:

Operating system:

* on Debian: the usual build packages (`build-essential`, `git`,
  `patch`, `diff`, `rsync`, etc.).
* make sure you have a full-fledged editor (emacs, vim, but not the
  basic vi that Debian installs by default).
* for documentation: `mkdocs`, either from the distribution or via `pip
  install mkdocs`.
* make sure to clone from somewhere else the git configs for your usual
  aliases.
* install the image tools, see the install doc, e.g. `apt install
  imagemagick libimage-exiftool-perl`.
* have some images for a basic library (clone from another development
  machine, or fake ones, etc.).

Haskell:

* stack itself, from distribution or upstream.
* yesod binary, used for `make devel`, via `stack install yesod`.
* hlint tool, via `stack install hlint`.
* todo tool, via `stack install lentil`.
* run a `make git-version` if a clean source dir, otherwise next step
  will fail.
* run a `stack build`, `make test` and `make devel`, to ensure all
  dependencies are there.

JS:

* npm, from distribution or upstream.
* run `npm install` and `make bootstrap` to create initial JS files.

App config:

* create `config/settings.yml` (start from `settings.yml.sample`).
* create the `db` directory - `mkdir -p db`.

Then continue with next section.

## Normal development

This is for normal, incremental work. See below for version upgrades.

You need up to two terminals, depending on which code you edit:

* one terminal for the Haskell part, in which you run `make devel`; this will
  rebuild Haskell sources and reload the development site.
* one terminal for the Typescript parts, in which you run `make ts-watch`; this
  will simply update the `static/corydalis` directory, which is referred to by
  the server part.

## Making a binary build/release

There are two targets for binaries:

* internal, private build; run `make release` for this, which will put
  everything into `dist/` that needs to be copied to the target machine.
* public/demo site, which only needs authentication for some parts of the site;
  run `make demo-release` for this, which again puts things into `dist`.

Both of these cleanup `dist/` before. Note that on the target machine, the
`static/tmp` directory must exist. And of course, you need the `config/`
directory as well. For more details, see the [install](install.md) document.

## Version upgrades

### Haskell code

* bump stack version in `stack.yaml`, run `make devel`, fix issues
* recommend to commit minimal diff to fix building with newer version, and run
  proper cleanup afterwards.

### Node/javascript code

Security problems:

* run `npm audit`, `npm audit fix --dry-run`, then without dry run.
* run `npm update` for minor version upgrades.
* run `npm outdated` and see what can be updated to newer major versions.

Then check behaviour, since there are no UI tests.

Note that `npm update` doesn't upgrade across major versions (of course). So
the process is:

* run `npm outdated` and choose one package.
* edit `package.json` and manually bump version.
* run `npm update`, check it gives no warnings.
* now build/run/test the code.

After each package change, run `make bootstrap` as a one-shot to rebuild the
javascript outputs, or run `make ts-watch` for continuous rebuild (although this
might be tricky during typescript updates/etc.).
