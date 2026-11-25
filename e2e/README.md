# End-to-end tests

These run the real Corydalis server, with a generated sample library,
and drive it in a real browser (Chromium, via Playwright). `make e2e`
runs them from scratch, and CI runs it in its own job.

## What goes here, and what in the unit tests

Each kind of test owns what only it can see:

- the Haskell tests (`test/`, `yesod-test`): what the server decides,
  such as the data a page or endpoint returns, authentication and the
  public-site rules, status codes, and the CSRF check itself;
- the TypeScript unit tests (`js/*.test.ts`, jsdom): the frontend
  logic and all its edge cases, such as sorting and filtering rules,
  viewer state changes, zoom maths and chart data shaping;
- the e2e tests: the wiring between the two, which only breaks when
  the real pieces meet. That is, the bundles working with the real
  templates (element ids, boot data), routes and the CSRF token end to
  end, and what a real browser does that jsdom doesn't (canvas sizing,
  layout, pointer and keyboard events, plotly rendering), plus no
  console errors on any page.

As a rule of thumb, an e2e test covers a user-visible feature with its
normal path and one failure; if it starts listing many cases, the
cases belong in a unit test.

## Files

- `run.sh` (`make e2e`): runs everything from scratch; see below;
- `playwright.config.ts`: the Playwright Test configuration, with two
  projects: `private` (the normal server) and `public` (the public-site
  server);
- `tests/`: the tests:
  - `fixtures.ts`: shared helpers. Every test runs logged in (unless it
    uses `anonymous`), and fails on any browser console error or
    warning, except for the ones it allows (`consoleErrors.allow`);
  - `smoke.spec.ts`: every page loads, with its navigation bar;
  - `tables.spec.ts`: the sortable tables (sorting, groups, filters,
    keyboard use);
  - `flagging.spec.ts`: flagging from the image page (the CSRF token,
    and a stale one);
  - `charts.spec.ts`: the charts are drawn, with hover labels, working
    update menus and resizing;
  - `viewer.spec.ts`: the viewer: navigation (buttons, keys, swipes,
    history), zoom and pan (keys, wheel, drag, double click), the
    overlays, downloading, flagging, and the keys that leave it;
  - `movie.spec.ts`: movies in the viewer (the first frame, and
    playing them), in both projects: next to a picture in the private
    library, and on its own in the public one;
  - `public-site.spec.ts`: what anonymous visitors can and can't see
    or do on a public-site build, and that a logged in user still sees
    everything;
- `setup.sh`: creates a work directory with a sample photo library (20
  folders, 120 small JPEG images with EXIF data), a `settings.yml` for
  it, and a test user;
- `add-movie.sh`: adds a short movie (VP9 in MP4, which Playwright's
  Chromium can play) to the library made by `setup.sh`: on its own in
  a new folder, or with `--paired`, next to an existing picture (which
  keeps the library's counts);
- `add-problem.sh`: adds a folder with a picture that has a problem (a
  truncated JPEG, for which exiftool warns "JPEG format error") to the
  library made by `setup.sh`;
- scripts that only record, for a person to look at or to compare
  before and after a change (they use `lib.cjs`):
  - `pages.cjs`: the state of every page with tables (rows, groups,
    filter controls), to a JSON report, plus screenshots;
  - `shots.cjs`: screenshots of the tables, in a few states;
  - `plots.cjs`: the state of every chart (traces, bars and points,
    labels, hover labels, update menus, the width after resizing), the
    plotly version the browser received, and any console errors, to a
    JSON report, plus screenshots; with `--compare`, compares two such
    reports.

Many tests depend on the exact library made by `setup.sh` (folder and
image counts, names, dates).

## Running

    npm ci && npm run build   # the bundles are embedded in the binaries
    npx playwright install chromium   # once
    make e2e    # or: e2e/run.sh WORKDIR (default _run/e2e)

This needs ImageMagick and exiftool, as for the application itself,
and ffmpeg, for the movie (made by `add-movie.sh`, and its first frame
extracted by the server). It builds the binaries (normal and public
site, without optimisation, in `.stack-e2e` and `.stack-e2e-public`),
makes the two libraries, starts a server for each (on ports 3001 and
3002, so as not to conflict with a development server on 3000), runs
the tests, then the recording scripts, and stops the servers. It exits
with an error if any of them failed. Set `STACK_ARGS` for extra stack
options (e.g. `--ghc-build tinfo6`).

The output goes to `WORKDIR/out`: the HTML test report in `report/`
(`npx playwright show-report WORKDIR/out/report`), the traces and
screenshots of failed tests in `test-results/`, and the recordings in
`pages/`, `plots/` and `shots/`. CI uploads it as an artifact, with the
server logs.

To run the tests again without rebuilding, with the servers from an
earlier `make e2e` run started by hand (see `run.sh` for how), set the
environment variables that `run.sh` sets, then e.g.:

    npx playwright test -c e2e/playwright.config.ts tables

## Comparing the charts before and after a change

To compare the plots before and after a change (e.g. a plotly update),
record them with each version, then compare the reports, and the
screenshots (e.g. with ImageMagick):

    node e2e/plots.cjs WORKDIR/plots-before
    # update, rebuild the bundles (npm ci && npm run build), then the
    # binaries, and restart the server
    node e2e/plots.cjs WORKDIR/plots-after
    node e2e/plots.cjs --compare WORKDIR/plots-before WORKDIR/plots-after
    compare -metric AE WORKDIR/plots-before/curate.png \
      WORKDIR/plots-after/curate.png null:

The comparison exits with an error if any chart differs, or if either
run had console errors. Check that the reported plotly versions are the
expected ones: the browser can only get the new version once the
bundles are rebuilt. The recording scripts use `CORYDALIS_URL`
(default `http://localhost:3000`), `CORYDALIS_USER` (default `tester`)
and `CORYDALIS_PASSWORD_FILE`.
