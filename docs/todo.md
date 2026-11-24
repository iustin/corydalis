# Random notes and TODOs

## Next release

- Review the user documentation (`docs/`, e.g. the manual and the
  installation instructions) against the user-visible changes in the
  change log's vNext section.

## Unit tests

The coverage over the entire codebase is quite low. Most of the
handlers are tested for very basic things, but the main logic of
images scanning/merging/etc. is not.

Some specific missing tests:

- the settings page: check that it shows the configuration;
- search: folders by year, and the quick search;
- the viewer (`js/viewer.ts`), which no test loads: make it testable
  (e.g. split out the logic, as for the tables), and test the
  fullscreen fallback.

## EXIF handling

- History When (XMP/Lightroom)
- Metadata Date (XMP/Lightroom)
- Move exif to fully generic? Track all fields, re-introduce Object
  but binary serialization.
- ISO values given as two words (e.g. for high ISO modes) keep only
  the number (`parseIsoString`); record the mode too.
- The title, caption and other fields can differ between the files of
  an image (raw, sidecar, JPEG), but only one value is kept; how to
  show the differences in the viewer?
- Make exiftool write the metadata of each file separately, instead of
  parsing its combined output (`extractExifs`).

## Image viewer

- from lightroom: region type: face but without name, means unknown
  person.
- sub-image view (copies, raw file, etc.)
- skip panoramas unless explicitly requested
- vertical swipes are ignored; use them for something?
- keyboard panning moves by a fixed 25 (or 50, with shift) pixels; make
  it relative to the canvas size.
- image information (`exif` widget): show the serial numbers, link the
  camera and lens to their pages, and show the capture time earlier.
- the "Loading…" indicator flickers when changing images.

## Statistics

Camera/lens database:

- pics/week average and median

Stats:

- focal length
- camera+focal length
- camera/lens
- aperture
- shutter speed

Maybe generic (1 val, 2 val, 3 val, N val) auto-compute?

- with result field: picture count, etc.

## Search

- Event searches: images don't match event atoms (`imageSearchFunction`,
  which returns `False` for them), and there are no atom statistics for
  events (`getAtoms`); should event atoms find images
  (`atomFindsFiles`)?
- Year search: also match the year of the folder's event?
- Folder class search for images (`FClass`, which finds no images):
  based on the parent folder's class, or something else?

## Image cache

- Build the cached images concurrently (e.g. with
  `pooledMapConcurrentlyN_`).
- Concurrent requests for the same preview not built yet each build it
  (`loadCachedOrBuild`, `extractEmbedded`, `extractFirstFrame`), in
  parallel as far as the tool slots allow, or later once a slot is
  free, repeating the work. A table of the builds in progress, by output
  file, would let the later requests wait for the first build instead,
  then use its result; checking again for the cache file once given a
  slot would already avoid the later builds.
- Make the search cache size (`emptySearchCache`, now 10 entries)
  configurable.
- Previews are also built in sizes larger than the image itself; skip
  those (this needs the image size in `Image`), and leave them out of
  the viewer's image sizes too (`Handler.Widgets`).
- Range files and their components get identical previews
  (`loadCachedOrBuild`).
- convert's warnings are dropped when it succeeds (e.g. "Premature end
  of JPEG file" for a truncated picture, whose preview is still built
  and cached); log them.
- The MIME type of a cached preview comes from the current
  configuration (`thumbnailsize`), not from the file: after changing
  `thumbnailsize` past an existing cached size, that preview is served
  with the wrong type (PNG vs. JPEG) until it is rebuilt. Naming cache
  files by format, or checking the signature when serving, would fix
  it.
- The output of the external tools (`runToolWith`: their error output,
  and the output of those not writing to a file) is kept in memory in
  full while they run, with no limit: a tool writing a lot (e.g. many
  warnings, for a crafted file) uses as much memory. Keeping only its
  start and end, past a limit, would bound it.
- A convert stopped on timeout (it is killed) can't remove its own
  temporary files, e.g. the pixel cache of a large image. Giving each
  run a private temporary directory (`MAGICK_TEMPORARY_PATH`), removed
  afterwards, would clean them up.

## Curate page

- List the movies, as for the other kinds of files.

## Logging

- The application's own log messages (`makeLogFn`) carry no level, so
  under journald a warning and an info message look the same. Prefixing
  each line with the `sd-daemon` priority (`<4>` for warnings, `<6>`
  for info, etc.; see `sd-daemon(3)`) would let journald store the
  level, e.g. for `journalctl -p warning`. Only for journald (and maybe
  as an option), since other destinations would show the prefix as is.

## Messages to the user

- The messages shown on the next page (e.g. "Rescan triggered", or the
  filters skipped by a quick search) are kept in the session, i.e. in
  the session cookie, until shown: this makes every request carry them,
  limits their size (the quick search message is capped for this), and
  they can show on an unrelated page (e.g. in another tab). Rework how
  the server tells the browser about such conditions, e.g. with a query
  parameter on the redirect, or in the page itself where possible.

## Folder cover image

- for consistency, should try to find a landscape-oriented image first
- and with a person, if possible

## Dependencies status

- wai-logger 2.5.1 (which escapes the Apache request log fields) is an
  extra-dep in `stack.yaml`; drop it once the Stackage LTS has it.

Workarounds for upstream bugs, to drop once fixed there:

- typed-process: `runTool` (`src/Pics.hs`) kills the tool's process
  group on interruption, and waits for its recorded exit, working
  around [#84](https://github.com/fpco/typed-process/issues/84) (fixed
  by [PR #93](https://github.com/fpco/typed-process/pull/93)) and
  [#69](https://github.com/fpco/typed-process/issues/69) (fixed by
  [PR #70](https://github.com/fpco/typed-process/pull/70)).
- Yesod: the CSRF token is replaced at login and logout by wrapping the
  session backend (`src/Utils/Session.hs`), relying on Yesod's
  internal `_TOKEN` session key; see
  [#1939](https://github.com/yesodweb/yesod/issues/1939) and
  [#1546](https://github.com/yesodweb/yesod/issues/1546).
- Yesod: the favicon is only cached privately, as handler responses
  carry the session cookie; see
  [#1938](https://github.com/yesodweb/yesod/issues/1938).

## Bugs

### Handling of copies

1. The current copy system is really bad. A copy is declared as such
   even when no master is present (for its deduced original name),
   leading to bad behaviour when multiple copies are possible;
   e.g. with '-' as separator, pic.xxx, pic-a.xxx, pic-a-b.xxx will
   lead to bad outcomes.

