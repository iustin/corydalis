# Changelog

## vNext

### Improvements

- The source and output directories no longer need a fixed two-level
  structure (e.g. `year/folder`): any directory whose name matches the
  `dirregex` setting is a folder (with all its sub-directories, as
  before), and other directories are searched for folders, down to
  the new `maxfolderdepth` setting (4 by default); directories at that
  depth that aren't folders are listed as errors on the status page.
  The trees also don't need to have the same structure, as folders
  are still matched by name. Note two changes for existing trees: a
  directory directly below a source or output directory whose name
  matches `dirregex` is now a folder, so any date-named directories
  inside it are no longer separate folders; and directories at the
  second level whose names don't match are now searched for folders,
  instead of being ignored.

- A dark theme: the theme button in the navigation bar switches
  between following the system's light or dark setting (the default),
  and always using the light or the dark theme; the choice is kept by
  the browser. The charts follow the theme too.

- The static files (scripts, style sheets, fonts) are now embedded in
  the `corydalis` binary, so the `static` directory no longer needs to
  be installed with it, and the files always match it; the scripts and
  style sheets are also sent compressed (with gzip), which they weren't
  before (unless a reverse proxy compressed them). Their URLs change from `/static/corydalis/…`
  to `/static/res/…`. When upgrading, the `static` directory next to the
  binary can be removed, as can the `static-dir` setting, which is
  now ignored; for a systemd service based on the sample, first remove
  `static/tmp` from its `ReadWritePaths`, as systemd doesn't start a
  service if one of these paths is missing.
- A rescan keeps less in memory, and gives it back once done. The
  previous scan's data is dropped as soon as the new data is in use;
  it used to be kept until the scan ended, through the rendering of
  the previews and the cleanup of the cache. And the memory the
  runtime returns to the system once the scan's garbage is collected
  is really released: the binaries are now built with the
  `--disable-delayed-os-memory-return` runtime option, without which
  that memory still counted as used by the process until the system
  needed it (on a test library of 24,000 pictures, the resident memory
  after a rescan went from about 580 MB to about 230 MB with this
  option). The end of a scan also forces a garbage collection, so
  that most of its memory is released when it ends, and not only at
  the next major collection, which can be much later with a long idle
  interval (`-I`); on the same library, the resident memory when a
  rescan ends goes from 340 to 420 MB to about 290 MB.
- A rescan does less work: the cache cleanup reads the cache directory
  once, instead of first counting its files, and only reads the names
  and types of its entries; the check of the previews resolves the
  picture directories once, instead of for each picture; and the exif
  caches are read with fewer file system calls. On a test library of
  24,000 pictures, a rescan where nothing changed allocates about a
  sixth less. The progress of the cache cleanup on the status page is
  now based on an estimate of the number of files, and stops short of
  complete (by about 5%) until the cleanup ends.
- After a restart, the library read from the repository cache takes
  as much memory as after a scan: reading the cache made copies of
  values that a scan shares (e.g. each picture's exif data with that
  of its files), so the library took about a quarter more memory
  (105 MB instead of 82 MB on another test library, of 25,000
  pictures).
  Reading the cache takes slightly longer (about 0.2 s more on the
  same library).
- The library takes about half as much memory as before, both after a
  scan and after a restart: equal exif values (the same camera, lens,
  place, keywords, exposure values, or the capture date of a raw and
  its JPEG) are now kept once per folder, instead of once for each
  file and picture. On a generated library of 25,800 pictures, each
  with a raw and a JPEG, the memory the library itself takes (its
  live heap) goes from 89 MB to 45 MB. Reading the cache takes about
  0.3 s more on that library (from about 1.0 s to 1.3 s), and a scan
  does the same extra work.

- The repository cache now has a format version, which changes with
  the shape of the data it holds, and a cache of another version is
  ignored (the application scans instead). Before, a cache written by
  an older version was only ignored if it failed to decode, and
  nothing guaranteed that it did. The first start after upgrading
  scans the library again, as the existing cache has no version.
- Composite images (panoramas, HDR merges, stacks), which the manual
  called "range" files, are now linked to their components by name,
  instead of lending them their files (and borrowing the first
  component's raw); the rules, with examples, are in the new
  "Composite images" page of the documentation. What changes:
  - The status and list pages now count the components, and a
    composite image without a master, as processed, as the folder
    pages already did. On the folder pages, sizes are no longer
    counted twice: a composite image's output was counted once per
    component too, and the first component's raw again for a
    composite image without a master. So the numbers change for
    folders with composite images.
  - A component is viewed from its own files (its raw, if it has no
    output of its own), not from the composite image's output.
  - A composite image without a master is processed when any of its
    components has a master, not only the first one.
  - A composite image takes the date, camera, lens and place its own
    files lack from its first component; and components no longer
    get the composite image's people and keywords.
  - The image page links a composite image to its components, and a
    component to the composite images covering it.
  - Browsing, the viewer and search results show a composite image
    instead of its components that have no output of their own, and
    a search that finds such a component shows the composite images
    covering it, each once; a search by file name or problem shows
    the component itself. The folder covers and the preview rendering
    after a scan skip these components too. They are still counted,
    listed on the folder's page and the composite image's, and can be
    opened from there.
  - Names are read by their numbers: a name is a range when its last
    number is not below the first and has at least as many digits
    (or it is a camera counter that wrapped, e.g. `dsc_9998-0002`);
    otherwise the copy suffix is removed as before. So `x_1-3` is now
    a composite image of `x_1` to `x_3`, not a copy of `x_1`, while
    `dsc_1920-2` and `dsc_0001-2` stay copies.
  - The most names a range can have is now the `maxrangesize` setting
    (1024 by default, as before).
  - A lone sidecar named like a range (e.g. `d_01-03.xmp`, left over
    from a deleted panorama) is now a composite image too, so its
    components count as processed; before, it covered nothing.
  - Movies are no longer part of composite images: a movie named like
    a range no longer covers anything, and a movie named like a frame
    is not a component.

### Fixed bugs

- The binary exif cache files (`*-bexif` in the cache directory) now
  have a format version, as the repository cache does. A file written
  with another format, which could be decoded into wrong values, is
  now ignored: the exif data is read again from the raw exif cache
  (`*-exif`), without running exiftool, and the binary file is
  rewritten. After upgrading, the first scan (at the first start, as
  the repository cache is ignored too, see above) does this for every
  file, so it takes somewhat longer.
- If a rescan fails while rendering the previews or cleaning up the
  cache, the application keeps the pictures that scan found (with the
  error on the status page, as before); it used to go back to the
  previous scan's data, bringing back the pictures removed since.
- When a rescan started rendering the previews, the status page could
  show, for a moment, the previous rendering's progress as the new
  one's.
- Two scans started at the same time (e.g. two rescan requests, or the
  first requests at startup without a cache) could leave the status at
  "starting", with no scan running, until the next rescan.
- A failed scan is now logged, with its error; before, the error was
  only shown on the status page, and was lost if a newer scan had
  started meanwhile.
- A search running while a rescan published its new data could cache
  results computed from the previous data: later identical searches
  then still listed the pictures removed (as broken entries), and
  missed the new ones, for the rest of that rescan, or until the next
  one. Such results are no longer cached, and the search is done again
  on the new data. The search cache is also no longer emptied by the
  rescan's status changes, only when it publishes new data.
- The copy and range regexes are now matched against the file name
  only, not against its path relative to the folder: previously, a
  picture in a sub-directory whose name the regex matched (e.g.
  `2024-01-02-day/b.jpg`, with the sample copy regex) got a truncated
  name (`2024-01`), and could be merged with unrelated pictures.
- The sample settings anchor the copy and range regexes (`^…$`), as
  otherwise they also match in the middle of names, e.g. the sample
  copy regex turned `IMG-20240101_1234` into `IMG`. If your settings
  were copied from the sample, consider anchoring them too; note that
  this can change some picture names, and thus how they are grouped.
- In the image viewer, the zoom button now shows a minus when zoomed
  in, as clicking it then zooms back out to fit; it always showed a
  plus.
- In the image viewer, the up arrow key now toggles the full screen
  state, as intended; the key was bound under a wrong name, so it did
  nothing.
- On the statistics pages, a chart that fails to draw no longer stops
  the charts after it from being drawn; such failures, chart data of an
  unexpected shape, and an unknown chart page are now logged to the
  browser console instead of passing silently.
- On a lens's page, the timeline chart's axis now says "Per camera
  timeline stats", as it shows the lens's use per camera, instead of
  "Per lens timeline stats".
- Changes to the static files (e.g. the JavaScript or CSS bundles)
  now make the next build recompile the URLs of the files, whose hash
  parameter tells browsers to fetch them again; previously, only clean
  builds (as `make release` does) did, and other builds kept the old
  URLs. The build writes a stamp file, `.static-files.stamp`, for this,
  and the Haskell build fails if it's missing: run `make bootstrap`
  first.
- The same name (e.g. of a camera, lens or folder) could rarely be
  stored as two separate values that don't compare equal, after the
  memory of unused names was reclaimed, which could split or misgroup
  pictures. This was a bug in the `vector-hashtables` library, used for
  interning names; Corydalis now builds with a fixed version of it (see
  Dependencies).
- A composite image's components could stay unprocessed when the
  folder also had files in a directory read after the composite
  image's (e.g. a TIFF composite in the source directory, with an
  output directory for the same folder; or a composite in the first
  of two output directories), as only the last directory's links
  survived the merge.
- A folder's year and first date could disagree with its pictures'
  dates, when the files of one picture carry different dates (e.g. a
  raw and an output with a corrected clock): they were the earliest
  over each directory's files, and are now the earliest of the
  pictures' dates.

### Internal changes

- The loading of the exif data (`getExif`, with the exif caches and the
  exiftool runs) moved from `Exif` to a new `Exif.Load` module, which
  encodes the binary exif cache behind a format version computed at
  compile time, like the repository cache's. The expected hash is
  pinned in `bExifCacheVersion` (in `Exif`); a change to the shape of
  `Exif` now fails the build until both it and `repoCacheVersion` are
  updated. See the development notes.
- `PicDir`'s `pdShadows` is gone: an image's `imgRole` holds its
  links, to its components or to the composite images covering it,
  set by `finishFolder`, which also computes all of a folder's derived
  values (time index, year, first date, statistics, group exif,
  implicit event) once its directories are merged, instead of per
  directory and again at each merge. As the statistics are empty
  until then, `mergeFolders` picks the main path by counting the
  images with a master.
- `imgRange` is now a `Maybe ImageRange` (root, numbers and padding)
  instead of a pair of names, read by `parseImageName`; the range
  limit is `cfgMaxRangeSize`.
- `Pics` is split by job: the repository's types and pure functions
  are in `Repository`, its live state (the context, ownership of
  updates, the search cache) in `Repository.State`, reading names in
  `Repository.Names`, building images and folders from scanned files in
  `Repository.Build`, and the scan with its render and cleanup phases
  and the repository cache in `Repository.Scan`. `Pics` keeps the
  previews and the choice of file to view. The tests are split the
  same way, with the helpers they share in `TestImport`.
- The types the repository cache holds have `Data` instances (mostly
  derived), from which the cache's format version is computed when
  compiling, in `Repository.Encoding`. A change to their shape fails the
  build until the version pinned in `repoCacheVersion` is updated; see
  the development notes.
- `static/` is now entirely generated by esbuild, which empties it
  before building; the web app manifest moved to the root of the
  repository, and is copied there. `make check-static` no longer looks
  for symbolic links, which esbuild doesn't write.
- The image viewer, the chart pages and the image grid are split into
  modules that the unit tests can load, with the browser-only libraries
  (Bootstrap, Plotly, screenfull, Masonry, Infinite Scroll) passed in by
  small entry points; they are now tested, raising the TypeScript test
  coverage from about 30% to 96% (the rest being mostly the entry
  points).
- The Haskell tests now cover the image viewer's navigation data, the
  camera and lens pages, the exiftool output parsing, the repository
  cache and more of the handlers and helpers, raising the Haskell test
  coverage from about 85% to 92% (the rest being mostly records, class
  instances and the application's entry points); the camera and lens
  pages now share their image counting code.
- The style sheets (Bootstrap, Font Awesome and ours) are bundled by
  `esbuild` into one minified file, as the TypeScript code is, with
  the fonts it uses; this replaces the links into `node_modules` in
  `static/`, the patching of the Font Awesome style sheets, and the
  combining of the style sheets by the Haskell build in
  `static/combined`.
- The repository cache is written once per scan, when it finishes; it
  used to be written a first time before rendering the previews, only
  so that a restart at that point would scan again, for which the
  previous cache is now simply removed.
- Many values built during a scan, and by the statistics pages and
  the searches, are now evaluated as they are built (strict maps, folds
  and fields), instead of being left as chains of pending computations
  holding on to their inputs; and the new repository is fully built
  before it replaces the current one, rather than partly inside that
  transaction.

### Dependencies

- vector-hashtables needs the fix for
  [#43](https://github.com/klapaucius/vector-hashtables/issues/43)
  (`alter` losing keys), not yet released; `stack.yaml` pins it from
  git (the fix from
  [PR #46](https://github.com/klapaucius/vector-hashtables/pull/46)),
  and builds without Stack need to apply that fix themselves.

## v2026.48.0 - "Security hardening"

Released: _Mon, 23 Nov 2026_.

This release started with a few features planned and under way: the
reworked view modes, more camera and lens statistics, and folder
events. Then security analyses of the code became available, and the
focus switched to them; as a result, some of these features are only
half-done, folder events most of all (see below).

The main goal of this release is thus to close a lot of gaps in the
security posture of Corydalis: how it handles untrusted files in the
picture folders and the external tools reading them, what anonymous
visitors of a public site can see, sessions and logins, HTTP headers,
logs, and resource limits. Along with this, the installation
documentation now describes the deployment much more completely:
serving over HTTPS or a Unix socket behind a reverse proxy, limiting
login attempts, the image decoders, movie demuxers and ImageMagick
policy, running as a sandboxed systemd service, session lifetime, and
who should be able to read or write the private, picture and cache
directories. The frontend was modernised along the way: it is now
entirely TypeScript, bundled with `esbuild`, and no longer uses jQuery.

In total, 32 security issues were fixed (the items of the Security
section below): 5 of high severity, 7 medium, and 20 low or hardening
only. The severity of many depends on who can write to the picture
folders: the high ones need a crafted file in them (e.g. a photo with
crafted metadata, or a symbolic link), except for the session key
readable by the other users of the machine. Four issues found along the
way were also reported to upstream libraries: two to Yesod, one to
yesod-auth-hashdb, and one to wai-logger, already fixed there (in
version 2.5.1).

In a way, this feels like a first real production release, given how
many gaps were filled.

The security analyses, and the fixes, were done with a combination of
Claude Opus 5.5, Claude Fable 5.2, Gemini 4 Neon, and Grok 4.7, with code
reviews done internally (e.g. Claude's pr-review-toolkit), by GitHub
Copilot, and across agents (Opus writing code, Neon reviewing it on top of
Copilot). I still read most code - all backend code, and to a large extent
the frontend. Since agents are diligent at writing tests, test coverage
went up significantly as well.

As a lot changed since v2025.21.0, read the upgrade notes first.

### Upgrading

Configuration changes:

- The new `viewableimages` key is required: it lists the file
  extensions that browsers can show directly, without conversion
  (previously hard-coded). The sample configuration has a starting
  point.
- `sourcedirs`, `outputdirs` and `cachedir` must be absolute paths;
  Corydalis refuses to start otherwise.
- `secure-sessions` is now optional and defaults to true, and
  `https: true` now makes the cookies secure when serving HTTPS directly
  (not on a Unix socket). Serving plain HTTP
  (other than on `localhost`) needs `secure-sessions: false`; Corydalis
  warns at startup if `approot` is a plain `http://` URL while the
  cookies are secure.
- The `Strict-Transport-Security` header is no longer tied to
  `secure-sessions`, but has its own `strict-transport-security`
  setting, which defaults to `https` (and to off with a Unix socket).
  Previously, Corydalis sent it whenever `secure-sessions` or `https`
  was set: behind a reverse proxy with `https: false`, either set
  `strict-transport-security: true`, or (better) have the proxy send it;
  if the proxy already sends it, leave it off here.
- Previews are now built with an explicit ImageMagick decoder (per
  file extension) and ffmpeg demuxer (per movie extension), with
  defaults for the usual formats: if you added extensions to
  `jpegexts`, `viewableimages` or `movieexts`, check that the new
  `imagedecoders` and `moviedemuxers` settings cover them, otherwise
  those files get no previews. The defaults cover JPEG, HEIC/HEIF,
  TIFF, PNG and WebP pictures, and MP4, M4V, MOV, 3GP, AVI, MKV, WebM,
  MTS/M2TS and MPEG movies; a decoder added to `imagedecoders` must
  also be allowed by the ImageMagick policy (see below).
- Corydalis now ships an ImageMagick security policy,
  `config/magick/policy.xml`, used when `magickpolicydir` points to its
  directory (as in the sample configuration). Copy it when installing:
  Corydalis then refuses to start unless ImageMagick loads it. Its
  resource limits (e.g. images of at most 32K pixels per side) replace
  the system ones, so check them against your largest pictures (e.g.
  panoramas).

Deployment changes:

- When building from source with Stack directly, add
  `--flag corydalis:-dev` (to both `stack build` and `stack install`):
  the `dev` flag is now on by default, for the tests, and disables the
  executables. `make release` already does this.
- The cache directory must be a single filesystem (files are written
  in its `.tmp` directory, emptied at startup, then moved into place),
  and each instance needs its own: a production and a development build
  can no longer share one.
- `/dev/fd/0` and `/dev/fd/1` must exist (as on any usual Linux system,
  including under systemd's `PrivateDevices`), since ImageMagick now
  reads and writes through them.
- HEIC/HEIF previews have always needed libheif's HEVC decoder, which
  recent Debian and Ubuntu releases package separately
  (`libheif-plugin-libde265`); the installation documentation now says
  so.
- Symbolic links to files, and special files, in the picture folders
  are now skipped, instead of being treated as pictures, and listed as
  errors on the status page; so are symbolic links to directories,
  which were already ignored, but silently. To keep pictures elsewhere,
  add their location to `sourcedirs` or `outputdirs`.
- The `config` directory (holding the session key) and the database
  directory are restricted to their owner at startup (with a warning if
  that's not possible), and may not be inside the picture or cache
  directories, otherwise Corydalis refuses to start.
- If a reverse proxy in front of Corydalis sets a
  `Content-Security-Policy` header, align it with the one Corydalis now
  sends (browsers apply both).

On the first start after upgrading:

- the database migrations (marking deleted users, the users' session
  generation, and dropping the foreign key on the flagged images'
  author) are done automatically;
- the repository is rescanned, as the configuration is part of its
  cache;
- everybody has to log in again once, and pages opened before the
  upgrade must be reloaded; scripts posting to Corydalis must now send
  a CSRF token, and real PUT/DELETE requests to flag images.

Optional clean-ups:

- Small pictures were upscaled in the cache; the fix doesn't remove the
  existing too-large previews, so consider wiping the cache.
- Thumbnails built before the upgrade contain the full server path of
  their picture (see below); they are the cache files ending in a dash
  and a size up to `thumbnailsize` (e.g. `-64`), and can be removed to
  rebuild them.
- If you used the detailed request logging, remove the old logs, which
  contain the login passwords (see below).
- If untrusted users can add files to your picture folders, check for
  files named starting with a dash, and for unexpected `_original`
  backups (see below).

### Security

These were found by security reviews of the code, using AI tools.

#### Files in the picture folders

Pictures and movies often come from elsewhere, so their contents
(including their metadata) are untrusted. The structure of the picture
folders (which files are there, and their names) is trusted, as only
the administrators should be able to change it (see the installation
documentation), but it is now handled defensively too:

- File names starting with a dash were interpreted as options by
  `exiftool` while scanning: a file named e.g. `-Comment=x.jpg` could
  switch it to write mode, and modify, rename or move the other files
  scanned with it. File names are no longer read as options.
- ImageMagick guessed the format of a file from its contents, so a
  PostScript or SVG document named e.g. `photo.jpg` (or embedded as the
  preview of a RAW file) was decoded as such, PostScript through
  Ghostscript, which has had several sandbox escapes. ImageMagick is
  now told which decoder to use, based on the extension (see
  `imagedecoders`; decoders running programs or reading other files are
  refused), and restricted by the shipped security policy.
- Similarly, ffmpeg guessed the format of movies, and some formats
  (e.g. a concat script named `movie.mp4`) make it read other files. It
  is now told which demuxer to use (see `moviedemuxers`; only plain
  container formats are accepted), and only allowed to read local
  files.
- Files were served with the type detected from their contents, so an
  SVG or HTML file named `photo.jpg` was served as a document, and could
  run scripts as the logged-in user. Only image and video types
  (excluding SVG) are passed through, and anything else is served as a
  JPEG image or MP4 movie; files opened directly are sandboxed; and all
  responses forbid content type guessing (`nosniff`).
- The scan followed symbolic links to files, so anyone able to add one
  to a picture folder (e.g. `key.jpg`) could download any file the
  application can read, such as the session key. Symbolic links and
  special files are now skipped, and before reading or serving a file,
  it is checked again that no part of its path has been replaced by a
  symbolic link since the scan.
- Previews embedded in RAW files and movies are only used if they are
  actually JPEG images.
- Files that are not a known image, movie or sidecar format (e.g.
  `SHA1SUMS`) are no longer treated as pictures, so `exiftool` and
  ImageMagick no longer run on them (see below).
- Markup in names coming from the files (folder names, or camera and
  lens names from the metadata) was turned into HTML elements in the
  drop-down filters of the image lists, and partly interpreted on the
  charts (e.g. as links). The charts now show such names as written,
  and these drop-down filters (folder, camera and lens) have been
  removed; the text filters still work on these columns.

The installation documentation now describes who should be able to
write to the picture folders.

#### Public sites

On public sites (built with the `public-site` flag), anonymous visitors
could see:

- the flagged images pages, which show the names of the users who
  flagged the images, i.e. valid login names, the settings page, which
  shows the server configuration, and the status pages, which show the
  errors of the scan; these pages now always require logging in;
- the full paths of the folders and files on the server, on the folder
  and image pages, and the raw image record; they now see the paths
  relative to the configured directories, prefixed by the kind of
  directory (e.g. `output: 2020/2020-01-01 Trip`), and no raw record;
- the error output of the external tools (which names the paths of the
  picture and its cache files) in the error image shown for a preview
  that could not be built; they now get a generic message, while the
  details are logged (previously, most weren't), and still shown to
  logged in users;
- the pictures' problems (the errors and warnings from reading their
  metadata, which can name paths), on the home and curate pages, in the
  problems list, on the image page, and by searching for them; these
  are now only shown to logged in users: searches by problem are
  refused, and quick searches don't look at problems;
- the repository status (including the error of a failed scan) on the
  login and "not found" pages, and in the navigation bar's status icon;
  these are now only shown to logged in users;
- the full server path of the picture inside thumbnails, as recorded by
  ImageMagick; new thumbnails no longer have it.

#### Logins, sessions and CSRF

- Logging in took much longer for existing users than for unknown ones,
  which showed whether a user name exists; both now take the same time.
  The login no longer accepts JSON requests, only the form.
- Sessions stayed logged in indefinitely if used at least every two
  hours. Logins are now valid for at most a week, configurable with the
  new `session-max-age` setting (in hours).
- Deleting a user or changing a password (with `db-util`) didn't end
  the user's sessions; worse, SQLite could give the id of a deleted user
  to the next user added, who then inherited the deleted user's
  sessions. Deleted users are now kept, marked as deleted, so their ids
  are never reused (adding one again restores it), and each user has a
  session generation, increased by these operations, which ends all its
  sessions. The new `db-util logout` command does only that.
- State-changing requests (flagging images, triggering a rescan) had no
  CSRF protection. They, and the login form, now need a CSRF token,
  which is replaced when logging in and out (so pages open in other
  tabs need a reload then); the session cookie is
  `SameSite=Lax`; and forms can no longer turn a POST into a PUT or
  DELETE request (via a `_method` field).
- After logging in or out, a rescan, or flagging an image from a form,
  Corydalis redirected to the page in the `Referer` header, even on
  another site. It now only returns to its own pages, and logging out
  always shows the login page.
- `https: true` was documented to force secure cookies, but didn't; it
  does now (when serving HTTPS directly), and secure cookies are the
  default (see the upgrade notes).
- The `Strict-Transport-Security` header had a `max-age` of two hours
  (the session timeout was used for it); it is now one year, and
  covers all responses, including static files.
- The session key (`config/client_session_key.aes`), with which anyone
  can create session cookies for any user, and the database, holding
  the password hashes, were created readable by all users of the
  machine. Their directories are now restricted at startup; see the
  installation documentation on replacing the key, if other users could
  have read it.
- (Development only) When run from GHCi via `DevelMain.update`, the
  server listened on all interfaces, with the password-less development
  login enabled; it now uses the configured host.

#### HTTP headers

- Dynamic pages, JSON, served media and errors had no `Cache-Control`
  header, and the favicon was `public` while setting the session cookie,
  so a caching reverse proxy or CDN could serve a logged-in response,
  or a session cookie, to other visitors. They now send
  `Cache-Control: private, no-cache` (the favicon `private`, with a
  long `max-age`); static assets remain publicly cacheable.
- All responses send a `Content-Security-Policy` that only allows
  scripts from Corydalis' own static files, so that an HTML injection
  can't run code, and forbids embedding the pages in frames.
- All responses send `Referrer-Policy: same-origin`, so that the URLs of
  Corydalis pages, which contain folder and image names and search
  terms, aren't sent to other sites.

#### Logs

- The detailed request logging (`request-logging: "detailed"`, the
  default in development builds) logged the password of each login in
  plain text. The password is now hidden, and raw request bodies are no
  longer logged.
- Text from outside the application (file names, tool output, request
  paths and headers) was logged unchanged, so newlines could forge log
  lines, and terminal escape sequences act on the terminal of whoever
  reads the logs. Control characters, invisible formatting characters
  (e.g. bidirectional text controls) and similar are now escaped, e.g.
  as `\x0a`. The request logs in the Apache format are escaped by
  wai-logger, since version 2.5.1, as Apache httpd does; Corydalis now
  needs this version, which the Stack configuration adds. The client
  address taken from `X-Forwarded-For` or `X-Real-IP` is now only the
  first address in the header.

#### Resource limits

- The external tools building previews (ImageMagick, ffmpeg, and
  exiftool extracting embedded previews) could run indefinitely, e.g.
  on a crafted file, and any number of them could run at once (e.g.
  scrolling quickly through a folder not yet rendered, or a crawler).
  They are now stopped after five minutes, with at most as many at once
  as the application uses processors, configurable with the new
  `render-tool-timeout` and `render-tool-slots` settings. A cancelled
  request now stops its tool too.
- Likewise, the scan's exiftool runs could hang (so that the scan never
  finished), and run one at once per directory directly under the
  picture directories. They are now stopped after ten minutes (all the
  files of the batch then get the error), with a separate limit on how
  many run at once (by default, also the number of processors): see the
  new `scan-tool-timeout` and `scan-tool-slots` settings.
- The ImageMagick policy limits the image size, memory and disk used.
- Quick searches are limited to 200 characters and 10 distinct words,
  as each word is checked against the whole repository.

### New features and changes

- The camera and lens pages have two more charts: the statistics and
  the timeline per paired item (for a camera, its lenses, and
  vice-versa). All the timeline charts are now bar charts, as the
  previous lines connected across gaps, which was misleading.
- The view modes are being reworked, to make moving around the site
  more natural. The "chips" for a year or a location go directly to a
  folder list, while the others follow the view mode. The inline image
  view of the image grid (with the `fancybox` library) is replaced by
  the application's standard viewer, now the only one; and the folder
  browse mode is removed (with its `/search/folders/grid/…` pages and
  the viewer's `B` shortcut), as one picture doesn't represent a folder
  well, leaving folder listing.
  Quick searches using filters that can't find pictures go to the folder
  list.
- The image viewer can show the picture's information over it (`i`),
  open the image information page (`o`), and shows a progress bar while
  loading an image.
- The pictures' dimensions and megapixels are now tracked: shown on the
  image information page (with the file size), and searchable with the
  new `megapixels` atom. The camera and lens make (manufacturer) is
  tracked too.
- The folder list shows more useful columns: the date of the first
  picture, and the countries, cities and people, instead of the counts
  per image status.
- The sortable tables (folder and image lists, camera and lens
  statistics, etc.) use a small built-in module instead of the
  `tablesorter` plugin: they load much faster on large lists, and can
  be sorted from the keyboard. Filters match any part of the text; on
  numeric columns, they can compare numbers (e.g. `>10`, `<=5`, `!=0`).
  The rest of tablesorter's filter syntax (e.g. `a|b`, or ranges) is no
  longer supported.
- The "Flag" and "Un-flag" buttons on the image and flagged images
  pages now work in place, as in the viewer, with a notification instead
  of reloading the page (they now need JavaScript).
- In the movie player, pressing space at the end of a movie goes to the
  next item, instead of replaying it.
- The logout button logs out directly, without an intermediate page.
- The rendering phase of the scan now first classifies what is already
  rendered and what is pending, checking only the cache files, then
  renders the pending items (extracting the previews of raw files and
  movies as needed), so the remaining count and ETA are more accurate;
  the status page shows the first pass as the images checked so far,
  and the estimated end time of the work (as a tooltip).
- The status page describes each phase's work in its own words (e.g.
  the previews built, or the obsolete cache files deleted), instead of
  the same generic "items" for all of them.
- Many icons changed, so that different things no longer share one.
- Usage periods in the camera and lens statistics are computed in
  calendar years and months (e.g. a full year is "1 year", not "1 year
  and 1 day" in leap years).
- Files that are not a known image, movie or sidecar format are listed
  separately on the folder page, and don't affect folder dates; a
  `type=unknown` search finds the folders containing them. With many
  such files, scans are faster.
- Folder events (work in progress; see the manual): a folder
  can declare an event (generic, birthday, getaway, grand vacation or
  work trip, with a name and people) in a `corydalis.yaml` file, read
  only as a regular file of at most 64 KiB; folders without one,
  spanning more than two days, get an inferred getaway (or grand
  vacation, from a week). Events are shown on the folder page and
  summarised on the home page, and folders can be searched by them (e.g.
  `event`, `birthday`, `event-kind`), but pictures don't match these
  searches yet, and the file format may still change.

### Deployment and configuration

New settings, all optional unless noted (see the sample configuration
and the installation documentation):

- `viewableimages` (required, see the upgrade notes);
- `unix-socket`, to listen on a Unix socket instead of a TCP port,
  behind a reverse proxy, with `unix-socket-group` and
  `unix-socket-mode` (e.g. `"0660"`) to let a proxy running as another
  user connect; at startup, only a socket left at that path is
  replaced, and anything else there makes Corydalis refuse to start;
- `strict-transport-security`, for the HSTS header;
- `session-max-age`, the maximum age of a login, in hours;
- `imagedecoders`, `moviedemuxers` and `magickpolicydir`, for building
  previews;
- `render-tool-timeout`, `render-tool-slots`, `scan-tool-timeout` and
  `scan-tool-slots`, the limits of the external tools.

Also:

- the new `db-util logout` command ends all the sessions of a user, and
  `db-util list` marks the deleted users;
- a sample systemd unit, `config/corydalis.service.sample`, uses
  systemd's sandboxing to limit what Corydalis and its tools can do;
- the installation documentation has an example of limiting login
  attempts in nginx;
- the flagged images' author is no longer a foreign key to the users
  table, which prevented removing users who had flagged images.

### Fixed bugs

Previews and the cache:

- A failed or interrupted preview could leave an empty or partial file
  in the cache, then served instead of being rebuilt; previews (and
  extracted embedded previews and movie frames) are now written to a
  temporary file and moved into place once complete, and empty files
  are rebuilt. Scaled previews are served as `image/jpeg`, instead of
  the non-standard `image/jpg`.
- Pictures holding several images (multi-page TIFF, animated WebP, HEIC
  with several images), or with a format specifier in their path (e.g.
  `100%d.jpg`), got no preview. The preview is now built from the first
  (or primary) image, with ImageMagick reading and writing through its
  standard input and output, and kept only if it has the expected
  format's signature.
- Small pictures were upscaled in the cache (see the upgrade notes).
- A preview that couldn't be written to the cache (e.g. a name too long
  with the cache suffix) failed the request with a server error, and
  stopped the scan's rendering; it now fails for that picture only.
- Previews extracted from RAW files and movies were cached forever; they
  are extracted again when the file changes. A non-JPEG embedded
  preview no longer prevents using the next one.
- Without `ffmpeg` installed, a single movie without an embedded preview
  made the whole scan fail, keeping the previous contents (or none).
  Failing to run `ffmpeg` or `exiftool` now only affects that preview,
  and tool output that is not valid UTF-8 no longer causes further
  failures. The error of a failed movie frame extraction only has
  ffmpeg's error messages, not its banner and the stream details.
- The cache cleanup deleted the files of a picture when another one's
  name started with its name and a character sorting before a dash
  (e.g. `a.nef` and `a.nef copy.jpg`), kept the previews of sizes no
  longer configured and some files of removed pictures, and deleted
  nothing if the cache directory had a trailing slash or went through a
  symbolic link. It now keeps exactly the files of the pictures in the
  library, for the configured sizes.

The scan:

- Names that look like a range of files (e.g.
  `img_3982-1773958749527.jpeg`) were expanded to all the names in the
  range, which could run the scan out of memory; ranges are now capped
  at 1024 files.
- The scan's exiftool runs read whole directories at once, which could
  run out of memory on large ones; they now read batches of 32 files.
- When a scan was aborted and restarted during rendering, the previous
  render threads kept running.
- The rendering held on to memory it no longer needed (a long-standing
  laziness bug), and the repository uses less memory (shared strings),
  which also makes searches faster.
- The `scanner` tool works again.

Searches and pages:

- Searching for a missing value of an attribute that always has one
  (e.g. `no-folder`) caused an internal error; it is now refused.
- `any`/`all` with a negative count are refused, instead of matching
  everything or nothing; browsing at a huge page number showed the first
  page with a broken link, and now shows an empty page; a `pagesize`
  that isn't positive is refused at startup.
- Finished work on the status page has its green border again.
- The cache cleaning's numbers on the status page didn't add up (e.g.
  a negative number of items left), as the cache was counted before the
  scan, which then added files to it; it is now counted just before the
  cleaning, and the finished results use the number of files actually
  examined. More generally, when the work exceeds a goal counted in
  advance (e.g. files added during a scan), the number of items left is
  no longer shown as negative, the estimated end time as past, nor the
  progress bars beyond their range.

The viewer and the image grid:

- Going back in history to the first image viewed didn't work.
- Failing to load the image information or a random image only logged
  to the browser console, so the viewer seemed to hang; such errors are
  now shown, and an expired session asks to reload the page.
- A movie that could not be played switched the button to "pause"
  anyway; it now stays "play", with a message saying why.
- If the browser refused full screen, nothing happened; the viewer now
  falls back to its emulated full screen.
- Failing to load more images while scrolling the grid silently stopped
  it; it now shows an error, as well as loading and end-of-list
  messages.

### Internal changes

- The frontend is entirely TypeScript, as ES modules bundled with
  `esbuild`, and no longer uses jQuery: requests use `fetch` through a
  small helper adding the CSRF token. This halves most bundles.
- The TypeScript code is linted with oxlint (instead of ESLint), with
  type-aware rules and type-checking, which found the movie and full
  screen bugs above; it has unit tests (`node:test`, with jsdom for the
  DOM), and a coverage report uploaded to Codecov next to the Haskell
  one.
- The development Node.js version is pinned in `.node-version` (Node
  24, also used by CI); Node 20 is no longer enough.
- `make bootstrap` installs the npm dependencies with `npm ci`, and a
  committed `.npmrc` disables their install scripts.
- `make check-static` (also run by `make release` and CI) fails on
  unused files and broken links in the static directory.
- The third-party GitHub Actions used by the workflows
  (`haskell-actions/setup`, `8c6794b6/hpc-codecov-action` and
  `codecov/codecov-action`) are pinned to commits, kept up to date by
  Dependabot.
- Development builds no longer name their cache files with a `-dev`
  suffix; their old `repo-cache-dev` file can be deleted.
- The tests need `exiftool`, ImageMagick (able to write HEIC, i.e. with
  `libheif-plugin-x265` on recent Debian and Ubuntu) and `ffmpeg`.
- The `e2e/` directory has browser scripts, run by hand, checking the
  tables, the charts and a public-site build.

### Dependencies

- The Stackage LTS is now 24.68 (GHC 9.10), from 23.22 (GHC 9.8), and
  wai-logger must be at least 2.5.1.
- `plotly` 4.3 (from 2.30), Font Awesome 7 (from 6), TypeScript 7 (the
  native Go port, from 5), `imagesloaded` 5, `infinite-scroll` 5, and
  `screenfull` 6.
- Removed: jQuery, `tablesorter`, `fancybox`, and ESLint.

## v2025.21.0 - "Zooming for all"

Released: _Sat, 24 May 2025_.

### Major improvement: image zooming and panning

After many years, finally add image zooming to the main image viewer,
together with mouse and gesture support for panning and pinch-zooming.
This was on the cards since movie support was implemented, but my previous
efforts failed to work well, for various reasons, one of them being the
HammerJS library not being flexible enough. I used Claude (3.7 Sonnet) to
help me implement fully-manual gesture handling, and then reverse engineer
my code - at which it was not so good. I spent then a lot of time to
understand the old viewer code, and it resulted in a significant
simplification as well.

The zoom was tested on both small images, and large images, up to a ~96
Megapixel image from NASA (~200MiB disk size). It takes a while to load
that, but afterwards panning and zooming is smooth.

As a side effect of the changes, I redid the way the HTML elements are
laid out in order to remove manual state management, and this uncovered
something that seems to be a bug in Safari iOS on iPads related to
fullscreen corrupting viewport sizing -
<https://bugs.webkit.org/show_bug.cgi?id=293514>. There's no workaround
for it, except either don't enter full-screen, or close the tab and open a
new one - page reload doesn't help.

Additional new feature: Added simple yearly and monthly timeline graphs to
the curate page, to help understand the growth of the collection over
time.

### Bug fixes

- When going to image view from a search, preserve (as intended) the
  search parameters; before it was going to the first image that matches
  the search, but the search filter was dropped, so it was simply jumping
  into all image search at a specific position.
- In the image viewer, when the navigation bar changes height due to
  wrapping (which in itself is a UI bug), the canvas was not resized
  correctly, as the positioning was done manually. Fix that by switching
  to a flex columnar layout, which automatically triggers a CSS resize of
  the page layout, including the image canvas. That, coupled with
  switching the method used to observe size changes (from window resize to
  ResizeObserver) fixes the issue. In the new model, the canvas will
  resize automatically with the navigation bar, which in some cases is
  more jarring than the over or underflow, but it's more correct, as it
  doesn't leave permanent wrong positioning, just a one-time visible
  change.
- Cover image generation for movies has been made more reliable, as
  sometimes `ffmpeg` was trying and failing to read from standard input,
  leading to no cover images.

### Small improvements

- The breadcrumbs are now so styled, that they will not wrap, but instead
  be clipped (and show an ellipsis on overflow), to keep the page
  structure constant no matter the folder/image names. Before, the
  wrapping could cause reflow when navigating between images.
- Furthermore, the breadcrumb has been customised for the viewer
  specifically, to not show the "Viewer" final item, and on small sizes,
  show only the folder name, since that's most representative of the
  context, on medium sizes the image name too, and on large size also the
  top level folder. This should result in more meaningful context of the
  picture being viewed, as the viewer only shows (briefly) the image name.
- The image viewer (the full page one) has been sped up by skipping some
  expensive CSS computations in the call path for every image drawing;
  this leads to a small performance improvement, but also potential visual
  bugs if display changes are missed.
- The image viewer's image navigation bar is now centred on small screens,
  which finally solves a very old UI annoyance (on small screens in
  portrait mode, it was consuming ~70% of the screen width, but not
  centred). Additionally, move the nav bar a bit higher, so that on iOS
  phones or tablets, it doesn't conflict with the bottom "swipe-up"
  element (the black bar used to go to the home screen).
- Make all `plotly` graphs responsive, as it seems this is not the default,
  even in 2024+.

## v2024.12.0 - "No accidental zooming"

Released: _Wed, 20 Mar 2024_.

For me, the highlight of this release is the disabling of unintended, and
accidental zooming in the image viewer, when using the navigation bar with
fast tapping. The viewer zoom level is now stable, except when
(intentionally) double tapping on the top navigation bar or search entry.

### Features

A new "presentation" mode is available, where the image viewer is
directly entered from any search. Previously, this was only either grid or
list mode (for images or folders, respectively), and this additional mode
enters the image viewer mode directly (without going through either grid
or list first). This mode is activated whenever the image view is
selected, which might not be optimal, so there might be tweaking to this
in the future.

### Improvements

Improvements to the image viewer:

- enable the `Tab` key to navigate to random images, similar to existing
  `r`; the reason being that in full screen, some browsers restrict
  alphanumeric input, but whitespace keys (such as tab/enter) are allowed.
- fix history navigation (backward/forward), by correctly tracking state;
  previous half-implementation didn't actually work.
- add `Backspace`/`Delete`/`<` as history backward navigation, again
  useful in full screen mode, and `>` for forward navigation.
- implement a small key shortcuts help page, activated by either `?` or
  `h`.
- disable "tap-to-zoom" on the navigation bar, since this was, in my own
  experience, one of the most confusing outcomes of trying to navigate
  fast by tapping (rather than swiping), and getting out of zoomed state
  was difficult since the canvas (which doesn't interact for this) takes
  90% of the page.

Other improvements:

- the image info page is now better organised, due to re-enabling the
  dynamic layout (via the masonry library); a CSS issue that prevented
  was finally identified and fixed.
- slight improvement to formatting of filters for a date (day of the
  month).

### Experimental features

Corydalis web endpoint now presents a manifest file, allowing installation
as a [Progressive Web
App](https://en.wikipedia.org/wiki/Progressive_web_app), running in
"standalone" mode. This means that on small screens, more of the available
screen space is used for Corydalis, even in normal mode, leading to a more
pleasant experience. To enable this, use the normal action for your
platform, for example on Apple devices, open the share menu, and select
"Add to home screen" (iOS/iPadOS) or "Add to doc" (MacOS).

Note this feature is experimental, and it is lacking good a good user
model, for example, forward/backward navigation on all pages.

## v2024.9.0

Released: _Sun, 03 Mar 2024_.

A medium sized release, with both internal and user-visible changes.

### New user features

The simple viewer (see view-vs-browse in the user manual) now has the
ability to play videos inline, rather than simply opening the raw video in
a new browser tab. This is new functionality, bugs are expected, and
please report them.

On the home page, a new card is present that allows browsing all pictures,
start at a random picture, "on this day" functionality, and on this
day-of-the-week. This will likely be expanded in the future, I assume
there are more interesting features to expose that are so to say
"cross-section" in the repository, rather than "city X, location Y, person
Z".

### Server admin changes

Two new config settings were introduced and one was deprecated:

- `request-logging` can now configure the HTTP logs as one of `none`,
  `apache`, and `detailed`; this replaces the old `detailed_logging`,
  which only toggled between `apache` and `detailed`;
- `log-level` can no configure the level of application logging; this
  overlaps with `should-log-all` (which, when set to true, is equivalent
  to `log-level: debug`, and when unset, it was previously equivalent to
  `info`); there are not many application logs, but a switch between the
  default of `info` and `warning` might be appropriate unless one wants to
  debug the server behaviour;
- it's likely that `should-log-all` will be removed in the future.

For installation, there's now an additional option to use pre-built
`amd64` binaries generated automatically by the GitHub Actions (at each
commit). Of course, building from source is still best, as it will use the
exact environment (rather than just Ubuntu latest).

### Internal changes

I've switched my editor environment from Emacs to VSCode, and as a result,
a lot of associated tooling has changed as well. This did result in quite
a bit of code churn due to formatting changes, and this might continue a
bit more as I investigate newer tooling.

Biggest miss: there's no meaningful unit test coverage improvement 🙁. I
still need to come up with a strategy here, and especially one that can
cover/integrates both Haskell and Typescript testing. On the slightly
positive side, the new logging changes (were written for and) allow very
clean test output, as all the logging noise is now hidden. This should
allow a more positive attitude to tests 😁.

Very minor notice: the code now builds with GHC 9.6.

## v2023.44.0

Released: _Sat, 04 Nov 2023_.

A new release (at a rather arbitrary point in time), and switching to
calendar-based versioning since it makes more sense for this application.

### HiDPI displays

Finally fixed image resolution handling for hi-dpi displays. Previously,
Corydalis was completely oblivious to this, so the images were fuzzy in the
native (built-in) image viewer (issue #10). Now they should show correctly.

### Performance changes

Image thumbnails in image/folder list make use of the HTML-native
lazy-loading mechanism, which (for browsers that implement it) makes
large image lists actually usable. Not a proper replacement for a
paged loading mechanism, but a working substitute.

Slight performance change for loading pages due to reduced and
combined CSS/JS resource serving. This can lead to 10-20% page loaded
speed-ups, depending on the page type and complexity.

### Exif changes

Now parses the rating as written by (at least) Lightroom. Value '0' is
unrated, otherwise the explicit rating.

### Search changes

Added new search atoms _season_, _month_, _day_, _rating_,
_people-count_, _keyword-count_.

Season is the usual _English_ name for season, i.e. _winter_, _spring_,
_summer_, _autumn_, and the season computation is based on month
boundaries: winter lasts from December to January, and the rest
follow.

Month is the usual _English_ name for the month, or alternatively the
numeric value (1-12).

Day is a bit more complex. It can take any of: day-of-week (English
names), 'weekday', 'weekend', or numerical month day (e.g. '10th').

Rating is the exif rating as written by camera or image processing
tools, usually using the values 1-5, with 0 being unrated.

People and keyword count atoms are what you'd expect, searching by how
many people or keywords are in an image.

All the numeric search atoms (year, rating, counts, etc.) can use '<'
and '>' in the quick search, e.g. `keyword-count:>3`.

### UI interaction changes

Previously, searching for things, or clicking to various links was
always going to either an image grid browsing view or folder grid
browsing view, based on static conditions. Now, the preference of how
to display search results has moved to a per-browser sticky
(long-lived) cookies, so that multiple searches will remain on last
view. The preference is updated whenever the view is manually changed
(via the alternate links in the page).

In case of missing preferences, the default is same as before, image
or folder grid.

### Internal changes

The way image name URLs are built for multi-level paths changed;
instead of encoded '%2F' elements in the path, with the image being a
single path element, now image names are multi-path elements:
`a%2Fb%2Fc` becomes `a/b/c`. This should help with reverse-proxying
corydalis, and in general is a cleaner way to represent file-system
paths in URLS.

The source tree no longer contains embedded libraries (yay!). They
have been replaced by symlinks (or transformations) of upstream files
as shipped in [`npm`](https://www.npmjs.com/) packages. This should
simplify future development.

## v0.4.0

Released: _Sun, 26 May 2019_.

This is yet another major release, getting closer to a friendly UI for
photo viewing in parallel with the expansion of the library curation
for people so interested. Around 380 commits, so even bigger than
v0.3…

### New features

A large number of new features, due to the long bake time. Will do
better next time ☺

#### Movie support

It is now possible to view movies formats that are natively supported
by the browser. This is not much, as many older cameras were creating
things like `AVI` files, or some cameras still generate `MOV`s that
are not well supported.

Nevertheless, for the formats that are supported, it is possible to:

- view them as such, in the image viewer and the new browsing mode
- search for movies by type
- see statistics (in the 'curate' module)

When just looking at images (in the new browsing UI, or as thumbnails,
etc.) the movie "cover" is taken either from an embedded thumbnail (if
available), or from the first frame via `ffmpeg`, which is a new
dependency.

#### A new alternative UI for browsing pictures

The existing image viewer, designed for fast sequential viewing, lack
the ability to browse through images, without necessarily wanting to
see them in detail. Furthermore, the existing image/folder views with
their data-oriented tabular formats were really not user friendly, but
more geared towards analysing the photo library.

There's now a new, parallel "browse" mode that is grid based, which
allows scrolling through the pictures. It deals much better with large
results as it's paginated and incrementally loading, and applies the
same to folder views and to image views.

On the folder view, selecting an image leads into the image viewer for
that folder, whereas for the image view, it just opens the image or
the movie in a "light box", allowing to view the image in more detail
(even at full resolution, which is better than the previous image
view) without losing track of browsing position.

It is also possible to switch between folder and image views and
between the list-based and the browse-based view for either.

#### Asynchronous repository scanning with progress report

The previous synchronous-scanning done in the reload handler was, of
course, a big kludge, and it was finally time to retire it. The reload
handler now just triggers the scan, and it's possible to see
progress statistics for both the scanning phase, and for the rendering
phase, which was completely hidden before.

The refactoring had a couple of nice side-effects:

- the UI is fully usable during the scans; if this is a subsequent
  scan, the previous results are kept until the in-progress scan is
  finished, leading to no "down-time" of accessing the data;
- rendering phase is better controlled and there's no significant
  duplicate work; previous implementation was forgetting the rendering
  thread and could lead to N parallel threads all doing the same work;

On top of all this, the repository state is cached at end of scanning,
so initial load (after task restart) is _much_ faster.

#### New search atoms

More search atoms, which means higher chance of confusion. Anyway ☺

- added title and caption (string) atoms
- added folder (string) atom which allow usual searches on folder name
- added filename (string) atom which allows searching on image name,
  including any intermediate directories (such as
  `day1/dsc_0590.jpg`).
- added image status (set) atom, which replaces the old by-status view.
- added folder class (set) atom, which replaces the old by-folder-type
  view.
- added a lens (string) search atom.

The addition of image status and folder class atoms allowed completely
removing the old style "by-category" views, completing the unification
of the search/browse/list paradigm onto searching (via filters).

#### Expanded library statistics (curate module)

Added camera statistics, including tracking of shutter count if
available, and expanded the lens statistics. One can now look to see
which cameras generate best keepers, or to track equipment use/last
use, etc. Multiple cameras of the same model are handled now via
serial info, which however breaks as many cameras don't store their
serial in movie metadata, only in images.

The over-time usage for both cameras and lenses now show graphs, but
I'm not sure they're very readable, so it's more like raw v1.

Improved handling of exif errors or (new) plain inconsistencies
(e.g. location fields existing but empty). This should allow removing

There is so much more data that could be shown here, but not yet…

### Performance improvements

Some folder statistics are now pre-computed and cached, leading to
significant speed-ups (up to one order of magnitude) in some cases
(e.g. the folder list view).

Quick search atoms now can be prefixed with an atom keyword followed
by a colon to restrict searches to that specific atom, and not try all
atoms that match. E.g. 'year:2018 country:Italy'.

As described above, repository state is cached, so application restart
is much faster.

There have also been some overall performance improvements by changing
the used types (some `String`→`Text` migration, or list to sets,
etc.), but with no hard data.

### Bugs fixed

Folder cover display should now more consistently find an image to
display, even if the first image(s) are not viewable or if the image
filter doesn't actually select an image from the folder.

Master (as in 'soft master') images that are not proper raw files, but
instead something like png/tiff/etc. are now directly viewable. The
logic for this is hard coded for now, but finally one can see master
bitmaps.

Quick searches for two (but exactly two) words which neither finds
anything was triggering a but in the atom parser; this is was fixed by
removing arbitrary-length operators in the parser.

Images are now viewed in capture time order, not alphabetical
order. This could be called an improvement as well, but it was IMO a
bug in the first place.

Full screen mode in the view library is now done via the `screenfull`
javascript library, leading hopefully to expanded compatibility. On
top of that, a fake full screen mode is there for browsers which don't
support a native on - looking at you, Safari on iOS, why oh why?

Relaxed the parsing of some exif fields, hopefully allowing parsing of
metadata generated by more exotic writers.

### Miscellaneous

#### Folder search semantics

Changed semantics of folder search. Before, a folder search (almost)
always meant "it contains at least one image that itself matches the
exact filter". Now, a folder search for a composite filter (e.g. "A
and B") means it contains an image matching "A" and also contains an
image matching "B".

The rationale for this change is that some atoms only have meaning at
folder level (e.g. folder class), and other have different meaning at
folder and image level (e.g. year), so the handling of atoms should be
as well different.

A (desired) side effect of the change is that many searches are faster
on folders now, since we can look at aggregate folder statistics in
many cases, rather than look at each individual image in turn.

#### Internal changes

The application state was moved from global variables (`MVar`s) to a
context structure, which unblocked testing of minimal repositories. As
such, there's a tiny bit of coverage improvement, but this serious
testing remains for the future.

Untracked files are now handled together with image files, which
should lead hopefully to a better handling of files, but has potential
for some bugs (e.g. one was found in list folder with regards to image
statistics).

## v0.3.0

Released: _Fri, 09 Mar 2018_.

A major release, with close to 300 commits.

**Important**: due to internal changes, all the cached exif metadata
will be regenerated, so the first startup should be slower. This
should proceed automatically, but as there is no versioning of the
cache, it might be a good idea to remove the files manually (as
there's no automated clean-up procedure yet):

```shell
find /path/to/cache/dir -name '*-bexif' -delete
find /path/to/cache/dir -name '*-exif' -delete
```

New features:

- Rewritten search system; besides just location and people/keyword
  searches, a number of other atoms have been added, and now the
  search supports arbitrary combinations of these.
- Image viewing/browsing have moved from folder-based (view all images
  in folder A, move to folder B, etc.) to the above search/filter
  based: show me (and view in order) all images with keyword flowers,
  no matter in which folders they are located.
- Exif parsing has been reworked, and a number of new fields have been
  added (e.g. ISO, shutter speed; location fields are now split into
  country/province/city/etc.)
- Exif parsing failures are recorded, can be viewed on the curate page
  and searched by the 'problem' atom.
- The security requirements have been somewhat relaxed, allowing the
  application to run in non-secure (https) mode, recommended only when
  using behind a reverse proxy, and to allow non-logged in browsing,
  recommended for a website/demo site.

Improvements:

- Directory scanning has been parallelised, leading to 2.5× and more
  speed-ups.
- Exif metadata is now cached even in the case of failures, and is
  only regenerated in case the source file is updated; this should
  allow for (much) faster rescans if there are many images that fail
  parsing.
- This might have been present in 0.2 too, but: exif metadata is now
  updated automatically if the source file is updated.
- The CSS and part of JS dependencies are combined and served as
  single file; this (the CSS part) has eliminated some flickering when
  used with Firefox.
- Added a lens statistics page.
- Added an about page, pointing back to GitHub for the sources and
  explaining a bit the application.

Bugs fixed:

- Exif parsing had significant issues with related to people parsing
  (at image level), and with folder-level aggregation (which was
  actually plain broken); these should be fixed now.
- UI usability on small screens was very low, mostly fixed now (but
  this can be always improved).
- Image viewer handles keystrokes better, eliminating their interception
  when modifiers are used; e.g. `CTRL+r` for reload was previously
  also triggering a `r` random image view.

UI changes/additions:

- Moved to Bootstrap 4, which allowed fixing the UI responsiveness fixed.
- Moved to Font Awesome 5 (new icon look).
- Small updates to the other dependencies as well.

## v0.2.0

Released: _Thu, 08 Feb 2018_.

Initial public release, after a very long bake time ☺

<!-- markdownlint-configure-file { "headings": { "siblings_only": true } } -->
