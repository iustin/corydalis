# Changelog

## vNext

### Breaking changes

A new key in the configuration has been added, `viewableimages`, which
represents what file extensions are directly viewable by the browser and
don't need to be converted. Previously this was hard-coded, but it is
likely that modern browsers support more formats, so it is now
configurable. Moreover, it has to be configured. A starting point can be
seen in the sample configuration file.

The picture and cache directories (`sourcedirs`, `outputdirs` and
`cachedir`) must now be absolute paths; Corydalis refuses to start
otherwise. Relative paths depended on the current directory, which can
differ between Corydalis and the other tools reading the same settings
(such as the scanner). The sample configuration already uses absolute
paths; if yours doesn't, write the paths out in full.

### New features

The camera and lens information pages now have two more graphs: per the
paired item (for a camera, the lenses, and vice-versa) statistics (bar
chart) and timeline, per-paired item trends (line chart). This should show
better the actual usage of a camera or lens, and including the pairings.
Of course, these are only useful in terms of timeline for fixed-lens
cameras.

Additionally, all the timeline graphs (either on global camera and lens
stats, or in the individual pages) have been switched to bars for better
readability, and even more, correctness. The previous method of
line+markers was connecting across gaps, which was misleading, and was
significantly complicating the shape of the graph. Will have to see if
this is indeed better.

### Changed behaviour

The complex 5 view modes (image list, image grid, folder list, folder grid
and image view) are being reworked. The longer goal is to make moving
around the site more natural and to simplify it.

Currently two main changes have been done.

On the navigation side, using the "chips" to go to a year, or
location-based attribute, will directly jump to a folder list, whereas the
others will obey the "view mode".

On the view modes, there was a difference between "folder browse"—which
navigated to the first folder image in "view" mode—and image browsing,
which opened an inline view of the image (using the `fancybox` library).
This latter inline view mode has been replaced with the standard view mode
of the application, which is now the only image/movie viewer model.

Furthermore, the above-mentioned folder browse mode, which looked similar
but behaved different from the image browse mode, has been entirely
removed. For folders, the only remaining mode is listing, which makes more
sense as showing just a picture was not representative for a folder.

In the movie play mode, if the end of the movie is reached, pressing
spacebar once again will advance to the next item, instead of replaying
the movie.

The "Flag" and "Un-flag" buttons on the image information page and on the
flagged images page now work in place, like flagging in the image viewer:
the result is shown as a notification in the bottom right corner of the
page, instead of reloading the image page. Notifications disappear after
a few seconds, except for errors, which stay until closed. The buttons
require JavaScript.

The rendering phase of the repository scan has now been changed to better
show the progress; it does two passes, first classifying what is already
rendered (and up to date) and what is pending, and in the second pass,
processing the pending items, so that both the remaining count and the ETA
of the computation are more accurate.

The logout button now logs out directly, instead of going through an
intermediate page, and afterwards always shows the login page.

Many icons have been changed so that different things no longer share the
same icon: the search attributes (e.g. lens vs. camera, person vs. people
count, season/month/day vs. year, flash mode vs. flash source), the viewer
buttons (zoom vs. search, the folder list vs. the image list), and a few
homepage and status icons. The "random image" button on the homepage now
uses the same icon as the one in the viewer.

The sortable tables (folder and image lists, camera and lens statistics,
and so on) no longer use the `tablesorter` plugin, but a small built-in
module that does what Corydalis used: sorting (shift-click on a header to
add a secondary sort column), the filter row, and the group headers. They
load much faster on large lists, and sorting now works from the keyboard,
as the column headers are buttons. The filters are "contains" matches,
except that on numeric columns (e.g. image counts) a filter starting with
a comparison operator compares the numbers: `>10`, `<=5`, `!=0`, with the
same operators as the numeric search atoms (`=`, `!=`, `<`, `<=`, `>`,
`>=`, plus their alternative spellings). The rest of the filter syntax
of `tablesorter` (e.g. `a|b` or ranges) is no longer supported. Group
headers are no longer collapsible (this was already disabled).

The usage periods in the camera and lens statistics are now computed in
calendar years and months, instead of approximating a year as 365 days
and a month as 30, so e.g. a full year is shown as "1 year" rather than
"1 year and 1 day" in leap years.

Quick searches are now limited to 200 characters and 10 (distinct)
words, as each word is checked against the whole repository; longer
searches are rejected. The message listing the filters skipped by a
quick search shows at most five of them, as it is kept in the session
cookie until shown.

The external tools building the previews (ImageMagick, ffmpeg, and
exiftool when extracting embedded previews) are now stopped if they run
for more than five minutes, e.g. on a crafted file, and the preview
fails. Previously, they could run indefinitely, and a cancelled request
waited for the tool to finish; the tool is now stopped as well.

At most as many preview tools run at once as the application uses
processors (by default, all of them); further requests for previews not
built yet wait for their turn. Previously, many such requests at once
(e.g. by scrolling quickly through a folder not yet rendered, or by a
crawler) could run any number of ImageMagick or ffmpeg processes at
once.

Both limits can be changed, with the new `preview-tool-timeout` (in
seconds) and `preview-tool-slots` settings.

The scan's exiftool runs, which read the metadata of the files in
batches, have the same kind of limits, separately (the previews the
scan renders use the preview ones): a run is stopped after ten
minutes, and all the files of its batch then get the error (again at
each scan, if one of them makes exiftool hang); and at most as many
runs happen at once as the application uses processors. Previously, a
run could run indefinitely (e.g. on a crafted file), so that the scan
never finished, and the scan could run one exiftool at once for each
directory directly under the picture directories (e.g. one per year).
They can be changed with the new `scan-tool-timeout` (in seconds) and
`scan-tool-slots` settings.

### Fixed bugs

The static files directory still had links to libraries that are now
bundled, or no longer used: the files were served, although unused, and
the link to jQuery, dropped earlier, was broken, which made the
installation step copying the directory (`cp -aL static`) fail. These
links are removed, and a new check (`make check-static`, also run by
`make release`) catches unused static files and broken links.

With ImageMagick 6 (as shipped by Debian and Ubuntu), the ImageMagick
policy added in this release (see below) made all the (non-thumbnail)
image previews fail: they were written as `jpg`, which ImageMagick 6
checks against the policy as such, while the policy only allows `JPEG`
(ImageMagick 7 resolves the alias first). Previews are now written as
`jpeg`, and served with the standard `image/jpeg` content type instead
of `image/jpg`.

Moreover, a failed preview build could leave an empty file in the cache,
which was then served (as an empty image) instead of being rebuilt.
Previews are now written to a temporary file, and moved into the cache
only once complete; empty files already in the cache are rebuilt.

Pictures holding several images (e.g. multi-page TIFF files, animated
WebP images, or HEIC files with several images) got no preview, and
were converted again at each attempt to view them: ImageMagick wrote
one numbered file per image instead of the preview. Pictures with a
format specifier in their path (e.g. `100%d.jpg`, or a folder named
`d%d`) got no preview either, as ImageMagick wrote it under another
name. The preview is now built from the first image only (for HEIC
files, the primary image); ImageMagick reads the picture from its
standard input and writes the preview to its standard output, so that
it has no file names to interpret; and a preview is only kept if it
starts with the signature of the expected format (PNG or JPEG).

A preview that couldn't be written to the cache at all (e.g. for a
picture whose name becomes too long with the cache suffix) made the
request fail with a server error, and, when rendering the previews
after a scan, stopped the scan with an error, leaving the other
pictures' previews unbuilt. It is now reported as any other failed
preview, for that picture only.

Thumbnails no longer include the full server path of their picture,
which ImageMagick records in them, and which on a public site was thus
visible to anyone looking inside a thumbnail file. Thumbnails built
earlier keep it until they are rebuilt, e.g. after removing them from
the cache directory (they are the files ending in a dash and a size up
to `thumbnailsize`, e.g. `-64`).

The cache cleanup after a scan deleted the cache files of a picture
when another picture's name starts with its name followed by a
character sorting before a dash (e.g. `a.nef`, next to
`a.nef copy.jpg`); they were then rebuilt when needed. It also kept
the previews of sizes no longer configured, and the cache files of a
removed picture whose name starts with another picture's (e.g. those
of a removed `a.nef copy 2.jpg`, next to `a.nef`). It now keeps
exactly the cache files of the pictures in the library, for the sizes
configured.

On the status page, finished work (scan, rendering and cache cleaning)
is shown with a green border again, as intended: a typo in the style
name meant it had no border.

In the image viewer, failing to load the image information or a random
image (e.g. because the server is unreachable) was only logged to the
browser console, so the viewer seemed to hang; such errors are now shown
in the viewer, and stay until the next message. An expired session (as
also when flagging an image, here and on the other pages) is reported as
such, asking to reload the page to log in again.

If `ffmpeg` was not installed, a single movie without an embedded preview
made the whole repository scan fail with an error status: on a rescan, the
repository kept the contents of the previous scan, missing any changes
since, and on a first scan (without cached data) it remained empty.
Failing to run `ffmpeg` (or `exiftool`, when extracting
previews) now only affects the preview of that one image or movie.
Similarly, error output from these tools (including `convert`) that is not
valid UTF-8, e.g. because it contains a file name, no longer causes
further failures.

Files that are not a known image, movie, or sidecar format (for example
`SHA1SUMS`, `corydalis.yaml`, or files with no extension) are no longer
treated as pictures. They are listed separately on the folder page, they
do not affect folder timestamps or date ranges, and the renderer no longer
tries to generate previews for them. A `type=unknown` search lists folders
that contain such files. If there are many such files in the repository,
this will speed up the scans, and furthermore it will not run `exiftool`
and `convert` on arbitrary files, which might be problematic.

When going from any other page to the image viewer, and then moving to
another image (still inside the image viewer), navigating back in history
to the first image was not working. This has been fixed.

The picture cache had a bug in that for small pictures, they would be
upscaled instead of left as-is. With many small pictures and with high
maximum size auto-render, this could lead to significant cache usage. This
has now been fixed, but the "too-large" thumbnails will not be
automatically cleaned, it's recommended to wipe the cache manually.

Security: file names starting with a dash were interpreted as options by
`exiftool` while scanning a folder. A file named, for example,
`-Comment=x.jpg` could switch `exiftool` to write mode, and modify, rename
or move the other files being scanned with it. File names can no longer be
interpreted as options. If untrusted users can add files to your picture
folders, consider checking for such files, and for unexpected `_original`
backups.

Security: previews were built by passing the file to ImageMagick's
`convert` without saying what format it is, so ImageMagick guessed it from
the file contents. A PostScript or SVG document named, for example,
`photo.jpg` (or embedded as the preview of a RAW file) was therefore
decoded as such, and PostScript is run through Ghostscript, which has had
several sandbox escapes. If untrusted users can add files to your picture
folders, this could read other files into a served preview, or worse.
ImageMagick is now always told which decoder to use, based on the file
extension (see the new `imagedecoders` setting below), and can
additionally be restricted by a security policy shipped with Corydalis.

Security: similarly, the first frame of movies without an embedded
preview was extracted by ffmpeg without saying what format the file is,
so ffmpeg guessed it from the contents. Some formats make ffmpeg read
other files: recent ffmpeg versions refuse the known cases of this
(playlists need their own extension, and concat scripts can only refer
to files next to them), but e.g. a concat script named `movie.mp4` was
still read as such. ffmpeg is now always told which demuxer to use,
based on the file extension (see the new `moviedemuxers` setting below),
limited to plain container formats, and only allowed to read local
files. Also, the extracted frames, and the previews extracted with
exiftool, are now written to a temporary file first, so that a failed
or interrupted extraction doesn't leave a partial preview behind, which
was then used as is.

Security: dynamic pages, JSON endpoints, served media, and error
responses did not send a `Cache-Control` header, and the favicon sent
`Cache-Control: public` alongside the session cookie. Behind a caching
reverse proxy or CDN (especially on a public site, where the same URLs
show storage paths, scan errors, raw records, and CSRF tokens to
logged-in users), a shared cache could serve a logged-in response, or a
cached favicon's session cookie and CSRF token, to other visitors.
Responses without their own `Cache-Control` header (other than
successful static assets) now send `Cache-Control: private, no-cache`,
which forbids shared caches from storing them and requires browsers to
revalidate with the server before reuse (served images and movies still
avoid re-downloading via `304 Not Modified`), while keeping browser
back/forward navigation working; the trade-off compared to `no-store` is
that browsers on shared devices may still retain visited pages in cache
after logout. Responses with their own `Cache-Control` (such as Yesod's
redirects) keep it, the favicon now sends `private, max-age=2592000`,
and static assets remain publicly cacheable.

Previews embedded in RAW files and movies are now only used if they are
actually JPEG images. Exiftool already checks this for most of them, but
not all; otherwise, a non-JPEG preview was saved and used, failing to
build the scaled-down versions, and preventing the use of the next
embedded preview, if any.

Previews extracted from RAW files and movies (the embedded preview, or
the first frame of the movie) were cached forever, so after editing or
replacing such a file, the old preview kept being shown. They are now
extracted again if the file changed since.

Security: with the detailed request logging (`request-logging:
"detailed"`, the default in development builds), the request parameters
were logged, including the password of each login, in plain text. The
password is now hidden, and the raw request bodies are no longer logged.
If you used detailed request logging, consider removing the old logs.

Security: the request logs in the Apache format contained the fields
taken from the request (the client address, if taken from the headers,
the method, the path, the referrer and the user agent) as they were:
newlines in them could make a request look like several log lines, e.g.
forged ones, terminal escape sequences could act on the terminal of
whoever reads the logs, and quotes could shift the fields seen by log
parsers. These fields are now escaped by the library formatting these
lines, wai-logger, since version 2.5.1
([logger#226](https://github.com/kazu-yamamoto/logger/issues/226)), as
Apache httpd does: quotes and backslashes with a backslash (`\"`,
`\\`), whitespace controls in C notation (e.g. `\n`), and other
non-printable or non-ASCII bytes as `\xhh`, so that log analysis tools
read them as they read Apache's logs. The client address taken from the
`X-Forwarded-For` or `X-Real-IP` headers is also only the first address
in the header. Corydalis now needs wai-logger 2.5.1 or later, which the
Stack configuration adds to the snapshot.

Security: the logs could contain text from outside the application
unchanged: file names and the output of the external tools in the
application logs, and parts of requests (e.g. the path and the headers)
in the detailed request logs. Newlines in it could make a message look
like several log lines, e.g. forged ones, and terminal escape sequences
could act on the terminal of whoever reads the logs. Such characters
are now escaped, as `\x0a` (or `\u{202e}` for characters above
U+00FF): control characters (including newlines, tabs and the escape
character), the Unicode line and paragraph separators, invisible
formatting characters (including the bidirectional text controls, which
can make a line display differently from its contents), spaces other
than the ASCII one, private use and unassigned characters, and the
backslash itself, so that the escapes are unambiguous. Other characters,
such as accented letters, are logged as they are. The detailed request
logs, meant for debugging, keep several lines per request, but each
line is escaped in the same way. The request logs in the Apache format
are escaped by wai-logger instead, as described above.

Security: on public sites (built with the `public-site` flag), anonymous
visitors could see the flagged images pages, which show the names of the
users who flagged the images, i.e. valid login names, and the settings
page, which shows the server configuration, e.g. the paths of the picture
folders. Similarly, the status pages show the errors of the repository
scan, which can also contain such paths. These pages now always require
logging in.

Security: on public sites, the folder and image pages showed anonymous
visitors the full paths of the folders and files on the server, which
reveal the configured picture directories, the same information that
the settings page only shows after logging in. Anonymous visitors now
see the paths relative to the configured directories, prefixed by the
kind of directory (for example, `output: 2020/2020-01-01 Trip`), and the
raw image record at the bottom of the image page is not shown to them.

Security: on public sites, when an image or movie preview could not be
read or built, the error image shown to anonymous visitors contained the
error output of the external tools (ImageMagick, `exiftool`, `ffmpeg`),
which names the full paths of the picture and its cache files. Anonymous
visitors now get a generic message, while logged in users still see the
details in the error image. In both cases, the error and its details are
now always logged (previously, most of them weren't logged at all).

Security: the repository status shown at the top of all pages, including
the error message of a failed scan (which can contain e.g. paths on the
server), was also shown to anonymous visitors, on the login page and the
"not found" page. It is now only shown to logged in users, and the
status icon in the navigation bar no longer shows the repository state
(e.g. a failed scan) to anonymous visitors.

Security: the session key (`config/client_session_key.aes`), which
encrypts and authenticates the session cookies, and the database, which
holds the password hashes, were created with the default permissions,
i.e. usually readable by all users on the machine; with the session key,
they could create session cookies and log in as any user. At startup,
the `config` directory and the database directory are now restricted to
their owner, if needed; if this isn't possible (e.g. if owned by another
user), a warning is logged instead. See the installation documentation
on replacing the key, if other users could have read it.

Security: the repository scan followed symbolic links to files, which
were then shown and served like any other picture, so anyone who could
add a symbolic link to a picture folder (named, for example, `key.jpg`)
could download any file the application can read, e.g. the session key.
Symbolic links, and other special files (pipes, sockets and devices),
are now skipped at all levels of the picture folders, and listed as
errors on the status page. This includes symbolic links to directories,
which were already ignored, but silently; if you use such links to keep
pictures elsewhere, add their targets to the configured source or output
directories instead. Additionally, before reading or serving a file, it
is checked again that no part of its path below the configured
directories has been replaced by a symbolic link since the scan.

Security: the per-folder `corydalis.yaml` files were still read through
symbolic links, and from special files: a link to e.g. `/dev/zero` made
the scan read until it ran out of memory, and one to a YAML file outside
the picture folders showed its contents (if valid) on the folder page.
These files are now only read if the scan found them as regular files,
like pictures (a symbolic link or special file is listed as an error on
the status page), and of at most 64 KiB; a file that is ignored for its
size, or can't be read or parsed, is logged as a warning.

Security: the application now refuses to start if the `config`
directory (holding the session key) or the directory holding the
database is within one of the configured source or output directories,
since their files would then be scanned, and possibly shown, like
pictures, or within the cache directory, where the cache cleanup could
delete them (as would clearing the cache).

Security: logging in with a password took much longer for existing users
than for unknown ones, since the (deliberately slow) password check was
only done for the former, so the response time showed whether a user
name exists. Logging in as an unknown user (or a user without a
password) now checks the password against a dummy one, taking the same
time. As a side effect, the login no longer accepts JSON requests, only
the login form, which is all Corydalis uses.

Security: deleting a user (with `db-util`) didn't end the user's
sessions, which kept working until they expired, and since the sessions
only hold the id of the user, and SQLite can reuse the id of the last
user once it's deleted, the next user added could get the same id, and
these sessions would then be logged in as the new user. Deleted users
are now kept in the database, marked as deleted, so their ids are never
reused; they can't log in, and their sessions are no longer valid.
Adding a deleted user again restores it, with the same id. The database
migration adding the mark is done automatically at the first start
after upgrading.

Security: a session that was used at least every two hours stayed
logged in indefinitely, even after, for example, the password of the
user was changed. Logins are now valid for at most a week, configurable
(in hours) with the new `session-max-age` setting, after which users
have to log in again. Since sessions from before upgrading don't record
the login time, everybody has to log in again once after upgrading.

Security: changing the password of a user didn't end its existing
sessions, which kept working until they expired (see above); neither
did restoring a deleted user, whose old sessions came back if they
hadn't expired yet. Each user now has a session generation, recorded in
the session at login, and sessions are only valid while it is current:
changing the password, deleting, or restoring a user (with `db-util`)
increases it, ending all the user's sessions. The new `db-util logout`
command does only that, e.g. after losing a device that was logged in.
The database migration adding the generation is done automatically at
the first start after upgrading, and doesn't end the existing sessions.

Security: state-changing requests (flagging and un-flagging images, and
triggering a rescan) had no CSRF protection, so any web site could trigger
them on behalf of a logged-in user. They, and the login form, now require
a CSRF token, and the session cookie is now `SameSite=Lax`. Pages opened
before upgrading must be reloaded, and any scripts posting to these
endpoints directly must now send a token. If the token in a page becomes
stale, for example after the session expired and you logged in again in
another tab, flagging an image in the viewer will ask you to reload the
page.

Security hardening: POST requests can no longer be turned into PUT or
DELETE requests via a `_method` form field. Since plain HTML forms can
only send GET and POST, this ensures that other web sites cannot reach the
flagging endpoints with forms, even independently of the CSRF protection.
Scripts flagging images must now send real PUT/DELETE requests.

Security hardening: all responses now include a `Content-Security-Policy`
header. It only allows scripts loaded from Corydalis' own static files, so
that a bug allowing HTML injection cannot be used to run code, and it
forbids embedding Corydalis pages in frames. If a reverse proxy in front
of Corydalis already sets such a header, browsers apply both policies, so
you might want to remove or align the proxy's one.

Security: image and movie files were served with the type detected from
their contents, so for example an SVG or HTML file named `photo.jpg` or
`clip.mp4` in the repository was served as a document, and could run
scripts as the logged-in user. Only image and video types (excluding SVG)
are now passed through, anything else being served as a JPEG image or MP4
movie; additionally, file contents are sandboxed when opened directly,
and all responses forbid content type guessing (`nosniff`).

Security: the drop-down filters on the folder, camera and lens columns of
image lists were built from the table cells as HTML, so markup in a folder
name or in a picture's camera or lens EXIF data was turned into actual
elements (the content security policy above prevents it from running
scripts). These drop-downs have been removed; the text filters on these
columns still work, matching any part of the name.

Security: after logging in or out, Corydalis redirected to the page given
in the request's `Referer` header, even if it was on another site, so a
malicious site linking to the login page could send users to a fake page
after they logged in. Now only pages of the Corydalis site itself are
returned to after login, and logging out always shows the login page.

Security (development only): when run from GHCi via `DevelMain.update`,
the server ignored the configured `host` and listened on all IPv4
interfaces, while the password-less development login was enabled. It now
uses the same Warp settings, including the host, as `yesod devel`.

In the image viewer, if a movie could not be played (for example because
the browser blocked playback, or the movie format is not supported), the
play button still switched to "pause", as if the movie was playing. The
button now reverts to "play", and a message explains why the movie did
not start.

Similarly, if the browser refused to switch the image viewer to full
screen (for example, due to a permissions policy when embedded), nothing
happened. The viewer now falls back to its emulated full screen mode, the
same as on browsers without full screen support.

In the image browse mode, if loading more images while scrolling failed
(e.g. due to a network error), the grid silently stopped growing. It now
shows an error message below the images, and also a "Loading more
images..." message while a page is loading, and "No more images" once the
end is reached.

Security: the documentation and the sample configuration said that
`https: true` forces `secure-sessions`, but the two settings were read
independently, so `https: true` with `secure-sessions: false` sent the
session cookies without the secure bit. When serving HTTPS directly
(without a Unix socket, which ignores `https`), the cookies are now
always secure. Moreover, `secure-sessions` is now optional, and defaults
to true, so that the cookies are secure by default, including behind a
reverse proxy; serving plain HTTP, other than on `localhost`, requires
`secure-sessions: false`, and Corydalis warns at startup if `approot` is
a plain `http://` URL while the cookies are secure.

When using a Unix socket, Corydalis removed whatever was at the
configured path at startup, to replace a socket left by a previous run;
with a mistake in the configuration, this could have been any file
(e.g. the database). It now only removes a socket, and refuses to start
if the path holds anything else.

The cache cleanup failed to delete any obsolete file if the configured
cache directory was relative, had a trailing slash, or went through a
symlink, as the check that a file is within the cache directory compared
the configured path with the resolved path of the file. Moreover, the
check compared the paths as strings, so e.g. `/pics/cache2` counted as
within `/pics/cache` (only reachable through a symlink in the cache
directory, which Corydalis doesn't create). The cache directory is now
resolved too, and the paths compared by whole components.

Searching for a missing value of an attribute that always has one, e.g.
`no-folder`, `no-filename` or `no-status` in the URL, made the server
fail with an internal error; it is now rejected as an invalid search.

After a rescan, or after flagging an image from a form, Corydalis
redirected to the page in the request's `Referer` header, even if it was
on another site; now it only returns to pages of the Corydalis site
itself, as already done after login. These requests need a valid CSRF
token, so in practice they came from Corydalis' own pages anyway.

Security hardening: all responses now include `Referrer-Policy:
same-origin`, so that browsers don't send the URLs of Corydalis pages,
which contain folder and image names and search terms, to other sites
(e.g. when following a link to the documentation).

The `Strict-Transport-Security` header (when enabled) had a `max-age` of
only two hours, as the session timeout (in minutes) was used for it; it
is now one year, the usual value, so browsers keep using HTTPS for the
site between visits.

Security hardening: the CSRF token is now replaced when logging in and
out. It was kept for the whole life of the session, so someone who knew
the token of a session before login (e.g. by planting the session cookie
in the browser) could forge requests as the user who then logged in.
Pages opened before logging in or out (e.g. in other tabs) need to be
reloaded before their buttons and forms work again.

### Deployment changes

HEIC/HEIF pictures have always needed libheif's HEVC decoder, which
recent Debian and Ubuntu releases package separately
(`libheif-plugin-libde265`); without it, their previews fail with an
"Unsupported codec" error. The installation documentation now says so.

ImageMagick now reads pictures as `/dev/fd/0` and writes previews as
`/dev/fd/1`, so these must exist (as on any usual Linux system,
including under systemd's `PrivateDevices`).

The cache files being written (previews, and images extracted from raw
pictures and movies) are now kept in a `.tmp` directory at the root of
the cache directory, emptied at startup, and moved into place once
complete. The cache directory must thus be a single filesystem, and
each instance of the application needs its own: it can no longer be
shared between a production and a development build either (see
below).

The secure sessions setting will no longer enforce also an
[HSTS](https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Strict-Transport-Security)
header, but instead only set the secure bit on cookies; there is a new
`strict-transport-security` config setting to control the header,
defaulting to the `https` setting. However, even in https mode, the header
can now be skipped, since usually the header is emitted in front-line
applications, and thus when Corydalis is being reverse proxied, it should
not be emitted from it too; duplicate HSTS headers are technically
incorrect and will prevent sites from reaching an A+ score on SSL tests.
Note that the sample configuration file initially named this setting
`secure-transport-security`, which is ignored; if you copied it from
there, rename it to `strict-transport-security`.

Additionally, it is now possible to bind the app to a Unix socket, instead
of a host:port TCP connection, in order to improve performance and
security of reverse-proxied deployments. This is controlled via the new
`unix-socket` configuration key.

Two new, optional, configuration keys control how previews are built:

- `imagedecoders` maps file extensions (case-insensitively) to the
  ImageMagick decoder to use for them. The default covers JPEG, HEIC/HEIF,
  TIFF, PNG and WebP; files with other extensions get no previews, so if
  you have added extensions to `jpegexts` or `viewableimages`, check that
  they are covered. Decoders that run programs or read other files (e.g.
  PostScript, PDF, SVG) are refused.
- `moviedemuxers` maps movie file extensions (case-insensitively) to the
  ffmpeg demuxer to use when extracting their first frame. The default
  covers MP4, M4V, MOV, 3GP, AVI, MKV, WebM, MTS/M2TS and MPEG; movies
  with other extensions get no extracted frame, so if you have added
  extensions to `movieexts`, check that they are covered. Only plain
  container formats (`mov`, `avi`, `matroska`, `mpegts`, `mpeg`, `asf`,
  `ogg`) are accepted.
- `magickpolicydir` points to a directory holding the ImageMagick security
  policy to use; a restrictive one is in `config/magick/policy.xml`. When
  set, Corydalis refuses to start unless ImageMagick actually loads that
  policy; when not set, it logs a warning. See the installation
  documentation for details. The sample configuration sets it, so copy
  the policy too if you start from the sample.

Since the configuration is part of the repository cache, the first start
after upgrading rescans the repository.

The author of a flagged image is no longer a foreign key to the users
table, which the database migration (done automatically at the first
start after upgrading) removes. The key wasn't needed, as flags of
unknown authors are shown as such, but it prevented removing users who
had flagged images, and it prevented any change to the users table,
since the migration rebuilds the table, which the key doesn't allow.

Two new, optional, configuration keys control access to the Unix socket
(`unix-socket`): `unix-socket-group` sets its group, and
`unix-socket-mode` its permissions (as a quoted octal string, e.g.
`"0660"`). Connecting to the socket requires write permission on it, so
a reverse proxy running as another user, as is usual for nginx, could
only connect with a permissive umask, or by running on a TCP port
instead; now it can share a dedicated group with Corydalis. See the
installation documentation for an example.

There is now a sample systemd unit, `config/corydalis.service.sample`,
using systemd's sandboxing to limit what Corydalis and the tools it runs
can do; and the installation documentation has an example of limiting
login attempts in nginx.

The ImageMagick policy (`config/magick/policy.xml`) now also sets
resource limits, as in the Debian and Ubuntu policy: images at most 32K
pixels wide and high, and limits on the memory and disk used. These
replace the limits of the system policy, so check them against your
pictures (e.g. large panoramas), and copy the new policy when upgrading.

### Internal changes

Development builds no longer name their exif and repository cache files
differently (with a `-dev` suffix), which was meant to let them share a
cache directory with a production build; the cache cleanup now deletes
the cache files it doesn't use, which would include the development
build's own `-dev` exif caches. A development build rebuilds these
files once; its old repository cache (`repo-cache-dev`, at the root of
the cache directory, which the cleanup doesn't visit) can be deleted
by hand.

Running the tests needs ImageMagick to write HEIC pictures too, as the
tests create some: on recent Debian and Ubuntu releases, this needs
libheif's HEVC encoder (`libheif-plugin-x265`) as well as its decoder.
CI installs both.

The remaining (trivial) corydalis JS code has been converted to
TypeScript, and the entire codebase has been converted to modules, and is
using `esbuild` as a bundler. This has plusses and minuses, but brings the
code into a more modern era and should simplify use/integration of new
libraries.

The TypeScript code is now linted with
[oxlint](https://oxc.rs/docs/guide/usage/linter) instead of ESLint. It
uses oxlint's default (correctness) rules plus a few extra ones, and
removes a large number of development dependencies. It also no longer
depends on the `typescript` package, which unblocks moving to TypeScript
7 (the native Go port). With TypeScript 7 in place, the linter now also
runs type-aware rules and type-checks the code, which found the movie
playback and full screen bugs listed above.

The frontend no longer uses jQuery: its remaining uses (requests to the
server, and some DOM manipulation in the viewer) were small, and are now
done with the standard browser APIs (`fetch` and the DOM). Requests go
through a small helper, which adds the CSRF token to state-changing
requests; the linter forbids using `fetch` directly elsewhere. This
halves the size of most JavaScript bundles. Since the sortable tables
are now small, their bundle has been merged into the basic one.

The Node.js version used for development is now pinned in a
`.node-version` file (currently Node 24), which CI uses as well; Node 20,
as shipped by Debian stable, is no longer sufficient.

Movie frames extracted with `ffmpeg` (for movies without an embedded
preview) are now explicitly encoded as JPEG, instead of relying on the
default of `ffmpeg`, which happened to be JPEG. The tests now need
ImageMagick and `ffmpeg`, besides `exiftool`.

The TypeScript code now has a coverage report too (`npm run coverage`,
using `c8`), uploaded to Codecov next to the Haskell one, each with its
own flag. It also lists the modules that no test loads (e.g. the image
viewer), at 0%, so that the untested parts of the frontend show up
instead of being left out.

`make bootstrap` now installs the npm dependencies with `npm ci`, i.e.
exactly the versions in `package-lock.json`, and a committed `.npmrc`
disables the install scripts of the dependencies (which can run
arbitrary code at install time; none are needed). The `allowScripts`
setting in `package.json`, which npm ignores, is removed.

### Dependencies

The dependencies have been updated as follows:

- The graphing library `plotly` has been updated to version 3.0.
- TypeScript (used only for type-checking, since `esbuild` does the
  bundling) has been updated to version 7, the native Go port, which
  type-checks the code about three times faster.
- The `tablesorter` jQuery plugin has been removed, replaced by a small
  built-in module (see above). It was unmaintained and incompatible with
  jQuery 4, which it was blocking.
- jQuery has been removed (see above), instead of being updated to
  version 4.

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
