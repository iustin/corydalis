# Installation

## Requirements

### Frontend

Being a web application, Corydalis has a quite a bit of JavaScript
dependencies. These are installed via `npm`, and to get everything up
and ready for compilation/integration to the backend, you need to
first install and post-process these (see actual instructions below).

### Backend

The backend is built using Haskell and the Yesod framework, which
means it has a lot of build dependencies; therefore, for source
builds, the simplest way is to use the
[stack](https://docs.haskellstack.org/en/stable/README/) tool.

It uses [ImageMagick](https://www.imagemagick.org/) to generate the
correctly-sized images for the client,
[ExifTool](https://exiftool.org/) to extract the EXIF/IPTC/XMP
information from the source pictures, and
[ffmpeg](https://www.ffmpeg.org/) to extract the first frame of movies
without an embedded preview, to be used as their preview image.

It also uses sqlite (via Haskell bindings), so you might need to have
devel libraries for it.

Because of the way the image viewer works with clients (sending
appropriately-sized images, instead of relying on in-client scaling),
and due to extraction of metadata from all images, it also needs a
relatively large cache space for itself; for my own usage, with two
pre-rendered resolutions, this amounts to around 10% of the actual
library space. Cache files belonging to pictures that are no longer in
the library, or to preview sizes no longer configured, are deleted at
the end of each scan.

All the pre-rendering and exif extraction needs as well quite a bit of
CPU; on my own collection, a cold start will probably take many hours
(for initial, required EXIF gathering) and follow-up work possibly a
day or so (for the pre-rendering).

## Building from source

Using stack:

    echo install npm packages and pre-process them
    make bootstrap
    echo build the backend
    stack build --flag corydalis:-dev
    stack install --flag corydalis:-dev --local-bin-path /path/to/target

The `dev` flag is on by default, for development and the tests, and
disables the executables, so a build for installing must turn it off
(`--flag corydalis:-dev`); otherwise, `stack install` installs nothing.

The static files (scripts, style sheets, fonts) are embedded in the
`corydalis` binary, so no `static` directory needs to be installed
with it; the scripts and style sheets are also embedded compressed
with gzip, so a reverse proxy doesn't need to compress them (proxies
leave such responses alone). Note that these are sent with
`Content-Encoding: gzip` even to clients that don't accept it, e.g.
`curl` without `--compressed`, or a proxy configured to rewrite
response bodies (e.g. nginx's `sub_filter`, which needs
`proxy_set_header Accept-Encoding ""`); browsers all accept gzip.

That should be it about the "build" phase, although if you don't
already have a stack work space already, this will take a bit of time
and CPU.

A simpler way for all this is to just run `make release` for the
backend part.

This step will generate three binaries:

- `corydalis`, the main application; this is what you will run later.
- `db-util`, a tool to set the users in the database; you will need
  this to initialise the first user(s).
- `scanner`, which is an internal tool and shouldn't be of much use.

## Completing the installation

Create a few needed directories:

    cd /path/to/target
    mkdir -m 0700 config db

These hold private files, so they should be accessible only to the user
running the application (see [private directories](#private-directories)
below).

At runtime, the application depends on ImageMagick, exiftool and
ffmpeg, so make sure to install them before you run the application,
e.g.:

    sudo apt install imagemagick libimage-exiftool-perl ffmpeg

Without ffmpeg, the application still works, but movies without an
embedded preview have no preview image.

For HEIC/HEIF pictures (e.g. from phones), ImageMagick needs libheif's
HEVC decoder. Releases where libheif uses plugins (e.g. Debian 13,
Ubuntu 24.04) package it separately:

    sudo apt install libheif-plugin-libde265

Without it, previews of these pictures fail with an "Unsupported codec"
error.

ImageMagick reads the pictures as `/dev/fd/0` and writes the previews
as `/dev/fd/1`, so that it never gets a file name to interpret; these
must exist, as on any usual Linux system (systemd's `PrivateDevices`
keeps them).

Install the sample configuration file and the ImageMagick policy from
the source code, in the `config` directory (where the session key is
also kept, see below, and as the sample systemd unit expects):

    mkdir -p /path/to/target/config/magick
    cp config/settings.yml.sample /path/to/target/config/settings.yml
    cp config/magick/policy.xml /path/to/target/config/magick/

## Serving over HTTPS

Corydalis can serve HTTPS itself, or run behind a reverse proxy that
handles HTTPS.

To serve HTTPS directly, set `https: true` (as in the sample
configuration), and install the certificate in `config/cert.pem` and
the key in `config/cert.key` in the target directory:

    cp cert.pem cert.key /path/to/target/config

The session cookies are then always secure, i.e. only sent over HTTPS,
whatever the `secure-sessions` setting.

Behind a reverse proxy:

- set `https: false`, and point the proxy either to the configured
  `host` and `port`, or to a Unix socket, configured via `unix-socket`
  (which is more efficient, and ignores the `host`, `port` and `https`
  settings);
- keep `secure-sessions` enabled (the default), so that the cookies
  are only sent over HTTPS;
- set `approot` to the public URL, as seen by the browser;
- set `ip-from-header: true`, so that the logs show the clients'
  addresses instead of the proxy's (see below).

Serving plain HTTP (without HTTPS, even in a proxy) requires
`secure-sessions: false`, except on `localhost`, which browsers treat as
secure: otherwise, browsers don't keep the session cookies, and logging
in silently fails, going back to the login page. If `approot` is set to
a plain `http://` URL while the cookies are secure, Corydalis logs a
warning at startup.

The `Strict-Transport-Security` (HSTS) header is controlled by the
`strict-transport-security` setting, which defaults to the `https` one,
except with `unix-socket` (which ignores `https`), where it defaults to
off; behind a proxy, it is usually better configured in the proxy, for
the whole domain. When enabled, Corydalis sends it with all its
responses (including static files and errors), with a `max-age` of one
year, and `includeSubDomains` (which only covers the names under the
one Corydalis is served on).

Corydalis sends its own `Content-Security-Policy`,
`X-Content-Type-Options: nosniff` and `Referrer-Policy: same-origin`
headers with all responses; the latter keeps browsers from sending the
URLs of Corydalis pages, which contain folder and image names, to other
sites. The proxy should not add a different `Content-Security-Policy`
for Corydalis: browsers enforce all the policies they receive, so an
additional one can break pages.

Responses without their own `Cache-Control` header (including dynamic
pages, JSON responses, served images and movies, authentication pages,
and errors) also carry `Cache-Control: private, no-cache`. This prevents
shared caches (such as a reverse proxy or CDN) from storing them and
requires browsers to revalidate with the server before reusing them
(served images and movies revalidate via `Last-Modified` / `304 Not
Modified`), while keeping browser back/forward navigation intact. The
trade-off compared to `no-store` is that on a shared device, the browser
may still retain previously visited pages or media on disk or in its
back/forward cache after logout. Responses that already set their own
`Cache-Control` header keep it: Yesod's redirects send
`no-cache, must-revalidate`, and the favicon sends
`private, max-age=2592000` (private because it carries the session
cookie). Static assets (under `/static/`) remain publicly cacheable.

### Unix socket

With `unix-socket` set, Corydalis listens on that Unix socket instead
of the configured host and port. A socket left by a previous run is
replaced; if the path holds anything else (e.g. a regular file, by a
mistake in the configuration), Corydalis refuses to start rather than
remove it.

Connecting to a Unix socket requires write permission on it. By
default, the socket belongs to the application's user and primary
group, and its permissions come from the umask (e.g. `0755` with the
usual `022`, or `0700` with the sample systemd unit's `UMask=0077`), so
only the application's own user can connect. To let a
proxy running as another user (e.g. `www-data` for nginx) connect, set:

- `unix-socket-group` to a group of which both the proxy's user and the
  application's user are members; the application's user doesn't need
  it as primary group;
- `unix-socket-mode` to permissions giving that group write access,
  as a quoted octal string (YAML may read an unquoted `0660` as a
  decimal number, so it is refused).

Both are applied before the socket accepts connections, and Corydalis
refuses to start if the group doesn't exist, or if its user is not a
member of it. A dedicated group is preferable to the proxy's own group
(e.g. `www-data`), which other applications served by it may run as.
For example, with a `corydalis-socket` group:

    groupadd --system corydalis-socket
    usermod -a -G corydalis-socket www-data

then, in the settings:

    unix-socket: /run/corydalis/socket
    unix-socket-group: corydalis-socket
    unix-socket-mode: "0660"

With systemd, `SupplementaryGroups=corydalis-socket` in the service
gives it the group, and `RuntimeDirectory=corydalis` creates
`/run/corydalis` (accessible to others by default, which the proxy
needs to reach the socket). The proxy must be restarted to get its new
group. In nginx, use the socket as:

    proxy_pass http://unix:/run/corydalis/socket:;

### Client addresses behind a proxy

With `ip-from-header`, the request logs show as client address the
first address in the `X-Real-IP` or `X-Forwarded-For` header,
whichever comes first in the request, and the address of the
connection (i.e. the proxy's) if neither is present. The setting is
only used for the logs.

Clients can send these headers too, so the proxy must replace them
with the address it sees, rather than pass them on or add to them;
otherwise, clients can choose the address that is logged. For nginx:

    proxy_set_header X-Real-IP       $remote_addr;
    proxy_set_header X-Forwarded-For $remote_addr;

`proxy_set_header` replaces the header sent by the client. Set both
headers, even if your setup normally uses only one: a header that the
proxy doesn't set is passed on as the client sent it, and can come
first. Don't use `$proxy_add_x_forwarded_for`, as in many examples: it
appends the address to the client's `X-Forwarded-For`, whose first
address, the one logged, is then the client's choice.

For Apache (`mod_proxy` with `mod_headers`, 2.4.10 or later):

    RequestHeader unset X-Forwarded-For
    RequestHeader set X-Real-IP "expr=%{REMOTE_ADDR}"

By default, `mod_proxy` also appends the address to the client's
`X-Forwarded-For`, and passes on its `X-Real-IP`; after removing the
client's header, it adds its own with just the client's address.

If several proxies are chained, only the first one (facing the clients)
should set these headers; the next ones must pass them on unchanged,
since for them the connection comes from the previous proxy. A
log showing the address of a proxy (e.g. `127.0.0.1`) as client usually
means that `ip-from-header` is not enabled, that a proxy doesn't set the
headers, or that a later one replaces them; or, with a proxy that
appends to the client's header, that the client sent that address.

### Limiting login attempts

Corydalis doesn't limit the number of login attempts. The passwords are
stored with a slow hash, so attempts also cost CPU time; it is best to
limit them in the proxy. With nginx, for example, at most ten attempts a
minute per client address:

    # In the http block:
    limit_req_zone $binary_remote_addr zone=corydalis_login:10m rate=10r/m;

    # In the server block, besides the location for Corydalis:
    location = /auth/page/hashdb/login {
        limit_req zone=corydalis_login burst=5 nodelay;
        # The same proxy settings as for the other Corydalis pages.
        proxy_pass http://unix:/run/corydalis/socket:;
    }

Apache has no such limit built in; modules such as `mod_security`, or
tools such as `fail2ban` (watching the request log for posts to
`/auth/page/hashdb/login`), can do it.

## Basic configuration

The `config/settings.yml` file has comments on the configuration
keys. Basically there are only three main that really need
configuration:

- where source (RAW) files live
- where the processed output (JPEGs) live
- what should be the cache directory

These must be absolute paths: Corydalis refuses to start otherwise, as
relative ones would depend on the current directory, which can differ
between Corydalis and the other tools reading the same settings (such
as the scanner).

The application uses a database to store users for authentication;
normally I expect only one or at most a few users - family - to be
defined, but it was easier to use a database than (e.g.) something
like htpasswd. The `db-util` tool can add users:

    ./db-util -c /path/to/target/config add <USER>

This will prompt for the password and then add (or update, if the user
already existed) the user. It can also list and delete users.
Deleted users are kept in the database, marked as such (so that their
ids, which sessions refer to, are never given to new users), and can't
log in; adding a deleted user again restores it.

Changing the password of a user, or deleting it, ends all its existing
sessions, on all devices, and so does restoring a deleted user: old
sessions don't come back. To end the sessions of a user without other
changes (e.g. after losing a device that was logged in), use:

    ./db-util -c /path/to/target/config logout <USER>

When first run, the tool will automatically create (or update, on
upgrade) the required database tables.

### Image decoders

Previews are built by ImageMagick, using the decoder configured for
each file extension in the `imagedecoders` setting (see the sample
configuration for the default list). The decoder is always passed
explicitly, since otherwise ImageMagick would guess it from the file
contents, and a malicious file (see [Picture and cache
directories](#picture-and-cache-directories)) could make it run e.g. a
PostScript document named `photo.jpg`.

Files whose extension has no configured decoder get no previews; if you
add an extension to `jpegexts` or `viewableimages` that is not in the
default list, also add it to `imagedecoders`.

For a file holding several images (e.g. a multi-page TIFF, or an
animated WebP), the preview is built from the first one (for a HEIC
file, its primary image).

### Movie demuxers

For movies without an embedded preview, the preview is the first frame,
extracted by ffmpeg, using the demuxer (container format) configured for
each file extension in the `moviedemuxers` setting (see the sample
configuration for the default list). As for images, the format is always
passed explicitly, since otherwise ffmpeg would guess it from the file
contents, and some formats make it read other files (e.g. playlists, or
concat scripts); ffmpeg is also only allowed to read local files. Only
plain container formats can be configured: `mov` (which covers MP4 and
3GP), `avi`, `matroska` (which covers WebM), `mpegts`, `mpeg`, `asf` and
`ogg`.

Movies whose extension has no configured demuxer, or whose contents
don't match it (e.g. an AVI file named `movie.mp4`), get no extracted
frame; if you add an extension to `movieexts` that is not in the default
list, also add it to `moviedemuxers`.

### ImageMagick policy

As a second layer of protection, the `magickpolicydir` setting points
ImageMagick to a restrictive security policy, which allows reading only
the decoders in the default `imagedecoders` list, writing only JPEG and
PNG (for the previews), and running no external programs such as
Ghostscript. The sample configuration uses the policy installed above,
in `config/magick`.

Corydalis runs ImageMagick with the `MAGICK_CONFIGURE_PATH` environment
variable set to this directory. ImageMagick also looks there for its
other configuration files, so keep only `policy.xml` in it. Since
ImageMagick silently ignores a policy it can't find, Corydalis checks at
startup that ImageMagick lists the policy file among the ones it
loaded, and refuses to start otherwise. Without the setting, it starts
normally, but logs a warning.

The system policy (e.g. `/etc/ImageMagick-7/policy.xml`) still applies.
ImageMagick evaluates its rules after the Corydalis ones, and the last
matching rule wins: restrictions in the system policy (for example,
after a security update) apply to Corydalis too, but so would rules
there that explicitly allow a decoder.

If you add a decoder to `imagedecoders`, also add it to the policy;
otherwise, previews for those files fail with an error saying the
operation is not allowed by the security policy.

The policy also limits the resources ImageMagick may use for an image,
as a crafted image can claim huge dimensions: images at most 32K pixels
wide and high, 1 GiB of memory, 2 GiB of memory map and of disk, with
large images (over 256 megapixels) kept on disk. These are the limits of
the Debian and Ubuntu policy; unlike the rules above, they replace the
system policy's own limits instead of adding to them. Adjust these
limits to your system and pictures: for example, previews of panoramas
wider than 32K pixels need a larger `width`, and, for very large
images, enough disk. `convert -list resource`, with
`MAGICK_CONFIGURE_PATH` set to the policy's directory, shows the limits
in effect. Don't add a limit on how many images ImageMagick reads from
one file (`list-length`): for an animated WebP, it decodes all the
frames even though previews only use the first, so previews of longer
animations would fail.

## Running corydalis

Once the configuration is updated, simply run the application:

    cd /path/to/target
    ./corydalis config/settings.yml

Note that at the first run, it will start scanning the configured
directories and parse metadata, so it will take a long while - maybe
up to hours - for the initial run to finish, depending on both CPU and
disk I/O for your storage.

I've also seen that due to the nontrivial amount of memory used by all
the filename tracking, the periodic idle GCs use non-trivial amount of
CPU time; increasing the idle GC timeout helps - I use slightly more
than one minute:

    ./corydalis +RTS -I63

The binaries are built with the `--disable-delayed-os-memory-return`
runtime option, so that the memory the runtime returns to the system
after a garbage collection is really released at once. Without it, on
Linux, the runtime only marks that memory as reclaimable, and the
kernel takes it back when it is short on memory; until then it still
counts as used by the process (in `top`, or in systemd's memory
accounting), which after a rescan (needing much more memory than
serving the pictures does) can be more than twice what the application
really uses.

### Running as a systemd service

`config/corydalis.service.sample` is a sample systemd unit. It runs
Corydalis as its own user, and uses systemd's sandboxing to limit what
Corydalis and the tools it runs (ImageMagick, ffmpeg and exiftool) can
do, should a crafted picture or movie exploit one of them (see
[Picture and cache directories](#picture-and-cache-directories)). The
files Corydalis creates are only readable by its user (`UMask=0077`),
and, when it listens on a Unix socket, it can be cut off from the
network entirely, as it makes no outgoing connections. Its comments
explain what to adjust (the paths, the socket settings, the network)
and the sandboxing options, some of which are not yet tested with
Corydalis.

### Note about the session key

The application needs a session key for the client session cookies. If
the file `config/client_session_key.aes` does not exist, it will be
automatically created; however, deleting it and restarting the binary
will invalidate any existing sessions.

### Session lifetime

Sessions expire after two hours without activity, and, independently of
activity, logins are valid for at most a week, after which users have to
log in again; this also bounds how long a stolen session can be used, if
nobody notices. The maximum age can be changed, in hours, with the
`session-max-age` setting. Sessions also end when the user's password is
changed, or the user is deleted or logged out with `db-util` (see
above).

### Private directories

Some files must not be readable by other users on the machine: the
session key, since anyone who can read it can log in as any user; the
database, which holds the password hashes; and the TLS key, if
installed in `config`. Rather than relying on the permissions of each of
these files (which are created with the default ones), the `config`
directory and the directory holding the database (normally `db`) should
be accessible only to their owner, i.e. have mode 0700.

At startup, `corydalis` (and `db-util`, for the database directory)
checks this, and if a directory is accessible to other users:

- if it is owned by the user running the application, its permissions
  are restricted, with a warning;
- otherwise, or if restricting them fails (e.g. on a read-only file
  system), only a warning is logged, and you should check the
  permissions yourself; for example, a `config` directory owned by root
  and readable by the application's group is fine, as long as only the
  application's user is in that group.

Since the directory is restricted, the database should be in its own
directory, not in the current directory (which is not changed), and the
Unix socket and the cache directory should not be in these directories.

These directories (the `config` directory, and the directory holding the
database, even if it is the current directory) must also not be within
the configured source or output directories, where their files would be
scanned, and possibly shown, like pictures, nor within the cache
directory, whose files are deleted by the cache cleanup if they don't
belong to a picture, and which one would expect to be able to clear
safely. If one of them is, the application refuses to start. The directories are compared after
resolving symbolic links, so a link to such a directory doesn't avoid
the check.

If the directories were accessible to other users before, e.g. with
older versions, and other users could have read the session key, delete
it and restart the application, to create a new key; similarly, consider
changing the passwords.

### Picture and cache directories

The structure of the picture directories is trusted, but the contents
of the pictures and movies in them are not.

The structure: only the administrators of the application (or people
trusted as much) should be able to add, remove or replace files and
directories in the source, output and cache directories while the
application runs; reading pictures from shared or synchronised folders
is fine, as long as the others can't write to them.

The scan only accepts regular files (not symbolic links, nor other
special files), and before reading or serving a file, the application
checks again that its path doesn't contain a symbolic link. However,
the file is then opened again by name, by the web server or by the
external tools; someone who can replace files in these directories at
the right moment can thus make the application serve (or convert) any
other file it can read, including the session key. The cache directory,
whose files are served without further checks, is only written by the
application, and should be writable only by its user.

Each instance of the application needs its own cache directory: the
cache cleanup deletes the files that aren't for a picture in its
library or a size it uses, and the previews and extracted images being
written are kept in the `.tmp` directory at its root, which is emptied
at startup (as is the output of the scan's exiftool runs, written
there too). They are then moved into place by renaming them, so the
cache directory must be a single filesystem (with no other filesystem
mounted inside it).

On a site built with the `public-site` flag, where anonymous visitors
can browse, anyone can request the on-demand sizes (`demandimgsizes`)
of every picture, and so fill the cache directory with large previews:
set `demandimgsizes: []` there, or put the cache directory on a
filesystem limited in size (or with a quota).

The contents: pictures and movies often come from elsewhere (downloads,
or files received from others), and a crafted file can try to exploit
the tools that read it. The application therefore restricts how these
read the files: ImageMagick only uses the decoder configured for the
file's extension (see [Image decoders](#image-decoders)) and its
security policy, ffmpeg only the demuxer configured for the extension
(see [Movie demuxers](#movie-demuxers)) and only local files, and
embedded previews are only used if they are JPEG images. Building a
preview is also stopped (and fails) if a tool runs for more than five
minutes, and at most as many tools run at once as the application uses
processors (all of them, unless limited with `+RTS -N`); further
previews wait. Both limits can be changed with the
`render-tool-timeout` (in seconds) and `render-tool-slots` settings,
e.g. to allow more time on a slow machine, or to leave processors free
for other services. The scan's exiftool runs, which read the metadata
of the files in batches, have the same kind of limits, separately (the
previews the scan renders use the render ones): a run is stopped
after ten minutes, and all the files of its batch then fail (again at
each scan, if one of them makes exiftool hang), and at most as many
runs happen at once as the application uses processors, changed with
the `scan-tool-timeout` (in seconds) and `scan-tool-slots` settings.
The two limits add up: by default, during a scan, up to twice as many
tools as processors can run at once. The decoder and demuxer
restrictions limit which parsers read a file, but a bug in one of them,
or in exiftool (which reads the metadata of every file), can still be
exploited: keep ImageMagick, ffmpeg and exiftool up to date,
and consider restricting what the application can do (e.g. with
systemd's sandboxing options), so that a compromised tool can do little
harm.

The folder settings files (`corydalis.yaml`) are checked when scanned
(they must be small, regular files), and their contents are only used
as settings.
