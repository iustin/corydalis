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
the library are deleted at the end of each scan.

All the pre-rendering and exif extraction needs as well quite a bit of
CPU; on my own collection, a cold start will probably take many hours
(for initial, required EXIF gathering) and follow-up work possibly a
day or so (for the pre-rendering).

## Building from source

Using stack:

    echo install npm packages and pre-process them
    make bootstrap
    echo build the backend
    stack build
    stack install --local-bin-path /path/to/target
    cp -aL static /path/to/target/

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
    mkdir config db

At runtime, the application depends on ImageMagick, exiftool and
ffmpeg, so make sure to install them before you run the application,
e.g.:

    sudo apt install imagemagick libimage-exiftool-perl ffmpeg

Without ffmpeg, the application still works, but movies without an
embedded preview have no preview image.

Install the sample configuration file from the source code:

    cp config/settings.yml.sample /path/to/target/settings.yml

## Serving over HTTPS

Corydalis can serve HTTPS itself, or run behind a reverse proxy that
handles HTTPS.

To serve HTTPS directly, set `https: true` (as in the sample
configuration), and install the certificate in `config/cert.pem` and
the key in `config/cert.key` in the target directory:

    cp cert.pem cert.key /path/to/target/config

Behind a reverse proxy:

- set `https: false`, and point the proxy either to the configured
  `host` and `port`, or to a Unix socket, configured via `unix-socket`
  (which is more efficient, and ignores the `host`, `port` and `https`
  settings);
- keep `secure-sessions: true` if the proxy serves HTTPS, so that the
  cookies are only sent over HTTPS;
- set `approot` to the public URL, as seen by the browser;
- set `ip-from-header: true`, so that the logs show the clients'
  addresses instead of the proxy's.

The `Strict-Transport-Security` (HSTS) header is controlled by the
`strict-transport-security` setting, which defaults to the `https` one;
behind a proxy, it is usually better configured in the proxy, for the
whole domain.

Corydalis sends its own `Content-Security-Policy` and
`X-Content-Type-Options: nosniff` headers with all responses. The proxy
should not add a different `Content-Security-Policy` for Corydalis:
browsers enforce all the policies they receive, so an additional one can
break pages.

## Basic configuration

The `settings.yml` file has comments on the configuration
keys. Basically there are only three main that really need
configuration:

- where source (RAW) files live
- where the processed output (JPEGs) live
- what should be the cache directory

The application uses a database to store users for authentication;
normally I expect only one or at most a few users - family - to be
defined, but it was easier to use a database than (e.g.) something
like htpasswd. The `db-util` tool can add users:

    ./db-util -c /path/to/target/config add <USER>

This will prompt for the password and then add (or update, if the user
already existed) the user. It can also list and delete users.

When first run, the tool will automatically create (or update, on
upgrade) the required database tables.

## Running corydalis

Once the configuration is updated, simply run the application:

    cd /path/to/target
    ./corydalis settings.yml

Note that at the first run, it will start scanning the configured
directories and parse metadata, so it will take a long while - maybe
up to hours - for the initial run to finish, depending on both CPU and
disk I/O for your storage.

I've also seen that due to the nontrivial amount of memory used by all
the filename tracking, the periodic idle GCs use non-trivial amount of
CPU time; increasing the idle GC timeout helps - I use slightly more
than one minute:

    ./corydalis +RTS -I63

### Note about the session key

The application needs a session key for the client session cookies. If
the file `config/client_session_key.aes` does not exist, it will be
automatically created; however, deleting it and restarting the binary
will invalidate any existing sessions.
