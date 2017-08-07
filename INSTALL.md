# Installation

Corydalis is built using Haskell and the Yesod framework, which means
it has a lot of build dependencies; therefore, the simplest way is to
use the `stack` tool:

        $ stack build
        $ stack install --local-bin-path /path/to/target
        $ cp -a static /path/to/target/

Create a few needed directories:

        $ cd /path/to/target
        $ mkdir config db

At runtime, the application depends on ImageMagick, so make sure to
install it before you run the application, e.g.:

        $ sudo apt-get install imagemagick

The application is configured to work only over https; as such, you
must install the certificate in `config/cert.pem` and the key in
`config/cert.key` in the target directory:

        $ mkdir /path/to/target/config
        $ cp cert.pem cert.key /path/to/target/config

Install the sample configuration file:

        $ cp config/settings.yml.sample /path/to/target/settings.yml

Update the `settings.yml` file as needed, and run the application:

        $ cd /path/to/target
        $ ./corydalis settings.yml

The application uses a database to store users for authentication;
normally I expect only one or at most a few users - family - to be
defined, but it was easier to use a database than (e.g.) something
like htpasswd. There's currently no way to define users in
non-development mode :(

Being a web application, it also has a few JavaScript dependencies:

* jquery
* jquery-metadata
* jquery-tablesorter

These are distributed with the application, so there's no need to do
anything, unless you want to use newer version of them; just update
the files in the `static/` subdirectory.

I've also seen that due to the nontrivial amount of memory used by all
the filename tracking, the periodic idle GCs use non-trivial amount of
CPU time; increasing the idle GC timeout helps - I use slightly more
than one minute:

        $ ./corydalis +RTS -I63

## Note about the session key

The application needs a session key for the client session cookies. If
the file `config/client_session_key.aes` does not exist, it will be
automatically created; however, deleting it and restarting the binary
will invalidate any existing sessions.
