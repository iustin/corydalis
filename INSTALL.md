# Installation

Corydalis is built using Haskell and the Yesod framework, which means
it has a lot of dependencies; therefore, the simplest way is to use
`cabal`:

        $ cabal configure -f production
        $ cabal build

Next, copy the resulting binary (`dist/build/corydalis/corydalis`)
somewhere, along with the `static` and `config` subdirectories. In the
`config` directory, make a copy of the `settings.yml.sample` file as
`settings.yml`, and update the configuration as needed.

Being a web application, it also has a few JavaScript dependencies:

* jquery
* jquery-metadata
* jquery-tablesorter

These are distributed with the application, so there's no need to do
anything, unless you want to use newer version of them; just update
the files in the `static/` subdirectory.

Once all these are in place, just run the binary, which will load the
settings from the environment (if the environment variables are
defined) or otherwise from config file or finally it will use
compiled-in defaults:

        $ ./corydalis

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
