# Installation

Corydalis is built using Haskell and the Yesod framework, which means
it has a lot of dependencies; therefore, the simplest way is to use
`cabal`:

        $ cabal configure -f production
        $ cabal build

Next, copy the resulting binary (`dist/build/corydalis/corydalis`)
somewhere, along with the `static` and `config` subdirectories. In the
`config` directory, make a copy of the `settings.yml.sample` file, and
update the configuration as needed.

Being a web application, it also has a few JavaScript dependencies:

* jquery
* jquery-metadata
* jquery-tablesorter

These are not distributed with the application, instead they're
expected to be served from an external server under the
'javascripturl` configuration key; more precisely, the URLs that will
be referenced will be:

* `$javascripturl/jquery/jquery.js`
* `$javascripturl/jquery-metadata/jquery.metadata.js`
* `$javascripturl/jquery-tablesorter/jquery.tablesorter.js`

Without these, file table sorting will not work.

Once all these are in place, just run the binary with the `Production`
argument, which will load the `Production` settings from the config
file:

        $ ./corydalis Production

I've also seen that due to the nontrivial amount of memory used by all
the filename tracking, the periodic idle GCs use non-trivial amount of
CPU time; increasing the idle GC timeout helps - I use slightly more
than one minute:

        $ ./corydalis Production +RTS -I63
