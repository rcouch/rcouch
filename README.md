rcouch
------

rcouch is a static distribution of couchdb using rebar. It is an
extraction of the refuge code to ease the distribution of couchdb.

Based on the couchdb code it is tagged from time to time for stability
purpose. At some point some supported branch may appear.

RCouch differs from refuge by only focusing on Apache CouchDB. It won't
contain any of the refuge features like P2P nodes handling, ... .
Internally we are using it to test couchdb and as an example on how to
embed couchdb in your own applications.

RCouch is brought you by the [Refuge project](http://refuge.io).

## Features:

- Apache CouchDB based
- Geocouch integrated
- rebar
- relocatable
- Fully opensource. All the sources are on refuge GIT repository
  (http://github.com/refuge) under Apache License 2.

## Requirements

- OS supported: Linux, OSX, BSDs (windows support is coming)
- Erlang R14
- Curl
- ICU (if not built statically)

## Installation

Installation is pretty simple. Just run the command line:

    $ make rel

and it will generate a couch folder in rel/couch. This release is
fully relocatable, so you can put it where you want on your system.


To create package for your system run `make package` . For now we build
packages for OSX, Debian, Redhat & Solaris.

##Notes on building a truly distributable package

The package built above will still depend on some libraries from your
system, so additional work has to be done to distribute it to
older/newer systems.

1. CouchDB will depend on the ICU library version that was present in
   your system at build time. To easily bundle this library with the
   package, build with:

         $ make rel USE_STATIC_ICU=1

1. Check whether your package depends on Ncurses:

         $ ldd ./rel/rcouch/erts-*/bin/erlexec|grep ncurses

    If it does, copy the .so file to ./rel/myapp/lib/ or rebuild Erlang
    without this dependency.

1. Decide whether you need SSL support in your package and check whether it
   depends on OpenSSL:

         $ ldd ./rel/rcouch/lib/ssl-*/priv/bin/ssl_esock|grep 'libcrypto\|libssl'

    If it does, copy the .so file to ./rel/rcouch/lib/ or rebuild Erlang
    without this dependency.

If you copied any .so files in the last 2 steps, run this command, so
that your app can find the libraries:

    $ sed -i '/^RUNNER_USER=/a\\nexport LD_LIBRARY_PATH="$RUNNER_BASE_DIR/lib"' ./rel/rcouch/bin/rcouch
