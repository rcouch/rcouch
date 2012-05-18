rcouch [![Build Status](https://secure.travis-ci.org/refuge/rcouch.png)](http://travis-ci.org/refuge/rcouch)
------------------------------------------------------------------------------------------------------------

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

The following operating systems are supported: Linux, OSX, BSDs, and Windows

Linux/Unix: 
- Erlang R14|15
- Git
- Curl
- ICU (if not built statically)

Windows:
- Erlang 15B01
- Windows SDK 7.1 including .NET 4 Framework
- Refuge-Build

## Installation

### Linux/Unix

Installation is pretty simple. Just run:

    $ make rel

### Windows

Refuge-Build is our project support for Windows for the tool chains necessary 
for compilation. Just download and run the refuge-build-setup.exe installer. 
Refuge-Build will install at c:\refuge-build. Refuge-Build includes Portable
msysGit.

![screenshot of Refuge-Build](https://github.com/refuge/refuge-media/blob/master/screenshots/refuge-build/installer.png?raw=true)

Refuge-Build also comes complete with automation for installing Windows 7.1 SDK
(MSVC10),.NET 4 Framework and Erlang 15B01. If you already have these on your 
system, anticipate approximately 2 minutes to download and install Refuge-Build.
If you need to install the prerequisite software, anticipate 15 min total to 
prepare your system.

Once installed, just run:
	
	$ refuge-make rel

### Where to Find Your Release

Building with the rel target will generate a couch folder in rel/couch. This 
release is fully relocatable, so you can put it where you want on your system.

To create package for your system run `make package`. We build
packages for OSX, Debian, Redhat, Solaris and Windows

## Building rcouchx for MacOSX

rcouchx is a macosx interface for rcouch based on [the excellent
work](https://github.com/janl/couchdbx-app) of Jan:

![screenshot of rcouchx](http://benoitc.im/vrac/rcouchx/Screen%20Shot%202012-04-20%20at%2011.55.48%20PM_thumb.png)


To build it, run the command line:

    $ make rcouchx

Then launch the `rcouchx.app` application.


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


## Binding port 80

On most UNIX systems binding port 80 is a privileged operation (requires
root). Running Erlang as root is not recommended so some configuration
will need to be done so that rcouch can bind port 80.

If you run a recent Linux kernel with capabilities you can give Erlang
the privilege using the setcap command (you may need to install a
package named lxc or similar to obtain this command).

    $ setcap 'cap_net_bind_service=+ep' /path/to/rel/refuge/erts-5.8.5/bin/beam`
    $ setcap 'cap_net_bind_service=+ep' /path/to/rel/refuge/erts-5.8.5/bin/beam.smp

On FreeBSD all ports can be made accessible to all users by issuing:

$ sysctl net.inet.ip.portrange.reservedhigh=0
