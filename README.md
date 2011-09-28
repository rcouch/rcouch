refuge - Data should be yours
-----------------------------

Refuge main goal is to design an opensource, free, decentralized and
secured and eventually anonymous platform. This platform will allow to
share, render information and exchange messages offline or online.
Information is on each nodes.


## Features:

- Apache CouchDB 1.2x based (current trunk right now)
- Geocouch integrated
- dnssd support
- upnp support (coming)
- P2P layer (coming)
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

and it will generate a refuge folder in rel/refuge. This release is
fully relocatable, so you can put it where you want on your system.

When developping on top of refuge, you could also run the command line:

    $ make dev

Then ./dev/dev[1,2,3]/bin/refuge


To create package for your system run `make package` . For now we build
packages for OSX, Debian, Redhat & Solaris.
