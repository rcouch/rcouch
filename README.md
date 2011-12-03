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
