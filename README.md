# rcouch

rcouch is a professional distribution of couchdb using rebar3. It is an
extraction of the refuge code to ease the distribution of couchdb.

Based on the couchdb code it is tagged from time to time for stability
purpose. At some point some supported branch may appear.

RCouch differs from refuge by only focusing on Apache CouchDB. It won't
contain any of the refuge features like P2P nodes handling, ... .
Internally we are using it to test couchdb and as an example on how to
embed couchdb in your own applications.

RCouch is brought you by the [Refuge project](http://refuge.io).


## Usage

Look at the [`rcouch`](http://docs.rcouch.org/en/latest/intro/api.html) module for the API usage.

Full documentation is available here:
https://docs.rcouch.org

main RCOUCH website is http://rcouch.org

## Features:

- Apache CouchDB based
- View Changes & view based replication
- Optimised for Mobile usage.
- Geospatial queries
- Easily customizable for your usage
- Fully opensource. All the sources are on refuge GIT repository
  (http://github.com/refuge) under Apache License 2.

## Requirements

- OS supported: Linux, OSX, BSDs (windows support is coming)
- Erlang R15
- Curl
- ICU
- Spidermonkey 1.8.5


## Installation

Installation is pretty simple. Just run the command line:

    $ make rel

and it will generate an rcouch release in `_build/default/rel/rcouch`. 

To Builds a compressed tar archive of release built of project, run the
following command line:

    $ make tar

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


## Ownership and License

The contributors are listed in AUTHORS. This project uses the Apache License 2
license, see LICENSE.

rcouch uses the [C4.1 (Collective Code Construction
Contract)](http://rfc.zeromq.org/spec:22) process for contributions.

## Development

Under C4.1 process, you are more than welcome to help us by:

* join the discussion over anything from design to code style try out
* and [submit issue reports](https://github.com/rcouch/rcouch/issues/new)
* or feature requests pick a task in
* [issues](https://github.com/rcouch/rcouch/issues) and get it done fork
* the repository and have your own fixes send us pull requests and even
* star this project ^_^

To  run the test suite:

```
    $ make test
```

## Packaging

RCOUCH allows you to buid a package on your platform by running the following
command line:

```
$ make package
```

Following packages systems are supported: RPM, DEB, Solaris, Freebsd, MacOSX
