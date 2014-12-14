.. Licensed under the Apache License, Version 2.0 (the "License"); you may not
.. use this file except in compliance with the License. You may obtain a copy of
.. the License at
..
..   http://www.apache.org/licenses/LICENSE-2.0
..
.. Unless required by applicable law or agreed to in writing, software
.. distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
.. WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
.. License for the specific language governing permissions and limitations under
.. the License.


.. _install:

===========================================
Installation of RCOUCH on Unix-like systems
===========================================

Requirements
------------

- OS supported: Linux, OSX, BSDs (windows support is coming)
- Erlang R15 or R16 (during build)
- Curl
- Git
- Zip (during build)
- ICU (if not built statically)
- Latest version of rebar_) installed on
  your system.

.. _rebar: http://github.com/basho/rebar

Installation
------------

Installation is pretty simple. Just run the command line::

    $ make 
    $ make rel

and it will generate a couch folder in rel/couch. This release is
fully relocatable, so you can put it where you want on your system.

To create package for your system run ``make package`` . For now we build
packages for OSX, Debian, Redhat & Solaris.

##Notes on building a truly distributable package

The package built above will still depend on some libraries from your
system, so additional work has to be done to distribute it to
older/newer systems.

1. CouchDB will depend on the ICU library version that was present in
   your system at build time. To easily bundle this library with the
   package, build with::

         $ make rel USE_STATIC_ICU=1

1. Check whether your package depends on Ncurses::

         $ ldd ./rel/rcouch/erts-*/bin/erlexec|grep ncurses


    If it does, copy the .so file to ./rel/myapp/lib/ or rebuild Erlang
    without this dependency.

1. Decide whether you need SSL support in your package and check whether it
   depends on OpenSSL::

         $ ldd ./rel/rcouch/lib/ssl-*/priv/bin/ssl_esock|grep 'libcrypto\|libssl'

    If it does, copy the .so file to ./rel/rcouch/lib/ or rebuild Erlang
    without this dependency.

If you copied any .so files in the last 2 steps, run this command, so
that your app can find the libraries::

    $ sed -i '/^RUNNER_USER=/a\\nexport LD_LIBRARY_PATH="$RUNNER_BASE_DIR/lib"' ./rel/rcouch/bin/rcouch


Binding port 80
---------------

On most UNIX systems binding port 80 is a privileged operation (requires
root). Running Erlang as root is not recommended so some configuration
will need to be done so that rcouch can bind port 80.

If you run a recent Linux kernel with capabilities you can give Erlang
the privilege using the setcap command (you may need to install a
package named lxc or similar to obtain this command)::

    $ setcap 'cap_net_bind_service=+ep' /path/to/rel/refuge/erts-5.8.5/bin/beam`
    $ setcap 'cap_net_bind_service=+ep' /path/to/rel/refuge/erts-5.8.5/bin/beam.smp

On FreeBSD all ports can be made accessible to all users by issuing::

   $ sysctl net.inet.ip.portrange.reservedhigh=0
