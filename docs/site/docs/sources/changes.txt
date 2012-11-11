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

.. _changes:

============
Changes Feed
============

Polling
=======

A list of changes made to documents in the database, in the order they were
made, can be obtained from the database's ``_changes`` resource. You can query
the ``_changes`` resource by issuing a ``GET`` request with the following
(optional) parameters:

+--------------+--------------------------------+---------------+--------------+
| Parameter    | Value                          | Default Value |  Notes       |
+==============+================================+===============+==============+
| since        | seqnum / now                   | 0             | \(1)         |
+--------------+--------------------------------+---------------+--------------+
| limit        | maxsequences                   | none          | \(2)         |
+--------------+--------------------------------+---------------+--------------+
| descending   | boolean                        | false         | \(3)         |
+--------------+--------------------------------+---------------+--------------+
| feed         | normal / longpoll / continuous | normal        | \(4)         |
+--------------+--------------------------------+---------------+--------------+
| heartbeat    | milliseconds                   | 60000         | \(5)         |
+--------------+--------------------------------+---------------+--------------+
| timeout      | milliseconds                   | 60000         | \(6)         |
+--------------+--------------------------------+---------------+--------------+
| filter       | designdoc/filtername / _view   | none          | \(7)         |
+--------------+--------------------------------+---------------+--------------+
| include_docs | boolean                        | false         | \(8)         |
+--------------+--------------------------------+---------------+--------------+
| style        | all_docs / main_only           | main_only     | \(9)         |
+--------------+--------------------------------+---------------+--------------+
| view         | designdoc/filtername           | none          | \(10)        |
+--------------+--------------------------------+---------------+--------------+

Notes:

(1) Start the results from the change immediately after the given sequence
    number.

(2) Limit number of result rows to the specified value (note that using 0 here
    has the same effect as 1).

(3) Return the change results in descending sequence order (most recent change
    first)

(4) Select the type of feed.

(5) Period in milliseconds after which an empty line is sent in the results.
    Only applicable for `longpoll` or `continuous` feeds. Overrides any timeout
    to keep the feed alive indefinitely.

(6) Maximum period in milliseconds to wait for a change before the response is
    sent, even if there are no results. Only applicable for `longpoll` or
    `continuous` feeds. Note that 60000 is also the default maximum timeout to
    prevent undetected dead connections.

    You can change the default maximum timeout in your ini-configuration:

    .. code-block:: ini

        [httpd]
        changes_timeout=#millisecs

(7) Reference a filter function from a design document to selectively get
    updates. See the `section in the book`_ for more information.

(8) Include the associated document with each result. If there are conflicts,
    only the winning revision is returned.

(9) Specifies how many revisions are returned in the changes array.
    The default, `main_only`, will only return the current "winning" revision;
    `all_docs` will return all leaf revisions (including conflicts and deleted
    former conflicts.)

(10) Allows to use view functions as filters. It requires to set ``filter``
     special value `_view` to enable this feature. Documents counted as "passed"
     for view filter in case if map function emits at least one record for them.

.. versionchanged:: 0.11.0 added ``include_docs`` parameter
.. versionchanged:: 1.2.0 added ``view`` parameter and special value `_view`
   for ``filter`` one
.. versionchanged:: 1.3.0 ``since`` parameter could take `now` value to start
   listen changes since current seq number.

By default all changes are immediately returned as a JSON object::

    GET /somedatabase/_changes HTTP/1.1

.. code-block:: javascript

    {"results":[
    {"seq":1,"id":"fresh","changes":[{"rev":"1-967a00dff5e02add41819138abb3284d"}]},
    {"seq":3,"id":"updated","changes":[{"rev":"2-7051cbe5c8faecd085a3fa619e6e6337"}]},
    {"seq":5,"id":"deleted","changes":[{"rev":"2-eec205a9d413992850a6e32678485900"}],"deleted":true}
    ],
    "last_seq":5}

``results`` is the list of changes in sequential order. New and changed
documents only differ in the value of the rev; deleted documents include the
``"deleted": true`` attribute. (In the ``style=all_docs mode``, deleted applies
only to the current/winning revision. The other revisions listed might be
deleted even if there is no deleted property; you have to ``GET`` them
individually to make sure.)

``last_seq`` is the sequence number of the last update returned. (Currently it
will always be the same as the seq of the last item in results.)

Sending a ``since`` param in the query string skips all changes up to and
including the given sequence number::

    GET /somedatabase/_changes?since=3 HTTP/1.1

.. code-block:: javascript

    {"results":[
    {"seq":5,"id":"deleted","changes":[{"rev":"2-eec205a9d413992850a6e32678485900"}],"deleted":true}
    ],
    "last_seq":5} 

Long Polling
============

The `longpoll` feed (probably most useful used from a browser) is a more
efficient form of polling that waits for a change to occur before the response
is sent. `longpoll` avoids the need to frequently poll CouchDB to discover
nothing has changed!

The response is basically the same JSON as is sent for the normal feed.

A timeout limits the maximum length of time the connection is open. If there
are no changes before the timeout expires the response's results will be an
empty list.  

Continuous
==========

Polling the CouchDB server is not a good thing to do. Setting up new HTTP
connections just to tell the client that nothing happened puts unnecessary
strain on CouchDB.

A continuous feed stays open and connected to the database until explicitly
closed and changes are sent to the client as they happen, i.e. in near
real-time.

The continuous feed's response is a little different than the other feed types
to simplify the job of the client - each line of the response is either empty
or a JSON object representing a single change, as found in the normal feed's
results.

.. code-block:: text

    GET /somedatabase/_changes?feed=continuous HTTP/1.1

.. code-block:: javascript

    {"seq":1,"id":"fresh","changes":[{"rev":"1-967a00dff5e02add41819138abb3284d"}]}
    {"seq":3,"id":"updated","changes":[{"rev":"2-7051cbe5c8faecd085a3fa619e6e6337"}]}
    {"seq":5,"id":"deleted","changes":[{"rev":"2-eec205a9d413992850a6e32678485900"}],"deleted":true}
    ... tum tee tum ...
    {"seq":6,"id":"updated","changes":[{"rev":"3-825cb35de44c433bfb2df415563a19de"}]}

Obviously, `... tum tee tum ...` does not appear in the actual response, but
represents a long pause before the change with seq 6 occurred.  

Filters
=======

Classic filters
---------------

By default changes feed emits all database documents changes. But if you're
waiting for some special changes it's not optimal to process each record.

Filters are special design document functions that allows changes feed to emit
only specific documents that passed filter rules.

Let assume that our database is mailbox and we need to listen changes to handle
only new mails (documents with status `new`). Assuming that, our filter function
would looks like next one:

.. code-block:: javascript

  function(doc, req){
    // we need only `mail` documents
    if (doc.type != 'mail'){
      return false;
    }
    // we're interested only in `new` ones
    if (doc.status != 'new'){
      return false;
    }
    return true; // passed!
  }
 
Filter function must return true in fact if document passed all defined rules.
Now, if you apply this function to changes feed, you're changes feed will emit
only changes about "new mail"::


    GET /somedatabase/_changes?filter=mailbox/new_mail HTTP/1.1

.. code-block:: javascript

    {"results":[
    {"seq":1,"id":"df8eca9da37dade42ee4d7aa3401f1dd","changes":[{"rev":"1-c2e0085a21d34fa1cecb6dc26a4ae657"}]},
    {"seq":7,"id":"df8eca9da37dade42ee4d7aa34024714","changes":[{"rev":"1-29d748a6e87b43db967fe338bcb08d74"}]},
    ],
    "last_seq":27}

Note, that ``last_seq`` number is 27, but we'd received only two records.
Seems like any other changes was about documents, that hadn't passed our filter.

Probably, we also need to filter changes feed of our mailbox not only by single
status value: we also interested in statuses like "spam" to update spam-filter
heuristic rules, "outgoing" to let mail daemon actually send mails and so on.
Creating a lot of similar functions that actually does same work isn't good
idea - so we need dynamic filter to go.

If you have noted, filter functions takes second argument as
:ref:`request <request_object>` object - it allows to create dynamic filters
based on query parameters, :ref:`user context <userctx_object>` and more.

The dynamic version of our filter now will be next:

.. code-block:: javascript

  function(doc, req){
    // we need only `mail` documents
    if (doc.type != 'mail'){
      return false;
    }
    // we're interested only in requested status
    if (doc.status != req.query.status){
      return false;
    }
    return true; // passed!
  }

and now we have pass `status` query parameter in request to let filter match
only required documents::

    GET /somedatabase/_changes?filter=mailbox/by_status&status=new HTTP/1.1

.. code-block:: javascript

    {"results":[
    {"seq":1,"id":"df8eca9da37dade42ee4d7aa3401f1dd","changes":[{"rev":"1-c2e0085a21d34fa1cecb6dc26a4ae657"}]},
    {"seq":7,"id":"df8eca9da37dade42ee4d7aa34024714","changes":[{"rev":"1-29d748a6e87b43db967fe338bcb08d74"}]},
    ],
    "last_seq":27}

and we can change filter behavior with easy::

    GET /somedatabase/_changes?filter=mailbox/by_status&status=spam HTTP/1.1

.. code-block:: javascript

    {"results":[
    {"seq":11,"id":"8960e91220798fc9f9d29d24ed612e0d","changes":[{"rev":"3-cc6ff71af716ddc2ba114967025c0ee0"}]},
    ],
    "last_seq":27}


Combining filters with `continuous` feed allows to create powerful event-driven
systems.

View filters
------------

View filters are the same as classic one with one small difference: they used
view map function instead to filter changes feed. Each time a value could be
emitted, a change is returned. This allows to avoid creating filter functions
that are mostly does same works as views.

To use them just specify `_view` value for ``filter`` parameter and
`designdoc/viewname` for ``view`` one::

    GET /somedatabase/_changes?filter=_view&view=dname/viewname  HTTP/1.1

.. note:: Since view filters are uses map function as filter they couldn't have
   dynamic behavior since :ref:`request object<request_object>` is not available

.. _section in the book: http://books.couchdb.org/relax/reference/change-notifications
