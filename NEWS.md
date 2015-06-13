# RCOUCH Changelog

## 2.0.0-rc1 / 2015/06/13

- build is now using [rebar3](http://rebar3.org) instead of rebar
- new code to improve the view changes in `couch_mrview`
- build is now using shared libraries on the system to make the release
  smaller an deasier to maintain

## 1.1.0 / 2015/02/04

- add `make package` target to build packages on your platform
- couch_httpd 1.1: paginate databases
- couch 1.6.4:
  - fix couchjs build under centos 7
  - remove couch_plugin_event handler which was using too much CPU for nothing.

### BREAKING CHANGES

- rebar is now not included in the release
- By default view_index_dir and database_dir settings in couch.ini are now identical.

## 1.0.3 / 2015/01/12

- improve defaults settings to handle more concurrency

## 1.0.2 / 2015/01/08

- fix view changes timeout issue with large databases.

## 1.0.1 / 2015/01/06

- make the possibility to set the run_dir via a template

## 1.0.0 / 2014/12/14

This is the first stable and long term supported release of rcouch.

### Changes

- Couchdb compatibility
 - Compatible with Apache COUCHDB 1.6.0
- HTTP Interface
  - bulk_get API support.
- Views
 - Major improvements in view changes indexing
 - Possibility to include deleted docs
 - Handle deleted keys in changes
- Replicator
 - Support Replication using a view
 - Fix Replication freezes (improve pooling)
- Core
 - Get ride of max_dbs settings
 - Create databases synchronously
 - UTF8 collation is using a ucol_nif
 - Add validate_doc_read property to design documents
 - JSON encoding using Jiffy
 - Major improvements in supervision tree
- Logging
 - Usage of Lager for logging
- Extensions
 - Support random doc fetching using the _random_doc handler
 - Support Geocouch 1.3, a spatial indexer
