%%% -*- erlang -*-
%%%
%%% This file is part of refuge released under the Apache 2 license. 
%%% See the NOTICE for more information.

-module(refuge_common_httpd).

-include_lib("couch/include/couch_db.hrl").

-export([couch_welcome_req/1, couch_welcome_req/2]).

%% @doc custom couchdb welcome on /
couch_welcome_req(Req) ->
    couch_welcome_req(Req, <<"Welcome">>).

couch_welcome_req(#httpd{method='GET'}=Req, WelcomeMessage) ->
    couch_httpd:send_json(Req, {[
        {couchdb, WelcomeMessage},
        {version, list_to_binary(couch:version())},
        {refuge_version, refuge_common:get_version()}
    ]});
couch_welcome_req(Req, _) ->
    couch_httpd:send_method_not_allowed(Req, "GET,HEAD").


