% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.


-module(couch_randomdoc_httpd).

-export([handle_req/2, parse_query/1]).

-include_lib("couch/include/couch_db.hrl").
-include("include/couch_randomdoc.hrl").

handle_req(#httpd{method='GET'}=Req, Db) ->
    #random_query{options = Opts, prefix=Prefix} =parse_query(Req),
    JsonObj = case couch_randomdoc:random_doc(Db, Prefix) of
        {ok, Doc} ->
            couch_doc:to_json_obj(Doc, Opts);
        _Else ->
            {[]}
    end,
    couch_httpd:send_json(Req, 200, JsonObj);

handle_req(Req, _Db) ->
    couch_httpd:send_method_not_allowed(Req, "GET").


%% internal
parse_query(Req) ->
    lists:foldl(fun({Key,Value}, Args) ->
        case {Key, Value} of
        {"attachments", "true"} ->
            Options = [attachments | Args#random_query.options],
            Args#random_query{options=Options};
        {"meta", "true"} ->
            Options = [revs_info, conflicts, deleted_conflicts | Args#random_query.options],
            Args#random_query{options=Options};
        {"revs", "true"} ->
            Options = [revs | Args#random_query.options],
            Args#random_query{options=Options};
        {"local_seq", "true"} ->
            Options = [local_seq | Args#random_query.options],
            Args#random_query{options=Options};
        {"revs_info", "true"} ->
            Options = [revs_info | Args#random_query.options],
            Args#random_query{options=Options};
        {"conflicts", "true"} ->
            Options = [conflicts | Args#random_query.options],
            Args#random_query{options=Options};
        {"deleted_conflicts", "true"} ->
            Options = [deleted_conflicts | Args#random_query.options],
            Args#random_query{options=Options};
        {"latest", "true"} ->
            Options = [latest | Args#random_query.options],
            Args#random_query{options=Options};
        {"att_encoding_info", "true"} ->
            Options = [att_encoding_info | Args#random_query.options],
            Args#random_query{options=Options};
        {"prefix", Prefix} ->
            Args#random_query{prefix=?l2b(Prefix)};
        _Else -> % unknown key value pair, ignore.
            Args
        end
    end, #random_query{}, couch_httpd:qs(Req)).
