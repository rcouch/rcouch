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

-export([handle_req/2]).

-include_lib("couch/include/couch_db.hrl").


handle_req(#httpd{method='GET'}=Req, Db) ->
    Opts = parse_query(Req),
    JsonObj = case couch_randomdoc:random_doc(Db) of
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
    lists:foldl(fun({Key,Value}, Options) ->
                case {Key, Value} of
                    {"attachments", "true"} ->
                        [attachments | Options];
                    {"meta", "true"} ->
                        [revs_info, conflicts, deleted_conflicts | Options];
                    {"revs", "true"} ->
                        [revs | Options];
                    {"local_seq", "true"} ->
                        [local_seq | Options];
                    {"revs_info", "true"} ->
                        [revs_info | Options];
                    {"conflicts", "true"} ->
                        [conflicts | Options];
                    {"deleted_conflicts", "true"} ->
                        [deleted_conflicts | Options];
                    {"latest", "true"} ->
                        [latest | Options];
                    {"att_encoding_info", "true"} ->
                        [att_encoding_info | Options];
                _Else -> % unknown key value pair, ignore.
                    Options
            end
    end, [], couch_httpd:qs(Req)).
