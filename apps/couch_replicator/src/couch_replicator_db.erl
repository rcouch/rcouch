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
-module(couch_replicator_db).
-behavior(couch_mods).

-export([before_doc_update/2, after_doc_read/2]).

%% couch_mods funtion
-export([start/1, stop/0]).

-include_lib("couch/include/couch_db.hrl").

-define(OWNER, <<"owner">>).
-define(replace(L, K, V), lists:keystore(K, 1, L, {K, V})).

start(_) ->
    ReplicatorDb = couch_config:get("replicator", "db", "_replicator"),
    couch_hooks:add(before_doc_update, ReplicatorDb, ?MODULE, before_doc_update, 0),
    couch_hooks:add(after_doc_read, ReplicatorDb, ?MODULE, after_doc_read,  0),
    ok.

stop() ->
    UserDb = couch_config:get("replicator", "db", "_replicator"),
    couch_hooks:del(before_doc_update, UserDb, ?MODULE, before_doc_update, 0),
    couch_hooks:del(after_doc_read, UserDb, ?MODULE, after_doc_read,  0),
    ok.

before_doc_update(#doc{id = <<?DESIGN_DOC_PREFIX, _/binary>>} = Doc, _Db) ->
    Doc;
before_doc_update(#doc{body = {Body}} = Doc, #db{user_ctx=UserCtx} = Db) ->
    #user_ctx{roles = Roles, name = Name} = UserCtx,
    case lists:member(<<"_replicator">>, Roles) of
    true ->
        Doc;
    false ->
        case couch_util:get_value(?OWNER, Body) of
        undefined ->
            Doc#doc{body = {?replace(Body, ?OWNER, Name)}};
        Name ->
            Doc;
        Other ->
            case (catch couch_db:check_is_admin(Db)) of
            ok when Other =:= null ->
                Doc#doc{body = {?replace(Body, ?OWNER, Name)}};
            ok ->
                Doc;
            _ ->
                throw({forbidden, <<"Can't update replication documents",
                    " from other users.">>})
            end
        end
    end.

after_doc_read(#doc{id = <<?DESIGN_DOC_PREFIX, _/binary>>} = Doc, _Db) ->
    Doc;
after_doc_read(#doc{body = {Body}} = Doc, #db{user_ctx=UserCtx} = Db) ->
    #user_ctx{name = Name} = UserCtx,
    case (catch couch_db:check_is_admin(Db)) of
    ok ->
        Doc;
    _ ->
        case couch_util:get_value(?OWNER, Body) of
        Name ->
            Doc;
        _Other ->
            Source = strip_credentials(couch_util:get_value(<<"source">>, Body)),
            Target = strip_credentials(couch_util:get_value(<<"target">>, Body)),
            NewBody0 = ?replace(Body, <<"source">>, Source),
            NewBody = ?replace(NewBody0, <<"target">>, Target),
            #doc{revs = {Pos, [_ | Revs]}} = Doc,
            NewDoc = Doc#doc{body = {NewBody}, revs = {Pos - 1, Revs}},
            NewRevId = couch_db:new_revid(NewDoc),
            NewDoc#doc{revs = {Pos, [NewRevId | Revs]}}
        end
    end.

strip_credentials(undefined) ->
    undefined;
strip_credentials(Url) when is_binary(Url) ->
    re:replace(Url,
        "http(s)?://(?:[^:]+):[^@]+@(.*)$",
        "http\\1://\\2",
        [{return, binary}]);
strip_credentials({Props}) ->
    {lists:keydelete(<<"oauth">>, 1, Props)}.
