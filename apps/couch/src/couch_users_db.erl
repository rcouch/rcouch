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

-module(couch_users_db).
-behaviour(couch_mods).

%% couch_mods funtion
-export([start/1, stop/0]).
-export([config_changes/2]).

-export([before_doc_update/2, after_doc_read/2, strip_non_public_fields/1]).

-include("couch_db.hrl").

-define(NAME, <<"name">>).
-define(PASSWORD, <<"password">>).
-define(DERIVED_KEY, <<"derived_key">>).
-define(PASSWORD_SCHEME, <<"password_scheme">>).
-define(PBKDF2, <<"pbkdf2">>).
-define(ITERATIONS, <<"iterations">>).
-define(SALT, <<"salt">>).
-define(replace(L, K, V), lists:keystore(K, 1, L, {K, V})).

start(_) ->
    UserDb = couch_config:get("couch_httpd_auth", "authentication_db", "_users"),
    %% cache the user db to reuse it on stop during a config change.
    application:set_env(couch, users_db, UserDb),
    couch_hooks:add(before_doc_update, UserDb, ?MODULE, before_doc_update, 0),
    couch_hooks:add(after_doc_read, UserDb, ?MODULE, after_doc_read,  0),
    %% register for config changes
    couch_config:register(fun ?MODULE:config_changes/2),
    ok.

stop() ->
    case application:get_env(couch, users_db) of
        {ok, UserDb} ->
            application:unset_env(couch, users_db),
            couch_hooks:delete(before_doc_update, UserDb, ?MODULE,
                               before_doc_update, 0),
            couch_hooks:delete(after_doc_read, UserDb, ?MODULE,
                               after_doc_read, 0);
        _ ->
            ok
    end,
    ok.

restart() ->
    case application:get_env(couch, users_db) of
        {ok, OldUserDb} ->
            couch_hooks:delete(before_doc_update, OldUserDb, ?MODULE,
                               before_doc_update, 0),
            couch_hooks:delete(after_doc_read, OldUserDb, ?MODULE,
                               after_doc_read,  0);
        _ ->
            ok
    end,
    UserDb = couch_config:get("couch_httpd_auth", "authentication_db", "_users"),
    application:set_env(couch, users_db, UserDb),
    couch_hooks:add(before_doc_update, UserDb, ?MODULE, before_doc_update, 0),
    couch_hooks:add(after_doc_read, UserDb, ?MODULE, after_doc_read,  0).


config_changes("couch_httpd_auth", "authentication_db") ->
    restart();
config_changes(_,_) ->
    ok.

% If the request's userCtx identifies an admin
%   -> save_doc (see below)
%
% If the request's userCtx.name is null:
%   -> save_doc
%   // this is an anonymous user registering a new document
%   // in case a user doc with the same id already exists, the anonymous
%   // user will get a regular doc update conflict.
% If the request's userCtx.name doesn't match the doc's name
%   -> 404 // Not Found
% Else
%   -> save_doc
before_doc_update(Doc, #db{user_ctx = UserCtx} = Db) ->
    couch_log:info("we are here in user ibefore doc update yes. ~n", []),
    #user_ctx{name=Name} = UserCtx,
    DocName = get_doc_name(Doc),
    case (catch couch_db:check_is_admin(Db)) of
    ok ->
        save_doc(Doc);
    _ when Name =:= DocName orelse Name =:= null ->
        save_doc(Doc);
    _ ->
        throw(not_found)
    end.

% If newDoc.password == null || newDoc.password == undefined:
%   ->
%   noop
% Else -> // calculate password hash server side
%    newDoc.password_sha = hash_pw(newDoc.password + salt)
%    newDoc.salt = salt
%    newDoc.password = null
save_doc(#doc{body={Body}} = Doc) ->
    couch_log:info("we are updating user before doc update yes. ~n", []),

    case couch_util:get_value(?PASSWORD, Body) of
    null -> % server admins don't have a user-db password entry
        Doc;
    undefined ->
        Doc;
    ClearPassword ->
        Iterations = list_to_integer(
                       couch_config:get("couch_httpd_auth", "iterations", "1000")
        ),
        Salt = couch_uuids:random(),
        DerivedKey = couch_passwords:pbkdf2(ClearPassword, Salt, Iterations),
        Body0 = [{?PASSWORD_SCHEME, ?PBKDF2}, {?ITERATIONS, Iterations}|Body],
        Body1 = ?replace(Body0, ?DERIVED_KEY, DerivedKey),
        Body2 = ?replace(Body1, ?SALT, Salt),
        Body3 = proplists:delete(?PASSWORD, Body2),
        Doc#doc{body={Body3}}
    end.

% If the doc is a design doc
%   If the request's userCtx identifies an admin
%     -> return doc
%   Else
%     -> 403 // Forbidden
% If the request's userCtx identifies an admin
%   -> return doc
% If the request's userCtx.name doesn't match the doc's name
%   -> 404 // Not Found
% Else
%   -> return doc
after_doc_read(#doc{id = <<?DESIGN_DOC_PREFIX, _/binary>>} = Doc, Db) ->
    case (catch couch_db:check_is_admin(Db)) of
    ok ->
        Doc;
    _ ->
        throw({forbidden,
        <<"Only administrators can view design docs in the users database.">>})
    end;
after_doc_read(Doc, #db{user_ctx = UserCtx} = Db) ->
    #user_ctx{name=Name} = UserCtx,
    DocName = get_doc_name(Doc),
    couch_log:info("we are here in user after_doc_read yes. ~n", []),
    case (catch couch_db:check_is_admin(Db)) of
    ok ->
        Doc;
    _ when Name =:= DocName ->
        Doc;
    _ ->
        Doc1 = strip_non_public_fields(Doc),
        case Doc1 of
          #doc{body={[]}} ->
              throw(not_found);
          _ ->
              Doc1
        end
    end.

get_doc_name(#doc{id= <<"org.couchdb.user:", Name/binary>>}) ->
    Name;
get_doc_name(_) ->
    undefined.

strip_non_public_fields(#doc{body={Props}}=Doc) ->
    Public = re:split(couch_config:get("couch_httpd_auth", "public_fields", ""),
                      "\\s*,\\s*", [{return, binary}]),
    Doc#doc{body={[{K, V} || {K, V} <- Props, lists:member(K, Public)]}}.
