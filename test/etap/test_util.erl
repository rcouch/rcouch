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

-module(test_util).

-export([init_code_path/0]).
-export([builddir/0, srcdir/0, depsdir/0, testdir/0, scriptdir/0]).
-export([source_file/1, build_file/1, test_file/1, config_files/0,
         script_file/1, js_test_file/1]).
-export([run/2]).
-export([request/3, request/4]).
-export([start_couch/0, start_couch/1, stop_couch/0]).

builddir() ->
    Current = filename:dirname(code:which(?MODULE)),
    filename:absname(filename:join([Current, "..", ".."])).

srcdir() ->
    filename:join([builddir(), "deps"]).

depsdir() ->
    filename:join([builddir(), "deps"]).

testdir() ->
    filename:join([builddir(), "test", "out"]).

scriptdir() ->
    filename:join([testdir(), "share", "www", "script"]).

source_file(Name) ->
    filename:join([srcdir(), Name]).

build_file(Name) ->
    filename:join([builddir(), Name]).

test_file(Name) ->
    filename:join([testdir(), Name]).

script_file(Name) ->
    filename:join([scriptdir(), Name]).

js_test_file(Name) ->
    filename:join([builddir(), "test", "javascript", Name]).


config_files() ->
    [
        filename:join([testdir(), "couch_test.ini"]),
        filename:join([testdir(), "local.ini"])
    ].

%%
%% Given a list of key value pairs, for each string value attempt to
%% render it using Dict as the context. Storing the result in Dict as Key.
%%
resolve_variables([], Dict) ->
    Dict;
resolve_variables([{Key, Value0} | Rest], Dict) when is_integer(Value0) ->
    Value = render(list_to_binary(integer_to_list(Value0)), Dict),
    resolve_variables(Rest, dict:store(Key, Value, Dict));
resolve_variables([{Key, Value0} | Rest], Dict) when is_list(Value0) ->
    Value = render(list_to_binary(Value0), Dict),
    resolve_variables(Rest, dict:store(Key, Value, Dict));
resolve_variables([{Key, {list, Dicts}} | Rest], Dict) when is_list(Dicts) ->
    %% just un-tag it so mustache can use it
    resolve_variables(Rest, dict:store(Key, Dicts, Dict));
resolve_variables([_Pair | Rest], Dict) ->
    resolve_variables(Rest, Dict).

%%
%% Render a binary to a string, using mustache and the specified context
%%

render(Bin, Context) ->
    %% Be sure to escape any double-quotes before rendering...
    ReOpts = [global, {return, list}],
    Str0 = re:replace(Bin, "\\\\", "\\\\\\", ReOpts),
    Str1 = re:replace(Str0, "\"", "\\\\\"", ReOpts),
    mustache:render(Str1, Context).


init_config() ->
    {ok, Vars} = file:consult(filename:join([builddir(), "test",
                                             "vars.config"])),

    Vars1 = resolve_variables(Vars, dict:from_list([{testdir, testdir()}])),

    %% create test config
    {ok, Bin} = file:read_file(filename:join([builddir(), "etc",
                                              "rcouch", "couch.ini"])),

    Rendered = render(Bin, Vars1),
    ok = file:write_file(filename:join([testdir(), "couch_test.ini"]),
                         Rendered),

    %% load config file path in the env
    IniFiles = config_files(),
    application:set_env(couch, config_files, IniFiles).

init_code_path() ->
    lists:foreach(fun(Name) ->
                code:add_patha(filename:join([depsdir(), Name, "ebin"]))
        end, filelib:wildcard("*", depsdir())),

    lists:foreach(fun(Name) ->
                code:add_patha(filename:join([srcdir(), Name, "ebin"]))
        end, filelib:wildcard("*", srcdir())),

    code:add_patha(filename:join([builddir(), "test", "etap"])),

    %% init config
    application:load(couch),
    init_config().


start_couch() ->
    start_couch(config_files()).

start_couch(IniFiles) ->
    ok = test_util:init_code_path(),

    application:start(syntax_tools),
    application:start(compiler),
    application:start(goldrush),
    application:start(lager),

    %% disable sasl
    application:load(sasl),
    application:set_env(sasl, errlog_type, error),
    application:set_env(sasl, sasl_error_logger, false),

    %% disable cpu and mem supervision in os_mon
    application:load(os_mon),
    application:set_env(os_mon, start_memsup, false),
    application:set_env(os_mon, start_cpu_sup, false),

    %% start couch
    application:load(couch),
    application:set_env(couch, config_files, IniFiles),
    couch_util:start_app_deps(couch),
    application:start(couch),

    %% start couch_httpd
    couch_util:start_app_deps(couch_httpd),
    application:start(couch_httpd),

    %% start couch_replicator
    couch_util:start_app_deps(couch_replicator),
    application:start(couch_replicator).

stop_couch() ->
    application:stop(couch_replicator),
    application:stop(couch_httpd),
    application:stop(couch).

run(Plan, Fun) ->
    test_util:init_code_path(),
    etap:plan(Plan),
    case (catch Fun()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally:~n~p", [Other])),
            timer:sleep(500),
            etap:bail(Other)
    end,
    ok.


request(Url, Headers, Method) ->
    request(Url, Headers, Method, []).

request(Url, Headers, Method, Body) ->
    request(Url, Headers, Method, Body, 3).

request(_Url, _Headers, _Method, _Body, 0) ->
    {error, request_failed};
request(Url, Headers, Method, Body, N) ->
    case code:is_loaded(ibrowse) of
    false ->
        {ok, _} = ibrowse:start();
    _ ->
        ok
    end,

    case ibrowse:send_req(Url, Headers, Method, Body) of
    {ok, Code0, RespHeaders, RespBody0} ->
        Code = list_to_integer(Code0),
        RespBody = iolist_to_binary(RespBody0),
        {ok, Code, RespHeaders, RespBody};
    {error, {'EXIT', {normal, _}}} ->
        % Connection closed right after a successful request that
        % used the same connection.
        request(Url, Headers, Method, Body, N - 1);
    Error ->
        Error
    end.
