#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./deps/*/ebin -pa ./apps/*/ebin -pa ./test/etap

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

% Test replication of documents with many leaf revisions.
% Motivated by COUCHDB-1340 and other similar issues where a document
% GET with a too long ?open_revs revision list doesn't work due to
% maximum web server limits for the HTTP request path.
%
%
-module(test_js).

-export([main/1]).

start_couch(Verbose) ->
    %% start couch
    test_util:start_couch(),
    %% set couch log level
    couch_config:set("log", "level", atom_to_list(Verbose), false),
    ok.

stop_couch() ->
    application:stop(os_mon),
    test_util:stop_couch().

restart_couch(Verbose) ->
    stop_couch(),
    start_couch(Verbose).

exec_loop(Port, Verbose, Acc) ->
    receive
        {Port, {data, {eol, "OK"}}} ->
            ok;
        {Port, {data, {eol, "restart"}}} ->
            restart_couch(Verbose),
            exec_loop(Port, Verbose, Acc);
        {Port, {data, {eol, Line}}} ->
            exec_loop(Port, Verbose, Acc ++ "\n" ++ Line);
        {Port, {data, {noeol, Verbose, Line}}} ->
            exec_loop(Port, Verbose, Acc ++ Line);
        {Port, {exit_status, _}} ->
            {error, Acc}
    end.

exec(Path, Verbose) ->
    COUCHJS = filename:join([test_util:builddir(), "apps", "couch", "priv",
                             "couchjs"]),
    CouchUri = filename:join([test_util:testdir(), "data", "couch.uri"]),
    Cmd = string:join([COUCHJS, "-H", "-u", CouchUri,
                       test_util:script_file("json2.js"),
                       test_util:script_file("sha1.js"),
                       test_util:script_file("oauth.js"),
                       test_util:script_file("couch.js"),
                       test_util:script_file("replicator_db_inc.js"),
                       test_util:script_file("couch_test_runner.js"),
                       test_util:js_test_file("couch_http.js"),
                       test_util:js_test_file("test_setup.js"),
                       Path,
                       test_util:js_test_file("cli_runner.js")], " "),

    PortSettings = [exit_status, {line, 16384}, use_stdio, stderr_to_stdout,
                    hide],

    io:format("~s ... testing~n", [filename:basename(Path)]),
    Port = open_port({spawn, Cmd}, PortSettings),
    Result = case exec_loop(Port, Verbose, "") of
        ok ->
            io:format("~s ... ok~n", [filename:basename(Path)]),
            ok;
        {error, Output} ->
            io:format("~s ... fail~n", [filename:basename(Path)]),
            io:format("javascript traceback:~n~s~n", [Output]),
            fail
    end,

    Result.

test(TestDir, Files, Verbose) ->
    start_couch(Verbose),
    timer:sleep(1000),

    io:format("==> run javascript tests.~n~n", []),
    {Failed, Success} = lists:foldl(fun(Name, {FAILs, OKs}) ->
                Path = filename:join([TestDir, Name]),
                Result = exec(Path, Verbose),
                case Result of
                    ok-> {FAILs, [{Name, ok} | OKs]};
                    _ -> {[{Name, fail} | FAILs], OKs}
                end

        end, {[], []}, Files),

    NFailed = length(Failed),
    NSuccess = length(Success),
    Count = NFailed + NSuccess,

    io:format("~n==> javascript tests results.~n~n", []),
    lists:foreach(fun({Name, Status}) ->
                io:format("~s ... ~s~n", [Name, Status])
        end, lists:usort(Failed ++ Success)),

    case NFailed of
        0 ->
            io:format("~nAll tests successful.~nTests: ~p~n", [Count]),
            halt(0);
        _ ->
            io:format("~n~p/~p tests failed~n", [NFailed, Count]),
            halt(1)
    end.

main([]) ->
    TestDir = filename:join([test_util:scriptdir(), "test"]),
    test(TestDir, filelib:wildcard("*.js", TestDir), none);
main(["-v", File | _]) ->
    Dir = filename:absname(filename:dirname(File)),
    test(Dir, [filename:basename(File)], info);
main(["-vv", File | _]) ->
    Dir = filename:absname(filename:dirname(File)),
    test(Dir, [filename:basename(File)], debug);
main([File |_]) ->
    Dir = filename:absname(filename:dirname(File)),
    test(Dir, [filename:basename(File)], none).
