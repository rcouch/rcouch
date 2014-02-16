#!/usr/bin/env escript
%% -*- erlang -*-

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

main(_) ->
    etap:plan(unknown),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            Msg = io_lib:format("Test died abnormally: ~p", [Other]),
            etap:diag(Msg),
            etap:bail(Msg)
        end,
    ok.

loop(0) ->
    ok;
loop(N) ->
    ok = cycle(),
    loop(N - 1).

cycle() ->
    ok = couch_server:delete(<<"etap-test-db">>, []),
    {ok, _Db} = couch_db:create(<<"etap-test-db">>, []),
    ok.

test() ->
    test_util:start_couch(),

    {ok, _Db} = couch_db:create(<<"etap-test-db">>, []),

    ok = loop(1),
    ok = loop(10),
    ok = loop(100),
    ok = loop(1000),

    % for more thorough testing:
    % ok = loop(10000),
    % ok = loop(100000),
    % ok = loop(1000000),
    % ok = loop(10000000),

    ok = couch_server:delete(<<"etap-test-db">>, []),

    etap:is(true, true, "lots of creating and deleting of a database"),
    ok.
