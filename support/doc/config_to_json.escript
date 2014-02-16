#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./src/deps/jiffy/ebin -pa .


%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. You may obtain a copy of
%% the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations under
%% the License.
%%
%%
%% @doc simple erlang script to export an erlang config file to json in the
%% console.

-module(config_to_json).

-export([main/1]).

to_binary({list, List}) ->
    lists:foldl(fun(Prop, Acc) ->
                Obj = {[{to_binary(K), to_binary(V)} || {K, V} <- Prop]},
                [Obj | Acc]
        end, [], List);
to_binary(V) when is_list(V) ->
    list_to_binary(V);
to_binary(V) when is_atom(V) ->
    list_to_binary(atom_to_list(V));
to_binary(V) ->
    V.

main([]) ->
    {error, missing_file};
main([Path | _]) ->
    {ok, Cfg} = file:consult(Path),
    Obj = {[{to_binary(K), to_binary(V)} || {K, V} <- Cfg]},
    io:format("~s", [jiffy:encode(Obj)]).
