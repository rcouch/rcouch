%%% -*- erlang -*-
%%%
%%% This file is part of refuge released under the Apache 2 license. 
%%% See the NOTICE for more information.


-module(refuge_common).

-export([get_version/0]).

get_version() ->
    Releases = release_handler:which_releases(),
    Version = case [V || {"refuge", V, _, current} <- Releases] of
    [] ->
        case [V || {"refuge", V, _, permanent} <- Releases] of
        [] ->
            "dev";
        [Permanent] ->
            Permanent
        end;
    [Current] ->
        Current
    end,
    list_to_binary(Version).
