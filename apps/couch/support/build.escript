%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%%%
%%% This file is part of couch_core released under the Apache 2 license. 
%%% See the NOTICE for more information.


main([]) ->
    case os:type() of
        {unix, darwin} ->
            build_darwin();
        {unix, _} ->
            build_unix();
        _ ->
            build_win()
    end;

main(["clean"|_]) ->
    case os:type() of
        {unix, darwin} ->
            clean_darwin();
        {unix, _} ->
            clean_unix();
        _ ->
            clean_win()
    end;
main(_) ->
    io:format("Operation not supported", []),
    erlang:halt(1).


build_darwin() ->
    Env = setup_env(),

    %% build static libs@
    sh(rootdir() ++ "/support/build_libs.sh", Env),
    
    %% make couchjs, icu driver and nifs
    MakeArgs = case is_arch("R14") of
        true -> " all";
        false -> ""
    end,
    io:format("==> couchjs, couch_collate (compile)~n", []),
    sh("make -f c_src/Makefile.osx" ++ MakeArgs, Env),
    erlang:halt(0).



build_unix() ->
    Env = setup_env(),

    %% build static libs@
    sh(rootdir() ++ "/support/build_libs.sh", Env),
    
    %% make couchjs, icu driver and nifs
    MakeArgs = case is_arch("R14") of
        true -> " all";
        false -> ""
    end,
    io:format("==> couchjs, couch_collate (compile)~n", []),
    sh("make -f c_src/Makefile.unix" ++ MakeArgs, Env),
    erlang:halt(0).

clean_darwin() ->
    Env = setup_env(),

    %% clean couchjs, icu driver and nifs
    io:format("==> couchjs, couch_collate (clean)~n", []),
    io:format("[INFO] To clean static libs run the command" ++
        "'cd " ++ rootdir() ++ " && ./support/build_libs.sh clean' .~n", []),

    sh("make -f c_src/Makefile.osx clean", Env),
    erlang:halt(0).


clean_unix() ->
    Env = setup_env(),

    %% clean couchjs, icu driver and nifs
    io:format("==> couchjs, couch_collate (clean)~n", []),
    io:format("[INFO] To clean static libs run the command" ++
        "'cd " ++ rootdir() ++ " && ./support/build_libs.sh clean' .~n", []),

    sh("make -f c_src/Makefile.unix clean", Env),
    erlang:halt(0).

build_win() ->
    io:format("Windows platform isn't supported yet", []),
    erlang:halt(1).

clean_win() ->
    io:format("Windows platform isn't supported yet", []),
    erlang:halt(1).



setup_env() ->
    DefaultEnv = filter_envs(default_env(), []),
    RawEnv = apply_defaults(os_env(), DefaultEnv),
    expand_vars_loop(merge_each_var(RawEnv, [])).


%% ===================================================================
%% Internal functions
%% ===================================================================

sh(Command, Env) ->
    PortSettings = [exit_status, {line, 16384}, use_stdio,
        stderr_to_stdout, hide, {env, Env}],

    Port = open_port({spawn, Command}, PortSettings),
    sh_loop(Port).

sh_loop(Port) ->
    receive
        {Port, {data, {_, "_port_cmd_status_ " ++ Status}}} ->
            (catch erlang:port_close(Port)), % sigh () for indentation
            case list_to_integer(Status) of
                0  -> 
                    ok;
                Rc -> 
                    io:format("error, ~p~n", [Rc]),
                    erlang:halt(1)
            end;
        {Port, {data, {eol, Line}}} ->
            io:format("~s~n", [Line]),
            sh_loop(Port);
        {Port, {data, {noeol, Line}}} ->
            io:format("~s", [Line]),
            sh_loop(Port);
        {Port, {exit_status, 0}} ->
            ok;
        {Port, {exit_status, Rc}} ->
            io:format("error, ~p~n", [Rc]),
            erlang:halt(1)
    end.


rootdir() ->
    {ok, Dir} = file:get_cwd(),
    Dir.

libsdir() ->
    filename:join(rootdir(), "libs").

is_arch(ArchRegex) ->
    case re:run(get_arch(), ArchRegex, [{capture, none}]) of
        match ->
            true;
        nomatch ->
            false
    end.

get_arch() ->
    Words = wordsize(),
    erlang:system_info(otp_release) ++ "-"
        ++ erlang:system_info(system_architecture) ++ "-" ++ Words.

os_env() ->
    Os = [list_to_tuple(re:split(S, "=", [{return, list}, {parts, 2}])) ||
             S <- os:getenv()],
    %% Drop variables without a name (win32)
    [T1 || {K, _V} = T1 <- Os, K =/= []].

wordsize() ->
    try erlang:system_info({wordsize, external}) of
        Val ->
            integer_to_list(8 * Val)
    catch
        error:badarg ->
            integer_to_list(8 * erlang:system_info(wordsize))
    end.
 

%%
%% Given a string, determine if it is expandable
%%
is_expandable(InStr) ->
    case re:run(InStr,"\\\$",[{capture,none}]) of
        match -> true;
        nomatch -> false
    end.

%%
%% Give a unique list of {Key, Value} environment variables, expand each one
%% for every other key until no further expansions are possible.
%%
expand_vars_loop(Vars) ->
    expand_vars_loop(Vars, 10).

expand_vars_loop(_, 0) ->
    io:format("Max. expansion reached for ENV vars!\n", []),
    halt(1);
expand_vars_loop(Vars0, Count) ->
    Vars = lists:foldl(fun({Key, Value}, Acc) ->
                               expand_vars(Key, Value, Acc)
                       end,
                       Vars0, Vars0),
    case orddict:from_list(Vars) of
        Vars0 ->
            Vars0;
        _ ->
            expand_vars_loop(Vars, Count-1)
    end.

%%
%% Expand all OTHER references to a given K/V pair
%%
expand_vars(Key, Value, Vars) ->
    lists:foldl(
      fun({AKey, AValue}, Acc) ->
              NewValue = case AKey of
                             Key ->
                                 AValue;
                             _ ->
                                 expand_env_variable(AValue, Key, Value)
                         end,
              [{AKey, NewValue} | Acc]
      end,
      [], Vars).

%%
%% Given env. variable FOO we want to expand all references to
%% it in InStr. References can have two forms: $FOO and ${FOO}
%% The end of form $FOO is delimited with whitespace or eol
%%
expand_env_variable(InStr, VarName, VarValue) ->
    R1 = re:replace(InStr, "\\\$" ++ VarName ++ "\\s", VarValue ++ " ",
                    [global]),
    R2 = re:replace(R1, "\\\$" ++ VarName ++ "\$", VarValue),
    re:replace(R2, "\\\${" ++ VarName ++ "}", VarValue,
               [global, {return, list}]).


%%
%% Given a list of {Key, Value} variables, and another list of default
%% {Key, Value} variables, return a merged list where the rule is if the
%% default is expandable expand it with the value of the variable list,
%% otherwise just return the value of the variable.
%%
apply_defaults(Vars, Defaults) ->
    dict:to_list(
      dict:merge(fun(Key, VarValue, DefaultValue) ->
                         case is_expandable(DefaultValue) of
                             true ->
                                 expand_env_variable(DefaultValue,
                                                     Key, VarValue);
                             false -> VarValue
                         end
                 end,
                 dict:from_list(Vars),
                 dict:from_list(Defaults))).

%%
%% Given a list of {Key, Value} environment variables, where Key may be defined
%% multiple times, walk the list and expand each self-reference so that we
%% end with a list of each variable singly-defined.
%%
merge_each_var([], Vars) ->
    Vars;
merge_each_var([{Key, Value} | Rest], Vars) ->
    Evalue = case orddict:find(Key, Vars) of
                 error ->
                     %% Nothing yet defined for this key/value.
                     %% Expand any self-references as blank.
                     expand_env_variable(Value, Key, "");
                 {ok, Value0} ->
                     %% Use previous definition in expansion
                     expand_env_variable(Value, Key, Value0)
             end,
    merge_each_var(Rest, orddict:store(Key, Evalue, Vars)).

%%
%% Filter a list of env vars such that only those which match the provided
%% architecture regex (or do not have a regex) are returned.
%%
filter_envs([], Acc) ->
    lists:reverse(Acc);
filter_envs([{ArchRegex, Key, Value} | Rest], Acc) ->
    case is_arch(ArchRegex) of
        true ->
            filter_envs(Rest, [{Key, Value} | Acc]);
        false ->
            filter_envs(Rest, Acc)
    end;
filter_envs([{Key, Value} | Rest], Acc) ->
    filter_envs(Rest, [{Key, Value} | Acc]).

default_env() ->
    [
     {"STATICLIBS", libsdir()},
     {"ROOTDIR", rootdir()},

     {"JS_CFLAGS", lists:concat(["-fPIC -Wall -Os",
                                " -I" ++ libsdir() ++ "/js/include",
                                " -DXP_UNIX"])},

     {"JS_LIBS", lists:concat([libsdir() ++ "/js/lib/libjs_static.a ",
                                libsdir() ++ "/nsprpub/lib/libnspr4.a",
                                " -lstdc++",
                                " "])},

     {"ICU_CFLAGS", lists:concat(["$DRV_CFLAGS"
                                " -I" ++ libsdir() ++ "/icu/include"])},

     {"ICU_LIBS", lists:concat(["$DRV_LDFLAGS"
                                " " ++ libsdir() ++ "/icu/lib/libicui18n.a",
                                " " ++ libsdir() ++ "/icu/lib/libicuuc.a",
                                " " ++ libsdir() ++ "/icu/lib/libicudata.a",
                                " -lstdc++"])},

     {"linux", "JS_LIBS", lists:concat([libsdir() ++ "/js/lib/libjs_static.a ",
                               libsdir() ++ "/nsprpub/lib/libnspr4.a",
                               " -lstdc++ -lpthread",
                               " "])}
    ].
