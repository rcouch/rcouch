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
%
%% simple helper to start/stop some modules inside couch to setup hooks
%% callback.

-module(couch_mods).
-behaviour(gen_server).

%% PUBLIC API
-export([start_module/2,  stop_module/1]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, code_change/3, handle_info/2,
         terminate/2]).

-record(couch_mod, {mod,
                    opts}).

-define(COUCH_MODS, ?MODULE).

%% behaviour definition
-callback start(Opts :: list()) -> any().
-callback stop() -> any().

start_module(Mod, Opts) ->
    try
        Mod:start(Opts),
        ets:insert(?COUCH_MODS, #couch_mod{mod=Mod, opts=Opts})
    catch
        Class:Reason ->
            couch_log:error("Problem starting the module ~p "
                            "with the options ~p~n, ~p: ~p~n~p~n",
                            [Mod, Opts, Class, Reason,
                             erlang:get_stacktrace()]),
            erlang:raise(Class, Reason, erlang:get_stack_trace())
    end.

stop_module(Mod) ->
    case catch Mod:stop() of
        {wait, Pids} when is_list(Pids) ->
            lists:foreach(fun wait_for_proc/1, Pids);
        {wait, Pid} when is_pid(Pid) ->
            wait_for_proc(Pid);
        _ ->
            ok
    end,
    ok.

start_link() ->
    gen_server:start_link(?MODULE, [], []).


init(_) ->
    ets:new(?COUCH_MODS, [named_table, public, {keypos, #couch_mod.mod}]),
    %% start modules
    self() ! start_modules,
    {ok, nil}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start_modules, State) ->
    ok = start_modules(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    stop_modules(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



start_modules() ->
    case application:get_env(couch, mods) of
        undefined -> ok;
        {ok, Mods} ->
            %% start all modules from the config
            lists:foreach(fun({Mod, Opts}) ->
                                  couch_log:info("start module ~p~n", [Mod]),
                                  start_module(Mod, Opts)
                          end, Mods),
            ok
    end.

stop_modules() ->
    ets:foldl(fun(#couch_mod{mod=Mod}, _Acc) ->
                      stop_module(Mod)
              end, ok, ?COUCH_MODS).


wait_for_proc(Pid) when is_pid(Pid) ->
    MRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MRef, _Type, Pid, _Info} ->
            ok
    after 5000 ->
              erlang:demonitor(MRef),
              couch_util:shutdown_sync(Pid)
    end.
