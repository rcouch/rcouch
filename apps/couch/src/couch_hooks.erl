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
-module(couch_hooks).
-behavior(gen_server).

-define(HOOKS, ?MODULE).

%% PUBLIC API
-export([add/3, add/4, add/5,
         delete/3, delete/4, delete/5,
         run/3, run_fold/4]).

-export([start_link/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, code_change/3, handle_info/2,
         terminate/2]).

add(Hook, Fun, Seq) ->
    add(Hook, global, undefined, Fun, Seq).

add(Hook, DbName, Fun, Seq)
  when is_binary(DbName) orelse is_list(DbName) ->
    add(Hook, DbName, undefined, Fun, Seq);
add(Hook, Mod, Fun, Seq) ->
    add(Hook, global, Mod, Fun, Seq).

add(Hook, DbName, Mod, Fun, Seq) when is_list(DbName) ->
    add(Hook, list_to_binary(DbName), Mod, Fun, Seq);
add(Hook, DbName, Mod, Fun, Seq) ->
    gen_server:call(?MODULE, {add, Hook, DbName, Mod, Fun, Seq}).

delete(Hook, Fun, Seq) ->
    delete(Hook, global, undefined, Fun, Seq).

delete(Hook, DbName, Fun, Seq)
  when is_binary(DbName) orelse is_list(DbName) ->
    delete(Hook, DbName, undefined, Fun, Seq);
delete(Hook, Mod, Fun, Seq) ->
    delete(Hook, global, Mod, Fun, Seq).

delete(Hook, DbName, Mod, Fun, Seq) when is_list(DbName) ->
    delete(Hook, list_to_binary(DbName), Mod, Fun, Seq);
delete(Hook, DbName, Mod, Fun, Seq) ->
    gen_server:call(?MODULE, {del, Hook, DbName, Mod, Fun, Seq}).

run(Hook, DbName, Args) ->
    case lookup(Hook, DbName) of
        [{_, Hooks}] ->
            run1(lists:sort(Hooks), Args);
        [] ->
            ok
    end.

run_fold(Hook, DbName, Acc, Args) ->
    case lookup(Hook, DbName) of
        [{_, Hooks}] ->
            run1_fold(lists:sort(Hooks), Acc, Args);
        [] ->
            Acc
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    ?HOOKS = ets:new(?HOOKS, [set, named_table, {read_concurrency, true}]),
    {ok, nil}.

handle_call({add, Hook, DbName, Module, Function, Seq}, _From, State) ->
    NewEls = case ets:lookup(?HOOKS, {Hook, DbName}) of
              [] ->
                  [{Seq, Module, Function}];
              [{_, Els}] ->
                  lists:umerge([{Seq, Module, Function}], Els)
           end,
    ets:insert(?HOOKS, {{Hook, DbName}, NewEls}),
    {reply, ok, State};
handle_call({del, Hook, DbName, Module, Function, Seq}, _From, State) ->
    case ets:lookup(?HOOKS, {Hook, DbName}) of
        [] -> ok;
        [{_, Els}] ->
            NewEls = lists:delete({Seq, Module, Function}, Els),
            ets:insert(?HOOKS, {{Hook, DbName}, NewEls}),
            ok
    end,
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

lookup(Hook, global) ->
    ets:lookup(?HOOKS, {Hook, global});
lookup(Hook, DbName) ->
    lists:merge(ets:lookup(?HOOKS, {Hook, DbName}),
                ets:lookup(?HOOKS, {Hook, '*'})).
 %   ets:select(?HOOKS, [{{{'$1', '$2'}, '$3'},
 %                        [{'==', '$1', Hook},
 %                         {'orelse', {'==', '$2', '*'}, {'==', '$2', DbName}}],
 %                        ['$_']}]).

run1([], _Args) ->
    ok;
run1([{_, Mod, Fun} | Rest], Args) ->
    case Mod of
        undefined -> catch erlang:apply(Fun, Args);
        _ -> catch erlang:apply(Mod, Fun, Args)
    end,
    run1(Rest, Args).


run1_fold([], Acc, _Args) ->
    Acc;
run1_fold([{_Seq, Mod, Fun} | Rest], Acc, Args) ->
    Res = case Mod of
        undefined -> catch erlang:apply(Fun, [Acc | Args]);
        _ -> catch erlang:apply(Mod, Fun, [Acc | Args])
    end,
    case Res of
        stop -> Acc;
        ok -> run1_fold(Rest, Acc, Args);
        _ -> run1_fold(Rest, Res, Args)
    end.
