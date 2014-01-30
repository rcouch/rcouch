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

-module(couch_index_event).
-behaviour(gen_event).

-export([start_link/1]).
-export([notify/1]).
-export([stop/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

start_link(Consumer) ->
    HandlerId = {?MODULE, make_ref()},
    couch_index_event_sup:start_link(couch_index_events, HandlerId,
                                     Consumer).

notify(Event) ->
    gen_event:notify(couch_index_events, Event).

stop(Pid) ->
    couch_index_event_sup:stop(Pid).


init(Consumer) ->
    process_flag(trap_exit, true),
    {ok, Consumer}.

handle_event(Event, Consumer) ->
    dispatch_event(Event, Consumer).

handle_call(_Req, Consumer) ->
    {reply, ok, Consumer}.

handle_info({'EXIT', _, _}, _Consumer) ->
    remove_handler;
handle_info(_Info, Consumer)->
    {ok, Consumer}.

code_change(_OldVsn, Consumer, _Extra) ->
    {ok, Consumer}.

terminate(_Reason, _consumer) ->
    ok.

dispatch_event(Event, Fun) when is_function(Fun) ->
    Fun(Event),
    {ok, Fun};
dispatch_event(Event, {Fun, Acc}) when is_function(Fun) ->
    Acc2 = Fun(Event, Acc),
    {ok, {Fun, Acc2}};
dispatch_event(Event, Pid) when is_pid(Pid) ->
    Pid ! Event,
    {ok, Pid}.
