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

-module(couch_index_event_sup).

-export([start_link/3]).
-export([stop/1]).

%% internal gen_server callbacks
-export([init/1, terminate/2, handle_call/3, handle_cast/2,
         handle_info/2,code_change/3]).

start_link(EventMgr, EventHandler, Args) ->
    gen_server:start_link(?MODULE, {EventMgr, EventHandler, Args}, []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

init({EventMgr, EventHandler, Args}) ->
    case gen_event:add_sup_handler(EventMgr, EventHandler, Args) of
    ok ->
        {ok, {EventMgr, EventHandler}};
    {stop, Error} ->
        {stop, Error}
    end.

handle_call(_Whatever, _From, State) ->
    {ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({gen_event_EXIT, _Handler, Reason}, State) ->
    {stop, Reason, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


