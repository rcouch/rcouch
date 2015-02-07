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

-module(couch_primary_sup).
-behaviour(supervisor).
-export([init/1, start_link/0]).

start_link() ->
    supervisor:start_link({local,couch_primary_services}, ?MODULE, []).

init([]) ->
    Children = [
        {couch_task_status,
            {couch_task_status, start_link, []},
            permanent,
            brutal_kill,
            worker,
            [couch_task_status]},
        {couch_server,
            {couch_server, sup_start_link, []},
            permanent,
            brutal_kill,
            worker,
            [couch_server]},
        {couch_db_update_event,
            {gen_event, start_link, [{local, couch_db_update}]},
            permanent,
            brutal_kill,
            worker,
            dynamic},
        {couch_log,
            {couch_log, start_link, []},
            permanent,
            brutal_kill,
            worker,
            [couch_log]},
        {couch_index_sup,
             {couch_index_sup, start_link, []},
             permanent,
             infinity,
             supervisor,
             [couch_index_sup]},
        {couch_mods,
         {couch_mods, start_link, []},
         permanent, brutal_kill, worker, [couch_mods]}

    ],
    {ok, {{one_for_one, 10, 3600}, Children}}.

