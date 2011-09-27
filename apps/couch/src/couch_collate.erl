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

-module(couch_collate).

-export([init/0, collate/3]).

-on_load(init/0).

init() ->
    LibDir = case couch_config:get("couchdb", "util_driver_dir") of
    undefined ->
        couch_util:priv_dir();
    LibDir0 ->
        LibDir0
    end,
    NumScheds = erlang:system_info(schedulers),
    (catch erlang:load_nif(filename:join([LibDir, ?MODULE]), NumScheds)),
    case erlang:system_info(otp_release) of
        "R13B03" -> true;
        _ -> ok
    end.

collate(BinaryA, BinaryB, 0) ->
    collate_nif(BinaryA, BinaryB, 0);
collate(BinaryA, BinaryB, 1) ->
    collate_nif(BinaryA, BinaryB, 1).

collate_nif(_BinaryA, _BinaryB, _HasCase) ->
    exit(couch_collate_not_loaded).
