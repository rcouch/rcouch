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

-module(couch_index_api).

-export([behaviour_info/1]).

%% @private
-spec behaviour_info(_)
	-> undefined | [{handle, 2} | {init, 3} | {terminate, 2}, ...].

behaviour_info(callbacks) ->
    [{get, 2},
     {init, 2}, 
     {open, 2},
     {close, 1},
     {delete, 1},
     {reset, 1},
     {start_update, 3},
     {purge, 4},
     {process_doc, 3},
     {finish_update, 1},
     {commit, 1},
     {compact, 3},
     {swap_compacted, 2}];
behaviour_info(_) ->
    undefined.
