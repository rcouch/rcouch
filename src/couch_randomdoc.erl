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


-module(couch_randomdoc).

-include_lib("couch/include/couch_db.hrl").

-export([random_doc/1, random_doc/2]).

random_doc(Db) ->
    random_doc(Db, []).

random_doc(Db, Opts) ->
    {ok, Info} = couch_db:get_db_info(Db),
    Offset = case couch_util:get_value(doc_count, Info) of
        T when T < 1 -> T;
        T -> crypto:rand_uniform(0, T)
    end,
    Fun = fun
        (#doc_info{} = DocInfo, _O, _Acc) ->
            {stop, DocInfo};
        (_, _, Acc) ->
            {stop, Acc}
    end,
    Fun1 = skip_deleted(Fun),
    {ok, _O, Result} = couch_db:enum_docs_since(Db, Offset, Fun1,
        nil, []),

    case Result of
        nil ->
           null;
        #doc_info{} ->
            {ok, couch_index_util:load_doc(Db, Result, Opts)}
    end.

skip_deleted(FoldFun) ->
    fun
        (visit, KV, Reds, Acc) ->
            FoldFun(KV, Reds, Acc);
        (traverse, _LK, {Undeleted, _Del, _Size}, Acc) when Undeleted == 0 ->
            {skip, Acc};
        (traverse, _, _, Acc) ->
            {ok, Acc}
    end.

