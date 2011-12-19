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

-export([random_doc/1, random_doc/2, random_doc/3]).

random_doc(Db) ->
    DefaultFun = fun(_Doc) -> true end,
    random_doc(Db, DefaultFun, []).

random_doc(Db, FilterFun) ->
    random_doc(Db, FilterFun, []).


random_doc(Db, FilterFun, Opts) ->
    {ok, Info} = couch_db:get_db_info(Db),
    N = case couch_util:get_value(doc_count, Info) of
        C when C < 1 -> C;
        C -> crypto:rand_uniform(0, C)
    end,

    Fun = fun
        (#full_doc_info{}, _O, Skip) when Skip > 0 ->
            {ok, Skip-1};
        (#full_doc_info{} = FullDocInfo, _O, Acc) ->
            case couch_doc:to_doc_info(FullDocInfo) of
                #doc_info{revs=[#rev_info{deleted=true}|_]} ->
                    {ok, Acc};
                DocInfo ->
                    Doc = couch_index_util:load_doc(Db, DocInfo, Opts),

                    case FilterFun(Db, Doc) of
                        true ->
                            {stop, Doc};
                        false ->
                            {ok, Acc}
                    end
            end;
        (_Other, _, Acc) ->
            {stop, Acc}
    end,
    {ok, _, Result} = couch_db:enum_docs(Db, Fun, N, []),

    case Result of
        #doc{} ->
            {ok, Result};
        _ ->
           null
    end.
