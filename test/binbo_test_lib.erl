%% Copyright (c) 2019-2025, Sergei Semichev <chessvegas@chessvegas.com>. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(binbo_test_lib).

-export([all_group_testcases_exported/1]).


%% all_group_testcases_exported
all_group_testcases_exported(Mod) ->
    Exports = Mod:module_info(exports),
    Groups = Mod:groups(),
    all_group_testcases_exported(Groups, Exports).


%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

%% all_group_testcases_exported/2
all_group_testcases_exported([], _Exports) ->
    ok;
all_group_testcases_exported([{GroupName, _Opts, TestCases} | Tail], Exports) ->
    case group_testcases_exported(TestCases, Exports) of
        ok ->
            all_group_testcases_exported(Tail, Exports);
        {error, Fun} ->
            {error, {not_exported, GroupName, Fun}}
    end.


%% group_testcases_exported/2
group_testcases_exported([], _Exports) ->
    ok;
group_testcases_exported([Fun | Tail], Exports) ->
    case lists:keyfind(Fun, 1, Exports) of
        {Fun, 1} ->
            group_testcases_exported(Tail, Exports);
        false ->
            {error, Fun}
    end.
