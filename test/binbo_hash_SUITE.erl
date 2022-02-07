%% Copyright (c) 2019-2022, Sergei Semichev <chessvegas@chessvegas.com>. All Rights Reserved.
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

-module(binbo_hash_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([random_hash_test/1]).

%% all/0
%% Generate random numbers (hashes), check them for uniqueness,
%% and repeat the test 10 times.
all() -> [{group, rnd}].

%% groups/0
groups() -> [{rnd, [{repeat_until_any_fail, 10}], [random_hash_test]}].

%% init_per_suite/1
init_per_suite(Config) ->
    ok = binbo_test_lib:all_group_testcases_exported(?MODULE),
    Config.

%% end_per_suite/1
end_per_suite(_Config) ->
    ok.

%% random_hash_test/1
random_hash_test(_Config) ->
    Mods = binbo_hash:init(),
    [_, _, _, _] = Mods,
    ok = unload_mods(Mods),
    % Modules were unloaded, load them again. Required for further test suites.
    _ = binbo_hash:init(),
    ok.

%% unload_mods/1
unload_mods([]) -> ok;
unload_mods([Mod|Tail]) ->
    true = erlang:is_atom(Mod),
    Bool = binbo_global:delete(Mod),
    true = erlang:is_boolean(Bool),
    unload_mods(Tail).
