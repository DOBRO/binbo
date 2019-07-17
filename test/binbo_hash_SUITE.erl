%% Copyright (c) 2019, Sergei Semichev <chessvegas@chessvegas.com>. All Rights Reserved.
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
-export([random_hash_test/1]).

%% all/0
%% Generate random numbers (hashes), check them for uniqueness,
%% and repeat the test 10 times.
all() -> [{testcase, random_hash_test, [{repeat_until_fail, 10}]}].


%% test_random_hash/1
random_hash_test(_Config) ->
	Mods = binbo_hash:init(),
	[_|_] = Mods,
	ok = unload_mods(Mods).

%% unload_mods/1
unload_mods([]) -> ok;
unload_mods([Mod|Tail]) ->
	true = erlang:is_atom(Mod),
	Bool = binbo_global:delete(Mod),
	true = erlang:is_boolean(Bool),
	unload_mods(Tail).
