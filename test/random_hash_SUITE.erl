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

-module(random_hash_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([test_random_hash/1]).

%% all/0
%% Generate random numbers (hashes), check them for uniqueness,
%% and repeat the test 100 times.
all() -> [{testcase, test_random_hash, [{repeat_until_fail, 100}]}].


%% test_random_hash/1
test_random_hash(_Config) ->
	[_|_] = binbo_hash:init().
