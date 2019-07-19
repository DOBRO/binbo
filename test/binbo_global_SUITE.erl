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

-module(binbo_global_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).
-compile(nowarn_export_all).

%% all/0
all() -> [test].


%% test/1
test(_Config) ->
	Mod = '$binbo_global_SUITE$test$',
	Value = 'test',
	ok = binbo_global:put(Mod, Value),
	{file, _} = code:is_loaded(Mod),
	Value = binbo_global:get(Mod),
	Bool = binbo_global:delete(Mod),
	true = erlang:is_boolean(Bool),
	false = code:is_loaded(Mod).
