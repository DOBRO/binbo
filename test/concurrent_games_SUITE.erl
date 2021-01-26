%% Copyright (c) 2019-2021, Sergei Semichev <chessvegas@chessvegas.com>. All Rights Reserved.
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

-module(concurrent_games_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("binbo_test_lib.hrl").

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([concurrent_games_test/1]).


%% all/0
all() -> [concurrent_games_test].

%% init_per_suite/1
init_per_suite(Config) ->
	{ok, _} = binbo:start(),
	[{games_total, 100} | Config].

%% end_per_suite/1
end_per_suite(_Config) ->
	ok.

%% concurrent_games_test/1
concurrent_games_test(Config) ->
	GamesTotal = ?value(games_total, Config),
	true = erlang:is_integer(GamesTotal) andalso (GamesTotal > 1),
	Parent = self(),
	Ref = make_ref(),
	Pids = lists:foldl(fun(_, Acc) ->
		Pid = spawn_link(fun() -> await_game_end(Parent, Ref) end),
		true = is_pid(Pid),
		[Pid | Acc]
	end, [], lists:seq(1, GamesTotal)),
	GamesTotal = erlang:length(Pids),
	ok = await_games(Pids, Ref),
	ok.

%% await_games/2
await_games([], _) ->
	ok;
await_games(Pids, Ref) ->
	receive
		{From, Ref, game_end} ->
			Pids2 = lists:delete(From, Pids),
			await_games(Pids2, Ref)
	after 5000 ->
			{error, timeout}
	end.

%% await_game_end/2
await_game_end(Parent, Ref) ->
	{ok, Pid} = binbo:new_server(),
	ok = play_game(Pid),
	ok = binbo:stop_server(Pid),
	Parent ! {self(), Ref, game_end},
	ok.

%% play_game/1
play_game(Pid) ->
	{ok, continue} = binbo:new_game(Pid),
	{ok, continue} = binbo:move(Pid, <<"e2e4">>),
	{ok, continue} = binbo:move(Pid, <<"e7e5">>),
	ok.
