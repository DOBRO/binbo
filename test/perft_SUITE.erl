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

%%%------------------------------------------------------------------------------
%%%   perft_SUITE virifies numbers from Perft Results:
%%%   https://www.chessprogramming.org/Perft_Results
%%%
%%%   Note: It passes tests for Depth=1 only, so far.
%%%------------------------------------------------------------------------------

-module(perft_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("binbo_test_lib.hrl").


-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([all_legal_moves_via_api/1]).
-export([position1/1, position2/1, position3/1, position4/1, position5/1, position6/1]).

%% all/0
all() -> [{group, perft_tests}].

%% groups/0
groups() ->
	[{perft_tests, [parallel], [
		all_legal_moves_via_api,
		position1, position2, position3, position4, position5, position6
	]}].

%% init_per_suite/1
init_per_suite(Config) ->
	{ok, _} = binbo:start(),
	Config.

%% end_per_suite/1
end_per_suite(_Config) ->
	ok = binbo:stop(),
	ok.

%% init_per_testcase/2
init_per_testcase(_TestCase, Config) ->
	{ok, Pid} = binbo:new_server(),
	[{pid, Pid} | Config].

%% end_per_testcase/2
end_per_testcase(_TestCase, Config) ->
	Pid = get_pid(Config),
	ok = binbo:stop_server(Pid),
	ok.

%% get_pid/1
get_pid(Config) ->
	?value(pid, Config).


%%%------------------------------------------------------------------------------
%%%   Tests
%%%------------------------------------------------------------------------------

%% all_legal_moves_via_api/1
all_legal_moves_via_api(Config) ->
	Pid = get_pid(Config),
	Nodes = 20,
	{ok, continue} = binbo:new_game(Pid),
	{ok, IntMovelist} = binbo:all_legal_moves(Pid, int),
	{ok, BinMovelist} = binbo:all_legal_moves(Pid, bin),
	{ok, StrMovelist} = binbo:all_legal_moves(Pid, str),
	Nodes = erlang:length(IntMovelist),
	Nodes = erlang:length(BinMovelist),
	Nodes = erlang:length(StrMovelist),
	ok.

%% perft_depth_1/3
perft_depth_1(Config, Fen, Nodes) ->
	Pid = get_pid(Config),
	{ok, continue} = binbo:new_game(Pid, Fen),
	Game = binbo:game_state(Pid),
	{ok, Movelist} = binbo_game:all_legal_moves(Game, int),
	Nodes = erlang:length(Movelist),
	ok.

%% position1/1
%% 'Initial Position' from Perft Results
position1(Config) ->
	Fen = <<"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1">>,
	Nodes = 20,
	ok = perft_depth_1(Config, Fen, Nodes),
	ok.

%% position2/1
%% 'Position 2' from Perft Results
position2(Config) ->
	Fen = <<"r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -">>,
	Nodes = 48,
	ok = perft_depth_1(Config, Fen, Nodes),
	ok.

%% position3/1
%% 'Position 3' from Perft Results
position3(Config) ->
	Fen = <<"8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -">>,
	Nodes = 14,
	ok = perft_depth_1(Config, Fen, Nodes),
	ok.

%% position4/1
%% 'Position 4' from Perft Results
position4(Config) ->
	Fen = <<"r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1">>,
	Nodes = 6,
	ok = perft_depth_1(Config, Fen, Nodes),
	ok.

%% position5/1
%% 'Position 5' from Perft Results
position5(Config) ->
	Fen = <<"rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8">>,
	Nodes = 44,
	ok = perft_depth_1(Config, Fen, Nodes),
	ok.

%% position6/1
%% 'Position 6' from Perft Results
position6(Config) ->
	Fen = <<"r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10">>,
	Nodes = 46,
	ok = perft_depth_1(Config, Fen, Nodes),
	ok.
