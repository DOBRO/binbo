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
%%%    and from some other resources
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
-export([promotion_bugs_position/1]).
-export([wide_open_position/1]).
-export([king_and_pawns_position/1]).

%% all/0
all() -> [{group, perft_tests}].

%% groups/0
groups() ->
	[{perft_tests, [parallel], [
		all_legal_moves_via_api,
		position1, position2, position3, position4, position5, position6,
		promotion_bugs_position,
		wide_open_position,
		king_and_pawns_position
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

%%%------------------------------------------------------------------------------
%%%   Helper functions
%%%------------------------------------------------------------------------------

%% get_pid/1
get_pid(Config) ->
	?value(pid, Config).

%% perft_init_game/3
perft_init_game(Config, Fen) ->
	Pid = get_pid(Config),
	{ok, continue} = binbo:new_game(Pid, Fen),
	binbo:game_state(Pid).

%%%------------------------------------------------------------------------------
%%%   Perft
%%%------------------------------------------------------------------------------

%% perft/2
perft(Depth, Game) ->
	perft(Depth, Game, 0).

%% perft/3
%% Here we call 'binbo_game:all_legal_moves/2' and 'binbo_game:move/3' directly
%% avoiding flooding the game process with messages.
perft(Depth, Game, Nodes) when Depth > 0 ->
	{ok, Movelist} = binbo_game:all_legal_moves(Game, bitint),
	case Depth =:= 1 of
		true ->
			Nodes + erlang:length(Movelist);
		false ->
			lists:foldl(fun(Move, Acc) ->
				{ok, {Game2, _Status2}} = binbo_game:move(int, Move, Game),
				perft(Depth - 1, Game2, Acc)
			end, Nodes, Movelist)
	end.

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

%% position1/1
%% 'Initial Position' from https://www.chessprogramming.org/Perft_Results
position1(Config) ->
	Fen = <<"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1">>,
	Game = perft_init_game(Config, Fen),
	20 = perft(1, Game),
	400 = perft(2, Game),
	8902 = perft(3, Game),
	197281 = perft(4, Game),
	ok.

%% position2/1
%% 'Position 2' from https://www.chessprogramming.org/Perft_Results
position2(Config) ->
	Fen = <<"r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -">>,
	Game = perft_init_game(Config, Fen),
	48 = perft(1, Game),
	2039 = perft(2, Game),
	97862 = perft(3, Game),
	ok.

%% position3/1
%% 'Position 3' from https://www.chessprogramming.org/Perft_Results
position3(Config) ->
	Fen = <<"8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -">>,
	Game = perft_init_game(Config, Fen),
	14 = perft(1, Game),
	191 = perft(2, Game),
	2812 = perft(3, Game),
	43238 = perft(4, Game),
	674624 = perft(5, Game),
	ok.

%% position4/1
%% 'Position 4' from https://www.chessprogramming.org/Perft_Results
position4(Config) ->
	Fen = <<"r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1">>,
	Game = perft_init_game(Config, Fen),
	6 = perft(1, Game),
	264 = perft(2, Game),
	9467 = perft(3, Game),
	422333 = perft(4, Game),
	ok.

%% position5/1
%% 'Position 5' from https://www.chessprogramming.org/Perft_Results
position5(Config) ->
	Fen = <<"rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8">>,
	Game = perft_init_game(Config, Fen),
	44 = perft(1, Game),
	1486 = perft(2, Game),
	62379 = perft(3, Game),
	ok.

%% position6/1
%% 'Position 6' from https://www.chessprogramming.org/Perft_Results
position6(Config) ->
	Fen = <<"r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10">>,
	Game = perft_init_game(Config, Fen),
	46 = perft(1, Game),
	2079 = perft(2, Game),
	89890 = perft(3, Game),
	ok.

%% promotion_bugs_position/1
%% Discover promotion bugs.
%% Test from http://www.rocechess.ch/perft.html
promotion_bugs_position(Config) ->
	Fen = <<"n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1">>,
	Game = perft_init_game(Config, Fen),
	24 = perft(1, Game),
	496 = perft(2, Game),
	9483 = perft(3, Game),
	ok.

%% wide_open_position/1
%% From http://www.talkchess.com/forum3/viewtopic.php?t=49000
wide_open_position(Config) ->
	Fen = <<"rnbqkbnr/8/8/8/8/8/8/RNBQKBNR w KQkq - 0 1">>,
	Game = perft_init_game(Config, Fen),
	50 = perft(1, Game),
	2125 = perft(2, Game),
	96062 = perft(3, Game),
	ok.


%% king_and_pawns_position/1
%% From http://www.talkchess.com/forum3/viewtopic.php?t=49000
king_and_pawns_position(Config) ->
	Fen = <<"4k3/pppppppp/8/8/8/8/PPPPPPPP/4K3 w - - 0 1">>,
	Game = perft_init_game(Config, Fen),
	18 = perft(1, Game),
	324 = perft(2, Game),
	5658 = perft(3, Game),
	98766 = perft(4, Game),
	ok.
