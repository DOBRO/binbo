%% Copyright (c) 2019-2020, Sergei Semichev <chessvegas@chessvegas.com>. All Rights Reserved.
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

-module(binbo_fen_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("binbo_test_lib.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-export([
	test_initial_fen/1,
	test_legal_string_fens/1,
	test_bad_fens/1
]).

%% all/0
all() -> [{group, fen_test_group}].

%% groups/0

groups() ->
	[{fen_test_group, [parallel], [
		test_initial_fen,
		test_legal_string_fens,
		test_bad_fens
	]}].

%% init_per_suite/1
init_per_suite(Config) ->
	ok = binbo_test_lib:all_group_testcases_exported(?MODULE),
	{ok, _} = binbo:start(),
	Config.

%% end_per_suite/1
end_per_suite(_Config) ->
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
%%%   Testcases
%%%------------------------------------------------------------------------------

%% test_initial_fen/1
test_initial_fen(Config) ->
	Pid = get_pid(Config),
	InitialFen = <<"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1">>,
	true = (InitialFen =:= binbo_fen:initial()),
	{ok,continue} = binbo:new_game(Pid, initial),
	{ok, InitialFen} = binbo:get_fen(Pid),
	% Fullmove = 0
	{ok,continue} = binbo:new_game(Pid, <<"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 0">>),
	ok.

%% test_legal_string_fens/1
test_legal_string_fens(Config) ->
	Pid = get_pid(Config),
	{ok,continue} = binbo:new_game(Pid, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"),
	ok.

%% test_bad_fens/1
test_bad_fens(Config) ->
	Pid = get_pid(Config),
	% No game initialized yet
	{error,{bad_game,undefined}} = binbo:get_fen(Pid),
	% Emty FEN
	{error,empty_fen} = binbo:new_game(Pid, <<>>),
	{error,empty_fen} = binbo:new_game(Pid, []),
	% Not valid binary, nor valid string
	{error,bad_data_type} = binbo:new_game(Pid, 3),
	{error,too_few_parts} = binbo:new_game(Pid, <<"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR">>),
	{error, invalid_fen_string} = binbo:new_game(Pid, [fen]),
	% Wrong active color
	{error, {invalid_active_color, <<"side">>}} = binbo:new_game(Pid, <<"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR side KQkq - 0 1">>),
	% Invalid nunber of ranks
	{error,{position,not_8_ranks}} = binbo:new_game(Pid, <<"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP-RNBQKBNR w KQkq - 0 1">>),
	% Wrong castling
	{error,{castling,empty_castling}} = binbo:new_game(Pid, <<"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w  - 0 1">>),
	{error,{castling,{invalid_character,"N"}}} = binbo:new_game(Pid, <<"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w N - 0 1">>),
	% Wrong en-passant square
	{error,{invalid_enpassant,<<"e0">>}} = binbo:new_game(Pid, <<"rnbqkbnr/1pp1pppp/p7/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq e0 0 1">>),
	{error,{invalid_enpassant,<<"e4">>}} = binbo:new_game(Pid, <<"rnbqkbnr/1pp1pppp/p7/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq e4 0 1">>),
	{error,{invalid_enpassant,<<"e5">>}} = binbo:new_game(Pid, <<"rnbqkbnr/1pp1pppp/p7/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq e5 0 1">>),
	{error,{invalid_enpassant,<<"e8">>}} = binbo:new_game(Pid, <<"rnbqkbnr/1pp1pppp/p7/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq e8 0 1">>),
	{error,{invalid_enpassant,<<"e34">>}} = binbo:new_game(Pid, <<"rnbqkbnr/1pp1pppp/p7/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq e34 0 1">>),
	{error,bb_invalid_enpassant} = binbo:new_game(Pid, <<"rnbqkbnr/1pp1pppp/p7/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 1">>),
	{error,bb_invalid_enpassant} = binbo:new_game(Pid, <<"rnbqkbnr/1pp1pppp/p7/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq e3 0 1">>),
	% Wrong halfmove
	{error,{invalid_halfmove,<<>>}} = binbo:new_game(Pid, <<"rnbqkbnr/1pp1pppp/p7/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq -  1">>),
	{error,{invalid_halfmove,<<"h">>}} = binbo:new_game(Pid, <<"rnbqkbnr/1pp1pppp/p7/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq - h 1">>),
	{error,{invalid_halfmove,<<"-1">>}} = binbo:new_game(Pid, <<"rnbqkbnr/1pp1pppp/p7/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq - -1 1">>),
	% Wrong fullmove
	{error,{invalid_fullmove,<<>>}} = binbo:new_game(Pid, <<"rnbqkbnr/1pp1pppp/p7/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq - 1 ">>),
	{error,{invalid_fullmove,<<"f">>}} = binbo:new_game(Pid, <<"rnbqkbnr/1pp1pppp/p7/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq - 0 f">>),
	{error,{invalid_fullmove,<<"-1">>}} = binbo:new_game(Pid, <<"rnbqkbnr/1pp1pppp/p7/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq - 1 -1">>),
	% Empty rank
	{error,{position,empty_rank}} = binbo:new_game(Pid, <<"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/ w KQkq - 0 1">>),
	{error,{position,empty_rank}} = binbo:new_game(Pid, <<"rnbqkbnr/pppppppp/8/8/8/8// w KQkq - 0 1">>),
	% Wrong position
	{error,{position,empty_position}} = binbo:new_game(Pid, <<" w KQkq - 0 1">>),
	{error,{position,{invalid_character,"0"}}} = binbo:new_game(Pid, <<"rnbqkbnr/pppppppp/0/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1">>),
	{error,{position,{too_many_pawns,black,9}}} = binbo:new_game(Pid, <<"rnbqkbnr/pppppppp/p7/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1">>),
	{error,{position,{too_many_kings,white,2}}} = binbo:new_game(Pid, <<"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKKNR w KQkq - 0 1">>),
	{error,{position,{index_out_of_range,16,{rank,2}}}} = binbo:new_game(Pid, <<"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPPP/RNBQKBNR w KQkq - 0 1">>),
	{error,{position,{last_index_mismatch,15,{rank,2}}}} = binbo:new_game(Pid, <<"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPP/RNBQKBNR w KQkq - 0 1">>),
	{error,{position,{no_kings,white}}} = binbo:new_game(Pid, <<"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RRNBQNRR w KQkq - 0 1">>),
	{error,{position,{no_kings,black}}} = binbo:new_game(Pid, <<"rnbqbnrr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1">>),
	{error,{position,{bad_totals,black,
                             {total,17},
                             {pawns,8},
                             {knights,2},
                             {bishops,2},
                             {rooks,2},
                             {queens,2},
                             {kings,1}}}} = binbo:new_game(Pid, <<"rnbqkbnr/pppppppp/8/3q4/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1">>),
	{error,{position,{bad_totals,white,
                             {total,17},
                             {pawns,8},
                             {knights,2},
                             {bishops,2},
                             {rooks,2},
                             {queens,2},
                             {kings,1}}}} = binbo:new_game(Pid, <<"rnbqkbnr/pppppppp/8/8/3Q4/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1">>),
	ok.



%%%------------------------------------------------------------------------------
%%%   Internal helpers
%%%------------------------------------------------------------------------------

%% get_pid/1
get_pid(Config) ->
	?value(pid, Config).
