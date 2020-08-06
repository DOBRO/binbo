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

-module(binbo_board_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("binbo_test_lib.hrl").
-include("binbo_board.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([
	bb_macro_test/1, fun_macro_test/1,
	board_tuple_test/1, index_list_test/1, file_list_test/1, side_list_test/1,
	enemy_color_test/1, castling_list_test/1, rank_of_index_test/1, file_of_index_test/1,
	sq_distance_test/1, is_valid_square_notation_test/1, notation_to_index_test/1,
	index_to_notation_test/1, castling_rook_squares_test/1
]).

%% all/0
all() -> [{group, all_tests}].

groups() -> [{all_tests, [parallel], [
	bb_macro_test, fun_macro_test,
	board_tuple_test, index_list_test, file_list_test, side_list_test,
	enemy_color_test, castling_list_test, rank_of_index_test, file_of_index_test,
	sq_distance_test, is_valid_square_notation_test, notation_to_index_test,
	index_to_notation_test, castling_rook_squares_test
]}].

%% init_per_suite/1
init_per_suite(Config) ->
	ok = binbo_test_lib:all_group_testcases_exported(?MODULE),
	[{white, 16#00}, {black, 16#10},
	{a1,0}, {b2,9}, {c3,18}, {d4,27}, {e5,36}, {f6,45}, {g7,54}, {h8,63},
	{a8,56}, {h1,7}, {d1,3}, {d8,59}, {f1,5},{f8,61}
	| Config].

%% end_per_suite/1
end_per_suite(_Config) ->
	ok.

%% board_tuple_test/1
board_tuple_test(_Config) ->
	Tuple = binbo_board:board_tuple(0),
	true = erlang:is_tuple(Tuple),
	64 = erlang:tuple_size(Tuple),
	ok.

%% index_list_test/1
index_list_test(_Config) ->
	List = binbo_board:index_list(),
	64 = erlang:length(List),
	[
		 0, 1, 2, 3, 4, 5, 6, 7,
	 	 8, 9,10,11,12,13,14,15,
		16,17,18,19,20,21,22,23,
		24,25,26,27,28,29,30,31,
		32,33,34,35,36,37,38,39,
		40,41,42,43,44,45,46,47,
		48,49,50,51,52,53,54,55,
		56,57,58,59,60,61,62,63
	] = List,
	ok.

%% file_list_test/1
file_list_test(_Config) ->
	[0,1,2,3,4,5,6,7] = binbo_board:file_list(),
	ok.

%% side_list_test/1
side_list_test(Config) ->
	White = ?value(white, Config),
	Black = ?value(black, Config),
	[White, Black] = binbo_board:side_list(),
	ok.

%% enemy_color_test/1
enemy_color_test(Config) ->
	White = ?value(white, Config),
	Black = ?value(black, Config),
	Black = binbo_board:enemy_color(White),
	White = binbo_board:enemy_color(Black),
	ok.

%% castling_list_test/1
castling_list_test(_Config) ->
	[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15] = binbo_board:castling_list(),
	ok.

%% rank_of_index_test/1
rank_of_index_test(Config) ->
	0 = binbo_board:rank_of_index(?value(a1, Config)),
	1 = binbo_board:rank_of_index(?value(b2, Config)),
	2 = binbo_board:rank_of_index(?value(c3, Config)),
	3 = binbo_board:rank_of_index(?value(d4, Config)),
	4 = binbo_board:rank_of_index(?value(e5, Config)),
	5 = binbo_board:rank_of_index(?value(f6, Config)),
	6 = binbo_board:rank_of_index(?value(g7, Config)),
	7 = binbo_board:rank_of_index(?value(h8, Config)),
	lists:foreach(fun(Idx) ->
		RankNum = binbo_board:rank_number(Idx),
		RankNum = binbo_board:rank_of_index(Idx) + 1
	end, lists:seq(0, 64, 1)),
	ok.

%% file_of_index_test/1
file_of_index_test(Config) ->
	0 = binbo_board:file_of_index(?value(a1, Config)),
	1 = binbo_board:file_of_index(?value(b2, Config)),
	2 = binbo_board:file_of_index(?value(c3, Config)),
	3 = binbo_board:file_of_index(?value(d4, Config)),
	4 = binbo_board:file_of_index(?value(e5, Config)),
	5 = binbo_board:file_of_index(?value(f6, Config)),
	6 = binbo_board:file_of_index(?value(g7, Config)),
	7 = binbo_board:file_of_index(?value(h8, Config)),
	ok.

%% sq_distance_test/1
sq_distance_test(Config) ->
	0 = binbo_board:sq_distance(?value(a1, Config), ?value(a1, Config)),
	1 = binbo_board:sq_distance(?value(a1, Config), ?value(b2, Config)),
	2 = binbo_board:sq_distance(?value(a1, Config), ?value(c3, Config)),
	3 = binbo_board:sq_distance(?value(a1, Config), ?value(d4, Config)),
	4 = binbo_board:sq_distance(?value(a1, Config), ?value(e5, Config)),
	5 = binbo_board:sq_distance(?value(a1, Config), ?value(f6, Config)),
	6 = binbo_board:sq_distance(?value(a1, Config), ?value(g7, Config)),
	7 = binbo_board:sq_distance(?value(a1, Config), ?value(h8, Config)),
	ok.

%% is_valid_square_notation_test/1
is_valid_square_notation_test(_Config) ->
	true = binbo_board:is_valid_square_notation(<<"a1">>),
	true = binbo_board:is_valid_square_notation(<<"b2">>),
	true = binbo_board:is_valid_square_notation(<<"c3">>),
	true = binbo_board:is_valid_square_notation(<<"d4">>),
	true = binbo_board:is_valid_square_notation(<<"e5">>),
	true = binbo_board:is_valid_square_notation(<<"f6">>),
	true = binbo_board:is_valid_square_notation(<<"g7">>),
	true = binbo_board:is_valid_square_notation(<<"h8">>),
	false = binbo_board:is_valid_square_notation(<<"a0">>),
	false = binbo_board:is_valid_square_notation(<<"a9">>),
	false = binbo_board:is_valid_square_notation(<<"h9">>),
	false = binbo_board:is_valid_square_notation(<<"i1">>),
	ok.

%% notation_to_index_test/1
notation_to_index_test(_Config) ->
	 0 = binbo_board:notation_to_index(<<"a1">>),
	 9 = binbo_board:notation_to_index(<<"b2">>),
	18 = binbo_board:notation_to_index(<<"c3">>),
	27 = binbo_board:notation_to_index(<<"d4">>),
	36 = binbo_board:notation_to_index(<<"e5">>),
	45 = binbo_board:notation_to_index(<<"f6">>),
	54 = binbo_board:notation_to_index(<<"g7">>),
	63 = binbo_board:notation_to_index(<<"h8">>),
	ok.

%% index_to_notation_test/1
index_to_notation_test(_Config) ->
	<<"a1">> = binbo_board:index_to_notation(0),
	<<"b2">> = binbo_board:index_to_notation(9),
	<<"c3">> = binbo_board:index_to_notation(18),
	<<"d4">> = binbo_board:index_to_notation(27),
	<<"e5">> = binbo_board:index_to_notation(36),
	<<"f6">> = binbo_board:index_to_notation(45),
	<<"g7">> = binbo_board:index_to_notation(54),
	<<"h8">> = binbo_board:index_to_notation(63),
	ok.

%% castling_rook_squares_test/1
castling_rook_squares_test(Config) ->
	CASTLING_W_OO  = 2#0001,
	CASTLING_B_OO  = 2#0100,
	CASTLING_W_OOO = 2#0010,
	CASTLING_B_OOO = 2#1000,
	H1 = ?value(h1, Config),
	F1 = ?value(f1, Config),
	H8 = ?value(h8, Config),
	F8 = ?value(f8, Config),
	A1 = ?value(a1, Config),
	D1 = ?value(d1, Config),
	A8 = ?value(a8, Config),
	D8 = ?value(d8, Config),
	{H1, F1} = binbo_board:castling_rook_squares(CASTLING_W_OO),
	{H8, F8} = binbo_board:castling_rook_squares(CASTLING_B_OO),
	{A1, D1} = binbo_board:castling_rook_squares(CASTLING_W_OOO),
	{A8, D8} = binbo_board:castling_rook_squares(CASTLING_B_OOO),
	ok.

%% bb_macro_test/1
bb_macro_test(_Config) ->
	% Bitboards of squares
	?A1_BB = 1,
	?B1_BB = 1 bsl ?B1_IDX,
	?B1_BB = 1 bsl ?B1_IDX,
	?C1_BB = 1 bsl ?C1_IDX,
	?D1_BB = 1 bsl ?D1_IDX,
	?E1_BB = 1 bsl ?E1_IDX,
	?F1_BB = 1 bsl ?F1_IDX,
	?G1_BB = 1 bsl ?G1_IDX,
	?H1_BB = 1 bsl ?H1_IDX,
	?A8_BB = 1 bsl ?A8_IDX,
	?B8_BB = 1 bsl ?B8_IDX,
	?C8_BB = 1 bsl ?C8_IDX,
	?D8_BB = 1 bsl ?D8_IDX,
	?E8_BB = 1 bsl ?E8_IDX,
	?F8_BB = 1 bsl ?F8_IDX,
	?G8_BB = 1 bsl ?G8_IDX,
	?H8_BB = 1 bsl ?H8_IDX,
	?A1A8H1H8_BB = ?A1_BB bor ?A8_BB bor ?H1_BB bor ?H8_BB,
	% Bitboards of files
	?FILE_B_BB = ?FILE_A_BB bsl 1,
	?FILE_C_BB = ?FILE_A_BB bsl 2,
	?FILE_D_BB = ?FILE_A_BB bsl 3,
	?FILE_E_BB = ?FILE_A_BB bsl 4,
	?FILE_F_BB = ?FILE_A_BB bsl 5,
	?FILE_G_BB = ?FILE_A_BB bsl 6,
	?FILE_H_BB = ?FILE_A_BB bsl 7,
	?FILE_AB_BB = ?FILE_A_BB bor ?FILE_B_BB,
	?FILE_GH_BB = ?FILE_G_BB bor ?FILE_H_BB,
	?NOT_FILE_A_BB  = ?ALL_SQUARES_BB bxor ?FILE_A_BB,
	?NOT_FILE_H_BB  = ?ALL_SQUARES_BB bxor ?FILE_H_BB,
	?NOT_FILE_AB_BB = ?ALL_SQUARES_BB bxor ?FILE_AB_BB,
	?NOT_FILE_GH_BB = ?ALL_SQUARES_BB bxor ?FILE_GH_BB,
	% Bitboards of ranks
	?RANK_2_BB = ?RANK_1_BB bsl  8,
	?RANK_3_BB = ?RANK_1_BB bsl 16,
	?RANK_4_BB = ?RANK_1_BB bsl 24,
	?RANK_5_BB = ?RANK_1_BB bsl 32,
	?RANK_6_BB = ?RANK_1_BB bsl 40,
	?RANK_7_BB = ?RANK_1_BB bsl 48,
	?RANK_8_BB = ?RANK_1_BB bsl 56,
	?NOT_RANK_1_BB = ?ALL_SQUARES_BB bxor ?RANK_1_BB,
	?NOT_RANK_8_BB = ?ALL_SQUARES_BB bxor ?RANK_8_BB,
	ok.

%% fun_macro_test/1
fun_macro_test(_Config) ->
	% SWITCH_COLOR
	?BLACK = ?SWITCH_COLOR(?WHITE),
	?WHITE = ?SWITCH_COLOR(?BLACK),
	% COLOR
	?WHITE = ?COLOR(?WHITE_PAWN),
	?WHITE = ?COLOR(?WHITE_KNIGHT),
	?WHITE = ?COLOR(?WHITE_BISHOP),
	?WHITE = ?COLOR(?WHITE_ROOK),
	?WHITE = ?COLOR(?WHITE_QUEEN),
	?WHITE = ?COLOR(?WHITE_KING),
	?BLACK = ?COLOR(?BLACK_PAWN),
	?BLACK = ?COLOR(?BLACK_KNIGHT),
	?BLACK = ?COLOR(?BLACK_BISHOP),
	?BLACK = ?COLOR(?BLACK_ROOK),
	?BLACK = ?COLOR(?BLACK_QUEEN),
	?BLACK = ?COLOR(?BLACK_KING),
	% PIECE_TYPE
	?PAWN   = ?PIECE_TYPE(?WHITE_PAWN),
	?KNIGHT = ?PIECE_TYPE(?WHITE_KNIGHT),
	?BISHOP = ?PIECE_TYPE(?WHITE_BISHOP),
	?ROOK   = ?PIECE_TYPE(?WHITE_ROOK),
	?QUEEN  = ?PIECE_TYPE(?WHITE_QUEEN),
	?KING   = ?PIECE_TYPE(?WHITE_KING),
	?PAWN   = ?PIECE_TYPE(?BLACK_PAWN),
	?KNIGHT = ?PIECE_TYPE(?BLACK_KNIGHT),
	?BISHOP = ?PIECE_TYPE(?BLACK_BISHOP),
	?ROOK   = ?PIECE_TYPE(?BLACK_ROOK),
	?QUEEN  = ?PIECE_TYPE(?BLACK_QUEEN),
	?KING   = ?PIECE_TYPE(?BLACK_KING),
	% TYPE_TO_PIECE
	?WHITE_PAWN   = ?TYPE_TO_PIECE(?PAWN, ?WHITE),
	?WHITE_KNIGHT = ?TYPE_TO_PIECE(?KNIGHT, ?WHITE),
	?WHITE_BISHOP = ?TYPE_TO_PIECE(?BISHOP, ?WHITE),
	?WHITE_ROOK   = ?TYPE_TO_PIECE(?ROOK, ?WHITE),
	?WHITE_QUEEN  = ?TYPE_TO_PIECE(?QUEEN, ?WHITE),
	?WHITE_KING   = ?TYPE_TO_PIECE(?KING, ?WHITE),
	?BLACK_PAWN   = ?TYPE_TO_PIECE(?PAWN, ?BLACK),
	?BLACK_KNIGHT = ?TYPE_TO_PIECE(?KNIGHT, ?BLACK),
	?BLACK_BISHOP = ?TYPE_TO_PIECE(?BISHOP, ?BLACK),
	?BLACK_ROOK   = ?TYPE_TO_PIECE(?ROOK, ?BLACK),
	?BLACK_QUEEN  = ?TYPE_TO_PIECE(?QUEEN, ?BLACK),
	?BLACK_KING   = ?TYPE_TO_PIECE(?KING, ?BLACK),
	ok.
