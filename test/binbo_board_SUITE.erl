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

-module(binbo_board_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-define(value(Key,Config), proplists:get_value(Key,Config)).


%% all/0
all() -> [board_tuple_test, index_list_test, file_list_test, side_list_test,
			enemy_color_test, castling_list_test, rank_of_index_test, file_of_index_test,
			sq_distance_test, is_valid_square_notation_test, notation_to_index_test,
			index_to_notation_test, castling_rook_squares_test
		].

%% init_per_suite/1
init_per_suite(Config) ->
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
	64 = erlang:tuple_size(Tuple).

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
	] = List.

%% file_list_test/1
file_list_test(_Config) ->
	[0,1,2,3,4,5,6,7] = binbo_board:file_list().

%% side_list_test/1
side_list_test(Config) ->
	White = ?value(white, Config),
	Black = ?value(black, Config),
	[White, Black] = binbo_board:side_list().

%% enemy_color_test/1
enemy_color_test(Config) ->
	White = ?value(white, Config),
	Black = ?value(black, Config),
	Black = binbo_board:enemy_color(White),
	White = binbo_board:enemy_color(Black).

%% castling_list_test/1
castling_list_test(_Config) ->
	[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15] = binbo_board:castling_list().

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
	end, lists:seq(0, 64, 1)).

%% file_of_index_test/1
file_of_index_test(Config) ->
	0 = binbo_board:file_of_index(?value(a1, Config)),
	1 = binbo_board:file_of_index(?value(b2, Config)),
	2 = binbo_board:file_of_index(?value(c3, Config)),
	3 = binbo_board:file_of_index(?value(d4, Config)),
	4 = binbo_board:file_of_index(?value(e5, Config)),
	5 = binbo_board:file_of_index(?value(f6, Config)),
	6 = binbo_board:file_of_index(?value(g7, Config)),
	7 = binbo_board:file_of_index(?value(h8, Config)).

%% sq_distance_test/1
sq_distance_test(Config) ->
	0 = binbo_board:sq_distance(?value(a1, Config), ?value(a1, Config)),
	1 = binbo_board:sq_distance(?value(a1, Config), ?value(b2, Config)),
	2 = binbo_board:sq_distance(?value(a1, Config), ?value(c3, Config)),
	3 = binbo_board:sq_distance(?value(a1, Config), ?value(d4, Config)),
	4 = binbo_board:sq_distance(?value(a1, Config), ?value(e5, Config)),
	5 = binbo_board:sq_distance(?value(a1, Config), ?value(f6, Config)),
	6 = binbo_board:sq_distance(?value(a1, Config), ?value(g7, Config)),
	7 = binbo_board:sq_distance(?value(a1, Config), ?value(h8, Config)).

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
	false = binbo_board:is_valid_square_notation(<<"i1">>).

%% notation_to_index_test/1
notation_to_index_test(_Config) ->
	 0 = binbo_board:notation_to_index(<<"a1">>),
	 9 = binbo_board:notation_to_index(<<"b2">>),
	18 = binbo_board:notation_to_index(<<"c3">>),
	27 = binbo_board:notation_to_index(<<"d4">>),
	36 = binbo_board:notation_to_index(<<"e5">>),
	45 = binbo_board:notation_to_index(<<"f6">>),
	54 = binbo_board:notation_to_index(<<"g7">>),
	63 = binbo_board:notation_to_index(<<"h8">>).

%% index_to_notation_test/1
index_to_notation_test(_Config) ->
	<<"a1">> = binbo_board:index_to_notation(0),
	<<"b2">> = binbo_board:index_to_notation(9),
	<<"c3">> = binbo_board:index_to_notation(18),
	<<"d4">> = binbo_board:index_to_notation(27),
	<<"e5">> = binbo_board:index_to_notation(36),
	<<"f6">> = binbo_board:index_to_notation(45),
	<<"g7">> = binbo_board:index_to_notation(54),
	<<"h8">> = binbo_board:index_to_notation(63).

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
	{A8, D8} = binbo_board:castling_rook_squares(CASTLING_B_OOO).
