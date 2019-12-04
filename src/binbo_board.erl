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

-module(binbo_board).

-export([rank_number/1, sq_distance/2]).
-export([rank_of_index/1, file_of_index/1]).
-export([notation_to_index/1, notation_to_index/2]).
-export([index_to_notation/1]).
-export([is_valid_square_notation/1]).
-export([index_list/0, file_list/0, side_list/0, board_tuple/1]).
-export([castling_list/0, castling_rook_squares/1]).
-export([enemy_color/1]).
-export([pieces/0]).
-export([int_move/2, int_move/3, int_move_from/1, int_move_to/1]).

%%%------------------------------------------------------------------------------
%%%   Includes
%%%------------------------------------------------------------------------------

-include("binbo_board.hrl").

%%%------------------------------------------------------------------------------
%%%   Types
%%%------------------------------------------------------------------------------

-type color() :: ?WHITE | ?BLACK.
-type atom_color() :: white | black.
-type rank() :: 0..7.
-type file() :: 0..7.
-type rank_number() :: 1..8.
% -type rank_char() :: $1 | $2 | $3 | $4 | $5 | $6 | $7 | $8.
-type square_index() :: ?A1_IDX .. ?H8_IDX.
-type square_notation() :: <<_:16>>. % a1 .. h8
-type piece() :: ?WHITE_PAWN | ?WHITE_KNIGHT | ?WHITE_BISHOP | ?WHITE_ROOK | ?WHITE_QUEEN | ?WHITE_KING
               | ?BLACK_PAWN | ?BLACK_KNIGHT | ?BLACK_BISHOP | ?BLACK_ROOK | ?BLACK_QUEEN | ?BLACK_KING.
-type piece_type() :: ?PAWN | ?KNIGHT | ?BISHOP | ?ROOK | ?QUEEN | ?KING.
-type empty_square() :: ?EMPTY_SQUARE.
-type unicode_char() :: 9817|9816|9815|9814|9813|9812|9823|9822|9821|9820|9819|9818.
-type distance() :: 0..7.
-type castling_rook_squares() :: {?H1_IDX, ?F1_IDX} | {?H8_IDX, ?F8_IDX} | {?A1_IDX, ?D1_IDX} | {?A8_IDX, ?D8_IDX}.
-type side_castling() :: ?CASTLING_W_OO | ?CASTLING_W_OOO | ?CASTLING_B_OO | ?CASTLING_B_OOO.

-export_type([rank/0, file/0, rank_number/0]).
-export_type([color/0, atom_color/0, piece/0, piece_type/0, empty_square/0]).
-export_type([square_index/0, square_notation/0]).
-export_type([side_castling/0, unicode_char/0]).

%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% rank_of_index/1
-spec rank_of_index(square_index()) -> rank().
rank_of_index(Idx) ->
	(Idx bsr 3).

%% file_of_index/1
-spec file_of_index(square_index()) -> file().
file_of_index(Idx) ->
	(Idx band 7).

%% sq_distance/2
-spec sq_distance(square_index(), square_index()) -> distance().
sq_distance(Idx1, Idx2) ->
	erlang:max(file_distance(Idx1, Idx2), rank_distance(Idx1, Idx2)).

%% rank_number/1
-spec rank_number(square_index()) -> rank_number().
rank_number(Idx) ->
	rank_of_index(Idx) + 1.

%% is_valid_square_notation/1
-spec is_valid_square_notation(binary()) -> boolean().
is_valid_square_notation(Sq) ->
	case Sq of
		<<F:8, R:8>> when (F >= $a) andalso (F =< $h) andalso (R >= $1) andalso (R =< $8) ->
			true;
		_ ->
			false
	end.

%% notation_to_index/1
-spec notation_to_index(square_notation()) -> square_index().
notation_to_index(<<F:8, R:8>>) ->
	notation_to_index(F, R).

%% notation_to_index/2
-spec notation_to_index($a..$h, $1..$8) -> square_index().
notation_to_index(F, R) ->
	Rank = R - $1, % 0..7
	File = F - $a, % 0..7
	(Rank bsl 3) + File.


%% index_to_notation/1
-spec index_to_notation(square_index()) -> square_notation().
index_to_notation(Idx) ->
	Rank = rank_number(Idx) + $0,
	File = file_of_index(Idx) + $a,
	<<File, Rank>>.

%% index_list/0
-spec index_list() -> [square_index()].
index_list() ->
	lists:seq(?A1_IDX, ?H8_IDX, 1).

%% file_list/0
-spec file_list() -> [file()].
file_list() ->
	[0,1,2,3,4,5,6,7].

%% castling_list/0
-spec castling_list() -> [binbo_position:castling()].
castling_list() ->
	lists:seq(?CASTLING_NONE, ?CASTLING_ANY, 1).

%% side_list/0
-spec side_list() -> [color()].
side_list() ->
	[?WHITE, ?BLACK].

%% enemy_color/1
-spec enemy_color(?WHITE) -> ?BLACK; (?BLACK) -> ?WHITE.
enemy_color(Color) ->
	?SWITCH_COLOR(Color).


%% board_tuple/0
-spec board_tuple(term()) -> tuple().
board_tuple(Value) ->
	erlang:make_tuple(?H8_IDX + 1, Value).

%% castling_rook_squares/1
-spec castling_rook_squares(side_castling()) -> castling_rook_squares().
castling_rook_squares(CastlingFlag) ->
	case CastlingFlag of
		?CASTLING_W_OO  -> {?H1_IDX, ?F1_IDX};
		?CASTLING_B_OO  -> {?H8_IDX, ?F8_IDX};
		?CASTLING_W_OOO -> {?A1_IDX, ?D1_IDX};
		?CASTLING_B_OOO -> {?A8_IDX, ?D8_IDX}
	end.

%% pieces/0
-spec pieces() -> [piece()].
pieces() ->
	[?WHITE_PAWN, ?WHITE_KNIGHT, ?WHITE_BISHOP, ?WHITE_ROOK, ?WHITE_QUEEN, ?WHITE_KING,
	 ?BLACK_PAWN, ?BLACK_KNIGHT, ?BLACK_BISHOP, ?BLACK_ROOK, ?BLACK_QUEEN, ?BLACK_KING].

%% int_move/2
-spec int_move(square_index(), square_index()) -> non_neg_integer().
int_move(FromIdx, ToIdx) ->
	(FromIdx bsl 6) + ToIdx.

%% int_move/3
-spec int_move(square_index(), square_index(), ?KNIGHT | ?BISHOP | ?ROOK | ?QUEEN) -> non_neg_integer().
int_move(FromIdx, ToIdx, PromoType) ->
	16384 + ((PromoType - ?KNIGHT) bsl 12) + (FromIdx bsl 6) + ToIdx.

%% int_move_from/1
-spec int_move_from(non_neg_integer()) -> square_index().
int_move_from(Move) ->
	(Move bsr 6) band 16#3F.

%% int_move_to/1
-spec int_move_to(non_neg_integer()) -> square_index().
int_move_to(Move) ->
	Move band 16#3F.


%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

%% file_distance/2
-spec file_distance(square_index(), square_index()) -> distance().
file_distance(Idx1, Idx2) ->
	erlang:abs(file_of_index(Idx1) - file_of_index(Idx2)).

%% rank_distance/2
-spec rank_distance(square_index(), square_index()) -> distance().
rank_distance(Idx1, Idx2) ->
	erlang:abs(rank_of_index(Idx1) - rank_of_index(Idx2)).
