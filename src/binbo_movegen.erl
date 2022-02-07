%% Copyright (c) 2019-2022, Sergei Semichev <chessvegas@chessvegas.com>. All Rights Reserved.
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

-module(binbo_movegen).

-export([all_valid_moves/2, all_valid_moves/3]).
-export([has_valid_moves/2]).


%%%------------------------------------------------------------------------------
%%%   Includes
%%%------------------------------------------------------------------------------

-include("binbo_board.hrl").


%%%------------------------------------------------------------------------------
%%%   Types
%%%------------------------------------------------------------------------------

-type bb() :: binbo_bb:bb().
-type piece() :: binbo_board:piece().
-type piece_type() :: binbo_board:piece_type().
-type color() :: binbo_board:color().
-type bb_game() :: binbo_position:bb_game().
-type sq_idx() :: binbo_board:square_index().
-type sq_notation() :: binbo_board:square_notation().
-type many() :: all | any.
-type move_type() :: int | bin | str | bb | count | bitint.
-type promo_to() :: 'q' | 'r' | 'b' | 'n'.
-type int_move() :: {sq_idx(), sq_idx()} | {sq_idx(), sq_idx(), promo_to()}.
-type bin_move() :: {sq_notation(), sq_notation()} | {sq_notation(), sq_notation(), promo_to()}.
-type str_move() :: {string(), string()} | {string(), string(), promo_to()}.
-type all_valid_moves() :: [int_move()] | [bin_move()] | [str_move()] | bb() | non_neg_integer() | [non_neg_integer()].

-export_type([move_type/0, int_move/0, bin_move/0, str_move/0]).

%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% all_valid_moves/2
-spec all_valid_moves(bb_game(), int) -> [int_move()]
				;    (bb_game(), bin) -> [bin_move()]
				;    (bb_game(), str) -> [str_move()]
				;    (bb_game(), bb) -> bb()
				;    (bb_game(), count) -> non_neg_integer()
				;    (bb_game(), bitint) -> [non_neg_integer()].
all_valid_moves(Game, MoveType) ->
	Color = binbo_position:get_sidetomove(Game),
	all_valid_moves(Color, Game, MoveType).


%% all_valid_moves/3
-spec all_valid_moves(color(), bb_game(), int) -> [int_move()]
				;    (color(), bb_game(), bin) -> [bin_move()]
				;    (color(), bb_game(), str) -> [str_move()]
				;    (color(), bb_game(), bb) -> bb()
				;    (color(), bb_game(), count) -> non_neg_integer()
				;    (color(), bb_game(), bitint) -> [non_neg_integer()].
all_valid_moves(Color, Game, MoveType) ->
	valid_moves(Color, Game, all, MoveType).

%% has_valid_moves/2
-spec has_valid_moves(color(), bb_game()) -> boolean().
has_valid_moves(Color, Game) ->
	(any_valid_move(Color, Game) > 0).


%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

%% any_valid_move/2
-spec any_valid_move(color(), bb_game()) -> non_neg_integer().
any_valid_move(Color, Game) ->
	valid_moves(Color, Game, any, count).

%% valid_moves/4
-spec valid_moves(color(), bb_game(), many(), int) -> [int_move()]
				; (color(), bb_game(), many(), bin) -> [bin_move()]
				; (color(), bb_game(), many(), str) -> [str_move()]
				; (color(), bb_game(), many(), bb) -> bb()
				; (color(), bb_game(), many(), count) -> non_neg_integer()
				; (color(), bb_game(), many(), bitint) -> [non_neg_integer()].
valid_moves(Color, Game, Many, MoveType) ->
	FromSquaresBB = binbo_position:own_side_bb(Color, Game),
	valid_moves_from(FromSquaresBB, Game, Many, MoveType).

%% valid_moves_from/4
-spec valid_moves_from(bb(), bb_game(), many(), int) -> [int_move()]
					; (bb(), bb_game(), many(), bin) -> [bin_move()]
					; (bb(), bb_game(), many(), str) -> [str_move()]
					; (bb(), bb_game(), many(), bb) -> bb()
					; (bb(), bb_game(), many(), count) -> non_neg_integer()
					; (bb(), bb_game(), many(), bitint) -> [non_neg_integer()].
valid_moves_from(FromSquaresBB, Game, Many, MoveType) ->
	MovesAcc0 = case MoveType of
		count -> 0;
		bb    -> ?EMPTY_BB;
		_     -> []
	end,
	acc_valid_moves_from(FromSquaresBB, Game, Many, MoveType, MovesAcc0).


%% acc_valid_moves_from/5
-spec acc_valid_moves_from(bb(), bb_game(), many(), move_type(), all_valid_moves()) -> all_valid_moves().
acc_valid_moves_from(0, _Game, _Many, _MoveType, MovesAcc) ->
	MovesAcc;
acc_valid_moves_from(FromSquaresBB, Game, Many, MoveType, MovesAcc) ->
	FromBB = FromSquaresBB band (-FromSquaresBB), % clear all bits but the least significant one bit
	FromIdx = binbo_bb:to_index(FromBB),
	RestBB = FromSquaresBB bxor FromBB,
	Piece = binbo_position:get_piece(FromIdx, Game),
	MovesAcc2 = add_valid_piece_moves(FromIdx, Piece, Game, Many, MoveType, MovesAcc),
	case (Many =:= any) andalso (MovesAcc2 =/= ?EMPTY_BB) andalso (MovesAcc2 =/= []) of
		true ->
			MovesAcc2;
		false ->
			acc_valid_moves_from(RestBB, Game, Many, MoveType, MovesAcc2)
	end.

%% add_valid_piece_moves/6
-spec add_valid_piece_moves(sq_idx(), piece(), bb_game(), many(), move_type(), all_valid_moves()) -> all_valid_moves().
add_valid_piece_moves(FromIdx, Piece, Game, Many, MoveType, MovesAcc) ->
	ToSquaresBB = position_piece_moves_bb(FromIdx, Piece, Game),
	add_valid_piece_moves_from_to(FromIdx, ToSquaresBB, Piece, Game, Many, MoveType, MovesAcc).


%% add_valid_piece_moves_from_to/7
-spec add_valid_piece_moves_from_to(sq_idx(), bb(), piece(), bb_game(), many(), move_type(), all_valid_moves()) -> all_valid_moves().
add_valid_piece_moves_from_to(_FromIdx, 0, _Piece, _Game, _Many, _MoveType, MovesAcc) ->
	MovesAcc;
add_valid_piece_moves_from_to(FromIdx, ToSquaresBB, Piece, Game, Many, MoveType, MovesAcc) ->
	ToBB = ToSquaresBB band (-ToSquaresBB), % clear all bits but the least significant one bit
	ToIdx = binbo_bb:to_index(ToBB),
	RestBB = ToSquaresBB bxor ToBB,
	Validate = binbo_move:validate_move(Game, Piece, FromIdx, ToIdx, ?QUEEN),
	case Validate of
		{ok, _, _} ->
			case MoveType of
				count when (Many =:= any) ->
					1;
				bb when (Many =:= any) ->
					ToBB;
				bb ->
					MovesAcc2 = MovesAcc bor ToBB,
					add_valid_piece_moves_from_to(FromIdx, RestBB, Piece, Game, Many, MoveType, MovesAcc2);
				_ ->
					Ptype = ?PIECE_TYPE(Piece),
					MovesAcc2 = maybe_movelist_with_promotion(Ptype, FromIdx, ToIdx, MoveType, MovesAcc),
					add_valid_piece_moves_from_to(FromIdx, RestBB, Piece, Game, Many, MoveType, MovesAcc2)
			end;
		{error, _} ->
			add_valid_piece_moves_from_to(FromIdx, RestBB, Piece, Game, Many, MoveType, MovesAcc)
	end.

%% position_piece_moves_bb/3
-spec position_piece_moves_bb(sq_idx(), piece(), bb_game()) -> bb().
position_piece_moves_bb(FromIdx, Piece, Game) ->
	Ptype = ?PIECE_TYPE(Piece),
	Pcolor = ?COLOR(Piece),
	binbo_position:piece_moves_bb(FromIdx, Ptype, Pcolor, Game).


%% maybe_movelist_with_promotion/5
-spec maybe_movelist_with_promotion(piece_type(), sq_idx(), sq_idx(), int, [int_move()]) -> [int_move()]
								;  (piece_type(), sq_idx(), sq_idx(), bin, [bin_move()]) -> [bin_move()]
								;  (piece_type(), sq_idx(), sq_idx(), str, [str_move()]) -> [str_move()]
								;  (piece_type(), sq_idx(), sq_idx(), count, non_neg_integer()) -> pos_integer()
								;  (piece_type(), sq_idx(), sq_idx(), bitint, [non_neg_integer()]) -> [non_neg_integer()].
maybe_movelist_with_promotion(Ptype, FromIdx, ToIdx, MoveType, MovesAcc) ->
	IsPromotion = case (Ptype =:= ?PAWN) of
		true  ->
			ToBB = ?SQUARE_BB(ToIdx),
			_ = ?IS_AND(ToBB, ?RANK_1_BB bor ?RANK_8_BB);
		false ->
			false
	end,
	case MoveType of
		bitint ->
			case IsPromotion of
				false ->
					[binbo_board:int_move(FromIdx, ToIdx) | MovesAcc];
				true  ->
					[binbo_board:int_move(FromIdx, ToIdx, ?QUEEN), binbo_board:int_move(FromIdx, ToIdx, ?ROOK), binbo_board:int_move(FromIdx, ToIdx, ?BISHOP), binbo_board:int_move(FromIdx, ToIdx, ?KNIGHT) | MovesAcc]
			end;
		count ->
			case IsPromotion of
				false ->
					MovesAcc + 1;
				true  ->
					MovesAcc + 4
			end;
		_ ->
			{From, To} = case MoveType of
				int ->
					{FromIdx, ToIdx};
				bin ->
					{binbo_board:index_to_notation(FromIdx), binbo_board:index_to_notation(ToIdx)};
				str ->
					{erlang:binary_to_list(binbo_board:index_to_notation(FromIdx)), erlang:binary_to_list(binbo_board:index_to_notation(ToIdx))}
			end,
			case IsPromotion of
				false ->
					[{From, To} | MovesAcc];
				true  ->
					[{From, To, q}, {From, To, r}, {From, To, b}, {From, To, n} | MovesAcc]
			end
	end.
