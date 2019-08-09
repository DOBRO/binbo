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
-type move_tuple() :: {sq_idx(), piece_type(), [sq_idx()]}.
-type promo_to() :: 'q' | 'r' | 'b' | 'n'.
-type int_move() :: {sq_idx(), sq_idx()} | {sq_idx(), sq_idx(), promo_to()}.
-type bin_move() :: {sq_notation(), sq_notation()} | {sq_notation(), sq_notation(), promo_to()}.
-type str_move() :: {string(), string()} | {string(), string(), promo_to()}.

-export_type([int_move/0, bin_move/0, str_move/0]).

%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% all_valid_moves/2
-spec all_valid_moves(bb_game(), int) -> [int_move()]
				;    (bb_game(), bin) -> [bin_move()]
				;    (bb_game(), str) -> [str_move()].
all_valid_moves(Game, MoveType) ->
	Color = binbo_position:get_sidetomove(Game),
	all_valid_moves(Color, Game, MoveType).


%% all_valid_moves/3
-spec all_valid_moves(color(), bb_game(), int) -> [int_move()]
				;    (color(), bb_game(), bin) -> [bin_move()]
				;    (color(), bb_game(), str) -> [str_move()].
all_valid_moves(Color, Game, MoveType) ->
	List = valid_moves(Color, Game, all, list),
	bblist_to_movelist(List, MoveType).

%% has_valid_moves/2
-spec has_valid_moves(color(), bb_game()) -> boolean().
has_valid_moves(Color, Game) ->
	(any_valid_move(Color, Game) > ?EMPTY_BB).


%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

%% any_valid_move/2
-spec any_valid_move(color(), bb_game()) -> bb().
any_valid_move(Color, Game) ->
	valid_moves(Color, Game, any, bb).

%% valid_moves/4
-spec valid_moves(color(), bb_game(), many(), bb) -> bb()
				; (color(), bb_game(), many(), list) -> [move_tuple()].
valid_moves(Color, Game, Many, DataType) ->
	FromSquares = binbo_position:get_side_indexes(Color, Game),
	valid_moves_from(FromSquares, Game, Many, DataType).

%% valid_moves_from/4
-spec valid_moves_from([sq_idx()], bb_game(), many(), bb) -> bb()
					; ([sq_idx()], bb_game(), many(), list) -> [move_tuple()].
valid_moves_from(FromSquares, Game, Many, DataType) ->
	case DataType of
		bb   -> acc_valid_moves_from(FromSquares, Game, Many, ?EMPTY_BB);
		list -> acc_valid_moves_from(FromSquares, Game, Many, [])
	end.


%% acc_valid_moves_from/4
-spec acc_valid_moves_from([sq_idx()], bb_game(), many(), bb()) -> bb()
					; ([sq_idx()], bb_game(), many(), [move_tuple()]) -> [move_tuple()].
acc_valid_moves_from([], _Game, _Many, MovesAcc) ->
	MovesAcc;
acc_valid_moves_from([FromIdx | Tail], Game, Many, MovesAcc) ->
	Piece = binbo_position:get_piece(FromIdx, Game),
	true = ?IS_PIECE(Piece), % ensure piece
	{PieceMovesBB, PieceMovesList} = valid_piece_moves(FromIdx, Piece, Game, Many),
	case PieceMovesBB > ?EMPTY_BB of
		true when (Many =:= any) ->
			PieceMovesBB;
		true when is_integer(MovesAcc) ->
			acc_valid_moves_from(Tail, Game, Many, MovesAcc bor PieceMovesBB);
		true when is_list(MovesAcc) ->
			acc_valid_moves_from(Tail, Game, Many, [{FromIdx, ?PIECE_TYPE(Piece), PieceMovesList} | MovesAcc]);
		false ->
			acc_valid_moves_from(Tail, Game, Many, MovesAcc)
	end.

%% valid_piece_moves/4
-spec valid_piece_moves(sq_idx(), piece(), bb_game(), many()) -> {bb(), [sq_idx()]}.
valid_piece_moves(FromIdx, Piece, Game, Many) ->
	PieceMovesBB = position_piece_moves_bb(FromIdx, Piece, Game),
	ToSquares = binbo_bb:to_index_list(PieceMovesBB),
	valid_piece_moves_from_to(FromIdx, ToSquares, Piece, Game, Many, {?EMPTY_BB, []}).


%% valid_piece_moves_from_to/6
-spec valid_piece_moves_from_to(sq_idx(), [sq_idx()], piece(), bb_game(), many(), {bb(), [sq_idx()]}) -> {bb(), [sq_idx()]}.
valid_piece_moves_from_to(_FromIdx, [], _Piece, _Game, _Many, Acc) ->
	Acc;
valid_piece_moves_from_to(FromIdx, [ToIdx | Tail], Piece, Game, Many, {MovesBB, Movelist} = Acc) ->
	ToBB = ?SQUARE_BB(ToIdx),
	Validate = binbo_move:validate_move(Game, Piece, FromIdx, ToIdx, ?QUEEN),
	case Validate of
		{ok, _, _} ->
			case Many of
				any ->
					{ToBB, [ToIdx]};
				all ->
					MovesBB2 = MovesBB bor ToBB,
					valid_piece_moves_from_to(FromIdx, Tail, Piece, Game, Many, {MovesBB2, [ToIdx | Movelist]})
			end;
		{error, _} ->
			valid_piece_moves_from_to(FromIdx, Tail, Piece, Game, Many, Acc)
	end.

%% position_piece_moves_bb/3
-spec position_piece_moves_bb(sq_idx(), piece(), bb_game()) -> bb().
position_piece_moves_bb(FromIdx, Piece, Game) ->
	Ptype = ?PIECE_TYPE(Piece),
	Pcolor = ?COLOR(Piece),
	binbo_position:piece_moves_bb(FromIdx, Ptype, Pcolor, Game).

%% bblist_to_movelist/2
-spec bblist_to_movelist([move_tuple()], int) -> [int_move()]
					;   ([move_tuple()], bin) -> [bin_move()]
					;   ([move_tuple()], str) -> [str_move()].
bblist_to_movelist(List, Movetype) ->
	bblist_to_movelist(List, Movetype, []).

%% bblist_to_movelist/3
-spec bblist_to_movelist([move_tuple()], int, [int_move()]) -> [int_move()]
					;   ([move_tuple()], bin, [bin_move()]) -> [bin_move()]
					;   ([move_tuple()], str, [str_move()]) -> [str_move()].
bblist_to_movelist([], _Movetype, Movelist) ->
	Movelist;
bblist_to_movelist([{FromIdx, Ptype, IdxList} | Tail], Movetype, Movelist) ->
	% IdxList = binbo_bb:to_index_list(MovesBB),
	Movelist2 = lists:foldl(fun(ToIdx, Acc) ->
		maybe_movelist_with_promotion(Ptype, FromIdx, ToIdx, Movetype, Acc)
	end, Movelist, IdxList),
	bblist_to_movelist(Tail, Movetype, Movelist2).

%% maybe_movelist_with_promotion/5
-spec maybe_movelist_with_promotion(piece_type(), sq_idx(), sq_idx(), int, [int_move()]) -> [int_move()]
								;  (piece_type(), sq_idx(), sq_idx(), bin, [bin_move()]) -> [bin_move()]
								;  (piece_type(), sq_idx(), sq_idx(), str, [str_move()]) -> [str_move()].
maybe_movelist_with_promotion(Ptype, FromIdx, ToIdx, Movetype, Movelist) ->
	IsPromotion = case (Ptype =:= ?PAWN) of
		true  ->
			ToBB = ?SQUARE_BB(ToIdx),
			_ = ?IS_AND(ToBB, ?RANK_8_BB) orelse ?IS_AND(ToBB, ?RANK_1_BB);
		false ->
			false
	end,
	{From, To} = case Movetype of
		int ->
			{FromIdx, ToIdx};
		bin ->
			{binbo_board:index_to_notation(FromIdx), binbo_board:index_to_notation(ToIdx)};
		str ->
			{erlang:binary_to_list(binbo_board:index_to_notation(FromIdx)), erlang:binary_to_list(binbo_board:index_to_notation(ToIdx))}
	end,
	case IsPromotion of
		false ->
			[{From, To} | Movelist];
		true  ->
			[{From, To, q}, {From, To, r}, {From, To, b}, {From, To, n} | Movelist]
	end.
