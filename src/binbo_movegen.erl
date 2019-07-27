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

-export([all_valid_moves/2, has_valid_moves/2]).


%%%------------------------------------------------------------------------------
%%%   Includes
%%%------------------------------------------------------------------------------

-include("binbo_board.hrl").


%%%------------------------------------------------------------------------------
%%%   Types
%%%------------------------------------------------------------------------------

-type bb() :: binbo_bb:bb().
-type piece() :: binbo_board:piece().
-type color() :: binbo_board:color().
-type bb_game() :: binbo_position:bb_game().
-type sq_idx() :: binbo_board:square_index().
-type many() :: all | any.

%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% all_valid_moves/2
%% Note: function 'all_valid_moves/2' returns just a bitboard of all *target* squares.
%% If it should be used for a chess engine, it should be reworked to return a desired value.
-spec all_valid_moves(color(), bb_game()) -> bb().
all_valid_moves(Color, Game) ->
	valid_moves(Color, Game, all).

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
	valid_moves(Color, Game, any).

%% valid_moves/3
-spec valid_moves(color(), bb_game(), many()) -> bb().
valid_moves(Color, Game, Many) ->
	FromSquares = binbo_position:get_side_indexes(Color, Game),
	valid_moves_from(FromSquares, Game, Many).

%% valid_moves_from/3
-spec valid_moves_from([sq_idx()], bb_game(), many()) -> bb().
valid_moves_from(FromSquares, Game, Many) ->
	valid_moves_from(FromSquares, Game, Many, ?EMPTY_BB).

%% valid_moves_from/4
-spec valid_moves_from([sq_idx()], bb_game(), many(), bb()) -> bb().
valid_moves_from([], _Game, _Many, MovesBB) ->
	MovesBB;
valid_moves_from([FromIdx | Tail], Game, Many, MovesBB) ->
	Piece = binbo_position:get_piece(FromIdx, Game),
	true = ?IS_PIECE(Piece), % ensure piece
	PieceMovesBB = valid_piece_moves(FromIdx, Piece, Game, Many),
	case PieceMovesBB > ?EMPTY_BB of
		true when (Many =:= any) ->
			PieceMovesBB;
		true ->
			valid_moves_from(Tail, Game, Many, MovesBB bor PieceMovesBB);
		false ->
			valid_moves_from(Tail, Game, Many, MovesBB)
	end.

%% valid_piece_moves/4
-spec valid_piece_moves(sq_idx(), piece(), bb_game(), many()) -> bb().
valid_piece_moves(FromIdx, Piece, Game, Many) ->
	PieceMovesBB = position_piece_moves_bb(FromIdx, Piece, Game),
	ToSquares = binbo_bb:to_index_list(PieceMovesBB),
	valid_piece_moves_from_to(FromIdx, ToSquares, Piece, Game, Many, ?EMPTY_BB).


%% valid_piece_moves_from_to/6
-spec valid_piece_moves_from_to(sq_idx(), [sq_idx()], piece(), bb_game(), many(), bb()) -> bb().
valid_piece_moves_from_to(_FromIdx, [], _Piece, _Game, _Many, MovesBB) ->
	MovesBB;
valid_piece_moves_from_to(FromIdx, [ToIdx | Tail], Piece, Game, Many, MovesBB) ->
	Validate = binbo_move:validate_move(Game, Piece, FromIdx, ToIdx, ?QUEEN),
	case Validate of
		{ok, _, _} ->
			case Many of
				any ->
					?SQUARE_BB(ToIdx);
				all ->
					MovesBB2 = MovesBB bor ?SQUARE_BB(ToIdx),
					valid_piece_moves_from_to(FromIdx, Tail, Piece, Game, Many, MovesBB2)
			end;
		{error, _} ->
			valid_piece_moves_from_to(FromIdx, Tail, Piece, Game, Many, MovesBB)
	end.

%% position_piece_moves_bb/3
-spec position_piece_moves_bb(sq_idx(), piece(), bb_game()) -> bb().
position_piece_moves_bb(FromIdx, Piece, Game) ->
	Ptype = ?PIECE_TYPE(Piece),
	Pcolor = ?COLOR(Piece),
	binbo_position:piece_moves_bb(FromIdx, Ptype, Pcolor, Game).
