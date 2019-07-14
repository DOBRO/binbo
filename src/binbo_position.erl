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

-module(binbo_position).

-export([init_bb_game/1, validate_loaded_fen/1]).
-export([get_piece/2, get_sidetomove/1]).
-export([get_status/1, with_status/3, is_status_inprogress/1, manual_draw/2]).
-export([make_move/2, finalize_move/2]).
-export([get_fen/1, pretty_board/2]).
-export([
	pawn_moves_bb/3,
	knight_moves_bb/3,
	bishop_moves_bb/2,
	rook_moves_bb/2,
	queen_moves_bb/2,
	king_moves_bb/3
]).
-export([is_in_check/2]).
-export([get_enpassant_bb/1]).
-export([get_side_indexes/2]).

%%%------------------------------------------------------------------------------
%%%   Includes
%%%------------------------------------------------------------------------------

-include("binbo_position.hrl").
-include("binbo_fen.hrl").
-include("binbo_move.hrl").

%%%------------------------------------------------------------------------------
%%%   Types
%%%------------------------------------------------------------------------------

-type bb() :: binbo_bb:bb().
-type sq_bb() :: binbo_bb:sq_bb().
-type color() :: binbo_board:color().
-type piece() :: binbo_board:piece().
-type parsed_fen() :: #parsed_fen{}.
-type pchar() :: binbo_fen:piece_char().
-type uchar() :: binbo_board:unicode_char().
-type sq_idx() :: binbo_board:square_index().
-type empty_sq() :: binbo_board:empty_square().
-type castling() :: ?CASTLING_NONE .. ?CASTLING_ANY.
-type side_bb() :: ?A1_BB .. ?ALL_SQUARES_BB.
-type empty_bb() :: binbo_bb:empty_bb().
-type enpa_bb() :: binbo_bb:enpa_bb().
-type oo_move_bb() :: ?G1_BB | ?G8_BB | empty_bb().
-type ooo_move_bb() :: ?C1_BB | ?C8_BB | empty_bb().
-type hash() :: binbo_hash:hash().
-type hashmap() :: #{hash() => pos_integer()}.
-type game_status_inprogress() :: ?GAME_STATUS_INPROGRESS.
-type game_status_checkmate() :: ?GAME_STATUS_CHECKMATE.
-type game_draw_stalemate() :: ?GAME_STATUS_DRAW_STALEMATE.
-type game_draw_rule50() :: ?GAME_STATUS_DRAW_RULE50.
-type game_draw_material() :: ?GAME_STATUS_DRAW_MATERIAL.
-type why_draw() :: game_draw_stalemate()
				| game_draw_rule50()
				| game_draw_material()
				| {manual, term()}.
-type game_status_draw() :: {draw, why_draw()}.
-type game_over_status() :: game_status_checkmate() | game_status_draw().
-type game_status() :: game_status_inprogress() | game_over_status().
-type halfmove() :: binbo_fen:halfmove().
-type fullmove() :: binbo_fen:fullmove().

-type bb_game() :: #{
	?GAME_KEY_WP := bb(), ?GAME_KEY_WN := bb(), ?GAME_KEY_WB := bb(),
	?GAME_KEY_WR := bb(), ?GAME_KEY_WQ := bb(), ?GAME_KEY_WK := bb(),
	?GAME_KEY_BP := bb(), ?GAME_KEY_BN := bb(), ?GAME_KEY_BB := bb(),
	?GAME_KEY_BR := bb(), ?GAME_KEY_BQ := bb(), ?GAME_KEY_BK := bb(),
	?GAME_KEY_WHITE	 := bb(), ?GAME_KEY_BLACK := bb(),
	?GAME_KEY_OCCUPIED := bb(),
	?GAME_KEY_SIDETOMOVE := undefined | color(),
	?GAME_KEY_CASTLING := castling(),
	?GAME_KEY_ENPASSANT := enpa_bb(),
	?GAME_KEY_HALFMOVE := halfmove(),
	?GAME_KEY_FULLMOVE := fullmove(),
	?GAME_KEY_LASTMOVE := undefined | {sq_idx(), sq_idx()},
	?GAME_KEY_LASTMOVE_PIECE := piece() | empty_sq(),
	?GAME_KEY_POS_HASH := 0 | hash(),
	?GAME_KEY_POSITION_HASHMAP := hashmap(),
	?GAME_KEY_STATUS := game_status(),
	sq_idx() => piece()
}.

-type empty_bb_game() :: #{
	?GAME_KEY_WP := empty_bb(), ?GAME_KEY_WN := empty_bb(), ?GAME_KEY_WB := empty_bb(),
	?GAME_KEY_WR := empty_bb(), ?GAME_KEY_WQ := empty_bb(), ?GAME_KEY_WK := empty_bb(),
	?GAME_KEY_BP := empty_bb(), ?GAME_KEY_BN := empty_bb(), ?GAME_KEY_BB := empty_bb(),
	?GAME_KEY_BR := empty_bb(), ?GAME_KEY_BQ := empty_bb(), ?GAME_KEY_BK := empty_bb(),
	?GAME_KEY_WHITE := empty_bb(), ?GAME_KEY_BLACK := empty_bb(),
	?GAME_KEY_OCCUPIED := empty_bb(),
	?GAME_KEY_SIDETOMOVE := undefined,
	?GAME_KEY_CASTLING := ?CASTLING_NONE,
	?GAME_KEY_ENPASSANT := empty_bb(),
	?GAME_KEY_HALFMOVE := 0,
	?GAME_KEY_FULLMOVE := 1,
	?GAME_KEY_LASTMOVE := undefined,
	?GAME_KEY_LASTMOVE_PIECE := empty_sq(),
	?GAME_KEY_POS_HASH := 0,
	?GAME_KEY_POSITION_HASHMAP := hashmap(),
	?GAME_KEY_STATUS := game_status_inprogress()
}.

-type castling_error() :: {white|black, both_sides|king_side|queen_side}.
-type bb_game_error() :: bb_kings_too_close | bb_edge_rank_occupied_by_pawns
						| bb_invalid_enpassant | {castling, castling_error()}.

-type pretty_board_opts() :: [flip|unicode].
-type move_info() :: binbo_move:move_info().
-type unset_castling_flag() :: ?CASTLING_W_ANY | ?CASTLING_B_ANY | binbo_board:side_castling().
-type make_move_step() :: remove_piece | set_piece | is_in_check | enpassant.
-type finalize_move_step() :: lastmove | sidetomove | halfmove | fullmove | hashmap | status.
-type make_move_error() :: own_king_in_check.

-export_type([bb_game/0, castling/0, pretty_board_opts/0]).
-export_type([bb_game_error/0, make_move_error/0]).
-export_type([game_status/0, game_over_status/0]).

%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% init_bb_game
-spec init_bb_game(parsed_fen()) -> bb_game().
init_bb_game(ParsedFen) ->
	load_parsed_fen(ParsedFen, empty_bb_game()).


%% get_piece/2
-spec get_piece(sq_idx(), bb_game()) -> piece() | empty_sq().
get_piece(Idx, Game) ->
	maps:get(Idx, Game, ?EMPTY_SQUARE).


%% get_sidetomove/1
-spec get_sidetomove(bb_game()) -> color().
get_sidetomove(Game) ->
	maps:get(?GAME_KEY_SIDETOMOVE, Game).

%% set_sidetomove/2
-spec set_sidetomove(color(), bb_game()) -> bb_game().
set_sidetomove(Color, Game) ->
	PosHash = maps:get(?GAME_KEY_POS_HASH, Game),
	PosHash2 = PosHash bxor binbo_hash:side_hash(Color),
	Game#{?GAME_KEY_SIDETOMOVE := Color, ?GAME_KEY_POS_HASH := PosHash2}.

%% get_status/1
-spec get_status(bb_game()) -> game_status().
get_status(Game) ->
	maps:get(?GAME_KEY_STATUS, Game).

%% is_rule50/1
%% https://en.wikipedia.org/wiki/Fifty-move_rule
-spec is_rule50(bb_game()) -> boolean().
is_rule50(Game) ->
	case (get_halfmove(Game) < 50) of
		true  ->
			false;
		false ->
			case (get_fullmove(Game) < 75) of
				true  -> false;
				false -> true
			end
	end.

%% is_k_vs_k/2
%% Returns true when:
%% King versus King.
-spec is_k_vs_k(bb(), bb()) -> boolean().
is_k_vs_k(Kings, AllPieces) ->
	(AllPieces bxor Kings) =:= ?EMPTY_BB.

%% is_kb_vs_k/2
%% Returns true when:
%% King and Bishop versus King.
-spec is_kb_vs_k(bb(), bb(), bb_game()) -> boolean().
is_kb_vs_k(Kings, AllPieces, Game) ->
	AllBishops = white_bishops_bb(Game) bor black_bishops_bb(Game),
	(((Kings bor AllBishops) bxor AllPieces) =:= ?EMPTY_BB)
	andalso
	uef_num:popcount(AllBishops) =:= 1.

%% is_kn_vs_k/2
%% Returns true when:
%% King and Knight versus King
-spec is_kn_vs_k(bb(), bb(), bb_game()) -> boolean().
is_kn_vs_k(Kings, AllPieces, Game) ->
	AllKnights = white_knights_bb(Game) bor black_knights_bb(Game),
	(((Kings bor AllKnights) bxor AllPieces) =:= ?EMPTY_BB)
	andalso
	uef_num:popcount(AllKnights) =:= 1.

%% is_kb_vs_kb/2
%% Returns true when:
%% King and Bishop versus King and Bishop with the Bishops on the same color.
-spec is_kb_vs_kb(bb(), bb(), bb_game()) -> boolean().
is_kb_vs_kb(Kings, AllPieces, Game) ->
	WBishops = white_bishops_bb(Game),
	BBishops = black_bishops_bb(Game),
	AllBishops = WBishops bor BBishops,
	(((Kings bor AllBishops) bxor AllPieces) =:= ?EMPTY_BB)
	andalso
	(
		((AllBishops band ?DARK_SQUARES_BB)  =:= AllBishops)
		orelse
		((AllBishops band ?LIGHT_SQUARES_BB) =:= AllBishops)
	)
	andalso
	(uef_num:popcount(WBishops) =:= 1)
	andalso
	(uef_num:popcount(BBishops) =:= 1).


%% is_insufficient_material/1
-spec is_insufficient_material(bb_game()) -> boolean().
is_insufficient_material(Game) ->
	WKing = white_king_bb(Game),
	BKing = black_king_bb(Game),
	Kings = WKing bor BKing,
	AllPieces = all_pieces_bb(Game),
	% King versus King
	is_k_vs_k(Kings, AllPieces)
	orelse
	% King and Bishop versus King
	is_kb_vs_k(Kings, AllPieces, Game)
	orelse
	% King and Knight versus King
	is_kn_vs_k(Kings, AllPieces, Game)
	orelse
	% King and Bishop versus King and Bishop with the Bishops on the same color
	is_kb_vs_kb(Kings, AllPieces, Game).


%% maybe_draw/1
-spec maybe_draw(bb_game()) -> false | {true, game_draw_rule50() | game_draw_material()}.
maybe_draw(Game) ->
	case is_rule50(Game) of
		true  ->
			{true, ?GAME_STATUS_DRAW_RULE50};
		false ->
			case is_insufficient_material(Game) of
				true  -> {true, ?GAME_STATUS_DRAW_MATERIAL};
				false -> false
			end
	end.

%% with_status/3
-spec with_status(bb_game(), boolean(), boolean()) -> bb_game().
with_status(Game, HasValidMoves, IsCheck) ->
	case HasValidMoves of
		true  ->
			case maybe_draw(Game) of
				false           -> Game;
				{true, WhyDraw} -> set_status_draw(WhyDraw, Game)
			end;
		false ->
			case IsCheck of
				true  -> set_status_checkmate(Game);
				false -> set_status_stalemate(Game)
			end
	end.

%% is_status_inprogress/1
-spec is_status_inprogress(game_status()) -> boolean().
is_status_inprogress(Status) ->
	case Status of
		?GAME_STATUS_INPROGRESS -> true;
		_ -> false
	end.

%% manual_draw/2
-spec manual_draw(bb_game(), term()) -> bb_game().
manual_draw(Reason, Game) ->
	set_status_draw({manual, Reason}, Game).

%% pawn_moves_bb/3
-spec pawn_moves_bb(sq_idx(), color(), bb_game()) -> bb().
pawn_moves_bb(FromIdx, Color, Game) ->
	PawnBB = ?SQUARE_BB(FromIdx),
	EnemySideBB = enemy_side_bb(Color, Game),
	EmptySquaresBB = empty_squares_bb(Game),
	AttacksBB = binbo_attacks:pawn_attacks_bb(FromIdx, Color),
	ValidAttacksBB = AttacksBB band EnemySideBB,
	PushesBB = binbo_bb:pawn_pushes_bb(Color, PawnBB, EmptySquaresBB),
	EnpaMoveBB = enpassant_moves_bb(Color, AttacksBB, Game) band get_enpassant_bb(Game),
	ValidAttacksBB bor PushesBB bor EnpaMoveBB.


%% knight_moves_bb/1
-spec knight_moves_bb(sq_idx(), color(), bb_game()) -> bb().
knight_moves_bb(FromIdx, Color, Game) ->
	AttacksBB = binbo_attacks:knight_attacks_bb(FromIdx),
	AttacksBB band not_own_side(Color, Game).

%% bishop_moves_bb/2
-spec bishop_moves_bb(sq_idx(), bb_game()) -> bb().
bishop_moves_bb(FromIdx, Game) ->
	Occupied = all_pieces_bb(Game),
	binbo_attacks:bishop_attacks_bb(FromIdx, Occupied).

%% rook_moves_bb/2
-spec rook_moves_bb(sq_idx(), bb_game()) -> bb().
rook_moves_bb(FromIdx, Game) ->
	Occupied = all_pieces_bb(Game),
	binbo_attacks:rook_attacks_bb(FromIdx, Occupied).

%% queen_moves_bb/2
-spec queen_moves_bb(sq_idx(), bb_game()) -> bb().
queen_moves_bb(FromIdx, Game) ->
	Occupied = all_pieces_bb(Game),
	binbo_attacks:queen_attacks_bb(FromIdx, Occupied).

%% king_moves_bb/3
-spec king_moves_bb(sq_idx(), color(), bb_game()) -> bb().
king_moves_bb(FromIdx, Color, Game) ->
	AttacksBB = binbo_attacks:king_attacks_bb(FromIdx) band not_own_side(Color, Game),
	CastlingMovesBB = valid_castling_moves_bb(Color, FromIdx, Game),
	AttacksBB bor CastlingMovesBB.

%% is_in_check/2
-spec is_in_check(color(), bb_game()) -> boolean().
is_in_check(Color, Game) ->
	KingIdx = king_index(Color, Game),
	is_in_check(KingIdx, Color, Game).

%% get_enpassant_bb/2
-spec get_enpassant_bb(bb_game()) -> sq_bb() | empty_bb().
get_enpassant_bb(Game) ->
	maps:get(?GAME_KEY_ENPASSANT, Game).


%% get_side_indexes/2
get_side_indexes(Color, Game) ->
	SideBB = own_side_bb(Color, Game),
	binbo_bb:to_index_list(SideBB).

%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

%% empty_bb_game/0
-spec empty_bb_game() -> empty_bb_game().
empty_bb_game() -> #{
	?GAME_KEY_WP => ?EMPTY_BB, ?GAME_KEY_WN => ?EMPTY_BB, ?GAME_KEY_WB => ?EMPTY_BB,
	?GAME_KEY_WR => ?EMPTY_BB, ?GAME_KEY_WQ => ?EMPTY_BB, ?GAME_KEY_WK => ?EMPTY_BB,
	?GAME_KEY_BP => ?EMPTY_BB, ?GAME_KEY_BN => ?EMPTY_BB, ?GAME_KEY_BB => ?EMPTY_BB,
	?GAME_KEY_BR => ?EMPTY_BB, ?GAME_KEY_BQ => ?EMPTY_BB, ?GAME_KEY_BK => ?EMPTY_BB,
	?GAME_KEY_WHITE => ?EMPTY_BB, ?GAME_KEY_BLACK => ?EMPTY_BB,
	?GAME_KEY_OCCUPIED => ?EMPTY_BB,
	?GAME_KEY_SIDETOMOVE => undefined,
	?GAME_KEY_CASTLING => ?CASTLING_NONE,
	?GAME_KEY_ENPASSANT => ?EMPTY_BB,
	?GAME_KEY_HALFMOVE => 0,
	?GAME_KEY_FULLMOVE => 1,
	?GAME_KEY_LASTMOVE => undefined,
	?GAME_KEY_LASTMOVE_PIECE => ?EMPTY_SQUARE,
	?GAME_KEY_POS_HASH => 0,
	?GAME_KEY_POSITION_HASHMAP => #{},
	?GAME_KEY_STATUS => ?GAME_STATUS_INPROGRESS
	}.


%% set_piece/3
-spec set_piece(sq_idx(), piece(), bb_game()) -> bb_game().
set_piece(Idx, Piece, Game) ->
	Pkey = ?PIECE_GAME_KEY(Piece),
	Pcolor = ?COLOR(Piece),
	SideKey = ?OWN_SIDE_KEY(Pcolor),
	#{Pkey := PiecesBB, SideKey := SideBB, ?GAME_KEY_OCCUPIED := AllPiecesBB, ?GAME_KEY_POS_HASH := PosHash} = Game,
	SquareBB = ?SQUARE_BB(Idx),
	PosHash2 = PosHash bxor binbo_hash:piece_hash(Piece, Idx),
	Game#{
		Idx => Piece,
		Pkey := (PiecesBB bor SquareBB),
		SideKey := (SideBB bor SquareBB),
		?GAME_KEY_OCCUPIED := (AllPiecesBB bor SquareBB),
		?GAME_KEY_POS_HASH := PosHash2
	}.

%% remove_piece/2
-spec remove_piece(sq_idx(), bb_game()) -> bb_game().
remove_piece(Idx, Game) ->
	Piece = maps:get(Idx, Game), % just to be shure if the piece exists (instead of 'get_piece/2')
	Pkey = ?PIECE_GAME_KEY(Piece),
	Pcolor = ?COLOR(Piece),
	SideKey = ?OWN_SIDE_KEY(Pcolor),
	#{Pkey := PiecesBB, SideKey := SideBB, ?GAME_KEY_OCCUPIED := AllPiecesBB, ?GAME_KEY_POS_HASH := PosHash} = Game,
	SquareBB = ?SQUARE_BB(Idx),
	Game2 = maps:remove(Idx, Game),
	PosHash2 = PosHash bxor binbo_hash:piece_hash(Piece, Idx),
	Game2#{
		Pkey := binbo_bb:bb_not(PiecesBB, SquareBB),
		SideKey := binbo_bb:bb_not(SideBB, SquareBB),
		?GAME_KEY_OCCUPIED := binbo_bb:bb_not(AllPiecesBB, SquareBB),
		?GAME_KEY_POS_HASH := PosHash2
	}.


%% unset_castling_flag/2
-spec unset_castling_flag(unset_castling_flag(), bb_game()) -> bb_game().
unset_castling_flag(Flag, Game) ->
	OldCastling = maps:get(?GAME_KEY_CASTLING, Game),
	case ?IS_AND(Flag, OldCastling) of
		true ->
			NewCastling = OldCastling band (bnot Flag),
			PosHash = maps:get(?GAME_KEY_POS_HASH, Game),
			PosHash2 = PosHash bxor binbo_hash:castling_hash(NewCastling),
			Game#{?GAME_KEY_CASTLING := NewCastling, ?GAME_KEY_POS_HASH := PosHash2};
		false ->
			Game
	end.


%% game_bulk_update/2
-spec game_bulk_update(bb_game(), [Operation]) -> bb_game() when
	Operation :: {set_piece,sq_idx(),piece()}
				| {remove_piece,sq_idx()}
				| {unset_castling, unset_castling_flag()}.
game_bulk_update(Game, []) ->
	Game;
game_bulk_update(Game, [Operation | Tail]) ->
	Game2 = case Operation of
		{set_piece, Idx, Piece} ->
			set_piece(Idx, Piece, Game);
		{remove_piece, Idx} ->
			remove_piece(Idx, Game);
		{unset_castling, Flag} ->
			unset_castling_flag(Flag, Game)
	end,
	game_bulk_update(Game2, Tail).


%% own_side_bb/2
%% Returns bitboard of friendly pieces.
-spec own_side_bb(color(), bb_game()) -> side_bb().
own_side_bb(Color, Game) ->
	OwnSideKey = ?OWN_SIDE_KEY(Color),
	maps:get(OwnSideKey, Game).

%% not_own_side/2
-spec not_own_side(color(), bb_game()) -> neg_integer().
not_own_side(Color, Game) ->
	(bnot own_side_bb(Color, Game)).


%% enemy_side_bb/2
%% Returns bitboard of enemy pieces.
%% 'OwnColor' is the color of friendly pieces.
-spec enemy_side_bb(color(), bb_game()) -> side_bb().
enemy_side_bb(OwnColor, Game) ->
	EnemySideKey = ?ENEMY_SIDE_KEY(OwnColor),
	maps:get(EnemySideKey, Game).


%% all_pieces_bb/1
-spec all_pieces_bb(bb_game()) -> bb().
all_pieces_bb(Game) ->
	maps:get(?GAME_KEY_OCCUPIED, Game).

%% empty_squares_bb/1
-spec empty_squares_bb(bb_game()) -> bb().
empty_squares_bb(Game) ->
	binbo_bb:bb_not(?ALL_SQUARES_BB, all_pieces_bb(Game)).


%% white_king_bb/1
-spec white_king_bb(bb_game()) -> sq_bb().
white_king_bb(Game) ->
	maps:get(?GAME_KEY_WK, Game).


%% black_king_bb/1
-spec black_king_bb(bb_game()) -> sq_bb().
black_king_bb(Game) ->
	maps:get(?GAME_KEY_BK, Game).


%% king_bb/2
-spec king_bb(color(), bb_game()) -> sq_bb().
king_bb(?WHITE, Game) -> white_king_bb(Game);
king_bb(?BLACK, Game) -> black_king_bb(Game).

%% king_index/2
-spec king_index(color(), bb_game()) -> sq_idx().
king_index(Color, Game) ->
	KingBB = king_bb(Color, Game),
	binbo_bb:to_index(KingBB).


%% all_pawns_bb/1
-spec all_pawns_bb(bb_game()) -> bb().
all_pawns_bb(Game) ->
	(white_pawns_bb(Game) bor black_pawns_bb(Game)).


%% white_pawns_bb/1
-spec white_pawns_bb(bb_game()) -> bb().
white_pawns_bb(Game) ->
	maps:get(?GAME_KEY_WP, Game).

%% white_knights_bb/1
-spec white_knights_bb(bb_game()) -> bb().
white_knights_bb(Game) ->
	maps:get(?GAME_KEY_WN, Game).

%% white_bishops_bb/1
-spec white_bishops_bb(bb_game()) -> bb().
white_bishops_bb(Game) ->
	maps:get(?GAME_KEY_WB, Game).

%% white_rooks_bb/1
-spec white_rooks_bb(bb_game()) -> bb().
white_rooks_bb(Game) ->
	maps:get(?GAME_KEY_WR, Game).

%% white_queens_bb/1
-spec white_queens_bb(bb_game()) -> bb().
white_queens_bb(Game) ->
	maps:get(?GAME_KEY_WQ, Game).


%% black_pawns_bb/1
-spec black_pawns_bb(bb_game()) -> bb().
black_pawns_bb(Game) ->
	maps:get(?GAME_KEY_BP, Game).

%% black_knights_bb/1
-spec black_knights_bb(bb_game()) -> bb().
black_knights_bb(Game) ->
	maps:get(?GAME_KEY_BN, Game).

%% black_bishops_bb/1
-spec black_bishops_bb(bb_game()) -> bb().
black_bishops_bb(Game) ->
	maps:get(?GAME_KEY_BB, Game).

%% black_rooks_bb/1
-spec black_rooks_bb(bb_game()) -> bb().
black_rooks_bb(Game) ->
	maps:get(?GAME_KEY_BR, Game).

%% black_queens_bb/1
-spec black_queens_bb(bb_game()) -> bb().
black_queens_bb(Game) ->
	maps:get(?GAME_KEY_BQ, Game).

%% enpassant_moves_bb/2
-spec enpassant_moves_bb(color(), bb(), bb_game()) -> bb().
enpassant_moves_bb(?WHITE, AttacksBB, Game) ->
	AttacksBB band ?RANK_6_BB band binbo_bb:shift_north(black_pawns_bb(Game));
enpassant_moves_bb(?BLACK, AttacksBB, Game) ->
	AttacksBB band ?RANK_3_BB band binbo_bb:shift_south(white_pawns_bb(Game)).


%% valid_castling_moves_bb/3
-spec valid_castling_moves_bb(color(), sq_idx(), bb_game()) -> bb().
valid_castling_moves_bb(?WHITE, ?E1_IDX, #{?GAME_KEY_CASTLING := Castling} = Game) when ?IS_AND(Castling, ?CASTLING_W_ANY) ->
	case is_attacked_by(?BLACK, ?E1_IDX, Game) of
		true  -> ?EMPTY_BB; % can't castle if the king is currently in check
		false -> oo_move_bb(?WHITE, Game) bor ooo_move_bb(?WHITE, Game)
	end;
valid_castling_moves_bb(?BLACK, ?E8_IDX, #{?GAME_KEY_CASTLING := Castling} = Game) when ?IS_AND(Castling, ?CASTLING_B_ANY) ->
	case is_attacked_by(?WHITE, ?E8_IDX, Game) of
		true  -> ?EMPTY_BB; % can't castle if the king is currently in check
		false -> oo_move_bb(?BLACK, Game) bor ooo_move_bb(?BLACK, Game)
	end;
valid_castling_moves_bb(_, _, _) ->
	?EMPTY_BB.

%% oo_move_bb/2
-spec oo_move_bb(color(), bb_game()) -> oo_move_bb().
oo_move_bb(?WHITE, #{?GAME_KEY_CASTLING := Castling} = Game) when ?IS_AND(Castling, ?CASTLING_W_OO) ->
	F1G1 = ?F1_BB bor ?G1_BB,
	IsWrong = (
		?IS_AND(F1G1, all_pieces_bb(Game))
		orelse is_attacked_by(?BLACK, ?F1_IDX, Game)
		orelse is_attacked_by(?BLACK, ?G1_IDX, Game)
	),
	case IsWrong of
		true  -> ?EMPTY_BB;
		false -> ?G1_BB % G1
	end;
oo_move_bb(?BLACK, #{?GAME_KEY_CASTLING := Castling} = Game) when ?IS_AND(Castling, ?CASTLING_B_OO) ->
	F8G8 = ?F8_BB bor ?G8_BB,
	IsWrong = (
		?IS_AND(F8G8, all_pieces_bb(Game))
		orelse is_attacked_by(?WHITE, ?F8_IDX, Game)
		orelse is_attacked_by(?WHITE, ?G8_IDX, Game)
	),
	case IsWrong of
		true  -> ?EMPTY_BB;
		false -> ?G8_BB % G8
	end;
oo_move_bb(_, _) ->
	?EMPTY_BB.

%% ooo_move_bb/2
-spec ooo_move_bb(color(), bb_game()) -> ooo_move_bb().
ooo_move_bb(?WHITE, #{?GAME_KEY_CASTLING := Castling} = Game) when ?IS_AND(Castling, ?CASTLING_W_OOO) ->
	B1C1D1 = ?B1_BB bor ?C1_BB bor ?D1_BB,
	IsWrong = (
		?IS_AND(B1C1D1, all_pieces_bb(Game))
		orelse is_attacked_by(?BLACK, ?C1_IDX, Game)
		orelse is_attacked_by(?BLACK, ?D1_IDX, Game)
	),
	case IsWrong of
		true  -> ?EMPTY_BB;
		false -> ?C1_BB % C1
	end;
ooo_move_bb(?BLACK, #{?GAME_KEY_CASTLING := Castling} = Game) when ?IS_AND(Castling, ?CASTLING_B_OOO) ->
	B8C8D8 = ?B8_BB bor ?C8_BB bor ?D8_BB,
	IsWrong = (
		?IS_AND(B8C8D8, all_pieces_bb(Game))
		orelse is_attacked_by(?WHITE, ?C8_IDX, Game)
		orelse is_attacked_by(?WHITE, ?D8_IDX, Game)
	),
	case IsWrong of
		true  -> ?EMPTY_BB;
		false -> ?C8_BB % C8
	end;
ooo_move_bb(_, _) ->
	?EMPTY_BB.


%% is_attacked_by/3
-spec is_attacked_by(color(), sq_idx(), bb_game()) -> boolean().
is_attacked_by(?WHITE, SqIdx, Game) -> % check if square attacked by white side
	Occupied = all_pieces_bb(Game),
	QueensBB = white_queens_bb(Game),
	?IS_AND(binbo_attacks:bishop_attacks_bb(SqIdx, Occupied), (white_bishops_bb(Game) bor QueensBB))
	orelse
	?IS_AND(binbo_attacks:rook_attacks_bb(SqIdx, Occupied), (white_rooks_bb(Game) bor QueensBB))
	orelse
	?IS_AND(binbo_attacks:knight_attacks_bb(SqIdx), white_knights_bb(Game))
	orelse
	?IS_AND(binbo_attacks:pawn_attacks_bb(SqIdx, ?BLACK), white_pawns_bb(Game))
	orelse
	?IS_AND(binbo_attacks:king_attacks_bb(SqIdx), white_king_bb(Game));
is_attacked_by(?BLACK, SqIdx, Game) -> % check if square attacked by black side
	Occupied = all_pieces_bb(Game),
	QueensBB = black_queens_bb(Game),
	?IS_AND(binbo_attacks:bishop_attacks_bb(SqIdx, Occupied), (black_bishops_bb(Game) bor QueensBB))
	orelse
	?IS_AND(binbo_attacks:rook_attacks_bb(SqIdx, Occupied), (black_rooks_bb(Game) bor QueensBB))
	orelse
	?IS_AND(binbo_attacks:knight_attacks_bb(SqIdx), black_knights_bb(Game))
	orelse
	?IS_AND(binbo_attacks:pawn_attacks_bb(SqIdx, ?WHITE), black_pawns_bb(Game))
	orelse
	?IS_AND(binbo_attacks:king_attacks_bb(SqIdx), black_king_bb(Game)).


%% is_in_check/3
-spec is_in_check(sq_idx(), color(), bb_game()) -> boolean().
is_in_check(KingIdx, Color, Game) ->
	EnemyColor = ?SWITCH_COLOR(Color),
	is_attacked_by(EnemyColor, KingIdx, Game).

%% set_status/2
-spec set_status(game_over_status(), bb_game()) -> bb_game().
set_status(Status, Game) ->
	maps:update(?GAME_KEY_STATUS, Status, Game).

%% set_status_checkmate/1
-spec set_status_checkmate(bb_game()) -> bb_game().
set_status_checkmate(Game) ->
	set_status(?GAME_STATUS_CHECKMATE, Game).

%% set_status_stalemate/1
-spec set_status_stalemate(bb_game()) -> bb_game().
set_status_stalemate(Game) ->
	set_status_draw(?GAME_STATUS_DRAW_STALEMATE, Game).

%% set_status_draw/2
-spec set_status_draw(why_draw(), bb_game()) -> bb_game().
set_status_draw(Reason, Game) ->
	set_status({draw, Reason}, Game).

%% get_halfmove/1
-spec get_halfmove(bb_game()) -> halfmove().
get_halfmove(Game) ->
	maps:get(?GAME_KEY_HALFMOVE, Game).

%% set_halfmove/1
-spec set_halfmove(halfmove(), bb_game()) -> bb_game().
set_halfmove(Halfmove, Game) ->
	maps:update(?GAME_KEY_HALFMOVE, Halfmove, Game).

%% increase_halfmove/1
-spec increase_halfmove(bb_game()) -> bb_game().
increase_halfmove(Game) ->
	Halfmove = get_halfmove(Game),
	set_halfmove(Halfmove + 1, Game).

%% get_fullmove/1
-spec get_fullmove(bb_game()) -> fullmove().
get_fullmove(Game) ->
	maps:get(?GAME_KEY_FULLMOVE, Game).

%% set_fullmove/1
-spec set_fullmove(fullmove(), bb_game()) -> bb_game().
set_fullmove(Fullmove, Game) ->
	maps:update(?GAME_KEY_FULLMOVE, Fullmove, Game).

%% increase_fullmove/1
-spec increase_fullmove(bb_game()) -> bb_game().
increase_fullmove(Game) ->
	Fullmove = get_fullmove(Game),
	set_fullmove(Fullmove + 1, Game).

%% get_lastmove/1
-spec get_lastmove(bb_game()) -> undefined | {sq_idx(), sq_idx()}.
get_lastmove(Game) ->
	maps:get(?GAME_KEY_LASTMOVE, Game).

%% get_lastmove_piece/1
-spec get_lastmove_piece(bb_game()) -> piece().
get_lastmove_piece(Game) ->
	maps:get(?GAME_KEY_LASTMOVE_PIECE, Game).


%%%------------------------------------------------------------------------------
%%%   Load parsed FEN and validate position according to the Chess rules
%%%------------------------------------------------------------------------------

%% load_parsed_fen/2
-spec load_parsed_fen(parsed_fen(), bb_game()) -> bb_game().
load_parsed_fen(ParsedFen, Game) ->
	Steps = [position, sidetomove, castling, enpassant, halfmove, fullmove],
	load_parsed_fen(Steps, ParsedFen, Game).

%% load_parsed_fen/3
-spec load_parsed_fen([Step], parsed_fen(), bb_game()) -> bb_game() when
	Step :: position | sidetomove | castling | enpassant | halfmove | fullmove.
load_parsed_fen([], _ParsedFen, Game) ->
	Game;
load_parsed_fen([position|Tail], #parsed_fen{position = Pos} = ParsedFen, Game) -> % position
	Game2 = load_fen_position(Pos, Game),
	load_parsed_fen(Tail, ParsedFen, Game2);
load_parsed_fen([sidetomove|Tail], #parsed_fen{sidetomove = Char} = ParsedFen, Game) -> % sidetomove
	SideToMove = case Char of
		$w -> ?WHITE;
		$b -> ?BLACK
	end,
	Game2 = set_sidetomove(SideToMove, Game),
	load_parsed_fen(Tail, ParsedFen, Game2);
load_parsed_fen([castling|Tail], ParsedFen, Game) -> % castling
	#parsed_fen{castling = Castling} = ParsedFen,
	PosHash = maps:get(?GAME_KEY_POS_HASH, Game),
	PosHash2  = PosHash bxor Castling,
	Game2 = Game#{?GAME_KEY_CASTLING := Castling, ?GAME_KEY_POS_HASH := PosHash2},
	load_parsed_fen(Tail, ParsedFen, Game2);
load_parsed_fen([enpassant|Tail], #parsed_fen{enpassant = Idx} = ParsedFen, Game) -> % enpassant
	EnpaBB = case is_integer(Idx) of
		true  -> ?SQUARE_BB(Idx);
		false -> ?EMPTY_BB
	end,
	Game2 = case (EnpaBB > ?EMPTY_BB) of
		true  ->
			PosHash = maps:get(?GAME_KEY_POS_HASH, Game),
			File = binbo_bb:to_file(EnpaBB),
			PosHash2 = PosHash bxor binbo_hash:enpa_hash(File),
			Game#{?GAME_KEY_ENPASSANT := EnpaBB, ?GAME_KEY_POS_HASH := PosHash2};
		false ->
			Game#{?GAME_KEY_ENPASSANT := EnpaBB}
	end,
	load_parsed_fen(Tail, ParsedFen, Game2);
load_parsed_fen([halfmove|Tail], #parsed_fen{halfmove = Halfmove} = ParsedFen, Game) -> % halfmove
	Game2 =  set_halfmove(Halfmove, Game),
	load_parsed_fen(Tail, ParsedFen, Game2);
load_parsed_fen([fullmove|Tail], #parsed_fen{fullmove = Fullmove} = ParsedFen, Game) -> % fullmove
	Game2 = set_fullmove(Fullmove, Game),
	load_parsed_fen(Tail, ParsedFen, Game2).


%% load_fen_position/2
-spec load_fen_position(binbo_fen:position(), bb_game()) -> bb_game().
load_fen_position([], Game) ->
	Game;
load_fen_position([{Idx, Piece} | Tail], Game) ->
	Game2 = set_piece(Idx, Piece, Game),
	load_fen_position(Tail, Game2).

%% validate_loaded_fen/1
-spec validate_loaded_fen(bb_game()) -> ok | {error, bb_game_error()}.
validate_loaded_fen(Game) ->
 	Steps = [kings_distance, pawns, enpassant, castling],
 	validate_loaded_fen(Steps, Game).

%% validate_loaded_fen/2
-spec validate_loaded_fen([Step], bb_game()) -> ok | {error, bb_game_error()} when
	Step :: kings_distance | pawns | enpassant | castling.
validate_loaded_fen([], _Game) ->
	ok;
validate_loaded_fen([kings_distance | Tail], Game) ->
	WKing = white_king_bb(Game),
	BKing = black_king_bb(Game),
	WKingAttacks = binbo_bb:king_attacks_bb(WKing),
	case ?IS_NOT(BKing, WKingAttacks) of
		true  -> validate_loaded_fen(Tail, Game);
		false -> {error, bb_kings_too_close}
	end;
validate_loaded_fen([pawns|Tail], Game) ->
	AllPawns = all_pawns_bb(Game),
	case ?IS_NOT(?RANK_1_BB, AllPawns) andalso ?IS_NOT(?RANK_8_BB, AllPawns) of
		true  -> validate_loaded_fen(Tail, Game);
		false -> {error, bb_edge_rank_occupied_by_pawns}
	end;
validate_loaded_fen([enpassant|Tail], Game) ->
	case validate_fen_enpassant(Game) of
		ok -> validate_loaded_fen(Tail, Game);
		error -> {error, bb_invalid_enpassant}
	end;
validate_loaded_fen([castling|Tail], Game) ->
	case validate_fen_castling(Game) of
		ok -> validate_loaded_fen(Tail, Game);
		{error, Reason} -> {error, {castling, Reason}}
	end.

%% validate_fen_castling/1
-spec validate_fen_castling(bb_game()) -> ok | {error, castling_error()}.
validate_fen_castling(#{?GAME_KEY_CASTLING := ?CASTLING_NONE}) ->
	ok;
validate_fen_castling(Game) ->
	validate_fen_castling([white, black], Game).

%% validate_fen_castling/2
-spec validate_fen_castling([white|black,...], bb_game()) -> ok | {error, castling_error()}.
validate_fen_castling([], _) ->
	ok;
validate_fen_castling([Color|Tail], #{?GAME_KEY_CASTLING := Castling} = Game) ->
	{
		KingBB, RooksBB,
		CastlingAny, CastlingOO, CastlingOOO,
		KingSqBB, SqABB, SqHBB
	} = case Color of
		white -> {
			white_king_bb(Game), white_rooks_bb(Game),
			?CASTLING_W_ANY, ?CASTLING_W_OO, ?CASTLING_W_OOO,
			?E1_BB, ?A1_BB, ?H1_BB
		};
		black -> {
			black_king_bb(Game), black_rooks_bb(Game),
			?CASTLING_B_ANY, ?CASTLING_B_OO, ?CASTLING_B_OOO,
			?E8_BB, ?A8_BB, ?H8_BB
		}
	end,
	IsKingOnPlace = ?IS_AND(KingSqBB, KingBB),
	case Castling of
		_ when ?IS_AND(CastlingAny, Castling) ->
			TwoRooks = SqABB bor SqHBB,
			case IsKingOnPlace andalso ?IS_AND(TwoRooks, RooksBB) of
				true  -> validate_fen_castling(Tail, Game);
				false -> {error, {Color, both_sides}}
			end;
		_ when ?IS_AND(CastlingOO, Castling) ->
			case IsKingOnPlace andalso ?IS_AND(SqHBB, RooksBB) of
				true  -> validate_fen_castling(Tail, Game);
				false -> {error, {Color, king_side}}
			end;
		_ when ?IS_AND(CastlingOOO, Castling) ->
			case IsKingOnPlace andalso ?IS_AND(SqABB, RooksBB) of
				true  -> validate_fen_castling(Tail, Game);
				false -> {error, {Color, queen_side}}
			end;
		_ ->
			validate_fen_castling(Tail, Game)
	end.


%% validate_fen_enpassant/3
-spec validate_fen_enpassant(bb_game()) -> ok | error.
validate_fen_enpassant(#{?GAME_KEY_ENPASSANT := ?EMPTY_BB}) ->
	ok;
validate_fen_enpassant(#{?GAME_KEY_ENPASSANT := EnpaBB}) when ?IS_NOT(EnpaBB, ?RANK_3_BB) andalso ?IS_NOT(EnpaBB, ?RANK_6_BB)->
	error;
validate_fen_enpassant(#{?GAME_KEY_ENPASSANT := EnpaBB, ?GAME_KEY_OCCUPIED := AllPieces}) when ?IS_AND(EnpaBB, AllPieces) ->
	error;
validate_fen_enpassant(#{?GAME_KEY_ENPASSANT := EnpaBB} = Game) ->
	{Pawns, From, To} = case EnpaBB of
		_ when ?IS_AND(EnpaBB, ?RANK_3_BB) ->
			{white_pawns_bb(Game), binbo_bb:shift_south(EnpaBB), binbo_bb:shift_north(EnpaBB)};
		_ when ?IS_AND(EnpaBB, ?RANK_6_BB) ->
			{black_pawns_bb(Game), binbo_bb:shift_north(EnpaBB), binbo_bb:shift_south(EnpaBB)}
	end,
	case ?IS_NOT(From, Pawns) andalso ?IS_AND(To, Pawns) of
		true  -> ok;
		false -> error
	end.


%%%------------------------------------------------------------------------------
%%%   Updating game state after move
%%%------------------------------------------------------------------------------

%% make_move/2
-spec make_move(move_info(), bb_game()) -> {ok, bb_game()} | {error, make_move_error()}.
make_move(MoveInfo, Game) ->
	Steps = [remove_piece, set_piece, is_in_check, enpassant],
	make_move(Steps, MoveInfo, Game).

%% make_move/3
-spec make_move([Step], move_info(), bb_game()) -> {ok, bb_game()} | {error, make_move_error()} when
	Step :: make_move_step().
make_move([], _MoveInfo, Game) ->
	{ok, Game};
make_move([remove_piece | Tail], MoveInfo, Game) ->
	% Remove moving piece from starting square and handle capture
	Game2 = make_move_remove_piece(MoveInfo, Game),
	make_move(Tail, MoveInfo, Game2);
make_move([set_piece | Tail], #move_info{ptype = Ptype} = MoveInfo, Game) ->
	% Set piece to target square
	Game2 = case Ptype of
		?PAWN -> % pawn
			make_pawn_move(MoveInfo, Game);
		?KING -> % king
			make_king_move(MoveInfo, Game);
		?ROOK -> % rook
			make_rook_move(MoveInfo, Game);
		_Other -> % other piece type
			#move_info{to_idx = ToIdx, piece = Piece} = MoveInfo,
			set_piece(ToIdx, Piece, Game)
	end,
	make_move(Tail, MoveInfo, Game2);
make_move([is_in_check | Tail], #move_info{pcolor = Pcolor} = MoveInfo, Game) ->
	% After updating position we should check whether the own king in check or not
	case is_in_check(Pcolor, Game) of
		false -> make_move(Tail, MoveInfo, Game);
		true  -> {error, own_king_in_check}
	end;
make_move([enpassant | Tail], MoveInfo, Game) ->
	% Update enpassant info
	#move_info{from_bb = FromBB, to_bb = ToBB, piece = Piece} = MoveInfo,
	EnpaBB = binbo_bb:enpassant_bb(Piece, FromBB, ToBB),
	Game2 = case (EnpaBB > ?EMPTY_BB) of
		true  ->
			PosHash = maps:get(?GAME_KEY_POS_HASH, Game),
			File = binbo_bb:to_file(EnpaBB),
			PosHash2 = PosHash bxor binbo_hash:enpa_hash(File),
			Game#{?GAME_KEY_ENPASSANT := EnpaBB, ?GAME_KEY_POS_HASH := PosHash2};
		false ->
			Game#{?GAME_KEY_ENPASSANT := EnpaBB}
	end,
	make_move(Tail, MoveInfo, Game2).



%% make_move_remove_piece/2
-spec make_move_remove_piece(move_info(), bb_game()) -> bb_game().
make_move_remove_piece(MoveInfo, Game) ->
	#move_info{from_idx = FromIdx, captured_idx = CaptIdx, captured = Captured} = MoveInfo,
	% Handle capture
	Game2 = case ?IS_PIECE(Captured) of
		true  -> remove_piece(CaptIdx, Game);
		false -> Game
	end,
	% Remove piece from starting square
	remove_piece(FromIdx, Game2).


%% make_pawn_move/2
-spec make_pawn_move(move_info(), bb_game()) -> bb_game().
make_pawn_move(#move_info{to_bb = ToBB} = MoveInfo, Game) when ?IS_AND(ToBB, ?RANK_1_BB bor ?RANK_8_BB) ->
	% Handle promotion
	#move_info{to_idx = ToIdx, pcolor = Pcolor, promo = PromoType} = MoveInfo,
	PromoPiece = ?TYPE_TO_PIECE(PromoType, Pcolor),
	set_piece(ToIdx, PromoPiece, Game);
make_pawn_move(#move_info{to_idx = ToIdx, piece = Pawn}, Game) ->
	set_piece(ToIdx, Pawn, Game).


%% make_king_move/2
-spec make_king_move(move_info(), bb_game()) -> bb_game().
make_king_move(#move_info{castling = ?CASTLING_NONE} = MoveInfo, Game) ->
	% Normal king move, no castling
	#move_info{to_idx = ToIdx, piece = King} = MoveInfo,
	CastlingFlag = case King of
		?WHITE_KING -> ?CASTLING_W_ANY;
		?BLACK_KING -> ?CASTLING_B_ANY
	end,
	game_bulk_update(Game, [
		{set_piece, ToIdx, King},      % set king
		{unset_castling, CastlingFlag} % unset castling
	]);
make_king_move(#move_info{castling = Castling} = MoveInfo, Game) ->
	% Handle castling move
	#move_info{to_idx = ToIdx, piece = King} = MoveInfo,
	{RookIdxFrom, RookIdxTo} = binbo_board:castling_rook_squares(Castling),
	Rook = get_piece(RookIdxFrom, Game),
	CastlingFlag = case Castling of
		_ when ?IS_AND(Castling, ?CASTLING_W_ANY) -> ?CASTLING_W_ANY;
		_ when ?IS_AND(Castling, ?CASTLING_B_ANY) -> ?CASTLING_B_ANY
	end,
	game_bulk_update(Game, [
		{set_piece, ToIdx, King},      % set king
		{remove_piece, RookIdxFrom},   % remove rook from its initial square
		{set_piece, RookIdxTo, Rook},  % now rook is the king's neighbour
		{unset_castling, CastlingFlag} % unset castling
	]).


%% make_rook_move/2
-spec make_rook_move(move_info(), bb_game()) -> bb_game().
make_rook_move(#move_info{piece = Rook} = MoveInfo, #{?GAME_KEY_CASTLING := Castling} = Game) when ?IS_AND(Castling, ?CASTLING_ANY) ->
	% If rook moved from its initial square we unset the corresponding castling flag
	% to disable castling on the kingside or on the queenside.
	#move_info{from_idx = FromIdx, to_idx = ToIdx} = MoveInfo,
	UnsetFlag = case {Rook, FromIdx} of
		{?WHITE_ROOK, ?H1_IDX} when ?IS_AND(Castling, ?CASTLING_W_OO) ->
			?CASTLING_W_OO;
		{?BLACK_ROOK, ?H8_IDX} when ?IS_AND(Castling, ?CASTLING_B_OO) ->
			?CASTLING_B_OO;
		{?WHITE_ROOK, ?A1_IDX} when ?IS_AND(Castling, ?CASTLING_W_OOO) ->
			?CASTLING_W_OOO;
		{?BLACK_ROOK, ?A8_IDX} when ?IS_AND(Castling, ?CASTLING_B_OOO) ->
			?CASTLING_B_OOO;
		{_, _} ->
			?CASTLING_NONE
	end,
	case UnsetFlag of
		?CASTLING_NONE ->
			set_piece(ToIdx, Rook, Game);
		_ ->
			game_bulk_update(Game, [
				{set_piece, ToIdx, Rook},    % set rook
				{unset_castling, UnsetFlag}  % unset castling
			])
	end;
make_rook_move(MoveInfo, Game) ->
	#move_info{to_idx = ToIdx, piece = Rook} = MoveInfo,
	set_piece(ToIdx, Rook, Game).


%% finalize_move/2
-spec finalize_move(move_info(), bb_game()) -> bb_game().
finalize_move(MoveInfo, Game) ->
	Steps = [lastmove, sidetomove, halfmove, fullmove, hashmap, status],
	finalize_move(Steps, MoveInfo, Game).

%% finalize_move/3
-spec finalize_move([finalize_move_step()], move_info(), bb_game()) -> bb_game().
finalize_move([], _MoveInfo, Game) ->
	Game;
finalize_move([lastmove | Tail], MoveInfo, Game) ->
	% Update last move info
	#move_info{from_idx = FromIdx, to_idx = ToIdx, piece = Piece} = MoveInfo,
	Game2 = Game#{
		?GAME_KEY_LASTMOVE := {FromIdx, ToIdx},
		?GAME_KEY_LASTMOVE_PIECE := Piece
	},
	finalize_move(Tail, MoveInfo, Game2);
finalize_move([sidetomove | Tail], MoveInfo, Game) ->
	% Switch side to move
	SideToMove = get_sidetomove(Game),
	SideToMove2 = ?SWITCH_COLOR(SideToMove),
	Game2 = set_sidetomove(SideToMove2, Game),
	finalize_move(Tail, MoveInfo, Game2);
finalize_move([halfmove | Tail], MoveInfo, Game) ->
	% This counter is reset to zero after a capture or a pawn move and incremented otherwise.
	#move_info{ptype = Ptype, captured = Captured} = MoveInfo,
	Game2 = case (Ptype =:= ?PAWN) orelse ?IS_PIECE(Captured) of
		true  -> set_halfmove(0, Game);
		false -> increase_halfmove(Game)
	end,
	finalize_move(Tail, MoveInfo, Game2);
finalize_move([fullmove | Tail], MoveInfo, Game) ->
	% The number of the full moves in a game. It starts at 1, and is incremented after each Black's move.
	#move_info{pcolor = Pcolor} = MoveInfo,
	Game2 = case Pcolor of
		?WHITE -> Game;
		?BLACK -> increase_fullmove(Game)
	end,
	finalize_move(Tail, MoveInfo, Game2);
finalize_move([hashmap | Tail], MoveInfo, Game) ->
	% Update position hashmap. We need it for detecting threefold repetition.
	Game2 = update_hashmap(Game),
	finalize_move(Tail, MoveInfo, Game2);
finalize_move([status | Tail], MoveInfo, Game) ->
	#move_info{is_check = IsCheck, has_valid_moves = HasValidMoves} = MoveInfo,
	Game2 = with_status(Game, HasValidMoves, IsCheck),
	finalize_move(Tail, MoveInfo, Game2).

%% update_hashmap/1
-spec update_hashmap(bb_game()) -> bb_game().
update_hashmap(Game) ->
	PosHash = maps:get(?GAME_KEY_POS_HASH, Game),
	Keys = [?GAME_KEY_POSITION_HASHMAP, PosHash],
	Repetitions = uef_maps:get_nested(Keys, Game, 0),
	uef_maps:put_nested(Keys, Repetitions + 1, Game).


%%%------------------------------------------------------------------------------
%%%   Getting FEN of the current position
%%%------------------------------------------------------------------------------

% get_fen/1
-spec get_fen(bb_game()) -> binary().
get_fen(Game) ->
	% 1. Pieces
	Ranks = get_fen_ranks(Game, [
		{?A1_IDX,?H1_IDX},{?A2_IDX,?H2_IDX},{?A3_IDX,?H3_IDX},{?A4_IDX,?H4_IDX},
		{?A5_IDX,?H5_IDX},{?A6_IDX,?H6_IDX},{?A7_IDX,?H7_IDX},{?A8_IDX,?H8_IDX}
	], []),
	% 2. Active color. "w" means White moves next, "b" means Black moves next.
	SideToMove = case get_sidetomove(Game) of ?WHITE -> <<$w>>; ?BLACK -> <<$b>> end,
	% 3. Castling availability. If neither side can castle, this is "-".
	Castling = get_fen_castling(maps:get(?GAME_KEY_CASTLING, Game)),
	% 4. En passant target square in algebraic notation. If there's no en passant target square, this is "-".
	Enpa = case get_enpassant_bb(Game) of
		?EMPTY_BB -> <<"-">>;
		EnpaBB    -> binbo_bb:to_notation(EnpaBB)
	end,
	% 5. Halfmove clock
	Halfmove = erlang:integer_to_binary(get_halfmove(Game)),
	% 6. Fullmove number
	Fullmove = erlang:integer_to_binary(get_fullmove(Game)),
	% Joining parts into binary
	FenParts = [Ranks, SideToMove, Castling, Enpa, Halfmove, Fullmove],
	uef_bin:binary_join(FenParts, <<$\s>>).


% get_fen_ranks/2
-spec get_fen_ranks(bb_game(), [{sq_idx(), sq_idx()}], [binary()]) -> binary().
get_fen_ranks(_Game, [], Ranks) ->
	uef_bin:binary_join(Ranks, <<$/>>);
get_fen_ranks(Game, [{A,H} | Tail], Ranks) ->
	{BinRank, RestCnt} = lists:foldl(fun(SqIdx, {Acc, Cnt}) ->
		Piece = get_piece(SqIdx, Game),
		case ?IS_PIECE(Piece) of
			true  ->
				Char = ?PIECE_TO_CHAR(Piece),
				case Cnt of
					0 -> {<<Acc/bits, Char>>, 0};
					_ -> {<<Acc/bits, ($0 + Cnt), Char>>, 0}
				end;
			false ->
				{Acc, Cnt + 1}
		end
	end, {<<>>, 0}, lists:seq(A, H, 1)),
	BinRank2 = case RestCnt of
		0 -> BinRank;
		_ -> <<BinRank/bits, ($0 + RestCnt)>>
	end,
	get_fen_ranks(Game, Tail, [BinRank2 | Ranks]).

%% get_fen_castling/1
-spec get_fen_castling(castling()) -> binary().
get_fen_castling(?CASTLING_NONE) -> <<"-">>;
get_fen_castling(Flag) ->
	lists:foldl(fun(Char, Acc) ->
		case Char of
			$K when ?IS_AND(Flag, ?CASTLING_W_OO)  -> <<Acc/bits, $K>>;
			$Q when ?IS_AND(Flag, ?CASTLING_W_OOO) -> <<Acc/bits, $Q>>;
			$k when ?IS_AND(Flag, ?CASTLING_B_OO)  -> <<Acc/bits, $k>>;
			$q when ?IS_AND(Flag, ?CASTLING_B_OOO) -> <<Acc/bits, $q>>;
			_ -> Acc
		end
	end, <<>>, "KQkq").


%%%------------------------------------------------------------------------------
%%%   Board visualisation in Erlang shell
%%%------------------------------------------------------------------------------

%% index_to_pchar_pretty/3
%% We could write it with much faster solution since we have
%% square indexes as keys of Game, but this is to ensure
%% the position is correct with our bitboard values.
-spec index_to_pchar_pretty(sq_idx(), bb_game(), ascii | unicode, non_neg_integer() | iodata()) -> pchar() | uchar() | $\s.
index_to_pchar_pretty(Idx, Game, CharsTable, EmptySymbol) ->
	Sq = ?SQUARE_BB(Idx),
	AllPieces = all_pieces_bb(Game),
	WP = white_pawns_bb(Game),
	WN = white_knights_bb(Game),
	WB = white_bishops_bb(Game),
	WR = white_rooks_bb(Game),
	WQ = white_queens_bb(Game),
	WK = white_king_bb(Game),
	BP = black_pawns_bb(Game),
	BN = black_knights_bb(Game),
	BB = black_bishops_bb(Game),
	BR = black_rooks_bb(Game),
	BQ = black_queens_bb(Game),
	BK = black_king_bb(Game),
	Piece = case Sq of
		_ when ?IS_AND(Sq, WP) -> ?WHITE_PAWN;
		_ when ?IS_AND(Sq, WN) -> ?WHITE_KNIGHT;
		_ when ?IS_AND(Sq, WB) -> ?WHITE_BISHOP;
		_ when ?IS_AND(Sq, WR) -> ?WHITE_ROOK;
		_ when ?IS_AND(Sq, WQ) -> ?WHITE_QUEEN;
		_ when ?IS_AND(Sq, WK) -> ?WHITE_KING;
		_ when ?IS_AND(Sq, BP) -> ?BLACK_PAWN;
		_ when ?IS_AND(Sq, BN) -> ?BLACK_KNIGHT;
		_ when ?IS_AND(Sq, BB) -> ?BLACK_BISHOP;
		_ when ?IS_AND(Sq, BR) -> ?BLACK_ROOK;
		_ when ?IS_AND(Sq, BQ) -> ?BLACK_QUEEN;
		_ when ?IS_AND(Sq, BK) -> ?BLACK_KING;
		_ when ?IS_NOT(Sq, AllPieces) -> ?EMPTY_SQUARE
	end,
	case ?IS_PIECE(Piece) of
		true ->
			case CharsTable of
				ascii   -> ?PIECE_TO_CHAR(Piece);
				unicode -> ?PIECE_TO_UNICODE(Piece)
			end;
		false ->
			EmptySymbol
	end.

%% parse_empty_pretty/2
-spec parse_empty_pretty([term()], $\s) -> non_neg_integer() | iodata().
parse_empty_pretty([], Default) -> Default;
parse_empty_pretty([{empty, EmptySymbol} | _], _) -> EmptySymbol;
parse_empty_pretty([_|Tail], Default) -> parse_empty_pretty(Tail, Default).

%% pretty_board/1
-spec pretty_board(bb_game(), pretty_board_opts()) -> {iolist(), game_status()}.
pretty_board(Game, Opts) ->
	AIdxs1 = [?A8_IDX, ?A7_IDX, ?A6_IDX, ?A5_IDX, ?A4_IDX, ?A3_IDX, ?A2_IDX, ?A1_IDX],
	RankList1 = "87654321",
	FileList1 = "ABCDEFGH",
	Flip = lists:any(fun(Opt) -> Opt =:= flip end, Opts),
	CharsTable = case lists:any(fun(Opt) -> Opt =:= unicode end, Opts) of
		true  -> unicode;
		false -> ascii
	end,
	EmptySymbol = parse_empty_pretty(Opts, $\s),
	{AIdxs, RankList, FileList} = case Flip of
		true  -> {lists:reverse(AIdxs1), lists:reverse(RankList1), lists:reverse(FileList1)};
		false -> {AIdxs1, RankList1, FileList1}
	end,
	Line = "   +---+---+---+---+---+---+---+---+",
	FileString = lists:join("   ", FileList),
	FileLine = ["     ", FileString, "  "],
	{PrettyRanks, []} = lists:foldl(fun(AIdx, {Acc, [Rank|RanksTail]}) ->
		RankChars0 = lists:foldl(fun(Idx, RankCharsAcc) ->
			Pchar = index_to_pchar_pretty(Idx, Game, CharsTable, EmptySymbol),
			[Pchar | RankCharsAcc]
		end, [], lists:seq(AIdx, AIdx + 7)),
		Last = case RanksTail of
			[]  -> "";
			_ -> $\n
		end,
		RankChars = case Flip of
			true  -> RankChars0;
			false -> lists:reverse(RankChars0)
		end,
		PrettyBoardRank = lists:join(" | ", RankChars),
		Acc2 = [Acc, " ", Rank, " | ", PrettyBoardRank, " |", $\n, Line, Last],
		{Acc2, RanksTail}
	end, {[], RankList}, AIdxs),
	InfoSpaces = "  ",
	% Board itself
	Board = [Line, $\n, PrettyRanks, $\n, FileLine],
	% FEN
	Fen = get_fen(Game),
	FenLine = [InfoSpaces, "FEN: ", $", Fen, $"],
	% SideTomove
	SideToMove = case get_sidetomove(Game) of ?WHITE -> <<"White">>; ?BLACK -> <<"Black">> end,
	SideToMoveLine = [InfoSpaces, "Side to move: ", SideToMove],
	% Lastmove
	LastmoveLine = case get_lastmove(Game) of
		{FromIdx, ToIdx} ->
			Lastmove = <<(binbo_board:index_to_notation(FromIdx))/bits, "-" , (binbo_board:index_to_notation(ToIdx))/bits>>,
			LastmovePiece = get_lastmove_piece(Game),
			LastmovePieceBin = erlang:atom_to_binary(?PIECE_TO_ATOM(LastmovePiece), latin1),
			[InfoSpaces, "Lastmove: ", Lastmove, ", ", LastmovePieceBin];
		undefined ->
			[InfoSpaces, "Lastmove: none"]
	end,
	% Fullmove
	Fullmove = erlang:integer_to_binary(get_fullmove(Game)),
	FullmoveLine = [InfoSpaces, "Fullmove: ", Fullmove],
	% Halfmove
	Halfmove = erlang:integer_to_binary(get_halfmove(Game)),
	HalfmoveLine = [InfoSpaces, "Halfmove: ", Halfmove],
	% Status
	Status = get_status(Game),
	% TotalPretty
	TotalPretty = [
		Board, $\n
		,$\n, SideToMoveLine
		,$\n, LastmoveLine
		,$\n, FullmoveLine
		,$\n, HalfmoveLine
		,$\n, FenLine
	],
	% Returns:
	{TotalPretty, Status}.
