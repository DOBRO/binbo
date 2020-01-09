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

-module(binbo_fen).

-export([parse/1]).
-export([initial/0]).

%%%------------------------------------------------------------------------------
%%%   Includes
%%%------------------------------------------------------------------------------

-include("binbo_fen.hrl").
-include("binbo_board.hrl").

%%%------------------------------------------------------------------------------
%%%   Macros
%%%------------------------------------------------------------------------------

-define(INITIAL_FEN, <<"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1">>).

-define(NO_PIECES_MAP, #{
	?WP => 0, ?WN => 0, ?WB => 0, ?WR => 0, ?WQ => 0, ?WK => 0,
	?BP => 0, ?BN => 0, ?BB => 0, ?BR => 0, ?BQ => 0, ?BK => 0
}).

%%%------------------------------------------------------------------------------
%%%   Types
%%%------------------------------------------------------------------------------

-type fen() :: binary() | [byte()].
-type initial_fen() :: <<_:448>>.
-type sq_idx() :: binbo_board:square_index().
-type piece() :: binbo_board:piece().
-type piece_char() :: ?WP | ?WN | ?WB | ?WR | ?WQ | ?WK | ?BP | ?BN | ?BB | ?BR | ?BQ | ?BK.
-type rank_number() :: binbo_board:rank_number().
-type halfmove() :: non_neg_integer().
-type fullmove() :: pos_integer().
-type fen_part_tuple() :: {position | sidetomove | castling | enpassant | halfmove | fullmove, binary()}.
-type fen_part_tuples() :: [fen_part_tuple(),...].
-type cur_idx() :: non_neg_integer().
-type fen_rank_idx() :: {binary(), ?A1_IDX | ?A2_IDX | ?A3_IDX | ?A4_IDX | ?A5_IDX | ?A6_IDX | ?A7_IDX | ?A8_IDX}.
-type atom_color() :: binbo_board:atom_color().
-type qty() :: non_neg_integer().
-type pieces_totals() :: #{
	?WP := qty(), ?WN := qty(), ?WB := qty(), ?WR := qty(), ?WQ := qty(), ?WK := qty(),
	?BP := qty(), ?BN := qty(), ?BB := qty(), ?BR := qty(), ?BQ := qty(), ?BK := qty()
}.
-type position() :: [{sq_idx(), piece()}].
-type rank_error() :: {last_index_mismatch | index_out_of_range, pos_integer(), {rank, rank_number()}}
                      | {invalid_character, [non_neg_integer(),...]}.
-type pieces_totals_error() :: {no_kings, atom_color()} | {too_many_kings | too_many_pawns, atom_color(), pos_integer()}
		| {bad_totals, atom_color(), {total, qty()},
			{pawns, qty()}, {knights, qty()}, {bishops, qty()},
			{rooks, qty()}, {queens, qty()}, {kings, qty()}
		}.
-type position_error() :: empty_position | not_8_ranks | empty_rank | rank_error() | pieces_totals_error().
-type castlig_error() :: empty_castling | {invalid_character, [byte()]}.
-type fen_error() :: empty_fen | invalid_fen_string | bad_data_type | too_few_parts
					| {position, position_error()}
					| {invalid_active_color, binary()}
					| {castling, castlig_error()}
					| {invalid_enpassant, binary()}
					| {invalid_halfmove, binary()}
					| {invalid_fullmove, binary()}.

-type parsed_fen() :: #parsed_fen{}.

-export_type([fen/0, position/0, piece_char/0, parsed_fen/0, fen_error/0]).
-export_type([halfmove/0, fullmove/0]).


%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% parse/1
-spec parse(fen()) -> {ok, parsed_fen()} | {error, fen_error()}.
parse(<<>>) ->
	fen_error(empty_fen);
parse(Fen) when is_binary(Fen) ->
	case split_fen(Fen) of
		{ok, Parts} ->
			parse(Parts, #parsed_fen{});
		error ->
			fen_error(too_few_parts)
	end;
parse(Fen) when is_list(Fen) ->
	try erlang:list_to_binary(Fen) of
		Bin -> parse(Bin)
	catch
		_:_ -> fen_error(invalid_fen_string)
	end;
parse(_) ->
	fen_error(bad_data_type).

%% initial/0
-spec initial() -> initial_fen().
initial() ->
	?INITIAL_FEN.

%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

%% fen_error/1
-spec fen_error(Reason) -> {error, Reason} when Reason :: fen_error().
fen_error(Reason) ->
	{error, Reason}.


%% split_fen/1
-spec split_fen(binary()) -> {ok, fen_part_tuples()} | error.
split_fen(Fen) ->
	case uef_bin:split(Fen, <<$\s>>) of
		[Position, SideToMove, Castling, Enpassant, Halfmove, Fullmove] -> % six parts
			{ok, [{position, Position}, {sidetomove, SideToMove}, {castling, Castling}, {enpassant, Enpassant}, {halfmove, Halfmove}, {fullmove, Fullmove}]};
		[Position, SideToMove, Castling, Enpassant] -> % four parts
			{ok, [{position, Position}, {sidetomove, SideToMove}, {castling, Castling}, {enpassant, Enpassant}, {halfmove, <<"0">>}, {fullmove, <<"1">>}]};
		_ ->
			error
	end.

%% split_fen_position/1
-spec split_fen_position(binary()) -> {ok, [fen_rank_idx(),...]} | error.
split_fen_position(Pos) ->
	case uef_bin:split(Pos, <<"/">>) of
		[R8, R7, R6, R5, R4, R3, R2, R1] ->
			{ok, [{R8,?A8_IDX}, {R7,?A7_IDX}, {R6,?A6_IDX}, {R5,?A5_IDX}, {R4,?A4_IDX}, {R3,?A3_IDX}, {R2,?A2_IDX}, {R1,?A1_IDX}]};
		_ ->
			error
	end.

%% parse/2
-spec parse([fen_part_tuple()], parsed_fen()) -> {ok, parsed_fen()} | {error, fen_error()}.
parse([], ParsedFen) ->
	{ok, ParsedFen};
parse([{position, FenPosition} | Tail], ParsedFen) -> % 1. Piece placement
	case parse_position(FenPosition) of
		{ok, Position} ->
			parse(Tail, ParsedFen#parsed_fen{position = Position});
		{error, Reason} ->
			fen_error({position, Reason})
	end;
parse([{sidetomove, SideToMove} | Tail], ParsedFen) -> % 2. Active color
	case SideToMove of
		<<"w">> -> parse(Tail, ParsedFen#parsed_fen{sidetomove = $w});
		<<"b">> -> parse(Tail, ParsedFen#parsed_fen{sidetomove = ?BB});
		_       -> fen_error({invalid_active_color, SideToMove})
	end;
parse([{castling, Castling} | Tail], ParsedFen) -> % 3. Castling availability
	case parse_castling(Castling, ParsedFen) of
		{castling, ParsedFen2} -> parse(Tail, ParsedFen2);
		{error, Reason} -> fen_error({castling, Reason})
	end;
parse([{enpassant, Enpassant} | Tail], ParsedFen) -> % 4. En passant target square in algebraic notation
	case parse_enpassant(Enpassant) of
		{ok, Enp} -> parse(Tail, ParsedFen#parsed_fen{enpassant = Enp});
		error -> fen_error({invalid_enpassant, Enpassant})
	end;
parse([{halfmove, Halfmove} | Tail], ParsedFen) -> % 5. Halfmove clock
	case parse_halfmove(Halfmove) of
		{ok, Num} -> parse(Tail, ParsedFen#parsed_fen{halfmove = Num});
		error -> fen_error({invalid_halfmove, Halfmove})
	end;
parse([{fullmove, Fullmove} | Tail], ParsedFen) -> % 5. Halfmove clock
	case parse_fullmove(Fullmove) of
		{ok, Num} -> parse(Tail, ParsedFen#parsed_fen{fullmove = Num});
		error -> fen_error({invalid_fullmove, Fullmove})
	end;
parse([_ | Tail], ParsedFen) ->
	parse(Tail, ParsedFen).


%% parse_position/1
-spec parse_position(binary()) -> {ok, position()} | {error, position_error()}.
parse_position(<<>>) ->
	{error, empty_position};
parse_position(FenPosition) ->
	case split_fen_position(FenPosition) of
		{ok, Tuples} -> parse_position(Tuples, [], ?NO_PIECES_MAP);
		error -> {error, not_8_ranks}
	end.

%% parse_position/3
-spec parse_position([fen_rank_idx()], position(), pieces_totals()) -> {ok, position()} | {error, Error} when
	Error :: pieces_totals_error() | empty_rank | rank_error().
parse_position([], Position, Totals) ->
	case validate_pieces_totals(Totals) of
		ok    -> {ok, Position};
		Error -> Error
	end;
parse_position([{<<>>, _} | _], _, _) ->
	{error, empty_rank};
parse_position([{Rank, Idx1} | Tail], Position, Totals) ->
	case parse_pos_rank(Rank, Idx1, Position, Totals, Idx1) of
		{ok, Position2, Totals2} -> parse_position(Tail, Position2, Totals2);
		Error -> Error
	end.

%% parse_pos_rank/5
-spec parse_pos_rank(binary(), sq_idx(), position(), pieces_totals(), cur_idx()) -> {ok, position(), pieces_totals()} | {error, rank_error()}.
parse_pos_rank(<<>>, Idx1, Position, Totals, CurIdx) ->
	case CurIdx =:= (Idx1 + 8) of
		true  ->
			{ok, Position, Totals};
		false ->
			{error, {last_index_mismatch, CurIdx, {rank, binbo_board:rank_number(Idx1)}}}
	end;
parse_pos_rank(<<_/bits>>, Idx1, _, _, CurIdx) when CurIdx > (Idx1 + 7) ->
	{error, {index_out_of_range, CurIdx, {rank, binbo_board:rank_number(Idx1)}}};
parse_pos_rank(<<Char:8, Rest/bits>>, Idx1, Position, Totals, CurIdx) ->
	case maps:find(Char, Totals) of
		{ok, Num} -> % piece
			Piece = ?CHAR_TO_PIECE(Char),
			Position2 = [{CurIdx, Piece}|Position],
			Totals2 = maps:update(Char, Num + 1, Totals),
			parse_pos_rank(Rest, Idx1, Position2, Totals2, CurIdx + 1);
		error -> % not piece
			case (Char >= $1 andalso Char =< $8) of
				true  ->
					% $0 =:= 48.
					CurIdx2 = CurIdx + (Char - $0),
					parse_pos_rank(Rest, Idx1, Position, Totals, CurIdx2);
				false ->
					{error, {invalid_character, [Char]}}
			end
	end.


% validate_pieces_totals/1
-spec validate_pieces_totals(pieces_totals() | [Tuple]) -> ok | {error, pieces_totals_error()} when
	Tuple :: {atom_color(), P :: qty(), N :: qty(), B :: qty(), R :: qty(), Q :: qty(), K :: qty()}.
validate_pieces_totals(Totals) when is_map(Totals) ->
	#{
		?WP := Wp, ?WN := Wn, ?WB := Wb, ?WR := Wr, ?WQ := Wq, ?WK := Wk,
		?BP := Bp, ?BN := Bn, ?BB := Bb, ?BR := Br, ?BQ := Bq, ?BK := Bk
	} = Totals,
	Wtuple = {white, Wp,Wn,Wb,Wr,Wq,Wk},
	Btuple = {black, Bp,Bn,Bb,Br,Bq,Bk},
	validate_pieces_totals([Wtuple, Btuple]);
validate_pieces_totals([]) -> ok;
validate_pieces_totals([{Color, P, N, B, R, Q, K} | Tail]) ->
	Total = P + N + B + R + Q + K,
	IsOk = (K =:= 1) andalso (Total < 17) andalso (P < 9),
	case IsOk of
		true  ->
			validate_pieces_totals(Tail);
		false when K < 1 ->
			{error, {no_kings, Color}};
		false when K > 1 ->
			{error, {too_many_kings, Color, K}};
		false when P > 8 ->
			{error, {too_many_pawns, Color, P}};
		false ->
			{error, {bad_totals, Color, {total, Total}, {pawns, P}, {knights, N}, {bishops, B}, {rooks, R}, {queens, Q}, {kings, K}}}
	end.


%% parse_castling/2
-spec parse_castling(binary(), parsed_fen()) -> {castling, parsed_fen()} | {error, Error} when
	Error :: empty_castling | {invalid_character, [byte()]}.
parse_castling(<<>>, _) ->
	{error, empty_castling};
parse_castling(<<"-">>, ParsedFen) -> % neither side can castle
	ParsedFen2 = ParsedFen#parsed_fen{castling = ?CASTLING_NONE},
	{castling, ParsedFen2};
parse_castling(Castling, ParsedFen) ->
	parse_castling_1(Castling, ParsedFen).

%% parse_castling_1/2
-spec parse_castling_1(binary(), parsed_fen()) -> {castling, parsed_fen()} | {error, {invalid_character, [byte()]}}.
parse_castling_1(<<>>, ParsedFen) ->
	{castling, ParsedFen};
parse_castling_1(<<Char:8, Rest/bits>>, #parsed_fen{castling = Castling} = ParsedFen) ->
	Castling2 = case Char of
		?WK -> (Castling bor ?CASTLING_W_OO);
		?WQ -> (Castling bor ?CASTLING_W_OOO);
		?BK -> (Castling bor ?CASTLING_B_OO);
		?BQ -> (Castling bor ?CASTLING_B_OOO);
		_   -> error
	end,
	case is_integer(Castling2) of
		true ->
			parse_castling_1(Rest, ParsedFen#parsed_fen{castling = Castling2});
		false ->
			{error, {invalid_character, [Char]}}
	end.


%% parse_enpassant/1
-spec parse_enpassant(binary()) -> {ok, none | binbo_board:square_index()} | error.
parse_enpassant(<<"-">>) ->
	{ok, none};
parse_enpassant(<<F:8, R:8>>) when (F >= $a andalso F =< $h) andalso (R =:= $3 orelse R =:= $6) ->
	{ok, binbo_board:notation_to_index(F, R)};
parse_enpassant(_) ->
	error.

%% parse_halfmove/1
-spec parse_halfmove(binary()) -> {ok, halfmove()} | error.
parse_halfmove(Bin) ->
	try erlang:binary_to_integer(Bin) of
		Num when Num >= 0 -> {ok, Num};
		_ -> error
	catch
		_:_ -> error
	end.


%% parse_fullmove/1
-spec parse_fullmove(binary()) -> {ok, fullmove()} | error.
parse_fullmove(Bin) ->
	try erlang:binary_to_integer(Bin) of
		Num when Num > 0 -> {ok, Num};
		0 -> {ok, 1};
		_ -> error
	catch
		_:_ -> error
	end.


%%%------------------------------------------------------------------------------
%%%   FEN Definition: https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
%%%------------------------------------------------------------------------------

%%% A FEN "record" defines a particular game position, all in one text line and using only the ASCII character set.
%%% A text file with only FEN data records should have the file extension ".fen".[1]
%%%
%%% A FEN record contains six fields. The separator between fields is a space. The fields are:

%%%------------------------------------------------------------------------------
%%% 1. Piece placement (from White's perspective).
%%%------------------------------------------------------------------------------
%%% Each rank is described, starting with rank 8 and ending with rank 1;
%%% within each rank, the contents of each square are described from file "a" through file "h".
%%% Following the Standard Algebraic Notation (SAN), each piece is identified by a single letter
%%% taken from the standard English names (pawn = "P", knight = "N", bishop = "B", rook = "R", queen = "Q" and king = "K").[1]
%%% White pieces are designated using upper-case letters ("PNBRQK") while black pieces use lowercase ("pnbrqk").
%%% Empty squares are noted using digits 1 through 8 (the number of empty squares), and "/" separates ranks.

%%%------------------------------------------------------------------------------
%%% 2. Active color.
%%%------------------------------------------------------------------------------
%%% "w" means White moves next, "b" means Black moves next.

%%%------------------------------------------------------------------------------
%%% 3. Castling availability.
%%%------------------------------------------------------------------------------
%%% If neither side can castle, this is "-".
%%% Otherwise, this has one or more letters:
%%% "K" (White can castle kingside),
%%% "Q" (White can castle queenside),
%%% "k" (Black can castle kingside),
%%% and/or "q" (Black can castle queenside).

%%%------------------------------------------------------------------------------
%%% 4. En passant target square in algebraic notation.
%%%------------------------------------------------------------------------------
%%% If there's no en passant target square, this is "-".
%%% If a pawn has just made a two-square move, this is the position "behind" the pawn.
%%% This is recorded regardless of whether there is a pawn in position to make an en passant capture.[2]

%%%------------------------------------------------------------------------------
%%% 5. Halfmove clock:
%%%------------------------------------------------------------------------------
%%% This is the number of halfmoves since the last capture or pawn advance.
%%% This is used to determine if a draw can be claimed under the fifty-move rule.

%%%------------------------------------------------------------------------------
%%% 6. Fullmove number:
%%%------------------------------------------------------------------------------
%%% The number of the full move. It starts at 1, and is incremented after Black's move.

%%%------------------------------------------------------------------------------
%%% Here is the FEN for the starting position:
%%%------------------------------------------------------------------------------
%%% rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1

%%%------------------------------------------------------------------------------
%%% Here is the FEN after the move 1. e4:
%%%------------------------------------------------------------------------------
%%% rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1

%%%------------------------------------------------------------------------------
%%% And then after 1. ... c5:
%%%------------------------------------------------------------------------------
%%% rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2

%%%------------------------------------------------------------------------------
%%% And then after 2. Nf3:
%%%------------------------------------------------------------------------------
%%% rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2



%%%------------------------------------------------------------------------------
%%% How to know when a FEN position is legal?
%%% https://chess.stackexchange.com/questions/1482/how-to-know-when-a-fen-position-is-legal
%%%------------------------------------------------------------------------------

% Here is a well organized list that should validate 99.99%+ of common positions:

% Board:

% There are exactly 8 cols
% The row sum of the empty squares and pieces add to 8
% Kings:

% See if there is exactly one w_king and one b_king
% Make sure kings are separated 1 square apart
% Checks:

% Non-active color is not in check
% Active color is checked less than 3 times; in case of 2
% that it is never pawn+(pawn, bishop, knight), bishop+bishop, knight+knight
% Pawns:

% There are no more than 8 pawns from each color
% There aren't any pawns in first or last rows
% In case of en passant square; see if it was legally created
% (e.g it must be on the x3 or x6 row,
% there must be a pawn (from the correct color) in front of it,
% and the en passant square and the one behind it are empty)
% Prevent having more promoted pieces than missing pawns (e.g extra_pieces = Math.max(0, num_queens-1) + Math.max(0, num_rooks-2)... and then extra_pieces <= (8-num_pawns)), also you should do special calculations for bishops, If you have two (or more) bishops from the same square color, these can only be created through pawn promotion and you should include this information to the formula above somehow
% The pawn formation is possible to reach (e.g in case of multiple pawns in a single col, there must be enough enemy pieces missing to make that formation), here are some useful rules:
% it is impossible to have more than 6 pawns in a single column (because pawns can't exist in the first and last ranks)
% the minimum number of enemy missing pieces to reach a multiple pawn in a single col B to G 2=1, 3=2, 4=4, 5=6, 6=9 ___ A and H 2=1, 3=3, 4=6, 5=10, 6=15, for example, if you see 5 pawns in A or H, the other player must be missing at least 10 pieces from his 15 captureable pieces
% if there are white pawns in a2 and a3, there can't legally be one in b2, and this idea can be further expanded to cover more possibilities
% Castling:

% If the king or rooks are not in their starting position; the castling ability for that side is lost (in the case of king, both are lost)
% Bishops:

% Look for bishops in the first and last rows trapped by pawns that haven't moved, for example:
% a bishop (any color) trapped behind 3 pawns
% a bishop trapped behind 2 non-enemy pawns (not by enemy pawns because we can reach that position by underpromoting pawns, however if we check the number of pawns and extra_pieces we could determine if this case is possible or not)
% Non-jumpers:

% If there are non-jumpers enemy pieces in between the king and rook and there are still some pawns without moving; check if these enemy pieces could have legally gotten in there. Also, ask yourself: was the king or rook needed to move to generate that position? (if yes, we need to make sure the castling abilities reflect this)
% If all 8 pawns are still in the starting position, all the non-jumpers must not have left their initial rank (also non-jumpers enemy pieces can't possibly have entered legally), there are other similar ideas, like if the white h-pawn moved once, the rooks should still be trapped inside the pawn formation, etc.
% Half/Full move Clocks:

% In case of an en passant square, the half move clock must equal to 0
% HalfMoves <= ((FullMoves-1)*2)+(if BlackToMove 1 else 0), the +1 or +0 depends on the side to move
% The HalfMoves must be x >= 0 and the FullMoves x >= 1
% Other:

% Make sure the FEN contains all the parts that are needed (e.g active color, castling ability, en passant square, etc)
% Note: there is no need to make the 'players should not have more than 16 pieces' check because the points 'no more than 8 pawns' + 'prevent extra promoted pieces' + the 'exactly one king' should already cover this point

% Note2: these rules are intended to validate positions arising from the starting position of normal chess, some of the rules will invalidate some positions from Chess960 (exception if started from arrangement Nº518) and generated puzzles.
