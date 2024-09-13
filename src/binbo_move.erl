%% Copyright (c) 2019-2024, Sergei Semichev <chessvegas@chessvegas.com>. All Rights Reserved.
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

-module(binbo_move).

-export([validate_sq_move/2]).
-export([validate_san_move/2]).
-export([validate_idx_move/2]).
-export([validate_int_move/2]).
-export([validate_move/5]).
-export([enemy_color/1]).


%%%------------------------------------------------------------------------------
%%%   Includes
%%%------------------------------------------------------------------------------

-include("binbo_board.hrl").
-include("binbo_move.hrl").

%%%------------------------------------------------------------------------------
%%%   Macros
%%%------------------------------------------------------------------------------

-define(IS_VALID_FILE(F), ((F >= $a) andalso (F =< $h))).
-define(IS_VALID_RANK(R), ((R >= $1) andalso (R =< $8))).
-define(IS_VALID_FILE_RANK(F,R), (?IS_VALID_FILE(F) andalso ?IS_VALID_RANK(R))).


%%%------------------------------------------------------------------------------
%%%   Types
%%%------------------------------------------------------------------------------

-type move_info() :: #move_info{}.
-type bb() :: binbo_bb:bb().
-type piece() :: binbo_board:piece().
-type piece_type() :: binbo_board:piece_type().
-type color() :: binbo_board:color().
-type sq_move() :: binary() | string().
-type idx_move() :: {sq_idx(), sq_idx(), q | r | b | n}.
-type bb_game() :: binbo_position:bb_game().
-type sq_idx() :: binbo_board:square_index().
-type sq_notation() :: binbo_board:square_notation().
-type promo_char() :: ?WN | ?WB | ?WR | ?WQ | ?BN | ?BB | ?BR | ?BQ.
-type promo_type() :: ?KNIGHT | ?BISHOP | ?ROOK | ?QUEEN.
-type pname() :: pawn | knight | bishop | rook | queen | king.
-type castling_flag() :: ?CASTLING_NONE | binbo_board:side_castling().
-type pre_from() :: {sq, $a..$h, $1..$8} | undefined | {file, binbo_board:file()} | {rank, binbo_board:rank()}.
-type pre_parsed_san2() :: {ok, piece_type(), pre_from(), {$a..$h, $1..$8}, promo_type()}.
-type pre_parsed_san() :: {ok, ?KING, 'O-O' | 'O-O-O'} | pre_parsed_san2().
-type san_from_error() :: illegal_san.
-type pre_parse_san_error() :: san_not_parsed | empty_san | bad_san_value | invalid_san_string | bad_data_type.
-type parse_san_error() :: pre_parse_san_error() | san_from_error().
-type game_status_error() :: {game_over, binbo_position:game_over_status()}.
-type sq_error() :: invalid_square_notation.
-type parse_error() :: empty_move | bad_move_value | invalid_move_string | bad_data_type | sq_error().
-type piece_error() :: no_piece | sidetomove_mismatch.
-type chess_error() :: same_square | king_capture | own_piece_capture
                    | {invalid_move, pname()}
                    | binbo_position:make_move_error().
-type idx_move_error() :: {invalid_promotion_type, term()}
                        | {invalid_square_index, term()}
                        | {invalid_move_datatype, term()}.
-type move_overall_error() :: game_status_error() | piece_error() | chess_error().
-type move_error() :: {parse, parse_error() | parse_san_error()} | move_overall_error().

-export_type([move_info/0, promo_type/0, sq_move/0, castling_flag/0, move_error/0]).

%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% validate_sq_move/2
-spec validate_sq_move(sq_move(), bb_game()) -> {ok, move_info(), bb_game()} | {error, move_error()}.
validate_sq_move(SqMove, Game) ->
    case parse_sq_move(SqMove) of
        {ok, FromIdx, ToIdx, PromoType} ->
            validate_move_overall(Game, FromIdx, ToIdx, PromoType);
        {error, Reason} ->
            {error, {parse, Reason}}
    end.

%% validate_san_move/2
-spec validate_san_move(sq_move(), bb_game()) -> {ok, move_info(), bb_game()} | {error, move_error()}.
validate_san_move(San, Game) ->
    case parse_san(San, Game) of
        {ok, FromIdx, ToIdx, PromoType} ->
            validate_move_overall(Game, FromIdx, ToIdx, PromoType);
        {error, Reason} ->
            {error, {parse, Reason}}
    end.

%% validate_idx_move/2
-spec validate_idx_move(idx_move(), bb_game()) -> {ok, move_info(), bb_game()} | {error, idx_move_error()} | {error, move_overall_error()}.
validate_idx_move({FromIdx, ToIdx, Promo}, Game) when (?IS_VALID_INDEX(FromIdx) andalso ?IS_VALID_INDEX(ToIdx)) ->
    CheckPromo = case Promo of
        q -> {ok, ?QUEEN};
        r -> {ok, ?ROOK};
        b -> {ok, ?BISHOP};
        n -> {ok, ?KNIGHT};
        _ -> error
    end,
    case CheckPromo of
        {ok, PromoType} -> validate_move_overall(Game, FromIdx, ToIdx, PromoType);
        error -> {error, {invalid_promotion_type, Promo}}
    end;
validate_idx_move({FromIdx, _ToIdx, _Promo}, _Game) when (not ?IS_VALID_INDEX(FromIdx)) ->
    {error, {invalid_square_index, FromIdx}};
validate_idx_move({_FromIdx, ToIdx, _Promo}, _Game) when (not ?IS_VALID_INDEX(ToIdx)) ->
    {error, {invalid_square_index, ToIdx}};
validate_idx_move(Move, _Game) ->
    {error, {invalid_move_datatype, Move}}.

%% validate_int_move/2
-spec validate_int_move(non_neg_integer(), bb_game()) -> {ok, move_info(), bb_game()} | {error, move_overall_error()}.
validate_int_move(Move, Game) ->
    FromIdx = binbo_board:int_move_from(Move),
    ToIdx = binbo_board:int_move_to(Move),
    PromoType = ((Move bsr 12) band 3) + ?KNIGHT,
    validate_move_overall(Game, FromIdx, ToIdx, PromoType).

%% validate_move/5
%% The only right way to call this function OUTSIDE is from 'binbo_movegen' module.
-spec validate_move(bb_game(), piece(), sq_idx(), sq_idx(), promo_type()) -> {ok, move_info(), bb_game()} | {error, chess_error()}.
validate_move(Game, Piece, FromIdx, ToIdx, PromoType) ->
    MoveInfo = #move_info{from_idx = FromIdx, to_idx = ToIdx, piece = Piece, promo = PromoType},
    do_validate_move(Game, MoveInfo).

%% enemy_color/1
-spec enemy_color(move_info()) -> color().
enemy_color(#move_info{pcolor = Pcolor}) ->
    binbo_board:enemy_color(Pcolor).


%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

%% validate_move_overall/4
-spec validate_move_overall(bb_game(), sq_idx(), sq_idx(), promo_type()) -> {ok, move_info(), bb_game()} | {error, move_overall_error()}.
validate_move_overall(Game, FromIdx, ToIdx, PromoType) ->
    case validate_game_status(Game) of
        ok ->
            case validate_piece(FromIdx, Game) of
                {ok, Piece} ->
                    validate_move(Game, Piece, FromIdx, ToIdx, PromoType);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% parse_sq_move/1
-spec parse_sq_move(sq_move()) -> {ok, sq_idx(), sq_idx(), promo_type()} | {error, parse_error()}.
parse_sq_move(<<>>) -> % empty binary
    {error, empty_move};
parse_sq_move(<<Sq1:16/bits, Sq2:16/bits>>) -> % example: <<"e2e4">>
    parse_sq_move(Sq1, Sq2, undefined);
parse_sq_move(<<Sq1:16/bits, Sq2:16/bits, Char:8>>) -> % example: <<"e2e4">>
    parse_sq_move(Sq1, Sq2, Char);
parse_sq_move(<<_/bits>>) -> % any other binary
    {error, bad_move_value};
parse_sq_move([_|_] = Move) -> % string value
    try erlang:list_to_binary(Move) of
        Bin -> parse_sq_move(Bin)
    catch
        _:_ -> {error, invalid_move_string}
    end;
parse_sq_move(_) -> % other
    {error, bad_data_type}.

%% parse_sq_move/3
-spec parse_sq_move(sq_notation(), sq_notation(), undefined | promo_char()) ->
    {ok, sq_idx(), sq_idx(), promo_type()} | {error, sq_error()}.
parse_sq_move(Sq1, Sq2, PromoChar) ->
    case are_squares_valid(Sq1, Sq2) of
        true  ->
            From = binbo_board:notation_to_index(Sq1),
            To   = binbo_board:notation_to_index(Sq2),
            {ok, From, To, char_to_promo_type(PromoChar)};
        false ->
            {error, invalid_square_notation}
    end.

%% char_to_promo_type/1
-spec char_to_promo_type(undefined | promo_char()) -> promo_type().
char_to_promo_type(undefined) ->
    ?QUEEN;
char_to_promo_type(PromoChar) ->
    case lists:member(PromoChar, "QqRrNnBb") of
        true  ->
            Piece = ?CHAR_TO_PIECE(PromoChar),
            ?PIECE_TYPE(Piece);
        false ->
            ?QUEEN
    end.

%% are_squares_valid/2
-spec are_squares_valid(sq_notation(), sq_notation()) -> boolean().
are_squares_valid(Sq1, Sq2) ->
    binbo_board:is_valid_square_notation(Sq1)
    andalso
    binbo_board:is_valid_square_notation(Sq2).


%% do_validate_move/2
-spec do_validate_move(bb_game(), move_info()) -> {ok, move_info(), bb_game()} | {error, chess_error()}.
do_validate_move(Game, MoveInfo) ->
    Steps = [same_square, piece, capture, ptype],
    do_validate_move(Steps, Game, MoveInfo).

%% do_validate_move/3
-spec do_validate_move([Step,...], bb_game(), move_info()) -> {ok, move_info(), bb_game()} | {error, chess_error()}
    when Step :: same_square | piece | capture | ptype.
do_validate_move([], Game, MoveInfo) ->
    % Ok, so far, change position now
    case binbo_position:make_move(MoveInfo, Game) of
        {ok, Game2} -> {ok, MoveInfo, Game2};
        {error, _} = Error -> Error
    end;
do_validate_move([same_square|Tail], Game, #move_info{from_idx = From, to_idx = To} = MoveInfo) ->
    %% Check if squares are the same
    case From =/= To of
        true  -> % different squares (ok)
            MoveInfo2 = MoveInfo#move_info{
                from_bb = ?SQUARE_BB(From),
                to_bb = ?SQUARE_BB(To)
            },
            do_validate_move(Tail, Game, MoveInfo2);
        false -> % same square (error)
            {error, same_square}
    end;
do_validate_move([piece|Tail], Game, #move_info{piece = Piece} = MoveInfo) ->
    true = ?IS_PIECE(Piece), % ensure piece
    MoveInfo2 = MoveInfo#move_info{
        piece = Piece,
        pcolor = ?COLOR(Piece),
        ptype = ?PIECE_TYPE(Piece)
    },
    do_validate_move(Tail, Game, MoveInfo2);
do_validate_move([capture|Tail], Game, #move_info{to_idx = To, pcolor = Pcolor} = MoveInfo) ->
    % Validate a capture.
    Captured = binbo_position:get_piece(To, Game),
    CaptColor = ?COLOR(Captured),
    case ?IS_PIECE(Captured) of
        true when ?PIECE_TYPE(Captured) =:= ?KING -> % king captured (error)
            {error, king_capture};
        true when Pcolor =:= CaptColor -> % own piece captured (error)
            {error, own_piece_capture};
        true -> % enemy piece captured (ok)
            do_validate_move(Tail, Game, MoveInfo#move_info{captured = Captured, captured_idx = To});
        false -> % moved on empty square (ok)
            % We handle en-passant move later
            do_validate_move(Tail, Game, MoveInfo)
    end;
do_validate_move([ptype|Tail], Game, #move_info{to_bb = ToBB} = MoveInfo) -> % ptype
    % Validate move regarding piece type (pawn, knight, bishop, rook, queen, king).
    case piece_move(Game, MoveInfo) of
        {MovesBB, MoveInfo2} when ?IS_AND(ToBB, MovesBB) -> % valid move
            do_validate_move(Tail, Game, MoveInfo2);
        {_, _} -> % invalid_move
            Piece = MoveInfo#move_info.piece,
            Pname = ?PIECE_TO_ATOM(Piece),
            {error, {invalid_move, Pname}}
    end.


%% validate_piece/2
-spec validate_piece(sq_idx(), bb_game()) -> {ok, piece()} | {error, piece_error()}.
validate_piece(From, Game) ->
    % Check whether the moving piece is really a piece or not.
    % If it is a piece, we check whether its color matches 'sidetomove' value or not.
    Piece = binbo_position:get_piece(From, Game),
    case ?IS_PIECE(Piece) of
        true -> % piece
            SideToMove = binbo_position:get_sidetomove(Game),
            case ?COLOR(Piece) of
                SideToMove ->
                    {ok, Piece};
                _ ->
                    {error, sidetomove_mismatch}
            end;
        false -> % empty square
            {error, no_piece}
    end.

%% validate_game_status/1
-spec validate_game_status(bb_game()) -> ok | {error, game_status_error()}.
validate_game_status(Game) ->
    Status = binbo_position:get_status(Game),
    case binbo_position:is_status_inprogress(Status) of
        true  -> ok;
        false -> {error, {game_over, Status}}
    end.


%% piece_move/2
-spec piece_move(bb_game(), move_info()) -> {bb(), move_info()}.
piece_move(Game, #move_info{ptype = Ptype} = MoveInfo) ->
    case Ptype of
        ?PAWN   -> pawn_move(Game, MoveInfo);
        ?KNIGHT -> knight_move(Game, MoveInfo);
        ?BISHOP -> bishop_move(Game, MoveInfo);
        ?ROOK   -> rook_move(Game, MoveInfo);
        ?QUEEN  -> queen_move(Game, MoveInfo);
        ?KING   -> king_move(Game, MoveInfo)
    end.


%% pawn_move/2
-spec pawn_move(bb_game(), move_info()) -> {bb(), move_info()}.
pawn_move(Game, MoveInfo) ->
    #move_info{from_idx = FromIdx, pcolor = Pcolor, to_bb = ToBB} = MoveInfo,
    PosEnpaBB = binbo_position:get_enpassant_bb(Game),
    MovesBB = binbo_position:pawn_moves_bb(FromIdx, Pcolor, Game, PosEnpaBB),
    MoveInfo2 = case (MovesBB =/= ?EMPTY_BB) andalso (ToBB =:= PosEnpaBB) of
        true -> % en-passant capture
            ToIdx = MoveInfo#move_info.to_idx,
            CapturedIdx = case Pcolor of
                ?WHITE -> ToIdx - 8;
                ?BLACK -> ToIdx + 8
            end,
            CapturedPiece = binbo_position:get_piece(CapturedIdx, Game),
            ?PAWN = ?PIECE_TYPE(CapturedPiece), % ensure pawn is on square
            true = (?COLOR(CapturedPiece) =:= ?SWITCH_COLOR(Pcolor)), % ensure pawn belongs to enemy side
            MoveInfo#move_info{captured = CapturedPiece, captured_idx = CapturedIdx};
        false -> % other
            MoveInfo
    end,
    {MovesBB, MoveInfo2}.


%% knight_move/2
-spec knight_move(bb_game(), move_info()) -> {bb(), move_info()}.
knight_move(Game, MoveInfo) ->
    #move_info{from_idx = FromIdx, pcolor = Pcolor} = MoveInfo,
    MovesBB = binbo_position:knight_moves_bb(FromIdx, Pcolor, Game),
    {MovesBB, MoveInfo}.


%% bishop_move/2
-spec bishop_move(bb_game(), move_info()) -> {bb(), move_info()}.
bishop_move(Game, MoveInfo) ->
    #move_info{from_idx = FromIdx} = MoveInfo,
    MovesBB = binbo_position:bishop_moves_bb(FromIdx, Game),
    {MovesBB, MoveInfo}.

%% rook_move/2
-spec rook_move(bb_game(), move_info()) -> {bb(), move_info()}.
rook_move(Game, MoveInfo) ->
    #move_info{from_idx = FromIdx} = MoveInfo,
    MovesBB = binbo_position:rook_moves_bb(FromIdx, Game),
    {MovesBB, MoveInfo}.

%% queen_move/2
-spec queen_move(bb_game(), move_info()) -> {bb(), move_info()}.
queen_move(Game, MoveInfo) ->
    #move_info{from_idx = FromIdx} = MoveInfo,
    MovesBB = binbo_position:queen_moves_bb(FromIdx, Game),
    {MovesBB, MoveInfo}.

%% king_move/2
-spec king_move(bb_game(), move_info()) -> {bb(), move_info()}.
king_move(Game, MoveInfo) ->
    #move_info{from_idx = FromIdx, to_idx = ToIdx, pcolor = Pcolor} = MoveInfo,
    MovesBB = binbo_position:king_moves_bb(FromIdx, Pcolor, Game),
    CastlingFlag = castling_flag(Pcolor, FromIdx, ToIdx),
    {MovesBB, MoveInfo#move_info{castling = CastlingFlag}}.


%% castling_flag/3
-spec castling_flag(color(), sq_idx(), sq_idx()) -> castling_flag().
castling_flag(?WHITE, ?E1_IDX, ?G1_IDX) -> ?CASTLING_W_OO;
castling_flag(?WHITE, ?E1_IDX, ?C1_IDX) -> ?CASTLING_W_OOO;
castling_flag(?BLACK, ?E8_IDX, ?G8_IDX) -> ?CASTLING_B_OO;
castling_flag(?BLACK, ?E8_IDX, ?C8_IDX) -> ?CASTLING_B_OOO;
castling_flag(_, _, _) -> ?CASTLING_NONE.


%% parse_san/2
-spec parse_san(sq_move(), bb_game()) -> {ok, sq_idx(), sq_idx(), promo_type()} | {error, parse_san_error()}.
parse_san(San, Game) ->
    Pcolor = binbo_position:get_sidetomove(Game),
    case pre_parse_san(San) of
        {ok, ?KING, 'O-O'} ->
            case Pcolor of
                ?WHITE -> {ok, binbo_board:notation_to_index(<<"e1">>), binbo_board:notation_to_index(<<"g1">>), ?QUEEN};
                ?BLACK -> {ok, binbo_board:notation_to_index(<<"e8">>), binbo_board:notation_to_index(<<"g8">>), ?QUEEN}
            end;
        {ok, ?KING, 'O-O-O'} ->
            case Pcolor of
                ?WHITE -> {ok, binbo_board:notation_to_index(<<"e1">>), binbo_board:notation_to_index(<<"c1">>), ?QUEEN};
                ?BLACK -> {ok, binbo_board:notation_to_index(<<"e8">>), binbo_board:notation_to_index(<<"c8">>), ?QUEEN}
            end;
        {ok, Ptype, From0, {ToFile, ToRank}, PromoType} ->
            ToIdx = binbo_board:notation_to_index(ToFile, ToRank),
            case san_starting_square(Ptype, Pcolor, From0, ToIdx, Game) of
                {ok, FromIdx} -> {ok, FromIdx, ToIdx, PromoType};
                {error, _} = Error -> Error
            end;
        {error, _} = Error ->
            Error
    end.

%% pre_parse_san/1
-spec pre_parse_san(sq_move()) -> pre_parsed_san() | {error, pre_parse_san_error()}.
pre_parse_san(<<>>) -> % empty binary
    {error, empty_san};
pre_parse_san(<<"O-O-O", _/binary>>) -> % castling queenside
    {ok, ?KING, 'O-O-O'};
pre_parse_san(<<"O-O", _/binary>>) -> % castling kingside
    {ok, ?KING, 'O-O'};
pre_parse_san(<<Pchar:8, Rest/binary>> = San) ->
    case lists:member(Pchar, "PNBRQK") of
        true  ->
            Piece = ?CHAR_TO_PIECE(Pchar),
            pre_parse_san(Rest, ?PIECE_TYPE(Piece));
        false ->
            pre_parse_san(San, ?PAWN)
    end;
pre_parse_san(<<_/bits>>) ->
    {error, bad_san_value};
pre_parse_san([_|_] = San) -> % string value
    try erlang:list_to_binary(San) of
        Bin -> pre_parse_san(Bin)
    catch
        _:_ -> {error, invalid_san_string}
    end;
pre_parse_san(_) -> % other
    {error, bad_data_type}.

%% pre_parse_san/2
-spec pre_parse_san(binary(), piece_type()) -> pre_parsed_san2() | {error, san_not_parsed}.
pre_parse_san(San, Ptype) ->
    Parsed = case San of
        % example: a1-b2, a1xb2
        <<F1,R1, C, F2,R2, Rest/binary>> when ?IS_VALID_FILE_RANK(F1,R1) andalso ?IS_VALID_FILE_RANK(F2,R2) andalso (C =:= $x orelse C =:= $-) ->
            {{sq, F1,R1}, {F2,R2}, Rest};
        % example: a1b2, a1b2+
        <<F1,R1, F2,R2, Rest/binary>> when ?IS_VALID_FILE_RANK(F1,R1) andalso ?IS_VALID_FILE_RANK(F2,R2) ->
            {{sq, F1,R1}, {F2,R2}, Rest};
        % example: Ngxe2, Ngxe2+
        <<F1, C, F2,R2, Rest/binary>> when ?IS_VALID_FILE(F1) andalso ?IS_VALID_FILE_RANK(F2,R2) andalso (C =:= $x orelse C =:= $-) ->
            {{file, F1 - $a}, {F2,R2}, Rest};
        % example: Nge2, Nge2+
        <<F1, F2,R2, Rest/binary>> when ?IS_VALID_FILE(F1) andalso ?IS_VALID_FILE_RANK(F2,R2) ->
            {{file, F1 - $a}, {F2,R2}, Rest};
        % example: R1xa3, R1xa3+
        <<R1, C, F2,R2, Rest/binary>> when ?IS_VALID_RANK(R1) andalso ?IS_VALID_FILE_RANK(F2,R2) andalso (C =:= $x orelse C =:= $-) ->
            {{rank, R1 - $1}, {F2,R2}, Rest};
        % example: R1a3, R1a3+
        <<R1, F2,R2, Rest/binary>> when ?IS_VALID_RANK(R1) andalso ?IS_VALID_FILE_RANK(F2,R2) ->
            {{rank, R1 - $1}, {F2,R2}, Rest};
        % example: e4, e4+
        <<F,R, Rest/binary>> when ?IS_VALID_FILE_RANK(F,R) ->
            {undefined, {F,R}, Rest};
        % example: Qxd5, Qxd5+
        <<C, F,R, Rest/binary>> when ?IS_VALID_FILE_RANK(F,R) andalso (C =:= $x orelse C =:= $-) ->
            {undefined, {F,R}, Rest};
        % error
        _ ->
            error
    end,
    case Parsed of
        {From, To, <<$=, Char, _/binary>>} when Ptype =:= ?PAWN ->
            {ok, Ptype, From, To, char_to_promo_type(Char)};
        {From, To, <<Char, _/binary>>} when Ptype =:= ?PAWN ->
            {ok, Ptype, From, To, char_to_promo_type(Char)};
        {From, To, _} ->
            {ok, Ptype, From, To, ?QUEEN};
        error ->
            {error, san_not_parsed}
    end.


%% san_starting_square/5
-spec san_starting_square(piece_type(), color(), pre_from(), sq_idx(), bb_game()) -> {ok, sq_idx()} | {error, san_from_error()}.
san_starting_square(_Ptype, _Pcolor, {sq, File,Rank}, _ToIdx, _Game) ->
    {ok, binbo_board:notation_to_index(File, Rank)};
san_starting_square(Ptype, Pcolor, From0, ToIdx, Game) ->
    Piece = ?TYPE_TO_PIECE(Ptype, Pcolor),
    IdxList = case From0 of
        undefined ->
            binbo_position:get_piece_indexes(Piece, Game);
        {file, File} ->
            binbo_position:get_piece_indexes_on_file(Piece, File, Game);
        {rank, Rank} ->
            binbo_position:get_piece_indexes_on_rank(Piece, Rank, Game)
    end,
    ToBB = ?SQUARE_BB(ToIdx),
    Search = uef_lists:search(fun(FromIdx) ->
        MovesBB = binbo_position:piece_moves_bb(FromIdx, Ptype, Pcolor, Game),
        ?IS_AND(ToBB, MovesBB)
    end, IdxList),
    case Search of
        {value, FromIdx} ->
            {ok, FromIdx};
        false ->
            {error, illegal_san}
    end.
