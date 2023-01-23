%% Copyright (c) 2019-2023, Sergei Semichev <chessvegas@chessvegas.com>. All Rights Reserved.
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

-module(binbo_bb).

-export([bb_not/2, bb_or/1]).
-export([to_index/1, to_file/1, to_index_list/1, to_notation/1]).
-export([rank_bb/1, file_bb/1]).
-export([edges_bb/1, empty_bb/0]).
-export([
    shift/2,
    shift_north/1,
    shift_south/1,
    shift_east/1,
    shift_west/1,
    shift_north_east/1,
    shift_north_west/1,
    shift_south_east/1,
    shift_south_west/1
]).
-export([
    pawn_attacks_bb/2,
    knight_attacks_bb/1,
    bishop_attacks_bb/2,
    rook_attacks_bb/2,
    king_attacks_bb/1,
    pawn_pushes_bb/3
]).
-export([enpassant_bb/3]).

%%%------------------------------------------------------------------------------
%%%   Includes
%%%------------------------------------------------------------------------------

-include("binbo_board.hrl").

%%%------------------------------------------------------------------------------
%%%   Types
%%%------------------------------------------------------------------------------
-type bb() :: ?EMPTY_BB .. ?ALL_SQUARES_BB. % any bitboard including empty
-type piece() :: binbo_board:piece().
-type sq_idx() :: binbo_board:square_index().
-type empty_bb() :: ?EMPTY_BB. % empty bitboard
-type color() :: binbo_board:color().
-type attacks_bb() :: ?A1_BB .. ?ALL_SQUARES_BB.
-type sq_bb() :: ?A1_BB .. ?H8_BB. % square bitboard
-type bishop_direction() :: ?NORTH_WEST | ?NORTH_EAST | ?SOUTH_WEST | ?SOUTH_EAST.
-type rook_direction() :: ?NORTH | ?SOUTH | ?EAST | ?WEST.
-type sliding_direction() :: bishop_direction() | rook_direction().
-type enpa_bb() :: sq_bb() | empty_bb().

-export_type([bb/0, sq_bb/0, empty_bb/0, enpa_bb/0]).

-compile({inline, [bb_not/2]}).
-compile({inline, [to_index/1]}).
-compile({inline, [shift/2]}).

%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% bb_not/2
-spec bb_not(bb(), bb()) -> bb().
bb_not(BB1, BB2) ->
    (BB1 band (bnot BB2)).

%% bb_or/1
-spec bb_or([bb()]) -> bb().
bb_or(List) ->
    bb_or(List, ?EMPTY_BB).

%% rank_bb/1
-spec rank_bb(sq_idx()) -> bb().
rank_bb(Idx) ->
    Rank = binbo_board:rank_of_index(Idx),
    ?RANK_1_BB bsl (8 * Rank).

%% file_bb/1
-spec file_bb(sq_idx()) -> bb().
file_bb(Idx) ->
    File = binbo_board:file_of_index(Idx),
    ?FILE_A_BB bsl File.

%% to_index/1
-spec to_index(sq_bb()) -> sq_idx().
to_index(SqBB) ->
    % The bitboard of square is a single populated bitboard, a power of two value.
    % Therefore, the index of square is equal to base-2 logarithm of the bitboard.
    erlang:trunc(math:log2(SqBB)).

%% to_file/1
-spec to_file(sq_bb()) -> binbo_board:file().
to_file(SqBB) ->
    Idx = to_index(SqBB),
    binbo_board:file_of_index(Idx).

%% to_index_list/1
-spec to_index_list(bb()) -> [sq_idx()].
to_index_list(BB) ->
    to_index_list(BB, []).

%% to_notation/1
-spec to_notation(sq_bb()) -> binbo_board:square_notation().
to_notation(SqBB) ->
    Idx = to_index(SqBB),
    binbo_board:index_to_notation(Idx).

%% edges_bb/1
-spec edges_bb(sq_idx()) -> bb().
edges_bb(Idx) ->
    bb_or([
        bb_not(?RANK_1_BB bor ?RANK_8_BB, rank_bb(Idx)),
        bb_not(?FILE_A_BB bor ?FILE_H_BB, file_bb(Idx))
    ]).

%% empty_bb/0
-spec empty_bb() -> empty_bb().
empty_bb() ->
    ?EMPTY_BB.


%% shift/1
-spec shift(bb(), integer()) -> bb().
shift(BB, Bits) ->
    (BB bsl Bits).

%% shift_north/1
-spec shift_north(bb()) -> bb().
shift_north(BB) ->
    shift(BB, ?NORTH).

%% shift_south/1
-spec shift_south(bb()) -> bb().
shift_south(BB) ->
    shift(BB, ?SOUTH).

%% shift_east/1
-spec shift_east(bb()) -> bb().
shift_east(BB) ->
    shift(BB band ?NOT_FILE_H_BB, ?EAST).

%% shift_west/1
-spec shift_west(bb()) -> bb().
shift_west(BB) ->
    shift(BB band ?NOT_FILE_A_BB, ?WEST).

%% shift_north_east/1
-spec shift_north_east(bb()) -> bb().
shift_north_east(BB) ->
    shift(BB band ?NOT_FILE_H_BB, ?NORTH_EAST).

%% shift_north_west/1
-spec shift_north_west(bb()) -> bb().
shift_north_west(BB) ->
    shift(BB band ?NOT_FILE_A_BB, ?NORTH_WEST).

%% shift_south_east/1
-spec shift_south_east(bb()) -> bb().
shift_south_east(BB) ->
    shift(BB band ?NOT_FILE_H_BB, ?SOUTH_EAST).

%% shift_south_west/1
-spec shift_south_west(bb()) -> bb().
shift_south_west(BB) ->
    shift(BB band ?NOT_FILE_A_BB, ?SOUTH_WEST).


%% enpassant_bb/3
-spec enpassant_bb(piece(), sq_bb(), sq_bb()) -> enpa_bb().
enpassant_bb(?WHITE_PAWN, FromBB, ToBB) when ?IS_AND(FromBB, ?RANK_2_BB) andalso ?IS_AND(ToBB, ?RANK_4_BB) ->
    shift_north(FromBB);
enpassant_bb(?BLACK_PAWN, FromBB, ToBB) when ?IS_AND(FromBB, ?RANK_7_BB) andalso ?IS_AND(ToBB, ?RANK_5_BB) ->
    shift_south(FromBB);
enpassant_bb(_, _, _) ->
    ?EMPTY_BB.


%%%------------------------------------------------------------------------------
%%%   Attack bitboards
%%%------------------------------------------------------------------------------

%% king_attacks_bb/1
%% https://www.chessprogramming.org/King_Pattern
%% https://www.chessprogramming.org/General_Setwise_Operations
%%--------------------
%%  . . . . . . . .
%%  . . . . . . . .
%%  . . . . . . . .
%%  . . . . . . . .
%%  . . . . . . . .
%%  . . . . . 1 2 3
%%  . . . . . 8 K 4
%%  . . . . . 7 6 5
%%--------------------
-spec king_attacks_bb(sq_bb()) -> attacks_bb().
king_attacks_bb(BB) ->
    bb_or([
        shift_north_west(BB), % 1
        shift_north(BB),      % 2
        shift_north_east(BB), % 3
        shift_east(BB),       % 4
        shift_south_east(BB), % 5
        shift_south(BB),      % 6
        shift_south_west(BB), % 7
        shift_west(BB)        % 8
    ]) band ?ALL_SQUARES_BB.


%% knight_attacks_bb/1
%% https://www.chessprogramming.org/Knight_Pattern
%%--------------------
%%  . . . . . . . .
%%  . . . . . . . .
%%  . . 2 . 3 . . .
%%  . 1 . . . 4 . .
%%  . . . N . . . .
%%  . 8 . . . 5 . .
%%  . . 7 . 6 . . .
%%  . . . . . . . .
%%--------------------
-spec knight_attacks_bb(sq_bb()) -> attacks_bb().
knight_attacks_bb(BB) ->
    NotFileA = ?NOT_FILE_A_BB,
    NotFileH = ?NOT_FILE_H_BB,
    NotFileAB = ?NOT_FILE_AB_BB,
    NotFileGH = ?NOT_FILE_GH_BB,
    bb_or([
        (BB band NotFileAB) bsl  6, % 1
        (BB band NotFileA)  bsl 15, % 2
        (BB band NotFileH)  bsl 17, % 3
        (BB band NotFileGH) bsl 10, % 4
        (BB band NotFileGH) bsr  6, % 5
        (BB band NotFileH)  bsr 15, % 6
        (BB band NotFileA)  bsr 17, % 7
        (BB band NotFileAB) bsr 10  % 8
    ]) band ?ALL_SQUARES_BB.


%% pawn_attacks_bb/2
-spec pawn_attacks_bb(color(), sq_bb()) -> bb().
pawn_attacks_bb(?WHITE, BB) ->
    BB2 = shift_north_west(BB) bor shift_north_east(BB),
    BB2 band ?ALL_SQUARES_BB;
pawn_attacks_bb(?BLACK, BB) ->
    shift_south_west(BB) bor shift_south_east(BB).

%% pawn_pushes_bb/3
-spec pawn_pushes_bb(color(), sq_bb(), bb()) -> bb().
pawn_pushes_bb(?WHITE, BB, EmptySquaresBB) ->
    Push1BB = shift_north(BB band ?NOT_RANK_1_BB) band EmptySquaresBB,
    Push2BB = shift_north(Push1BB band ?RANK_3_BB) band EmptySquaresBB,
    (Push1BB bor Push2BB) band ?ALL_SQUARES_BB;
pawn_pushes_bb(?BLACK, BB, EmptySquaresBB) ->
    Push1BB = shift_south(BB band ?NOT_RANK_8_BB) band EmptySquaresBB,
    Push2BB = shift_south(Push1BB band ?RANK_6_BB) band EmptySquaresBB,
    Push1BB bor Push2BB.


%% bishop_attacks_bb/2
-spec bishop_attacks_bb(sq_idx(), bb()) -> bb().
bishop_attacks_bb(FromIdx, OccupiedBB) ->
    sliding_attacks_bb(FromIdx, ?BISHOP_DIRECTIONS, OccupiedBB).


%% rook_attacks_bb/2
-spec rook_attacks_bb(sq_idx(), bb()) -> bb().
rook_attacks_bb(FromIdx, OccupiedBB) ->
    sliding_attacks_bb(FromIdx, ?ROOK_DIRECTIONS, OccupiedBB).



%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

%% bb_or/2
-spec bb_or([bb()], bb()) -> bb().
bb_or([], AccBB)->
    AccBB;
bb_or([BB|Tail], AccBB) ->
    bb_or(Tail, AccBB bor BB).


%% to_index_list/2
-spec to_index_list(bb(), [sq_idx()]) -> [sq_idx()].
to_index_list(0, List) ->
    % We don't care about the order of indices.
    % So, it's not necessary to reverse the list.
    % lists:reverse(List);
    List;
to_index_list(BB, List) ->
    Idx = to_index(BB band (-BB)), % the index of the least significant bit (LSB)
    to_index_list(BB band (BB - 1), [Idx | List]). % reset LSB and continue


%% sliding_attacks_bb/3
-spec sliding_attacks_bb(sq_idx(), [sliding_direction()], bb()) -> bb().
sliding_attacks_bb(Idx, Directions, OccupiedBB) ->
    sliding_attacks_bb(Idx, Directions, OccupiedBB, Idx, ?EMPTY_BB).

%% sliding_attacks_bb/4
-spec sliding_attacks_bb(sq_idx(), [sliding_direction()], bb(), sq_idx(), bb()) -> bb().
sliding_attacks_bb(_Idx0, [], _OccupiedBB, _Idx, AccBB) ->
    AccBB;
sliding_attacks_bb(Idx0, [Bits|Tail] = Directions, OccupiedBB, Idx, AccBB) ->
    Idx2 = Idx + Bits,
    IsOk = ?IS_VALID_INDEX(Idx2) andalso (binbo_board:sq_distance(Idx, Idx2) =:= 1),
    case IsOk of
        true  ->
            SqBB = ?SQUARE_BB(Idx2),
            AccBB2 = AccBB bor SqBB,
            case ?IS_AND(SqBB, OccupiedBB) of
                true  -> sliding_attacks_bb(Idx0, Tail, OccupiedBB, Idx0, AccBB2);
                false -> sliding_attacks_bb(Idx0, Directions, OccupiedBB, Idx2, AccBB2)
            end;
        false ->
            sliding_attacks_bb(Idx0, Tail, OccupiedBB, Idx0, AccBB)
    end.
