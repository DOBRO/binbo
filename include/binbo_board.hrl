%% Copyright (c) 2019-2021, Sergei Semichev <chessvegas@chessvegas.com>. All Rights Reserved.
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

-define(BINBO_BOARD_HRL, true).

%%%----------------------------
%%%   A8 B8 C8 D8 E8 F8 G8 H8
%%%   A7 B7 C7 D7 E7 F7 G7 H7
%%%   A6 B6 C6 D6 E6 F6 G6 H6
%%%   A5 B5 C5 D5 E5 F5 G5 H5
%%%   A4 B4 C4 D4 E4 F4 G4 H4
%%%   A3 B3 C3 D3 E3 F3 G3 H3
%%%   A2 B2 C2 D2 E2 F2 G2 H2
%%%   A1 B1 C1 D1 E1 F1 G1 H1
%%%----------------------------
%%%   56 57 58 59 60 61 62 63
%%%   48 49 50 51 52 53 54 55
%%%   40 41 42 43 44 45 46 47
%%%   32 33 34 35 36 37 38 39
%%%   24 25 26 27 28 29 30 31
%%%   16 17 18 19 20 21 22 23
%%%    8  9 10 11 12 13 14 15
%%%    0  1  2  3  4  5  6  7
%%%----------------------------


%%%------------------------------------------------------------------------------
%%%   Color
%%%------------------------------------------------------------------------------

-define(WHITE, 16#00).
-define(BLACK, 16#10).

%%%------------------------------------------------------------------------------
%%%   Pieces
%%%------------------------------------------------------------------------------

-define(EMPTY_SQUARE, 0).

-define(PAWN,         16#01).
-define(KNIGHT,       16#02).
-define(BISHOP,       16#03).
-define(ROOK,         16#04).
-define(QUEEN,        16#05).
-define(KING,         16#06).

-define(WHITE_PAWN,   16#01).
-define(WHITE_KNIGHT, 16#02).
-define(WHITE_BISHOP, 16#03).
-define(WHITE_ROOK,   16#04).
-define(WHITE_QUEEN,  16#05).
-define(WHITE_KING,   16#06).

-define(BLACK_PAWN,   16#11).
-define(BLACK_KNIGHT, 16#12).
-define(BLACK_BISHOP, 16#13).
-define(BLACK_ROOK,   16#14).
-define(BLACK_QUEEN,  16#15).
-define(BLACK_KING,   16#16).


%%%------------------------------------------------------------------------------
%%%   Characters of pieces
%%%------------------------------------------------------------------------------

-define(WP, $P). % (80) white pawn character
-define(WN, $N). % (78) white knight character
-define(WB, $B). % (66) white bishop character
-define(WR, $R). % (82) white rook character
-define(WQ, $Q). % (81) white queen character
-define(WK, $K). % (75) white king character
-define(BP, $p). % (112) black pawn character
-define(BN, $n). % (110) black knight character
-define(BB, $b). % (98) black bishop character
-define(BR, $r). % (114) black rook character
-define(BQ, $q). % (113) black queen character
-define(BK, $k). % (107) black king character

%%%------------------------------------------------------------------------------
%%%   Square indexes
%%%------------------------------------------------------------------------------

-define(A1_IDX,  0).
-define(A2_IDX,  8).
-define(A3_IDX, 16).
-define(A4_IDX, 24).
-define(A5_IDX, 32).
-define(A6_IDX, 40).
-define(A7_IDX, 48).
-define(A8_IDX, 56).

-define(B1_IDX,  1).
-define(C1_IDX,  2).
-define(D1_IDX,  3).
-define(E1_IDX,  4).
-define(F1_IDX,  5).
-define(G1_IDX,  6).

-define(B8_IDX, 57).
-define(C8_IDX, 58).
-define(D8_IDX, 59).
-define(E8_IDX, 60).
-define(F8_IDX, 61).
-define(G8_IDX, 62).

-define(H1_IDX,  7).
-define(H2_IDX, 15).
-define(H3_IDX, 23).
-define(H4_IDX, 31).
-define(H5_IDX, 39).
-define(H6_IDX, 47).
-define(H7_IDX, 55).
-define(H8_IDX, 63).

%%%------------------------------------------------------------------------------
%%%   Bitboards of squares
%%%------------------------------------------------------------------------------

-define(A1_BB, 16#01).
-define(B1_BB, 16#02).               % (1 bsl ?B1_IDX)
-define(C1_BB, 16#04).               % (1 bsl ?C1_IDX)
-define(D1_BB, 16#08).               % (1 bsl ?D1_IDX)
-define(E1_BB, 16#10).               % (1 bsl ?E1_IDX)
-define(F1_BB, 16#20).               % (1 bsl ?F1_IDX)
-define(G1_BB, 16#40).               % (1 bsl ?G1_IDX)
-define(H1_BB, 16#80).               % (1 bsl ?H1_IDX)
-define(A8_BB, 16#100000000000000).  % (1 bsl ?A8_IDX)
-define(B8_BB, 16#200000000000000).  % (1 bsl ?B8_IDX)
-define(C8_BB, 16#400000000000000).  % (1 bsl ?C8_IDX)
-define(D8_BB, 16#800000000000000).  % (1 bsl ?D8_IDX)
-define(E8_BB, 16#1000000000000000). % (1 bsl ?E8_IDX)
-define(F8_BB, 16#2000000000000000). % (1 bsl ?F8_IDX)
-define(G8_BB, 16#4000000000000000). % (1 bsl ?G8_IDX)
-define(H8_BB, 16#8000000000000000). % (1 bsl ?H8_IDX)

-define(A1A8H1H8_BB, 16#8100000000000081). % (?A1_BB bor ?A8_BB bor ?H1_BB bor ?H8_BB)


%%%------------------------------------------------------------------------------
%%%   Castling
%%%------------------------------------------------------------------------------

-define(CASTLING_NONE,  2#0000). %  0
-define(CASTLING_W_OO,  2#0001). %  1
-define(CASTLING_W_OOO, 2#0010). %  2
-define(CASTLING_B_OO,  2#0100). %  4
-define(CASTLING_B_OOO, 2#1000). %  8
-define(CASTLING_W_ANY, 2#0011). %  3
-define(CASTLING_B_ANY, 2#1100). % 12
-define(CASTLING_ANY,   2#1111). % 15


%%%------------------------------------------------------------------------------
%%%   Helper bitboards
%%%------------------------------------------------------------------------------

-define(EMPTY_BB,         0).
-define(ALL_SQUARES_BB,   16#FFFFFFFFFFFFFFFF).
-define(DARK_SQUARES_BB,  16#AA55AA55AA55AA55).
-define(LIGHT_SQUARES_BB, 16#55AA55AA55AA55AA).


%%%------------------------------------------------------------------------------
%%%   Bitboards of files
%%%------------------------------------------------------------------------------

-define(FILE_A_BB,      16#101010101010101).
-define(FILE_B_BB,      16#202020202020202).  % (?FILE_A_BB bsl 1)
-define(FILE_C_BB,      16#404040404040404).  % (?FILE_A_BB bsl 2)
-define(FILE_D_BB,      16#808080808080808).  % (?FILE_A_BB bsl 3)
-define(FILE_E_BB,      16#1010101010101010). % (?FILE_A_BB bsl 4)
-define(FILE_F_BB,      16#2020202020202020). % (?FILE_A_BB bsl 5)
-define(FILE_G_BB,      16#4040404040404040). % (?FILE_A_BB bsl 6)
-define(FILE_H_BB,      16#8080808080808080). % (?FILE_A_BB bsl 7)

-define(FILE_AB_BB,     16#303030303030303).  % (?FILE_A_BB bor ?FILE_B_BB)
-define(FILE_GH_BB,     16#C0C0C0C0C0C0C0C0). % (?FILE_G_BB bor ?FILE_H_BB)

-define(NOT_FILE_A_BB,  16#FEFEFEFEFEFEFEFE). % (?ALL_SQUARES_BB bxor ?FILE_A_BB)
-define(NOT_FILE_H_BB,  16#7F7F7F7F7F7F7F7F). % (?ALL_SQUARES_BB bxor ?FILE_H_BB)

-define(NOT_FILE_AB_BB, 16#FCFCFCFCFCFCFCFC). % (?ALL_SQUARES_BB bxor ?FILE_AB_BB)
-define(NOT_FILE_GH_BB, 16#3F3F3F3F3F3F3F3F). % (?ALL_SQUARES_BB bxor ?FILE_GH_BB)


%%%------------------------------------------------------------------------------
%%%   Bitboards of ranks
%%%------------------------------------------------------------------------------

-define(RANK_1_BB,     16#FF).
-define(RANK_2_BB,     16#FF00).             % (?RANK_1_BB bsl  8)
-define(RANK_3_BB,     16#FF0000).           % (?RANK_1_BB bsl 16)
-define(RANK_4_BB,     16#FF000000).         % (?RANK_1_BB bsl 24)
-define(RANK_5_BB,     16#FF00000000).       % (?RANK_1_BB bsl 32)
-define(RANK_6_BB,     16#FF0000000000).     % (?RANK_1_BB bsl 40)
-define(RANK_7_BB,     16#FF000000000000).   % (?RANK_1_BB bsl 48)
-define(RANK_8_BB,     16#FF00000000000000). % (?RANK_1_BB bsl 56)

-define(NOT_RANK_1_BB, 16#FFFFFFFFFFFFFF00). % (?ALL_SQUARES_BB bxor ?RANK_1_BB)
-define(NOT_RANK_8_BB, 16#FFFFFFFFFFFFFF).   % (?ALL_SQUARES_BB bxor ?RANK_8_BB)


%%%------------------------------------------------------------------------------
%%%   Directions
%%%------------------------------------------------------------------------------

-define(NORTH,       8).
-define(SOUTH,      -8).
-define(WEST,       -1).
-define(EAST,        1).
-define(NORTH_WEST,  7).
-define(NORTH_EAST,  9).
-define(SOUTH_WEST, -9).
-define(SOUTH_EAST, -7).

-define(ROOK_DIRECTIONS, [?NORTH, ?SOUTH, ?EAST, ?WEST]).
-define(BISHOP_DIRECTIONS, [?NORTH_WEST, ?NORTH_EAST, ?SOUTH_WEST, ?SOUTH_EAST]).


%%%------------------------------------------------------------------------------
%%%   Helper functions
%%%------------------------------------------------------------------------------

-define(COLOR(Piece), ((Piece) band 16#10)).
-define(PIECE_TYPE(Piece), ((Piece) band 16#0F)).
-define(IS_PIECE(Piece), ((Piece) > ?EMPTY_SQUARE)).
-define(SWITCH_COLOR(Color), ((Color) bxor 16#10)).
-define(TYPE_TO_PIECE(Ptype, Pcolor), ((Ptype) bor (Pcolor))).

-define(IS_VALID_INDEX(Idx), (erlang:is_integer(Idx) andalso ((Idx) >= ?A1_IDX) andalso ((Idx) =< ?H8_IDX))).

-define(SQUARE_BB(Idx),  (1 bsl (Idx))).

-define(IS_AND(X1, X2),  (((X1) band (X2))  >  ?EMPTY_BB)).
-define(IS_NOT(X1, X2),  (((X1) band (X2)) =:= ?EMPTY_BB)).


-define(CHAR_TO_PIECE(C), (case (C) of
	?WP -> ?WHITE_PAWN;
	?WN -> ?WHITE_KNIGHT;
	?WB -> ?WHITE_BISHOP;
	?WR -> ?WHITE_ROOK;
	?WQ -> ?WHITE_QUEEN;
	?WK -> ?WHITE_KING;
	?BP -> ?BLACK_PAWN;
	?BN -> ?BLACK_KNIGHT;
	?BB -> ?BLACK_BISHOP;
	?BR -> ?BLACK_ROOK;
	?BQ -> ?BLACK_QUEEN;
	?BK -> ?BLACK_KING
end)).


-define(PIECE_TO_CHAR(P), (case (P) of
	?WHITE_PAWN   -> ?WP;
	?WHITE_KNIGHT -> ?WN;
	?WHITE_BISHOP -> ?WB;
	?WHITE_ROOK   -> ?WR;
	?WHITE_QUEEN  -> ?WQ;
	?WHITE_KING   -> ?WK;
	?BLACK_PAWN   -> ?BP;
	?BLACK_KNIGHT -> ?BN;
	?BLACK_BISHOP -> ?BB;
	?BLACK_ROOK   -> ?BR;
	?BLACK_QUEEN  -> ?BQ;
	?BLACK_KING   -> ?BK
end)).


-define(PIECE_TO_ATOM(P), (case (P) of
	?WHITE_PAWN   -> 'WHITE_PAWN';
	?WHITE_KNIGHT -> 'WHITE_KNIGHT';
	?WHITE_BISHOP -> 'WHITE_BISHOP';
	?WHITE_ROOK   -> 'WHITE_ROOK';
	?WHITE_QUEEN  -> 'WHITE_QUEEN';
	?WHITE_KING   -> 'WHITE_KING';
	?BLACK_PAWN   -> 'BLACK_PAWN';
	?BLACK_KNIGHT -> 'BLACK_KNIGHT';
	?BLACK_BISHOP -> 'BLACK_BISHOP';
	?BLACK_ROOK   -> 'BLACK_ROOK';
	?BLACK_QUEEN  -> 'BLACK_QUEEN';
	?BLACK_KING   -> 'BLACK_KING'
end)).


-define(PIECE_TO_TUPLE(P), (case (P) of
	?WHITE_PAWN   -> {white, pawn};
	?WHITE_KNIGHT -> {white, knight};
	?WHITE_BISHOP -> {white, bishop};
	?WHITE_ROOK   -> {white, rook};
	?WHITE_QUEEN  -> {white, queen};
	?WHITE_KING   -> {white, king};
	?BLACK_PAWN   -> {black, pawn};
	?BLACK_KNIGHT -> {black, knight};
	?BLACK_BISHOP -> {black, bishop};
	?BLACK_ROOK   -> {black, rook};
	?BLACK_QUEEN  -> {black, queen};
	?BLACK_KING   -> {black, king}
end)).
