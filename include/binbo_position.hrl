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

-ifndef(BINBO_BOARD_HRL).
-include("binbo_board.hrl").
-endif.

%%%------------------------------------------------------------------------------
%%%   Keys for map of game state
%%%------------------------------------------------------------------------------

-define(GAME_KEY_WP,       bbwp).
-define(GAME_KEY_WN,       bbwn).
-define(GAME_KEY_WB,       bbwb).
-define(GAME_KEY_WR,       bbwr).
-define(GAME_KEY_WQ,       bbwq).
-define(GAME_KEY_WK,       bbwk).
-define(GAME_KEY_BP,       bbbp).
-define(GAME_KEY_BN,       bbbn).
-define(GAME_KEY_BB,       bbbb).
-define(GAME_KEY_BR,       bbbr).
-define(GAME_KEY_BQ,       bbbq).
-define(GAME_KEY_BK,       bbbk).

-define(GAME_KEY_WHITE,    bbw).
-define(GAME_KEY_BLACK,    bbb).
-define(GAME_KEY_OCCUPIED, bball).


-define(GAME_KEY_SIDETOMOVE, sidetomove).
-define(GAME_KEY_CASTLING,   castling).
-define(GAME_KEY_ENPASSANT,  bbenpa).

-define(GAME_KEY_LASTMOVE,       lastmove).
-define(GAME_KEY_LASTMOVE_PIECE, lastmovepc).

-define(GAME_KEY_HALFMOVE, halfmove).
-define(GAME_KEY_FULLMOVE, fullmove).

-define(GAME_KEY_STATUS, status).

%%%------------------------------------------------------------------------------
%%%   Game status
%%%------------------------------------------------------------------------------

-define(GAME_STATUS_INPROGRESS,     continue).
-define(GAME_STATUS_CHECKMATE,      checkmate).
-define(GAME_STATUS_DRAW_STALEMATE, stalemate).
-define(GAME_STATUS_DRAW_RULE50,    rule50).

%%%------------------------------------------------------------------------------
%%%   Helper functions
%%%------------------------------------------------------------------------------

-define(OWN_SIDE_KEY(Color), (case (Color) of
	?WHITE -> ?GAME_KEY_WHITE;
	?BLACK -> ?GAME_KEY_BLACK
end)).

-define(ENEMY_SIDE_KEY(Color), (case (Color) of
	?WHITE -> ?GAME_KEY_BLACK;
	?BLACK -> ?GAME_KEY_WHITE
end)).

-define(PIECE_GAME_KEY(P), (case (P) of
	?WHITE_PAWN   -> ?GAME_KEY_WP;
	?WHITE_KNIGHT -> ?GAME_KEY_WN;
	?WHITE_BISHOP -> ?GAME_KEY_WB;
	?WHITE_ROOK   -> ?GAME_KEY_WR;
	?WHITE_QUEEN  -> ?GAME_KEY_WQ;
	?WHITE_KING   -> ?GAME_KEY_WK;
	?BLACK_PAWN   -> ?GAME_KEY_BP;
	?BLACK_KNIGHT -> ?GAME_KEY_BN;
	?BLACK_BISHOP -> ?GAME_KEY_BB;
	?BLACK_ROOK   -> ?GAME_KEY_BR;
	?BLACK_QUEEN  -> ?GAME_KEY_BQ;
	?BLACK_KING   -> ?GAME_KEY_BK
end)).


%% Chess symbols in Unicode: https://en.wikipedia.org/wiki/Chess_symbols_in_Unicode
-define(PIECE_TO_UNICODE(P), (case (P) of
	?WHITE_PAWN   -> 9817;
	?WHITE_KNIGHT -> 9816;
	?WHITE_BISHOP -> 9815;
	?WHITE_ROOK   -> 9814;
	?WHITE_QUEEN  -> 9813;
	?WHITE_KING   -> 9812;
	?BLACK_PAWN   -> 9823;
	?BLACK_KNIGHT -> 9822;
	?BLACK_BISHOP -> 9821;
	?BLACK_ROOK   -> 9820;
	?BLACK_QUEEN  -> 9819;
	?BLACK_KING   -> 9818
end)).
