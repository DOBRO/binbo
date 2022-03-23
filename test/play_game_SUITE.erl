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

-module(play_game_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("binbo_test_lib.hrl").


-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([move_all_pieces/1,
    checkmate_white/1, checkmate_black/1,
    stalemate_white/1, stalemate_black/1,
    rule50/1,
    castling_kingside/1, castling_queenside/1,
    castling_white_after_king_move/1,
    castling_white_after_rook_move/1,
    castling_black_after_king_move/1,
    castling_black_after_rook_move/1,
    castling_white_when_attacked/1,
    castling_black_when_attacked/1,
    enpassant_moves/1, simple_game/1,
    set_game_state/1,
    get_pieces_list/1,
    index_moves/1,
    set_game_winner/1,
    draw_when_king_and_bishop_versus_king/1,
    draw_when_king_and_knight_versus_king/1,
    print_board_when_game_undefined/1
]).


%% all/0
all() -> [{group, games}].

%% groups/0
groups() ->
    [{games, [parallel], [
        move_all_pieces,
        checkmate_white, checkmate_black,
        stalemate_white, stalemate_black,
        castling_kingside, castling_queenside,
        castling_white_after_king_move,
        castling_black_after_king_move,
        castling_white_after_rook_move,
        castling_black_after_rook_move,
        castling_white_when_attacked,
        castling_black_when_attacked,
        enpassant_moves, simple_game,
        set_game_state,
        get_pieces_list,
        index_moves,
        set_game_winner,
        rule50,
        draw_when_king_and_bishop_versus_king,
        draw_when_king_and_knight_versus_king,
        print_board_when_game_undefined
    ]}].

%% init_per_suite/1
init_per_suite(Config) ->
    ok = binbo_test_lib:all_group_testcases_exported(?MODULE),
    {ok, _} = binbo:start(),
    Config.

%% end_per_suite/1
end_per_suite(_Config) ->
    ok.

%% init_per_testcase/2
init_per_testcase(_TestCase, Config) ->
    {ok, Pid} = binbo:new_server(),
    [{pid, Pid} | Config].

%% end_per_testcase/2
end_per_testcase(_TestCase, Config) ->
    Pid = get_pid(Config),
    ok = binbo:stop_server(Pid),
    ok.


%%%------------------------------------------------------------------------------
%%%   Testcases
%%%------------------------------------------------------------------------------

%% move_all_pieces/1
move_all_pieces(Config) ->
    Pid = get_pid(Config),
    % Init game from initial position
    {ok, continue} = binbo:new_game(Pid),
    ok = make_legal_moves(Pid, [
        % white and black pawn push
          <<"a2a3">>, <<"a7a6">>
        , <<"c2c3">>, <<"c7c6">>
        % white and black pawn double push
        , <<"e2e4">>, <<"e7e5">>
        , <<"d2d4">>, <<"d7d5">>
        % white and black bishop (light squares)
        , <<"f1e2">>, <<"f8e7">>
        % white and black bishop (dark squares)
        , <<"c1e3">>, <<"c8e6">>
        % white and black knight
        , <<"g1h3">>, <<"g8h6">>
        , <<"b1d2">>, <<"b8d7">>
        % white and black queen
        , <<"d1a4">>, <<"d8b6">>
        % white and black rook
        , <<"h1g1">>, <<"h8g8">>
        , <<"a1d1">>, <<"a8d8">>
        % white and black king
        , <<"e1f1">>, <<"e8f8">>
    ]),
    ok.

%% checkmate_white/1
checkmate_white(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo:new_game(Pid, <<"rnbqkbnr/3ppppp/ppp5/8/2B1P3/5Q2/PPPP1PPP/RNB1K1NR w KQkq -">>),
    {ok, <<"rnbqkbnr/3ppppp/ppp5/8/2B1P3/5Q2/PPPP1PPP/RNB1K1NR w KQkq - 0 1">>} = binbo:get_fen(Pid),
    {ok, {checkmate, white_wins}} = binbo:move(Pid, <<"c4f7">>),
    {ok,{checkmate, white_wins}} = binbo:game_status(Pid),
    ok.

%% checkmate_black/1
checkmate_black(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo:new_game(Pid, <<"rnb1k1nr/pppp1ppp/8/2b1p3/P6q/NP6/2PPPPPP/R1BQKBNR b KQkq -">>),
    {ok, <<"rnb1k1nr/pppp1ppp/8/2b1p3/P6q/NP6/2PPPPPP/R1BQKBNR b KQkq - 0 1">>} = binbo:get_fen(Pid),
    {ok, {checkmate, black_wins}} = binbo:move(Pid, <<"h4f2">>),
    {ok, {checkmate, black_wins}} = binbo:game_status(Pid),
    ok.

%% stalemate_white/1
stalemate_white(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo:new_game(Pid, <<"k7/8/8/8/7Q/8/3K4/1R6 w - -">>),
    {ok, <<"k7/8/8/8/7Q/8/3K4/1R6 w - - 0 1">>} = binbo:get_fen(Pid),
    {ok, {draw,stalemate}} = binbo:move(Pid, <<"h4h7">>),
    {ok, {draw,stalemate}} = binbo:game_status(Pid),
    ok.

%% stalemate_black/1
stalemate_black(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo:new_game(Pid, <<"1q2b1b1/8/8/8/8/7k/8/K7 b - -">>),
    {ok, {draw,stalemate}} = binbo:move(Pid, <<"e8g6">>),
    {ok, {draw,stalemate}} = binbo:game_status(Pid),
    ok.

%% rule50/1
rule50(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo:new_game(Pid, <<"6R1/7k/8/8/1r3B2/5K2/8/8 w - - 99 119">>),
    {ok, {draw,rule50}} = binbo:move(Pid, <<"g8a8">>),
    {ok, {draw,rule50}} = binbo:game_status(Pid),
    ok.

%% castling_kingside/1
castling_kingside(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo:new_game(Pid, <<"r3k2r/ppp1qppp/2np1n2/2b1p1B1/2B1P1b1/2NP1N2/PPP1QPPP/R3K2R w KQkq -">>),
    % White castling
    {ok, continue} = binbo:move(Pid, <<"e1g1">>),
    % Black castling
    {ok, continue} = binbo:move(Pid, <<"e8g8">>),
    {ok, continue} = binbo:game_status(Pid),
    ok.

%% castling_queenside/1
castling_queenside(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo:new_game(Pid, <<"r3k2r/ppp1qppp/2np1n2/2b1p1B1/2B1P1b1/2NP1N2/PPP1QPPP/R3K2R w KQkq -">>),
    % White castling
    {ok, continue} = binbo:move(Pid, <<"e1c1">>),
    % Black castling
    {ok, continue} = binbo:move(Pid, <<"e8c8">>),
    ok.

%% castling_white_after_king_move/1
castling_white_after_king_move(Config) ->
    Pid = get_pid(Config),
    Fen = <<"r3k2r/ppp1qppp/2np1n2/2b1p1B1/2B1P1b1/2NP1N2/PPP1QPPP/R3K2R w KQkq -">>,
    {ok, continue} = binbo:new_game(Pid, Fen),
    ok = make_legal_moves(Pid, [
        <<"e1d2">>, % White king moves from E1
        <<"a7a6">>, % Any black move
        <<"d2e1">>, % White king comes back to E1
        <<"a6a5">>  % Any black move
    ]),
    % No castling allowed
    {error, {{invalid_move, 'WHITE_KING'}, <<"e1g1">>}} = binbo:move(Pid, <<"e1g1">>),
    {error, {{invalid_move, 'WHITE_KING'}, <<"e1c1">>}} = binbo:move(Pid, <<"e1c1">>),
    ok.

%% castling_black_after_king_move/1
castling_black_after_king_move(Config) ->
    Pid = get_pid(Config),
    Fen = <<"r3k2r/ppp1qppp/2np1n2/2b1p1B1/2B1P1b1/2NP1N2/PPP1QPPP/R3K2R b KQkq -">>,
    {ok, continue} = binbo:new_game(Pid, Fen),
    ok = make_legal_moves(Pid, [
        <<"e8d7">>, % Black king moves from E8
        <<"a2a3">>, % Any white move
        <<"d7e8">>, % Black king comes back to E8
        <<"a3a4">>  % Any white move
    ]),
    % No castling allowed
    {error, {{invalid_move, 'BLACK_KING'}, <<"e8g8">>}} = binbo:move(Pid, <<"e8g8">>),
    {error, {{invalid_move, 'BLACK_KING'}, <<"e8c8">>}} = binbo:move(Pid, <<"e8c8">>),
    ok.

%% castling_white_after_rook_move/1
castling_white_after_rook_move(Config) ->
    Pid = get_pid(Config),
    Fen = <<"r3k2r/ppp1qppp/2np1n2/2b1p1B1/2B1P1b1/2NP1N2/PPP1QPPP/R3K2R w KQkq -">>,
    {ok, continue} = binbo:new_game(Pid, Fen),
    ok = make_legal_moves(Pid, [
        <<"h1g1">>, % White Rook moves from H1
        <<"a7a6">>, % Any black move
        <<"g1h1">>, % White rook comes back to H1
        <<"a6a5">>  % Any black move
    ]),
    % Castling kingside not allowed
    {error, {{invalid_move, 'WHITE_KING'}, <<"e1g1">>}} = binbo:move(Pid, <<"e1g1">>),

    % Load new game
    {ok, continue} = binbo:new_game(Pid, Fen),
    ok = make_legal_moves(Pid, [
        <<"a1b1">>, % White Rook moves from A1
        <<"a7a6">>, % Any black move
        <<"b1a1">>, % White rook comes back to A1
        <<"a6a5">>  % Any black move
    ]),
    % Castling queenside not allowed
    {error, {{invalid_move, 'WHITE_KING'}, <<"e1c1">>}} = binbo:move(Pid, <<"e1c1">>),

    % Load new game
    {ok, continue} = binbo:new_game(Pid, Fen),
    ok = make_legal_moves(Pid, [
        <<"a1b1">>, % White Rook moves from A1
        <<"a7a6">>, % Any black move
        <<"b1a1">>, % White rook comes back to A1
        <<"a6a5">>, % Any black move
        <<"h1g1">>, % White Rook moves from H1
        <<"h7h6">>, % Any black move
        <<"g1h1">>, % White rook comes back to H1
        <<"h6h5">>  % Any black move
    ]),
    % Castling kingside not allowed
    {error, {{invalid_move, 'WHITE_KING'}, <<"e1g1">>}} = binbo:move(Pid, <<"e1g1">>),
    % Castling queenside not allowed
    {error, {{invalid_move, 'WHITE_KING'}, <<"e1c1">>}} = binbo:move(Pid, <<"e1c1">>),
    ok.

%% castling_black_after_rook_move/1
castling_black_after_rook_move(Config) ->
    Pid = get_pid(Config),
    Fen = <<"r3k2r/ppp1qppp/2np1n2/2b1p1B1/2B1P1b1/2NP1N2/PPP1QPPP/R3K2R b KQkq -">>,
    {ok, continue} = binbo:new_game(Pid, Fen),
    ok = make_legal_moves(Pid, [
        <<"h8g8">>, % Black rook moves from H1
        <<"a2a3">>, % Any white move
        <<"g8h8">>, % Black rook comes back to H1
        <<"a3a4">>  % Any white move
    ]),
    % Castling kingside not allowed
    {error, {{invalid_move, 'BLACK_KING'}, <<"e8g8">>}} = binbo:move(Pid, <<"e8g8">>),

    % Load new game
    {ok, continue} = binbo:new_game(Pid, Fen),
    ok = make_legal_moves(Pid, [
        <<"a8b8">>, % Black rook moves from A1
        <<"a2a3">>, % Any white move
        <<"b8a8">>, % Black rook comes back to A1
        <<"a3a4">>  % Any white move
    ]),
    % Castling queenside not allowed
    {error, {{invalid_move, 'BLACK_KING'}, <<"e8c8">>}} = binbo:move(Pid, <<"e8c8">>),

    % Load new game
    {ok, continue} = binbo:new_game(Pid, Fen),
    ok = make_legal_moves(Pid, [
        <<"a8b8">>, % Black rook moves from A1
        <<"a2a3">>, % Any white move
        <<"b8a8">>, % Black rook comes back to A1
        <<"a3a4">>, % Any white move
        <<"h8g8">>, % Black rook moves from H1
        <<"h2h3">>, % Any white move
        <<"g8h8">>, % Black rook comes back to H1
        <<"h3h4">>  % Any white move
    ]),
    % Castling kingside not allowed
    {error, {{invalid_move, 'BLACK_KING'}, <<"e8g8">>}} = binbo:move(Pid, <<"e8g8">>),
    % Castling queenside not allowed
    {error, {{invalid_move, 'BLACK_KING'}, <<"e8c8">>}} = binbo:move(Pid, <<"e8c8">>),
    ok.


%% castling_white_when_attacked/1
castling_white_when_attacked(Config) ->
    Pid = get_pid(Config),
    % New game, white king is in check
    {ok, continue} = binbo:new_game(Pid, <<"r3k2r/pppbq1pp/n2p1p1n/4p1B1/1bB1P3/N2P1P1N/PPP1Q1PP/R3K2R w KQkq -">>),
    % No castling allowed
    {error, {{invalid_move, 'WHITE_KING'}, <<"e1g1">>}} = binbo:move(Pid, <<"e1g1">>),
    {error, {{invalid_move, 'WHITE_KING'}, <<"e1c1">>}} = binbo:move(Pid, <<"e1c1">>),

    % New game, G1 is attacked. Castling kingside not allowed
    {ok, continue} = binbo:new_game(Pid, <<"r3k2r/pppbq1pp/n2p1p1n/2b1p1B1/2B1P3/N2P1P1N/PPP1Q1PP/R3K2R w KQkq -">>),
    {error, {{invalid_move, 'WHITE_KING'}, <<"e1g1">>}} = binbo:move(Pid, <<"e1g1">>),

    % New game, F1 is attacked. Castling kingside not allowed
    {ok, continue} = binbo:new_game(Pid, <<"r3k2r/pppbq1pp/n2p1p2/b3p1B1/2B1P3/N1PP1PnN/PP2Q1PP/R3K2R w KQkq -">>),
    {error, {{invalid_move, 'WHITE_KING'}, <<"e1g1">>}} = binbo:move(Pid, <<"e1g1">>),

    % New game, C1 is attacked. Castling queenside not allowed
    {ok, continue} = binbo:new_game(Pid, <<"r3k2r/ppp1q1pp/n2p1pbn/4p1b1/2B1P2B/N1PP1P1N/PP2Q1PP/R3K2R w KQkq -">>),
    {error, {{invalid_move, 'WHITE_KING'}, <<"e1c1">>}} = binbo:move(Pid, <<"e1c1">>),

    % New game, D1 is attacked. Castling queenside not allowed
    {ok, continue} = binbo:new_game(Pid, <<"r3k2r/ppp1q1pp/n2p1p1n/4p1b1/b1B1P2B/N1PP1P1N/PP2Q1PP/R3K2R w KQkq -">>),
    {error, {{invalid_move, 'WHITE_KING'}, <<"e1c1">>}} = binbo:move(Pid, <<"e1c1">>),

    % New game, B1 is attacked. Castling queenside is allowed
    {ok, continue} = binbo:new_game(Pid, <<"r3k2r/ppp1q1pp/nb3pbn/3PP3/2B4B/N1P2P1N/PP2Q1PP/R3K2R w KQkq -">>),
    {ok, continue} = binbo:move(Pid, <<"e1c1">>),
    ok.

%% castling_black_when_attacked/1
castling_black_when_attacked(Config) ->
    Pid = get_pid(Config),
    % New game, black king is in check
    {ok, continue} = binbo:new_game(Pid, <<"r3k2r/ppp1q1pp/nb3pbn/3PP3/Q6B/N1P2P1N/PP2B1PP/R3K2R b KQkq -">>),
    % No castling allowed
    {error, {{invalid_move, 'BLACK_KING'}, <<"e8g8">>}} = binbo:move(Pid, <<"e8g8">>),
    {error, {{invalid_move, 'BLACK_KING'}, <<"e8c8">>}} = binbo:move(Pid, <<"e8c8">>),

    % New game, G8 is attacked. Castling kingside not allowed
    {ok, continue} = binbo:new_game(Pid, <<"r3k2r/ppp1q1pp/nb3pbn/4P3/3P3B/NQP2P1N/PP2B1PP/R3K2R b KQkq -">>),
    {error, {{invalid_move, 'BLACK_KING'}, <<"e8g8">>}} = binbo:move(Pid, <<"e8g8">>),

    % New game, F8 is attacked. Castling kingside not allowed
    {ok, continue} = binbo:new_game(Pid, <<"r3k2r/ppp1qbpp/nb3pNn/4P3/3P3B/NQP2P2/PP2B1PP/R3K2R b KQkq -">>),
    {error, {{invalid_move, 'BLACK_KING'}, <<"e8g8">>}} = binbo:move(Pid, <<"e8g8">>),

    % New game, C8 is attacked. Castling queenside not allowed
    {ok, continue} = binbo:new_game(Pid, <<"r3k2r/ppp1qbpp/nb3p1n/4P3/3PN2B/N1P2P1B/PPQ3PP/R3K2R b KQkq -">>),
    {error, {{invalid_move, 'BLACK_KING'}, <<"e8c8">>}} = binbo:move(Pid, <<"e8c8">>),

    % New game, D8 is attacked. Castling queenside not allowed
    {ok, continue} = binbo:new_game(Pid, <<"r3k2r/ppp2bpp/nb2q2n/4Pp2/3P3B/N1P2P1B/PPQ1N1PP/R3K2R b KQkq -">>),
    {error, {{invalid_move, 'BLACK_KING'}, <<"e8c8">>}} = binbo:move(Pid, <<"e8c8">>),

    % New game, B8 is attacked. Castling queenside is allowed
    {ok, continue} = binbo:new_game(Pid, <<"r3k2r/pp3bpp/nbp1q2n/5p2/3P4/N1P1PPBB/PPQ1N1PP/R3K2R b KQkq -">>),
    {ok, continue} = binbo:move(Pid, <<"e8c8">>),
    ok.

%% enpassant_moves/1
enpassant_moves(Config) ->
    Pid = get_pid(Config),
    % New game, black king is in check
    {ok, continue} = binbo:new_game(Pid),
    ok = make_legal_moves(Pid, [
          <<"g2g4">>, <<"a7a5">>
        , <<"g4g5">>, <<"a5a4">>
        , <<"b2b4">>, <<"a4b3">> % black pawn enpassant move
        , <<"c2b3">>, <<"h7h5">>
        , <<"g5h6">> % white pawn enpassant move
    ]),
    ok.

%% simple_game/1
simple_game(Config) ->
    Pid = get_pid(Config),
    {error, {bad_game, undefined}} = binbo:move(Pid, <<"e2e4">>),
    {error, {bad_game, undefined}} = binbo:san_move(Pid, <<"e4">>),
    {error, {bad_game, undefined}} = binbo:side_to_move(Pid),
    {error, {bad_game, undefined}} = binbo:game_draw(Pid),
    {error, {bad_game, undefined}} = binbo:game_draw(Pid, test_draw),
    {error, {bad_game, undefined}} = binbo:all_legal_moves(Pid),
    {error, {bad_game, undefined}} = binbo:all_legal_moves(Pid, int),
    {error, {bad_game, undefined}} = binbo:all_legal_moves(Pid, bin),
    {error, {bad_game, undefined}} = binbo:all_legal_moves(Pid, str),
    InitialFen = <<"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1">>,
    InitialFen = binbo_fen:initial(),
    {ok, continue} = binbo:new_game(Pid),
    {ok, InitialFen} = binbo:get_fen(Pid),
    {ok, white} = binbo:side_to_move(Pid),
    {ok, continue} = binbo:game_status(Pid),

    {ok, IntMovelist} = binbo:all_legal_moves(Pid),
    {ok, IntMovelist2} = binbo:all_legal_moves(Pid, int),
    true = (IntMovelist =:= IntMovelist2),
    {ok, BinMovelist} = binbo:all_legal_moves(Pid, bin),
    {ok, StrMovelist} = binbo:all_legal_moves(Pid, str),

    20 = erlang:length(IntMovelist),
    20 = erlang:length(BinMovelist),
    20 = erlang:length(StrMovelist),
    ok = check_int_movelist(IntMovelist),
    ok = check_bin_movelist(BinMovelist),
    ok = check_str_movelist(StrMovelist),

    {ok, continue} = binbo:move(Pid, "e2e4"),
    {ok, black} = binbo:side_to_move(Pid),
    {ok, continue} = binbo:game_status(Pid),
    {ok, <<"rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1">>} = binbo:get_fen(Pid),

    ok = binbo:print_board(Pid),
    ok = binbo:print_board(Pid, [unicode, flip]),

    {ok, continue} = binbo:move(Pid, "e7e5"),
    {ok, white} = binbo:side_to_move(Pid),
    {ok, continue} = binbo:game_status(Pid),
    {ok, <<"rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2">>} = binbo:get_fen(Pid),

    ok = binbo:print_board(Pid),
    ok = binbo:print_board(Pid, [unicode, flip]),

    {ok, continue} = binbo:new_game(Pid),
    {ok, continue} = binbo:san_move(Pid, <<"e4">>),
    {ok, <<"rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1">>} = binbo:get_fen(Pid),
    {ok, continue} = binbo:san_move(Pid, "e5"),
    {ok, <<"rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2">>} = binbo:get_fen(Pid),

    ok = binbo:game_draw(Pid),
    {ok,{draw,{manual,undefined}}} = binbo:game_status(Pid),
    {error,{already_has_status,{draw,{manual,undefined}}}} = binbo:game_draw(Pid),

    {ok, continue} = binbo:new_game(Pid),
    ok = binbo:game_draw(Pid, for_test),
    {ok, {draw, {manual, for_test}}} = binbo:game_status(Pid),
    {error, {already_has_status, {draw, {manual, for_test}}}} = binbo:game_draw(Pid),

    ok = binbo:print_board(Pid),
    ok = binbo:print_board(Pid, [unicode, flip]),
    ok.

%% set_game_state/1
set_game_state(Config) ->
    Pid = get_pid(Config),
    % Game is not initialized yet
    undefined = binbo:game_state(Pid),
    {error, {bad_game, undefined}} = binbo:game_status(Pid),
    {error, {bad_game, undefined}} = binbo:set_game_state(Pid, undefined),
    % Start new game
    {ok, continue} = binbo:new_game(Pid),
    {ok, continue} = binbo:game_status(Pid),
    Game = binbo:game_state(Pid),
    true = erlang:is_map(Game),
    % Set undefined state
    {error, {bad_game, undefined}} = binbo:set_game_state(Pid, undefined),
    % Set normal game state
    {ok, continue} = binbo:set_game_state(Pid, Game),
    {ok, continue} = binbo:game_status(Pid),
    % Set undefined state again
    {error, {bad_game, undefined}} = binbo:set_game_state(Pid, undefined),
    % Save game state as binary
    BinGame = erlang:term_to_binary(Game),
    % Convert from binary and set state
    Game2 = erlang:binary_to_term(BinGame),
    true = erlang:is_map(Game2),
    {ok, continue} = binbo:set_game_state(Pid, Game2),
    {ok, continue} = binbo:game_status(Pid),
    ok.

%% get_pieces_list/1
get_pieces_list(Config) ->
    Pid = get_pid(Config),
    {error, {bad_game, undefined}} = binbo:get_pieces_list(Pid, index),
    {error, {bad_game, undefined}} = binbo:get_pieces_list(Pid, notation),
    % Init game from initial position
    {ok, continue} = binbo:new_game(Pid),
    {ok, List1} = binbo:get_pieces_list(Pid, index),
    {ok, List2} = binbo:get_pieces_list(Pid, notation),
    true = erlang:is_list(List1),
    true = erlang:is_list(List2),
    32 = erlang:length(List1),
    32 = erlang:length(List2),
    _ = test_pieces_list_with_square_index(List1),
    _ = test_pieces_list_with_square_notation(List2),
    ok.

%% index_moves/1
index_moves(Config) ->
    Pid = get_pid(Config),
    {error, {bad_game, undefined}} = binbo:index_move(Pid, 12, 20),
    % Initial game
    {ok, continue} = binbo:new_game(Pid),
    % e2-e4
    {ok, continue} = binbo:index_move(Pid, 12, 20),
    % e7-e5
    {ok, continue} = binbo:index_move(Pid, 52, 36),

    % Initial game
    {ok, continue} = binbo:new_game(Pid),
    % e2-e4
    {ok, continue} = binbo:index_move(Pid, 12, 20, q),
    % e7-e5
    {ok, continue} = binbo:index_move(Pid, 52, 36, r),
    % a2-a3
    {ok, continue} = binbo:index_move(Pid, 8, 16, b),
    % a7-a6
    {ok, continue} = binbo:index_move(Pid, 48, 40, n),

    % invalid moves
    {error, _} = binbo:index_move(Pid, 9, 17, 'Q'),
    {error, _} = binbo:index_move(Pid, -1, 2),
    {error, _} = binbo:index_move(Pid, 1, -1),
    {error, _} = binbo:index_move(Pid, -1, 2, q),
    {error, _} = binbo:index_move(Pid, 1, -1, q),
    {error, _} = binbo:index_move(Pid, 1, -1, q),
    {error, _} = binbo:index_move(Pid, -1, 64, q),

    ok.

%% set_game_winner/1
set_game_winner(Config) ->
    Pid = get_pid(Config),
    % winner1
    {error,{bad_game,undefined}} = binbo:set_game_winner(Pid, winner1),
    {ok, continue} = binbo:new_game(Pid),
    ok = binbo:set_game_winner(Pid, winner1),
    {ok,{winner,winner1,{manual,undefined}}} = binbo:game_status(Pid),
    {error,{already_has_status,{winner,winner1,{manual,undefined}}}} = binbo:set_game_winner(Pid, winner1),
    % winner2
    {ok, continue} = binbo:new_game(Pid),
    ok = binbo:set_game_winner(Pid, winner2, test_reason2),
    {ok,{winner,winner2,{manual,test_reason2}}} = binbo:game_status(Pid),
    {error,{already_has_status,{winner,winner2,{manual,test_reason2}}}} = binbo:set_game_winner(Pid, winner2),
    % after manual draw
    {ok, continue} = binbo:new_game(Pid),
    ok = binbo:game_draw(Pid, test_draw),
    {error,{already_has_status,{draw,{manual,test_draw}}}} = binbo:set_game_winner(Pid, winner3),
    % after checkmate
    {ok, continue} = binbo:new_game(Pid, <<"rnbqkbnr/3ppppp/ppp5/8/2B1P3/5Q2/PPPP1PPP/RNB1K1NR w KQkq -">>),
    {ok, <<"rnbqkbnr/3ppppp/ppp5/8/2B1P3/5Q2/PPPP1PPP/RNB1K1NR w KQkq - 0 1">>} = binbo:get_fen(Pid),
    {ok, {checkmate, white_wins}} = binbo:move(Pid, <<"c4f7">>),
    {ok,{checkmate, white_wins}} = binbo:game_status(Pid),
    {error,{already_has_status,{checkmate, white_wins}}} = binbo:set_game_winner(Pid, winner3, test_reason3),
    % after stalemate
    {ok, continue} = binbo:new_game(Pid, <<"k7/8/8/8/7Q/8/3K4/1R6 w - -">>),
    {ok, <<"k7/8/8/8/7Q/8/3K4/1R6 w - - 0 1">>} = binbo:get_fen(Pid),
    {ok, {draw,stalemate}} = binbo:move(Pid, <<"h4h7">>),
    {ok, {draw,stalemate}} = binbo:game_status(Pid),
    {error,{already_has_status,{draw,stalemate}}} = binbo:set_game_winner(Pid, winner3, test_reason3),
    % move after the winner is set
    {ok, continue} = binbo:new_game(Pid),
    ok = binbo:set_game_winner(Pid, winner2, test_reason2),
    {error,{{game_over,{winner,winner2,{manual,test_reason2}}}, <<"e2e4">>}} = binbo:move(Pid, <<"e2e4">>),

    ok.

%% draw_when_king_and_bishop_versus_king/1
draw_when_king_and_bishop_versus_king(Config) ->
    Pid = get_pid(Config),
    % White King and Bishop on light square vs Black King
    {ok,{draw,insufficient_material}} = binbo:new_game(Pid, <<"8/8/8/2k5/8/8/1K2B3/8 w - - 0 1">>),
    {ok,{draw,insufficient_material}} = binbo:game_status(Pid),

    % White King and Bishop on dark square vs Black King
    {ok,{draw,insufficient_material}} = binbo:new_game(Pid, <<"8/8/8/2k5/8/8/1K1B4/8 w - - 0 1">>),
    {ok,{draw,insufficient_material}} = binbo:game_status(Pid),

    % Black King and Bishop on light square vs White King
    {ok,{draw,insufficient_material}} = binbo:new_game(Pid, <<"8/5b2/2k5/8/8/8/1K6/8 w - - 0 1">>),
    {ok,{draw,insufficient_material}} = binbo:game_status(Pid),

    % Black King and Bishop on dark square vs White King
    {ok,{draw,insufficient_material}} = binbo:new_game(Pid, <<"8/4b3/2k5/8/8/8/1K6/8 w - - 0 1">>),
    {ok,{draw,insufficient_material}} = binbo:game_status(Pid),

    ok.

%% draw_when_king_and_knight_versus_king/1
draw_when_king_and_knight_versus_king(Config) ->
    Pid = get_pid(Config),
    % White King and Knight vs Black King
    {ok,{draw,insufficient_material}} = binbo:new_game(Pid, <<"8/8/2k5/8/6N1/8/1K6/8 w - - 0 1">>),
    {ok,{draw,insufficient_material}} = binbo:game_status(Pid),

    % Black King and Knight vs White King
    {ok,{draw,insufficient_material}} = binbo:new_game(Pid, <<"8/8/2k5/3n4/8/8/1K6/8 w - - 0 1">>),
    {ok,{draw,insufficient_material}} = binbo:game_status(Pid),

    ok.

%% print_board_when_game_undefined
print_board_when_game_undefined(Config) ->
    Pid = get_pid(Config),
    {error,{bad_game,undefined}} = binbo:print_board(Pid),
    ok.


%%%------------------------------------------------------------------------------
%%%   Internal helpers
%%%------------------------------------------------------------------------------

%% get_pid/1
get_pid(Config) ->
    ?value(pid, Config).

%% make_legal_moves/1
make_legal_moves(_Pid, []) ->
    ok;
make_legal_moves(Pid, [Move | Tail]) ->
    case binbo:move(Pid, Move) of
        {ok, continue} ->
            make_legal_moves(Pid, Tail);
        {error, Reason} ->
            {error, Reason}
    end.

%% check_int_movelist/1
check_int_movelist([]) -> ok;
check_int_movelist([{From,To}|Tail]) ->
    true = (erlang:is_integer(From) andalso (From >= 0) andalso (From =< 63)),
    true = (erlang:is_integer(To) andalso (To >= 0) andalso (To =< 63)),
    true = (From =/= To),
    check_int_movelist(Tail).

%% check_bin_movelist/1
check_bin_movelist([]) -> ok;
check_bin_movelist([{From,To}|Tail]) ->
    true = (erlang:is_binary(From) andalso (erlang:byte_size(From) =:= 2)),
    true = (erlang:is_binary(To) andalso (erlang:byte_size(To) =:= 2)),
    true = (From =/= To),
    check_bin_movelist(Tail).


%% check_str_movelist/1
check_str_movelist([]) -> ok;
check_str_movelist([{From,To}|Tail]) ->
    true = (erlang:is_list(From) andalso (erlang:length(From) =:= 2)),
    true = (erlang:is_list(To) andalso (erlang:length(To) =:= 2)),
    true = (From =/= To),
    check_str_movelist(Tail).


%% test_pieces_list_with_square_index/1
test_pieces_list_with_square_index(List) ->
    % a1 - h1
    true = lists:member({0,  white, rook}, List),
    true = lists:member({1,  white, knight}, List),
    true = lists:member({2,  white, bishop}, List),
    true = lists:member({3,  white, queen}, List),
    true = lists:member({4,  white, king}, List),
    true = lists:member({5,  white, bishop}, List),
    true = lists:member({6,  white, knight}, List),
    true = lists:member({7,  white, rook}, List),
    % a2 - h2
    true = lists:member({8,  white, pawn}, List),
    true = lists:member({9,  white, pawn}, List),
    true = lists:member({10, white, pawn}, List),
    true = lists:member({11, white, pawn}, List),
    true = lists:member({12, white, pawn}, List),
    true = lists:member({13, white, pawn}, List),
    true = lists:member({14, white, pawn}, List),
    true = lists:member({15, white, pawn}, List),
    % a8 - h8
    true = lists:member({56, black, rook}, List),
    true = lists:member({57, black, knight}, List),
    true = lists:member({58, black, bishop}, List),
    true = lists:member({59, black, queen}, List),
    true = lists:member({60, black, king}, List),
    true = lists:member({61, black, bishop}, List),
    true = lists:member({62, black, knight}, List),
    true = lists:member({63, black, rook}, List),
    % a7 - h7
    true = lists:member({48, black, pawn}, List),
    true = lists:member({49, black, pawn}, List),
    true = lists:member({50, black, pawn}, List),
    true = lists:member({51, black, pawn}, List),
    true = lists:member({52, black, pawn}, List),
    true = lists:member({53, black, pawn}, List),
    true = lists:member({54, black, pawn}, List),
    true = lists:member({55, black, pawn}, List),
    ok.


%% test_pieces_list_with_square_notation/1
test_pieces_list_with_square_notation(List) ->
    % a1 - h1
    true = lists:member({<<"a1">>, white, rook}, List),
    true = lists:member({<<"b1">>, white, knight}, List),
    true = lists:member({<<"c1">>, white, bishop}, List),
    true = lists:member({<<"d1">>, white, queen}, List),
    true = lists:member({<<"e1">>, white, king}, List),
    true = lists:member({<<"f1">>, white, bishop}, List),
    true = lists:member({<<"g1">>, white, knight}, List),
    true = lists:member({<<"h1">>, white, rook}, List),
    % a2 - h2
    true = lists:member({<<"a2">>, white, pawn}, List),
    true = lists:member({<<"b2">>, white, pawn}, List),
    true = lists:member({<<"c2">>, white, pawn}, List),
    true = lists:member({<<"d2">>, white, pawn}, List),
    true = lists:member({<<"e2">>, white, pawn}, List),
    true = lists:member({<<"f2">>, white, pawn}, List),
    true = lists:member({<<"g2">>, white, pawn}, List),
    true = lists:member({<<"h2">>, white, pawn}, List),
    % a8 - h8
    true = lists:member({<<"a8">>, black, rook}, List),
    true = lists:member({<<"b8">>, black, knight}, List),
    true = lists:member({<<"c8">>, black, bishop}, List),
    true = lists:member({<<"d8">>, black, queen}, List),
    true = lists:member({<<"e8">>, black, king}, List),
    true = lists:member({<<"f8">>, black, bishop}, List),
    true = lists:member({<<"g8">>, black, knight}, List),
    true = lists:member({<<"h8">>, black, rook}, List),
    % a7 - h7
    true = lists:member({<<"a7">>, black, pawn}, List),
    true = lists:member({<<"b7">>, black, pawn}, List),
    true = lists:member({<<"c7">>, black, pawn}, List),
    true = lists:member({<<"d7">>, black, pawn}, List),
    true = lists:member({<<"e7">>, black, pawn}, List),
    true = lists:member({<<"f7">>, black, pawn}, List),
    true = lists:member({<<"g7">>, black, pawn}, List),
    true = lists:member({<<"h7">>, black, pawn}, List),
    ok.
