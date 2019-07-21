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
	castling_kingside/1, castling_queenside/1,
	castling_white_after_king_move/1,
	castling_white_after_rook_move/1,
	castling_black_after_king_move/1,
	castling_black_after_rook_move/1,
	castling_white_when_attacked/1,
	castling_black_when_attacked/1
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
		castling_black_when_attacked
	]}].

%% init_per_suite/1
init_per_suite(Config) ->
	{ok, _} = binbo:start(),
	Config.

%% end_per_suite/1
end_per_suite(_Config) ->
	ok = binbo:stop(),
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
			{error, {Reason, Move}}
	end.



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
	{ok, checkmate} = binbo:move(Pid, <<"c4f7">>),
	ok.

%% checkmate_black/1
checkmate_black(Config) ->
	Pid = get_pid(Config),
	{ok, continue} = binbo:new_game(Pid, <<"rnb1k1nr/pppp1ppp/8/2b1p3/P6q/NP6/2PPPPPP/R1BQKBNR b KQkq -">>),
	{ok, checkmate} = binbo:move(Pid, <<"h4f2">>),
	ok.

%% stalemate_white/1
stalemate_white(Config) ->
	Pid = get_pid(Config),
	{ok, continue} = binbo:new_game(Pid, <<"k7/8/8/8/7Q/8/3K4/1R6 w - -">>),
	{ok, {draw,stalemate}} = binbo:move(Pid, <<"h4h7">>),
	ok.

%% stalemate_black/1
stalemate_black(Config) ->
	Pid = get_pid(Config),
	{ok, continue} = binbo:new_game(Pid, <<"1q2b1b1/8/8/8/8/7k/8/K7 b - -">>),
	{ok, {draw,stalemate}} = binbo:move(Pid, <<"e8g6">>),
	ok.

%% castling_kingside/1
castling_kingside(Config) ->
	Pid = get_pid(Config),
	{ok, continue} = binbo:new_game(Pid, <<"r3k2r/ppp1qppp/2np1n2/2b1p1B1/2B1P1b1/2NP1N2/PPP1QPPP/R3K2R w KQkq -">>),
	% White castling
	{ok, continue} = binbo:move(Pid, <<"e1g1">>),
	% Black castling
	{ok, continue} = binbo:move(Pid, <<"e8g8">>),
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
	{error, {invalid_move, 'WHITE_KING'}} = binbo:move(Pid, <<"e1g1">>),
	{error, {invalid_move, 'WHITE_KING'}} = binbo:move(Pid, <<"e1c1">>),
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
	{error, {invalid_move, 'BLACK_KING'}} = binbo:move(Pid, <<"e8g8">>),
	{error, {invalid_move, 'BLACK_KING'}} = binbo:move(Pid, <<"e8c8">>),
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
	{error, {invalid_move, 'WHITE_KING'}} = binbo:move(Pid, <<"e1g1">>),

	% Load new game
	{ok, continue} = binbo:new_game(Pid, Fen),
	ok = make_legal_moves(Pid, [
		<<"a1b1">>, % White Rook moves from A1
		<<"a7a6">>, % Any black move
		<<"b1a1">>, % White rook comes back to A1
		<<"a6a5">>  % Any black move
	]),
	% Castling queenside not allowed
	{error, {invalid_move, 'WHITE_KING'}} = binbo:move(Pid, <<"e1c1">>),

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
	{error, {invalid_move, 'WHITE_KING'}} = binbo:move(Pid, <<"e1g1">>),
	% Castling queenside not allowed
	{error, {invalid_move, 'WHITE_KING'}} = binbo:move(Pid, <<"e1c1">>),
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
	{error, {invalid_move, 'BLACK_KING'}} = binbo:move(Pid, <<"e8g8">>),

	% Load new game
	{ok, continue} = binbo:new_game(Pid, Fen),
	ok = make_legal_moves(Pid, [
		<<"a8b8">>, % Black rook moves from A1
		<<"a2a3">>, % Any white move
		<<"b8a8">>, % Black rook comes back to A1
		<<"a3a4">>  % Any white move
	]),
	% Castling queenside not allowed
	{error, {invalid_move, 'BLACK_KING'}} = binbo:move(Pid, <<"e8c8">>),

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
	{error, {invalid_move, 'BLACK_KING'}} = binbo:move(Pid, <<"e8g8">>),
	% Castling queenside not allowed
	{error, {invalid_move, 'BLACK_KING'}} = binbo:move(Pid, <<"e8c8">>),
	ok.


%% castling_white_when_attacked/1
castling_white_when_attacked(Config) ->
	Pid = get_pid(Config),
	% New game, white king is in check
	{ok, continue} = binbo:new_game(Pid, <<"r3k2r/pppbq1pp/n2p1p1n/4p1B1/1bB1P3/N2P1P1N/PPP1Q1PP/R3K2R w KQkq -">>),
	% No castling allowed
	{error, {invalid_move, 'WHITE_KING'}} = binbo:move(Pid, <<"e1g1">>),
	{error, {invalid_move, 'WHITE_KING'}} = binbo:move(Pid, <<"e1c1">>),

	% New game, G1 is attacked. Castling kingside not allowed
	{ok, continue} = binbo:new_game(Pid, <<"r3k2r/pppbq1pp/n2p1p1n/2b1p1B1/2B1P3/N2P1P1N/PPP1Q1PP/R3K2R w KQkq -">>),
	{error, {invalid_move, 'WHITE_KING'}} = binbo:move(Pid, <<"e1g1">>),

	% New game, F1 is attacked. Castling kingside not allowed
	{ok, continue} = binbo:new_game(Pid, <<"r3k2r/pppbq1pp/n2p1p2/b3p1B1/2B1P3/N1PP1PnN/PP2Q1PP/R3K2R w KQkq -">>),
	{error, {invalid_move, 'WHITE_KING'}} = binbo:move(Pid, <<"e1g1">>),

	% New game, C1 is attacked. Castling queenside not allowed
	{ok, continue} = binbo:new_game(Pid, <<"r3k2r/ppp1q1pp/n2p1pbn/4p1b1/2B1P2B/N1PP1P1N/PP2Q1PP/R3K2R w KQkq -">>),
	{error, {invalid_move, 'WHITE_KING'}} = binbo:move(Pid, <<"e1c1">>),

	% New game, D1 is attacked. Castling queenside not allowed
	{ok, continue} = binbo:new_game(Pid, <<"r3k2r/ppp1q1pp/n2p1p1n/4p1b1/b1B1P2B/N1PP1P1N/PP2Q1PP/R3K2R w KQkq -">>),
	{error, {invalid_move, 'WHITE_KING'}} = binbo:move(Pid, <<"e1c1">>),

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
	{error, {invalid_move, 'BLACK_KING'}} = binbo:move(Pid, <<"e8g8">>),
	{error, {invalid_move, 'BLACK_KING'}} = binbo:move(Pid, <<"e8c8">>),

	% New game, G8 is attacked. Castling kingside not allowed
	{ok, continue} = binbo:new_game(Pid, <<"r3k2r/ppp1q1pp/nb3pbn/4P3/3P3B/NQP2P1N/PP2B1PP/R3K2R b KQkq -">>),
	{error, {invalid_move, 'BLACK_KING'}} = binbo:move(Pid, <<"e8g8">>),

	% New game, F8 is attacked. Castling kingside not allowed
	{ok, continue} = binbo:new_game(Pid, <<"r3k2r/ppp1qbpp/nb3pNn/4P3/3P3B/NQP2P2/PP2B1PP/R3K2R b KQkq -">>),
	{error, {invalid_move, 'BLACK_KING'}} = binbo:move(Pid, <<"e8g8">>),

	% New game, C8 is attacked. Castling queenside not allowed
	{ok, continue} = binbo:new_game(Pid, <<"r3k2r/ppp1qbpp/nb3p1n/4P3/3PN2B/N1P2P1B/PPQ3PP/R3K2R b KQkq -">>),
	{error, {invalid_move, 'BLACK_KING'}} = binbo:move(Pid, <<"e8c8">>),

	% New game, D8 is attacked. Castling queenside not allowed
	{ok, continue} = binbo:new_game(Pid, <<"r3k2r/ppp2bpp/nb2q2n/4Pp2/3P3B/N1P2P1B/PPQ1N1PP/R3K2R b KQkq -">>),
	{error, {invalid_move, 'BLACK_KING'}} = binbo:move(Pid, <<"e8c8">>),

	% New game, B8 is attacked. Castling queenside is allowed
	{ok, continue} = binbo:new_game(Pid, <<"r3k2r/pp3bpp/nbp1q2n/5p2/3P4/N1P1PPBB/PPQ1N1PP/R3K2R b KQkq -">>),
	{ok, continue} = binbo:move(Pid, <<"e8c8">>),

	ok.
