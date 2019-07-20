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
	castling_kingside/1, castling_queenside/1
]).


%% all/0
all() -> [{group, games}].

%% groups/0
groups() ->
	[{games, [parallel], [
		move_all_pieces,
		checkmate_white, checkmate_black,
		stalemate_white, stalemate_black,
		castling_kingside, castling_queenside
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
