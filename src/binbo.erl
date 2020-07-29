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

-module(binbo).

-export([start/0, stop/0]).
-export([new_server/0, new_server/1]).
-export([get_server_options/1, set_server_options/2]).
-export([new_game/1, new_game/2]).
-export([game_state/1, game_status/1, side_to_move/1]).
-export([move/2, san_move/2, get_fen/1]).
-export([load_pgn/2, load_pgn_file/2]).
-export([game_draw/1, game_draw/2]).
-export([print_board/1, print_board/2]).
-export([all_legal_moves/1, all_legal_moves/2]).
-export([stop_server/1]).
-export([new_uci_game/2]).
-export([uci_command_call/2, uci_command_cast/2]).
-export([uci_mode/1, uci_bestmove/1, uci_bestmove/2]).
-export([uci_play/2, uci_play/3]).
-export([uci_set_position/2, uci_sync_position/1]).
-export([set_uci_handler/2]).

-define(APPLICATION, ?MODULE).

%% Test:
%% {ok, Pid} = binbo:new_server(), binbo:new_game(Pid).
%% binbo:print_board(Pid, [unicode]).
%% binbo:game_state(Pid).
%% binbo:move(Pid, "e2e4").

%%%------------------------------------------------------------------------------
%%%   API (application)
%%%------------------------------------------------------------------------------

%% start/0
-spec start() -> {ok, [atom()]} | {error, term()}.
%% @doc
%% Call to start application.
%% @end
start() ->
	application:ensure_all_started(?APPLICATION).

%% stop/0
-spec stop() -> ok.
%% @doc
%% Call to stop application.
%% @end
stop() ->
	application:stop(?APPLICATION).


%%%------------------------------------------------------------------------------
%%%   API (server)
%%%------------------------------------------------------------------------------

%% new_server/0
-spec new_server() -> {ok, pid()} | {error, term()}.
%% @equiv new_server(#{})
new_server() ->
	new_server(#{}).

%% new_server/1
-spec new_server(Opts :: binbo_server:server_opts()) -> {ok, pid()} | {error, term()}.
%% @doc
%% Call to start new game process.
%% @end
new_server(Opts) ->
	binbo_sup:start_child(Opts).

%% stop_server/1
-spec stop_server(pid()) -> binbo_server:stop_ret().
%% @doc
%% Call to stop the game process.
%% @end
stop_server(Pid) ->
	binbo_server:stop(Pid).

%% get_server_options/1
-spec get_server_options(pid()) -> {ok, binbo_server:server_opts()}.
get_server_options(Pid) ->
	binbo_server:get_server_options(Pid).

%% set_server_options/2
-spec set_server_options(pid(), binbo_server:server_opts()) -> ok | {error, term()}.
set_server_options(Pid, Opts) ->
	binbo_server:set_server_options(Pid, Opts).


%%%------------------------------------------------------------------------------
%%%   API (game)
%%%------------------------------------------------------------------------------

%% new_game/1
-spec new_game(pid()) -> binbo_server:new_game_ret().
%% @equiv new_game(Pid, initial)
new_game(Pid) ->
	new_game(Pid, initial).

%% new_game/2
-spec new_game(pid(), initial | binbo_fen:fen()) -> binbo_server:new_game_ret().
new_game(Pid, Fen) ->
	binbo_server:new_game(Pid, Fen).

%% move/2
-spec move(pid(), binbo_move:sq_move()) -> binbo_server:game_move_ret().
move(Pid, Move) ->
	binbo_server:game_move(Pid, Move).

%% san_move/2
-spec san_move(pid(), binbo_move:sq_move()) -> binbo_server:game_move_ret().
san_move(Pid, SanMove) ->
	binbo_server:game_san_move(Pid, SanMove).

%% load_pgn/2
-spec load_pgn(pid(), binbo_pgn:pgn()) -> binbo_server:load_pgn_ret().
load_pgn(Pid, Pgn) ->
	binbo_server:load_pgn(Pid, Pgn).

%% load_pgn_file/2
-spec load_pgn_file(pid(), binbo_game:filename()) -> binbo_server:load_pgn_file_ret().
load_pgn_file(Pid, Filename) ->
	binbo_server:load_pgn_file(Pid, Filename).

%% game_state/1
-spec game_state(pid()) -> binbo_server:game_state_ret().
game_state(Pid) ->
	binbo_server:game_state(Pid).

%% game_status/1
-spec game_status(pid()) -> binbo_server:game_status_ret().
game_status(Pid) ->
	binbo_server:game_status(Pid).

%% game_draw/1
-spec game_draw(pid()) -> binbo_server:game_draw_ret().
game_draw(Pid) ->
	game_draw(Pid, undefined).

%% game_draw/2
-spec game_draw(pid(), Reason :: term()) -> binbo_server:game_draw_ret().
game_draw(Pid, Reason) ->
	binbo_server:game_draw(Pid, Reason).

%% print_board/1
-spec print_board(pid()) -> ok | {error, binbo_game:pretty_board_error()}.
print_board(Pid) ->
	print_board(Pid, []).

%% print_board/1
-spec print_board(pid(), binbo_position:pretty_board_opts()) -> ok | {error, binbo_game:pretty_board_error()}.
print_board(Pid, Opts) ->
	Game = game_state(Pid),
	% Here we call 'binbo_game:pretty_board/2' directly
	% avoiding sending large 'PrettyBoard' between processes
	case binbo_game:pretty_board(Game, Opts) of
		{ok, {PrettyBoard, GameStatus}} ->
			io:format("~n~ts~n  Status: ~w~n~n", [PrettyBoard, GameStatus]);
		Error ->
			Error
	end.

%% get_fen/1
-spec get_fen(pid()) -> binbo_server:get_fen_ret().
get_fen(Pid) ->
	binbo_server:get_fen(Pid).

%% all_legal_moves/1
-spec all_legal_moves(pid()) -> binbo_server:all_legal_moves_ret().
all_legal_moves(Pid) ->
	all_legal_moves(Pid, int).

%% all_legal_moves/2
-spec all_legal_moves(pid(), int | bin | str) -> binbo_server:all_legal_moves_ret().
all_legal_moves(Pid, MoveType) ->
	binbo_server:all_legal_moves(Pid, MoveType).

%% side_to_move/1
-spec side_to_move(pid()) -> binbo_game:side_to_move_ret().
side_to_move(Pid) ->
	binbo_server:side_to_move(Pid).

%% new_uci_game/2
-spec new_uci_game(pid(), binbo_server:uci_game_opts()) -> binbo_server:new_uci_game_ret().
new_uci_game(Pid, Opts) ->
	binbo_server:new_uci_game(Pid, Opts).

%% uci_command_call/2
-spec uci_command_call(pid(), iodata()) -> ok | {error, term()}.
uci_command_call(Pid, Command) ->
	binbo_server:uci_command_call(Pid, Command).

%% uci_command_cast/2
-spec uci_command_cast(pid(), iodata()) -> ok.
uci_command_cast(Pid, Command) ->
	binbo_server:uci_command_cast(Pid, Command).

%% uci_mode/1
-spec uci_mode(pid()) -> ok | {error, term()}.
uci_mode(Pid) ->
	binbo_server:uci_mode(Pid).

%% uci_bestmove/1
-spec uci_bestmove(pid()) -> binbo_server:uci_bestmove_ret().
uci_bestmove(Pid) ->
	binbo_server:uci_bestmove(Pid, #{}).

%% uci_bestmove/2
-spec uci_bestmove(pid(), binbo_uci:bestmove_opts()) -> binbo_server:uci_bestmove_ret().
uci_bestmove(Pid, Opts) ->
	binbo_server:uci_bestmove(Pid, Opts).

%% set_uci_handler/2
-spec set_uci_handler(pid(), binbo_server:uci_handler()) -> ok.
set_uci_handler(Pid, Handler) ->
	binbo_server:set_uci_handler(Pid, Handler).

%% uci_play/2
-spec uci_play(pid(), binbo_uci:bestmove_opts()) -> binbo_server:uci_play_ret().
uci_play(Pid, BestMoveOpts) ->
	binbo_server:uci_play(Pid, BestMoveOpts).

%% uci_play/3
-spec uci_play(pid(), binbo_uci:bestmove_opts(), binbo_move:sq_move()) -> binbo_server:uci_play_ret().
uci_play(Pid, BestMoveOpts, Move) ->
	binbo_server:uci_play(Pid, BestMoveOpts, Move).

%% uci_set_position/2
-spec uci_set_position(pid(), binbo_fen:fen()) -> {ok, binbo_game:game_status()} | {error, term()}.
uci_set_position(Pid, Fen) ->
	binbo_server:uci_set_position(Pid, Fen).

%% uci_sync_position/1
-spec uci_sync_position(pid()) -> ok | {error, term()}.
uci_sync_position(Pid) ->
	binbo_server:uci_sync_position(Pid).
