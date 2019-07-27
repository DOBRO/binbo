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

-module(binbo).

-export([start/0, stop/0]).
-export([new_server/0, new_server/1]).
-export([new_game/1, new_game/2, game_state/1, game_status/1]).
-export([move/2, san_move/2, get_fen/1]).
-export([game_draw/1, game_draw/2]).
-export([print_board/1, print_board/2]).
-export([stop_server/1]).

-define(APPLICATION, ?MODULE).

%% Test:
%% {ok, Pid} = binbo:new_server(), binbo:new_game(Pid).
%% binbo:print_board(Pid, [unicode]).
%% binbo:game_state(Pid).
%% binbo:move(Pid, "e2e4").

%%%------------------------------------------------------------------------------
%%%   API
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

%% new_server/0
-spec new_server() -> {ok, pid()}.
%% @equiv new_server([])
new_server() ->
	new_server([]).

%% new_server/0
-spec new_server(Args :: term()) -> {ok, pid()}.
%% @doc
%% Call to start new game process.
%% @end
new_server(Args) ->
	binbo_sup:start_child(Args).

%% stop_server/1
-spec stop_server(pid()) -> binbo_server:stop_ret().
%% @doc
%% Call to stop the game process.
%% @end
stop_server(Pid) ->
	binbo_server:stop(Pid).

%% new_game/1
-spec new_game(pid()) -> binbo_server:new_game_ret().
%% @equiv new_game(Pid, initial)
new_game(Pid) ->
	new_game(Pid, initial).

%% new_game/2
-spec new_game(pid(), initial | binbo_fen:fen()) -> binbo_server:new_game_ret().
new_game(Pid, Fen) ->
	binbo_server:new_game(Pid, Fen).

%% new_game/2
-spec move(pid(), binbo_move:sq_move()) -> binbo_server:game_move_ret().
move(Pid, Move) ->
	binbo_server:game_move(Pid, Move).

%% san_move/2
-spec san_move(pid(), binbo_move:sq_move()) -> binbo_server:game_move_ret().
san_move(Pid, SanMove) ->
	binbo_server:game_san_move(Pid, SanMove).

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
	% avoiding sending large PrettyBoard between processes
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
