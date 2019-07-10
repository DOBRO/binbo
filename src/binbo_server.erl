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

-module(binbo_server).
-behaviour(gen_server).

-export([start_link/1]).
-export([stop/1]).
-export([new_game/2, game_move/2, get_fen/1]).
-export([game_state/1, game_status/1, game_draw/2]).

%%% gen_server export.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%%%------------------------------------------------------------------------------
%%%   Types
%%%------------------------------------------------------------------------------

-type stop_ret() :: ok | {error, {not_pid, term()}}.
-type from() :: {pid(), reference()}.
-type bb_game() :: binbo_position:bb_game().
-type fen() :: binbo_game:game_fen().
-type sq_move() :: binbo_move:sq_move().
-type game_status() :: binbo_game:game_status().
-type new_game_ret() :: {ok, game_status()} | {error, binbo_game:init_error()}.
-type game_move_ret() :: {ok, game_status()} | {error, binbo_game:move_error()}.
-type game_state_ret() :: undefined | bb_game().
-type game_status_ret() :: binbo_game:status_ret().
-type game_draw_ret() :: ok | {error, binbo_game:draw_error()}.
-type get_fen_ret() :: binbo_game:get_fen_ret().
-record(state, {
	game = undefined :: undefined | bb_game()
}).
-type state() :: #state{}.

-export_type([new_game_ret/0, game_move_ret/0, get_fen_ret/0]).
-export_type([game_state_ret/0, game_status_ret/0, game_draw_ret/0]).
-export_type([stop_ret/0]).

%%%------------------------------------------------------------------------------
%%% start/stop
%%%------------------------------------------------------------------------------

%% start_link/0
-spec start_link(Args :: term()) -> {ok, pid()}.
start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

%% stop/1
-spec stop(pid()) -> stop_ret().
stop(Pid) ->
	case is_pid(Pid) of
		true  -> gen_server:cast(Pid, stop);
		false -> {error, {not_pid, Pid}}
	end.


%%%------------------------------------------------------------------------------
%%%   gen_server API
%%%------------------------------------------------------------------------------

%% init/1
-spec init(Args :: term()) -> {ok, state()}.
init(_Args) ->
	process_flag(trap_exit, true),
	State = #state{},
	{ok, State}.

%% handle_call/3
-spec handle_call({new_game, fen()}, from(), state()) -> {reply, new_game_ret(), state()}
				; ({game_move, sq_move()}, from(), state()) -> {reply, game_move_ret(), state()}
				; (game_state, from(), state()) -> {reply, game_state_ret(), state()}
				; (game_status, from(), state()) -> {reply, game_status_ret(), state()}
				; (get_fen, from(), state()) -> {reply, get_fen_ret(), state()}
				; (game_draw, from(), state()) -> {reply, game_draw_ret(), state()}.
handle_call({new_game, Fen}, _From, State) ->
	{Reply, State2} = case binbo_game:new(Fen) of
		{ok, {Game, GameStatus}} ->
			{{ok, GameStatus}, State#state{game = Game}};
		Error ->
			{Error, State}
	end,
	{reply, Reply, State2};
handle_call({game_move, Move}, _From, #state{game = Game0} = State0) ->
	{Reply, State} = case binbo_game:move(Move, Game0) of
		{ok, {Game, GameStatus}} ->
			{{ok, GameStatus}, State0#state{game = Game}};
		{error, _} = Error ->
			{Error, State0}
	end,
	{reply, Reply, State};
handle_call(game_state, _From, #state{game = Game} = State) ->
	{reply, Game, State};
handle_call(game_status, _From, #state{game = Game} = State) ->
	Reply = binbo_game:status(Game),
	{reply, Reply, State};
handle_call(get_fen, _From, #state{game = Game} = State) ->
	Reply = binbo_game:get_fen(Game),
	{reply, Reply, State};
handle_call({game_draw, Reason}, _From, #state{game = Game0} = State0) ->
	{Reply, State} = case binbo_game:draw(Reason, Game0) of
		{ok, Game} -> {ok, State0#state{game = Game}};
		Error      -> {Error, State0}
	end,
	{reply, Reply, State};
handle_call(_Msg, _From, State) ->
	{reply, ignored, State}.

%% handle_cast/2
handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

%% handle_info/2
handle_info(_Msg, State) ->
	{noreply, State}.

%% terminate/2
terminate(_Reason, _State) ->
	ok.

%% code_change/3
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% new_game/2
-spec new_game(pid(), fen()) -> new_game_ret().
new_game(Pid, Fen) ->
	call(Pid, {new_game, Fen}).

%% game_move/2
-spec game_move(pid(), sq_move()) -> game_move_ret().
game_move(Pid, Move) ->
	call(Pid, {game_move, Move}).

%% game_state/1
-spec game_state(pid()) -> game_state_ret().
game_state(Pid) ->
	call(Pid, game_state).

%% game_status/1
game_status(Pid) ->
	call(Pid, game_status).

%% game_draw/2
game_draw(Pid, Reason) ->
	call(Pid, {game_draw, Reason}).

%% get_fen/2
get_fen(Pid) ->
	call(Pid, get_fen).

%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

call(Pid, Msg) ->
	gen_server:call(Pid, Msg).
