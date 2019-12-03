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
-export([new_game/2, game_move/2, game_san_move/2, get_fen/1]).
-export([load_pgn/2, load_pgn_file/2]).
-export([game_state/1, game_status/1, game_draw/2]).
-export([all_legal_moves/2]).
-export([new_uci_game/2]).
-export([uci_command_call/2]).
-export([uci_mode/1, uci_bestmove/2]).
-export([uci_play/3]).
-export([set_uci_handler/2]).

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
-type new_uci_game_ret() :: {ok, game_status()} | {error, init_uci_game_error()}.
-type game_move_ret() :: {ok, game_status()} | {error, binbo_game:move_error()}.
-type game_state_ret() :: undefined | bb_game().
-type game_status_ret() :: binbo_game:status_ret().
-type game_draw_ret() :: ok | {error, binbo_game:draw_error()}.
-type get_fen_ret() :: binbo_game:get_fen_ret().
-type load_pgn_ret() :: {ok, game_status()} | {error, binbo_game:load_pgn_error()}.
-type load_pgn_file_ret() :: {ok, game_status()} | {error, any()}.
-type all_legal_moves_ret() :: binbo_game:all_legal_moves_ret().
-type uci_bestmove_ret() :: {ok, binary()} | {error, term()}.
-type engine_path() :: binbo_uci:engine_path().
-type uci_game_opts() :: #{
	engine_path := engine_path(),
	fen => fen()
}.
-type init_uci_game_error() :: {game_over_with_status, game_status()} | could_not_open_port.
-type uci_handler() :: undefined | default | fun((binary()) -> term()).

%% @todo Add comments for record fields
-record(state, {
	game = undefined :: undefined | bb_game(),
	uci_port = undefined :: undefined | port(),
	uci_from = undefined :: undefined | from(),
	uci_wait_prefix = undefined :: undefined | binary(),
	uci_wait_prefix_size = undefined :: undefined | pos_integer(),
	uci_wait_prefix_handler = undefined :: undefined | fun(),
	uci_handler = undefined :: uci_handler()
}).
-type state() :: #state{}.

-export_type([new_game_ret/0, game_move_ret/0, get_fen_ret/0]).
-export_type([load_pgn_ret/0, load_pgn_file_ret/0]).
-export_type([game_state_ret/0, game_status_ret/0, game_draw_ret/0]).
-export_type([all_legal_moves_ret/0]).
-export_type([stop_ret/0]).
-export_type([new_uci_game_ret/0, uci_bestmove_ret/0]).
-export_type([uci_game_opts/0]).
-export_type([uci_handler/0]).

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
-spec handle_call(term(), from(), state()) -> {reply, term(), state()} | {noreply, state()}.
handle_call({new_game, Fen}, _From, State) ->
	{Reply, State2} = case binbo_game:new(Fen) of
		{ok, {Game, GameStatus}} ->
			{{ok, GameStatus}, State#state{game = Game}};
		Error ->
			{Error, State}
	end,
	{reply, Reply, State2};
handle_call({game_move, MoveNotation, Move}, _From, #state{game = Game0} = State0) ->
	{Reply, State} = case binbo_game:move(MoveNotation, Move, Game0) of
		{ok, {Game, GameStatus}} ->
			{{ok, GameStatus}, State0#state{game = Game}};
		{error, _} = Error ->
			{Error, State0}
	end,
	{reply, Reply, State};
handle_call({load_pgn, Type, Data}, _From, State0) ->
	Result = case Type of
		binary -> binbo_game:load_pgn(Data);
		file   -> binbo_game:load_pgn_file(Data)
	end,
	{Reply, State} = case Result of
		{ok, {Game, GameStatus}} ->
			{{ok, GameStatus}, State0#state{game = Game}};
		{error, _} = Error ->
			{Error, State0}
	end,
	{reply, Reply, State};
handle_call(game_state, _From, #state{game = Game} = State) ->
	{reply, Game, State};
handle_call({update_game_state, Game}, _From, State) ->
	{reply, ok, State#state{game = Game}};
handle_call(game_status, _From, #state{game = Game} = State) ->
	Reply = binbo_game:status(Game),
	{reply, Reply, State};
handle_call(get_fen, _From, #state{game = Game} = State) ->
	Reply = binbo_game:get_fen(Game),
	{reply, Reply, State};
handle_call({all_legal_moves, MoveType}, _From, #state{game = Game} = State) ->
	Reply = binbo_game:all_legal_moves(Game, MoveType),
	{reply, Reply, State};
handle_call({game_draw, Reason}, _From, #state{game = Game0} = State0) ->
	{Reply, State} = case binbo_game:draw(Reason, Game0) of
		{ok, Game} -> {ok, State0#state{game = Game}};
		Error      -> {Error, State0}
	end,
	{reply, Reply, State};
handle_call({new_uci_game, Opts}, _From, State0) ->
	case init_uci_game(Opts, State0) of
		{ok, #state{game = Game} = State} ->
			{reply, binbo_game:status(Game), State};
		{error, Reason, State} ->
			{reply, {error, Reason}, State}
	end;
handle_call({uci_command, _}, _From, #state{uci_port = undefined} = State0) ->
	{reply, {error, uci_port_not_open}, State0};
handle_call({uci_command, {Command, WaitPrefix, PrefixHandler}}, From, #state{uci_port = Port} = State0) ->
	_ = uci_port_command(Port, Command),
	State = State0#state{
		uci_from = From,
		uci_wait_prefix = WaitPrefix,
		uci_wait_prefix_size = erlang:byte_size(WaitPrefix),
		uci_wait_prefix_handler = PrefixHandler
	},
	% Do NOT reply here. Reply later in handle_info/2.
	{noreply, State};
handle_call({uci_command, Command}, _From, #state{uci_port = Port} = State0) ->
	_ = uci_port_command(Port, Command),
	{reply, ok, State0};
handle_call(_Msg, _From, State) ->
	{reply, ignored, State}.

%% handle_cast/2
handle_cast({set_uci_handler, Handler}, State0) ->
	NewHandler = case Handler of
		default ->
			fun binbo_uci:default_handler/1;
		_ when is_function(Handler) ->
			case erlang:fun_info(Handler, arity) of
				{arity,1} -> Handler;
				_ -> undefined
			end;
		_ ->
			undefined
	end,
	{noreply, State0#state{uci_handler = NewHandler}};
handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

%% handle_info/2
handle_info({Port, {data, Data}}, #state{uci_port = Port, uci_wait_prefix = Prefix} = State0) when is_binary(Prefix) ->
	#state{uci_wait_prefix_size = PrefixSize, uci_wait_prefix_handler = PrefixHandler} = State0,
	State = case PrefixHandler(Data, Prefix, PrefixSize) of
		skip ->
			State0;
		Reply ->
			Result = case Reply of
				reply_ok -> ok;
				{reply, Val} -> {ok, Val}
			end,
			ReplyTo = State0#state.uci_from,
			_ = gen_server:reply(ReplyTo, Result), % reply here
			State0#state{
				uci_from = undefined,
				uci_wait_prefix = undefined,
				uci_wait_prefix_size = undefined,
				uci_wait_prefix_handler = undefined
			}
	end,
	_ = maybe_handle_uci_message(State0#state.uci_handler, Data),
	{noreply, State};
handle_info({Port, {data, Data}}, #state{uci_port = Port} = State0) ->
	_ = maybe_handle_uci_message(State0#state.uci_handler, Data),
	{noreply, State0};
handle_info({'EXIT', Port, _}, #state{uci_port = Port} = State0) ->
	% @todo Handle port exit
	State = State0#state{uci_port = undefined},
	{noreply, State};
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
	call(Pid, {game_move, sq, Move}).

%% game_san_move/2
-spec game_san_move(pid(), sq_move()) -> game_move_ret().
game_san_move(Pid, Move) ->
	call(Pid, {game_move, san, Move}).

%% load_pgn/2
-spec load_pgn(pid(), binbo_pgn:pgn()) -> load_pgn_ret().
load_pgn(Pid, Pgn) ->
	call(Pid, {load_pgn, binary, Pgn}).

%% load_pgn_file/2
-spec load_pgn_file(pid(), binbo_game:filename()) -> load_pgn_file_ret().
load_pgn_file(Pid, Filename) ->
	call(Pid, {load_pgn, file, Filename}).

%% game_state/1
-spec game_state(pid()) -> game_state_ret().
game_state(Pid) ->
	call(Pid, game_state).

%% game_status/1
-spec game_status(pid()) -> game_status_ret().
game_status(Pid) ->
	call(Pid, game_status).

%% game_draw/2
-spec game_draw(pid(), term()) -> game_draw_ret().
game_draw(Pid, Reason) ->
	call(Pid, {game_draw, Reason}).

%% get_fen/2
-spec get_fen(pid()) -> get_fen_ret().
get_fen(Pid) ->
	call(Pid, get_fen).

%% all_legal_moves/2
-spec all_legal_moves(pid(), int | bin | str) -> all_legal_moves_ret().
all_legal_moves(Pid, MoveType) ->
	call(Pid, {all_legal_moves, MoveType}).

%% new_uci_game/2
-spec new_uci_game(pid(), uci_game_opts()) -> new_uci_game_ret().
new_uci_game(Pid, Opts) ->
	call(Pid, {new_uci_game, Opts}).

%% uci_command_call/2
-spec uci_command_call(pid(), iodata() | binbo_uci:command_spec()) -> ok | {ok, term()} | {error, term()}.
uci_command_call(Pid, Command) ->
	uci_command_call(Pid, Command, 5000).

%% uci_command_call/3
-spec uci_command_call(pid(), iodata() | binbo_uci:command_spec(), timeout()) -> ok | {ok, term()} | {error, term()}.
uci_command_call(Pid, Command, Timeout) ->
	call_uci(Pid, Command, Timeout).

%% uci_mode/1
-spec uci_mode(pid()) -> ok | {error, term()}.
uci_mode(Pid) ->
	uci_command_call(Pid, binbo_uci:command_spec_uci()).

%% uci_bestmove/2
-spec uci_bestmove(pid(), binbo_uci:bestmove_opts()) -> uci_bestmove_ret().
uci_bestmove(Pid, Opts) ->
	Movetime = binbo_uci:bestmove_search_time(Opts),
	CommandSpec = binbo_uci:command_spec_bestmove(Opts, Movetime),
	Timeout = case is_integer(Movetime) of
		true  ->
			% The engine searches the best move exactly Movetime milliseconds.
			% That's why we add extra time (1000 milliseconds) to give a possibility for gen_server
			% to receive the message from port after search and not to crash by timeout (Movetime)
			(Movetime + 1000);
		false ->
			infinity
	end,
	uci_command_call(Pid, CommandSpec, Timeout).

%% set_uci_handler/2
-spec set_uci_handler(pid(), uci_handler()) -> ok.
set_uci_handler(Pid, Handler) ->
	gen_server:cast(Pid, {set_uci_handler, Handler}).

%% uci_play/3
%% @todo Add spec
uci_play(Pid, BestMoveOpts, Move) ->
	Game0 = game_state(Pid),
	case binbo_game:move(sq, Move, Game0) of
		{ok, {Game, _GameStatus}} ->
			uci_play(Pid, BestMoveOpts, Move, Game);
		{error, _} = Error ->
			Error
	end.



%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

%% call/2
-spec call(pid(), term()) -> term().
call(Pid, Msg) ->
	call(Pid, Msg, 5000).

%% call/3
-spec call(pid(), term(), timeout()) -> term().
call(Pid, Msg, Timeout) ->
	gen_server:call(Pid, Msg, Timeout).

%% call_uci/2
-spec call_uci(pid(), iodata() | binbo_uci:command_spec(), timeout()) -> term().
call_uci(Pid, CommandSpec, Timeout) ->
	call(Pid, {uci_command, CommandSpec}, Timeout).

%% init_uci_game/2
-spec init_uci_game(uci_game_opts(), state()) -> {ok, state()} | {error, init_uci_game_error(), state()}.
init_uci_game(Opts, State) ->
	Steps = [fen, close_port, open_port, uci_commands],
	init_uci_game(Steps, Opts, State).

%% init_uci_game/3
-spec init_uci_game([fen|close_port|open_port|uci_commands], uci_game_opts(), state()) -> {ok, state()} | {error, init_uci_game_error(), state()}.
init_uci_game([], _Opts, State) ->
	{ok, State};
init_uci_game([fen|Tail], Opts, State) ->
	Fen = maps:get(fen, Opts, binbo_fen:initial()),
	case binbo_game:new(Fen) of
		{ok, {Game, GameStatus}} ->
			State2 = State#state{game = Game},
			case binbo_position:is_status_inprogress(GameStatus) of
				true  ->
					init_uci_game(Tail, Opts, State2);
				false ->
					{error, {game_over_with_status, GameStatus}, State2}
			end;
		{error, Reason} ->
			{error, Reason, State}
	end;
init_uci_game([close_port|Tail], Opts, State) ->
	#state{uci_port = Port} = State,
	_ = maybe_close_port(Port),
	State2 = state_without_uci_port(State),
	init_uci_game(Tail, Opts, State2);
init_uci_game([open_port|Tail], Opts, State) ->
	EnginePath = maps:get(engine_path, Opts, undefined),
	case open_uci_port(EnginePath) of
		{ok, Port} ->
			State2 = state_with_uci_port(State, Port),
			init_uci_game(Tail, Opts, State2);
		{error, Reason} ->
			{error, {could_not_open_port, Reason}, State}
	end;
init_uci_game([uci_commands|Tail], Opts, State) ->
	#state{uci_port = Port, game = Game} = State,
	{ok, Fen} = binbo_game:get_fen(Game),
	Command = [
		"uci", $\n,
		"ucinewgame", $\n,
		"position fen ", Fen
	],
	_ = uci_port_command(Port, Command),
	init_uci_game(Tail, Opts, State).


%% state_with_uci_port/1
-spec state_with_uci_port(state(), port()) -> state().
state_with_uci_port(State, Port) ->
	State#state{uci_port = Port}.

%% state_without_uci_port/1
-spec state_without_uci_port(state()) -> state().
state_without_uci_port(State) ->
	State#state{uci_port = undefined}.

%% maybe_close_port/1
-spec maybe_close_port(term()) -> ok.
maybe_close_port(Port) ->
	_ = case erlang:port_info(Port, id) of
		undefined -> undefined;
		_ -> erlang:port_close(Port)
	end,
	ok.

%% open_uci_port/1
-spec open_uci_port(engine_path()) -> {ok, port()} | {error, any()}.
open_uci_port(EnginePath) ->
	binbo_uci:open_port(EnginePath).

%% uci_port_command/2
-spec uci_port_command(port(), iodata()) -> ok.
uci_port_command(Port, Command) ->
	binbo_uci:send_command(Port, Command).

%% maybe_handle_uci_message/2
-spec maybe_handle_uci_message(uci_handler(), binary()) -> term().
maybe_handle_uci_message(Handler, Data) ->
	case Handler of
		undefined -> undefined;
		_ -> Handler(Data)
	end.


%% uci_play/4
%% @todo Add spec
uci_play(Pid, BestMoveOpts, Lastmove, Game) ->
	BestMoveRet = case Lastmove of
		undefined ->
			uci_bestmove(Pid, BestMoveOpts);
		_ ->
			{ok, Fen} = binbo_game:get_fen(Game),
			Command = ["position fen ", Fen],
			case uci_command_call(Pid, Command) of
				ok ->
					uci_bestmove(Pid, BestMoveOpts);
				{error, _} = Err ->
					Err
			end
	end,
	case BestMoveRet of
		{ok, BestMove} ->
			do_uci_play(Pid, BestMove, Game);
		{error, _} = Error ->
			Error
	end.

%% do_uci_play/3
%% @todo Add spec
do_uci_play(Pid, BestMove, Game0) ->
	case binbo_game:move(sq, BestMove, Game0) of
		{ok, {Game, GameStatus}} ->
			{ok, Fen} = binbo_game:get_fen(Game),
			Command = ["position fen ", Fen],
			case uci_command_call(Pid, Command) of
				ok ->
					ok = call(Pid, {update_game_state, Game}),
					{ok, GameStatus, BestMove};
				{error, _} = Err ->
					Err
			end;
		{error, _} = Error ->
			Error
	end.
