%% Copyright (c) 2019-2024, Sergei Semichev <chessvegas@chessvegas.com>. All Rights Reserved.
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
-export([get_server_options/1, set_server_options/2]).
-export([stop/1]).
-export([new_game/2, game_move/2, game_san_move/2, game_index_move/4, get_fen/1]).
-export([load_pgn/2, load_pgn_file/2]).
-export([game_state/1, set_game_state/2, game_status/1, game_draw/2, set_game_winner/3]).
-export([all_legal_moves/2, side_to_move/1]).
-export([new_uci_game/2]).
-export([uci_command_call/2, uci_command_cast/2]).
-export([uci_mode/1, uci_bestmove/2]).
-export([uci_play/2, uci_play/3]).
-export([uci_set_position/2, uci_sync_position/1]).
-export([set_uci_handler/2]).
-export([get_pieces_list/2]).

%%% gen_server export.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%%%------------------------------------------------------------------------------
%%%   Macros
%%%------------------------------------------------------------------------------
-ifdef(OTP_RELEASE). % since OTP-21
    -if(?OTP_RELEASE >= 25).
        -type from() :: {pid(), gen_server:reply_tag()}.
    -elif(?OTP_RELEASE < 25).
        -type from() :: {pid(), reference()}.
    -endif.
-else. % OTP-20
    -type from() :: {pid(), reference()}.
-endif.


%%%------------------------------------------------------------------------------
%%%   Types
%%%------------------------------------------------------------------------------
-type onterminate_callback() :: fun((pid(), Reason :: term(), Game :: game_state_ret(), Arg :: term()) -> term()).
-type server_opts() :: #{
    idle_timeout => timeout(),
    onterminate  => undefined | {onterminate_callback(), Arg :: term()}
}.
-type stop_ret() :: ok | {error, {not_pid, term()}}.
-type bb_game() :: binbo_position:bb_game().
-type fen() :: binbo_game:game_fen().
-type sq_move() :: binbo_move:sq_move().
-type game_status() :: binbo_game:game_status().
-type new_game_ret() :: {ok, game_status()} | {error, binbo_game:init_error()}.
-type new_uci_game_ret() :: {ok, game_status()} | {error, init_uci_game_error()}.
-type game_move_ret() :: {ok, game_status()} | {error, binbo_game:move_error()}.
-type game_state_ret() :: undefined | bb_game().
-type game_status_ret() :: binbo_game:status_ret().
-type game_draw_ret() :: ok | {error, binbo_game:gameover_status_error()}.
-type set_game_winner_ret() :: ok | {error, binbo_game:gameover_status_error()}.
-type get_fen_ret() :: binbo_game:get_fen_ret().
-type load_pgn_ret() :: {ok, game_status()} | {error, binbo_game:load_pgn_error()}.
-type load_pgn_file_ret() :: {ok, game_status()} | {error, any()}.
-type all_legal_moves_ret() :: binbo_game:all_legal_moves_ret().
-type uci_bestmove_ret() :: {ok, binary()} | {error, term()}.
-type uci_play_ret() :: {ok, game_status(), sq_move()} | {error, term()}.
-type engine_path() :: binbo_uci_connection:engine_path().
-type uci_game_opts() :: #{
    engine_path := engine_path(),
    fen => fen()
}.
-type init_uci_game_error() :: {uci_connection_failed, term()}.
-type uci_handler() :: undefined | default | fun((binary()) -> term()).
-type uci_cmd_spec() :: iodata() | binbo_uci_protocol:command_spec() | {set_position, fen()} | sync_position.

-type uci_wait_prefix() :: binary().
-type uci_wait_prefix_size() :: pos_integer().
-type uci_wait_prefix_handler() :: fun((Message :: binary(), uci_wait_prefix(), uci_wait_prefix_size()) -> reply_ok | {reply, term()} | skip).

-record(state, {
    game = undefined :: undefined | bb_game(),
    idle_timestamp = undefined :: undefined | integer(),
    server_opts = #{} :: server_opts(),
    uci_sockinfo = undefined :: undefined | binbo_uci_connection:socket_info(),
    uci_from = undefined :: undefined | from(),
    uci_wait_prefix = undefined :: undefined | uci_wait_prefix(),
    uci_wait_prefix_size = undefined :: undefined | uci_wait_prefix_size(),
    uci_wait_prefix_handler = undefined :: undefined | uci_wait_prefix_handler(),
    uci_handler = undefined :: uci_handler()
}).
-type state() :: #state{}.

-export_type([server_opts/0]).
-export_type([new_game_ret/0, game_move_ret/0, get_fen_ret/0]).
-export_type([load_pgn_ret/0, load_pgn_file_ret/0]).
-export_type([game_state_ret/0, game_status_ret/0, game_draw_ret/0, set_game_winner_ret/0]).
-export_type([all_legal_moves_ret/0]).
-export_type([stop_ret/0]).
-export_type([new_uci_game_ret/0, uci_bestmove_ret/0, uci_play_ret/0]).
-export_type([uci_game_opts/0]).
-export_type([uci_handler/0]).

%%%------------------------------------------------------------------------------
%%% start/stop
%%%------------------------------------------------------------------------------

%% start_link/0
-spec start_link(ServerOpts :: server_opts()) -> {ok, pid()}.
start_link(ServerOpts) ->
    gen_server:start_link(?MODULE, ServerOpts, []).

%% stop/1
-spec stop(pid()) -> stop_ret().
stop(Pid) ->
    case is_pid(Pid) of
        true  -> cast(Pid, stop);
        false -> {error, {not_pid, Pid}}
    end.


%%%------------------------------------------------------------------------------
%%%   gen_server API
%%%------------------------------------------------------------------------------

%% init/1
-spec init(ServerOpts :: server_opts()) -> {ok, state(), timeout()} | {stop, {error, term()}}.
init(ServerOpts) ->
    process_flag(trap_exit, true),
    case update_server_opts(ServerOpts, #state{}) of
        {ok, State} ->
            {ok, set_idle_timestamp(State), get_idle_timeout(State)};
        {error, Reason} ->
            {stop, Reason}
    end.

%% handle_call/3
-spec handle_call(term(), from(), state()) -> {reply, term(), state(), timeout()} | {noreply, state(), timeout()}.
handle_call(Msg, From, State) ->
    case do_handle_call(Msg, From, State) of
        {reply, Reply, NewState} ->
            reply_with_timeout(Reply, NewState);
        {noreply, NewState} ->
            noreply_with_timeout(NewState)
    end.

%% handle_cast/2
-spec handle_cast(term(), state()) -> {noreply, state(), timeout()} | {stop, normal, state()}.
handle_cast(Msg, State) ->
    case do_handle_cast(Msg, State) of
        {noreply, NewState} ->
            noreply_with_timeout(NewState);
        {stop, Reason, NewState} ->
            {stop, Reason, NewState}
    end.

%% handle_info/2
-spec handle_info(term(), state()) -> {noreply, state(), timeout()} | {stop, Reason :: term(), state()}.
handle_info(Msg, State) ->
    case do_handle_info(Msg, State) of
        {noreply, NewState} ->
            noreply_with_timeout(NewState);
        {stop, Reason, NewState} ->
            {stop, Reason, NewState}
    end.

%% terminate/2
-spec terminate(term(), state()) -> ok.
terminate(Reason, State) ->
    {ok, State2} = uci_disconnect(State),
    _ = handle_onterminate(Reason, State2),
    ok.

%% code_change/3
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%%------------------------------------------------------------------------------
%%%   gen_server intermediate handlers:
%%%      do_handle_call/3, do_handle_cast/2, do_handle_info/2
%%%------------------------------------------------------------------------------

%% do_handle_call/3
-spec do_handle_call(term(), from(), state()) -> {reply, term(), state()} | {noreply, state()}.
do_handle_call({new_game, Fen}, _From, State) ->
    {Reply, NewState} = case binbo_game:new(Fen) of
        {ok, {Game, GameStatus}} ->
            {{ok, GameStatus}, State#state{game = Game}};
        {error, _} = Error ->
            {Error, State}
    end,
    {reply, Reply, NewState};
do_handle_call({game_move, MoveNotation, Move}, _From, #state{game = Game0} = State0) ->
    {Reply, NewState} = case binbo_game:move(MoveNotation, Move, Game0) of
        {ok, {Game, GameStatus}} ->
            {{ok, GameStatus}, State0#state{game = Game}};
        {error, _} = Error ->
            {Error, State0}
    end,
    {reply, Reply, NewState};
do_handle_call({load_pgn, Type, Data}, _From, State) ->
    Result = case Type of
        binary -> binbo_game:load_pgn(Data);
        file   -> binbo_game:load_pgn_file(Data)
    end,
    {Reply, NewState} = case Result of
        {ok, {Game, GameStatus}} ->
            {{ok, GameStatus}, State#state{game = Game}};
        {error, _} = Error ->
            {Error, State}
    end,
    {reply, Reply, NewState};
do_handle_call(game_state, _From, #state{game = Game} = State) ->
    {reply, Game, State};
do_handle_call({set_game_state, Game}, _From, State) ->
    Reply = binbo_game:status(Game),
    {reply, Reply, State#state{game = Game}};
do_handle_call(game_status, _From, #state{game = Game} = State) ->
    Reply = binbo_game:status(Game),
    {reply, Reply, State};
do_handle_call(get_fen, _From, #state{game = Game} = State) ->
    Reply = binbo_game:get_fen(Game),
    {reply, Reply, State};
do_handle_call({all_legal_moves, MoveType}, _From, #state{game = Game} = State) ->
    Reply = binbo_game:all_legal_moves(Game, MoveType),
    {reply, Reply, State};
do_handle_call(side_to_move, _From, #state{game = Game} = State) ->
    Reply = binbo_game:side_to_move(Game),
    {reply, Reply, State};
do_handle_call({game_draw, Reason}, _From, #state{game = Game0} = State0) ->
    {Reply, NewState} = case binbo_game:draw(Reason, Game0) of
        {ok, Game} ->
            {ok, State0#state{game = Game}};
        {error, _} = Error ->
            {Error, State0}
    end,
    {reply, Reply, NewState};
do_handle_call({set_game_winner, Winner, Reason}, _From, #state{game = Game0} = State0) ->
    {Reply, NewState} = case binbo_game:set_winner(Game0, Winner, Reason) of
        {ok, Game} ->
            {ok, State0#state{game = Game}};
        {error, _} = Error ->
            {Error, State0}
    end,
    {reply, Reply, NewState};
do_handle_call({new_uci_game, Opts}, _From, State0) ->
    {Reply, NewState} = case init_uci_game(Opts, State0) of
        {ok, #state{game = Game} = State} ->
            {binbo_game:status(Game), State};
        {error, Reason, State} ->
            {{error, Reason}, State}
    end,
    {reply, Reply, NewState};
do_handle_call({uci_command, _}, _From, #state{uci_sockinfo = undefined} = State0) ->
    {reply, {error, no_uci_connection}, State0};
do_handle_call({uci_command, {set_position, Fen}}, _From, #state{uci_sockinfo = SocketInfo} = State) ->
    {Reply, NewState} = case binbo_game:new(Fen) of
        {ok, {Game, GameStatus}} ->
            Command = [
                "stop", $\n,
                "ucinewgame", $\n,
                "position fen ", Fen
            ],
            _ = uci_send_command(SocketInfo, Command),
            {{ok, GameStatus}, State#state{game = Game}};
        {error, _} = Error ->
            {Error, State}
    end,
    {reply, Reply, NewState};
do_handle_call({uci_command, sync_position}, _From, #state{game = Game, uci_sockinfo = SocketInfo} = State) ->
    Reply = case binbo_game:get_fen(Game) of
        {ok, Fen} ->
            Command = [
                "stop", $\n,
                "ucinewgame", $\n,
                "position fen ", Fen
            ],
            _ = uci_send_command(SocketInfo, Command),
            ok;
        {error, _} = Error ->
            Error
    end,
    {reply, Reply, State};
do_handle_call({uci_command, {Command, WaitPrefix, PrefixHandler}}, From, #state{uci_sockinfo = SocketInfo} = State0) ->
    _ = uci_send_command(SocketInfo, Command),
    State = State0#state{
        uci_from = From,
        uci_wait_prefix = WaitPrefix,
        uci_wait_prefix_size = erlang:byte_size(WaitPrefix),
        uci_wait_prefix_handler = PrefixHandler
    },
    % Do NOT reply here. Reply later in handle_info/2.
    {noreply, State};
do_handle_call({uci_command, Command}, _From, #state{uci_sockinfo = SocketInfo} = State) ->
    _ = uci_send_command(SocketInfo, Command),
    {reply, ok, State};
do_handle_call({set_server_options, Opts}, _From, State) ->
    {Reply, NewState} = case update_server_opts(Opts, State) of
        {ok, State2} ->
            {ok, State2};
        {error, _} = Error ->
            {Error, State}
    end,
    {reply, Reply, NewState};
do_handle_call(get_server_options, _From, #state{server_opts = Opts} = State) ->
    {reply, {ok, Opts}, State};
do_handle_call({get_pieces_list, SquareType}, _From, #state{game = Game} = State) ->
    Reply = binbo_game:get_pieces_list(Game, SquareType),
    {reply, Reply, State};
do_handle_call({set_uci_handler, Handler}, _From, State0) ->
    {Reply, NewState} = case Handler of
        default ->
            {ok, State0#state{uci_handler = fun binbo_uci_protocol:default_handler/1}};
        _ when is_function(Handler) ->
            case erlang:fun_info(Handler, arity) of
                {arity,1} ->
                    {ok, State0#state{uci_handler = Handler}};
                _ ->
                    {{error, bad_function_arity}, State0}
            end;
        undefined ->
            {ok, State0#state{uci_handler = undefined}};
        _ ->
            {{error, invalid_handler}, State0}
    end,
    {reply, Reply, NewState};
do_handle_call(_Msg, _From, State) ->
    {reply, ignored, State}.


%% do_handle_cast/2
-spec do_handle_cast(term(), state()) -> {noreply, state()} | {stop, normal, state()}.
do_handle_cast({uci_command, Command}, #state{uci_sockinfo = SocketInfo} = State) ->
    _ = case SocketInfo of
        undefined -> ok;
        _ -> uci_send_command(SocketInfo, Command)
    end,
    {noreply, State};
do_handle_cast({update_game_state, Game}, State) ->
    {noreply, State#state{game = Game}};
do_handle_cast(stop, State) ->
    {stop, normal, State};
do_handle_cast(_Msg, State) ->
    {noreply, State}.


%% do_handle_info/2
-spec do_handle_info(term(), state()) -> {noreply, state()} | {stop, Reason :: term(), state()}.
do_handle_info({Port, {data, Data}}, #state{uci_sockinfo = {erlang, Port}} = State0) ->
    NewState = handle_uci_packet(Data, State0),
    {noreply, NewState};
do_handle_info({tcp, Socket, Data}, #state{uci_sockinfo = {gen_tcp, Socket}} = State0) ->
    NewState = handle_uci_packet(Data, State0),
    {noreply, NewState};
do_handle_info({'EXIT', Port, _}, #state{uci_sockinfo = {erlang, Port}} = State0) -> % Handle UCI port exit
    {noreply, State0#state{uci_sockinfo = undefined}};
do_handle_info({tcp_closed, Socket}, #state{uci_sockinfo = {gen_tcp, Socket}} = State0) -> % TCP connection closed
    {noreply, State0#state{uci_sockinfo = undefined}};
do_handle_info(timeout, State) -> % Handle idle timeout
    % Since any process can send 'timeout' to our process (e.g. just for fun),
    % we should check if it is a really valid message.
    IdleTimeout = get_idle_timeout(State),
    case is_integer(IdleTimeout) of
        true  ->
            case milliseconds_passed(get_idle_timestamp(State)) >= IdleTimeout of
                true  ->
                    Reason = {shutdown, {idle_timeout_reached, IdleTimeout}},
                    {stop, Reason, State};
                false ->
                    {noreply, State}
            end;
        false -> % no timeout set, don't terminate
            {noreply, State}
    end;
do_handle_info(_Msg, State) ->
    {noreply, State}.


%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% get_server_options/1
-spec get_server_options(pid()) -> {ok, server_opts()}.
get_server_options(Pid) ->
    call(Pid, get_server_options).

%% set_server_options/2
-spec set_server_options(pid(), server_opts()) -> ok | {error, term()}.
set_server_options(Pid, Opts) ->
    call(Pid, {set_server_options, Opts}).

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

%% game_index_move/4
-spec game_index_move(pid(), binbo_board:square_index(), binbo_board:square_index(), q | r | b | n) -> game_move_ret().
game_index_move(Pid, FromIdx, ToIdx, PromoType) ->
    call(Pid, {game_move, idx, {FromIdx, ToIdx, PromoType}}).

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

%% set_game_state/2
-spec set_game_state(pid(), term()) -> game_status_ret().
set_game_state(Pid, Game) ->
    call(Pid, {set_game_state, Game}).

%% game_status/1
-spec game_status(pid()) -> game_status_ret().
game_status(Pid) ->
    call(Pid, game_status).

%% game_draw/2
-spec game_draw(pid(), term()) -> game_draw_ret().
game_draw(Pid, Reason) ->
    call(Pid, {game_draw, Reason}).

%% set_game_winner/3
-spec set_game_winner(pid(), binbo_game:winner(), term()) -> set_game_winner_ret().
set_game_winner(Pid, Winner, Reason) ->
    call(Pid, {set_game_winner, Winner, Reason}).

%% get_fen/2
-spec get_fen(pid()) -> get_fen_ret().
get_fen(Pid) ->
    call(Pid, get_fen).

%% all_legal_moves/2
-spec all_legal_moves(pid(), int | bin | str) -> all_legal_moves_ret().
all_legal_moves(Pid, MoveType) ->
    call(Pid, {all_legal_moves, MoveType}).

%% side_to_move/1
-spec side_to_move(pid()) -> binbo_game:side_to_move_ret().
side_to_move(Pid) ->
    call(Pid, side_to_move).

%% new_uci_game/2
-spec new_uci_game(pid(), uci_game_opts()) -> new_uci_game_ret().
new_uci_game(Pid, Opts) ->
    call(Pid, {new_uci_game, Opts}).

%% uci_command_call/2
-spec uci_command_call(pid(), uci_cmd_spec()) -> ok | {ok, term()} | {error, term()}.
uci_command_call(Pid, Command) ->
    uci_command_call(Pid, Command, 5000).

%% uci_command_call/3
-spec uci_command_call(pid(), uci_cmd_spec(), timeout()) -> ok | {ok, term()} | {error, term()}.
uci_command_call(Pid, Command, Timeout) ->
    call_uci(Pid, Command, Timeout).

%% uci_command_cast/2
-spec uci_command_cast(pid(), iodata()) -> ok.
uci_command_cast(Pid, Command) ->
    cast(Pid, {uci_command, Command}).

%% uci_mode/1
-spec uci_mode(pid()) -> ok | {error, term()}.
uci_mode(Pid) ->
    uci_command_call(Pid, binbo_uci_protocol:command_spec_uci()).

%% uci_bestmove/2
-spec uci_bestmove(pid(), binbo_uci_protocol:bestmove_opts()) -> uci_bestmove_ret().
uci_bestmove(Pid, Opts) ->
    Movetime = binbo_uci_protocol:bestmove_search_time(Opts),
    CommandSpec = binbo_uci_protocol:command_spec_bestmove(Opts, Movetime),
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
    call(Pid, {set_uci_handler, Handler}).

%% uci_play/2
-spec uci_play(pid(), binbo_uci_protocol:bestmove_opts()) -> uci_play_ret().
uci_play(Pid, BestMoveOpts) ->
    Game = game_state(Pid),
    % Do NOT call uci_play/3 here! Do call uci_play_bestmove/3 directly!
    uci_play_bestmove(Pid, BestMoveOpts, Game).

%% uci_play/3
-spec uci_play(pid(), binbo_uci_protocol:bestmove_opts(), sq_move()) -> uci_play_ret().
uci_play(Pid, BestMoveOpts, Move) ->
    Game0 = game_state(Pid),
    case binbo_game:move(sq, Move, Game0) of
        {ok, {Game, _GameStatus}} ->
            % Sync game position with the engine's one to find the correct best move
            ok = uci_send_game_position(Pid, Game),
            uci_play_bestmove(Pid, BestMoveOpts, Game);
        {error, _} = Error ->
            Error
    end.

%% uci_set_position/2
-spec uci_set_position(pid(), fen()) -> {ok, game_status()} | {error, term()}.
uci_set_position(Pid, Fen) ->
    uci_command_call(Pid, {set_position, Fen}).

%% uci_sync_position/1
-spec uci_sync_position(pid()) -> ok | {error, term()}.
uci_sync_position(Pid) ->
    uci_command_call(Pid, sync_position).

%% get_pieces_list/2
-spec get_pieces_list(pid(), index | notation) -> {ok, [binbo_position:sq_piece_tuple()]} | {error, term()}.
get_pieces_list(Pid, SquareType) ->
    call(Pid, {get_pieces_list, SquareType}).


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
-spec call_uci(pid(), uci_cmd_spec(), timeout()) -> term().
call_uci(Pid, CommandSpec, Timeout) ->
    call(Pid, {uci_command, CommandSpec}, Timeout).

%% cast/2
-spec cast(pid(), term()) -> ok.
cast(Pid, Msg) ->
    gen_server:cast(Pid, Msg).

%% reply_with_timeout/2
-spec reply_with_timeout(Reply, state()) -> {reply, Reply, state(), timeout()} when Reply :: term().
reply_with_timeout(Reply, State) ->
    case get_idle_timeout(State) of
        infinity ->
            {reply, Reply, State, infinity};
        Timeout  ->
            {reply, Reply, set_idle_timestamp(State), Timeout}
    end.

%% noreply_with_timeout/1
-spec noreply_with_timeout(state()) -> {noreply, state(), timeout()}.
noreply_with_timeout(State) ->
    case get_idle_timeout(State) of
        infinity ->
            {noreply, State, infinity};
        Timeout  ->
            {noreply, set_idle_timestamp(State), Timeout}
    end.

%% get_idle_timeout/1
-spec get_idle_timeout(state()) -> timeout().
get_idle_timeout(#state{server_opts = Opts}) ->
    maps:get(idle_timeout, Opts, infinity).

%% idle_timestamp/0
-spec timestamp() -> integer().
timestamp() ->
    erlang:monotonic_time().

%% milliseconds_passed/1
-spec milliseconds_passed(integer()) -> integer().
milliseconds_passed(Timestamp) ->
    erlang:convert_time_unit(timestamp() - Timestamp, native, microsecond) div 1000.

%% get_idle_timestamp/1
-spec get_idle_timestamp(state()) -> integer().
get_idle_timestamp(#state{idle_timestamp = Time}) ->
    Time.

%% set_idle_timestamp/1
-spec set_idle_timestamp(state()) -> state().
set_idle_timestamp(State) ->
    State#state{idle_timestamp = timestamp()}.

%% update_server_opts/2
-spec update_server_opts(server_opts(), state()) -> {ok, state()} | {error, {bad_server_option, term()}} | {error, {bad_server_options_type, term()}}.
update_server_opts(Opts, State) when is_map(Opts) ->
    do_update_server_opts(maps:to_list(Opts), State);
update_server_opts(Opts, _State) ->
    {error, {bad_server_options_type, Opts}}.

%% do_update_server_opts/2
-spec do_update_server_opts([{term(), term()}], state()) -> {ok, state()} | {error, {bad_server_option, term()}}.
do_update_server_opts([], State) ->
    {ok, State};
do_update_server_opts([{OptKey, OptVal} | OtherOpts], #state{server_opts = Opts} = State) ->
    Validity = case {OptKey, OptVal} of
        {idle_timeout, Timeout} when is_integer(Timeout) andalso (Timeout > 0) ->
            ok;
        {idle_timeout, infinity} ->
            ok;
        {onterminate, {Fun, _Arg}} when is_function(Fun, 4) ->
            ok;
        {onterminate, {Fun, _Arg}} ->
            {error, {onterminate, bad_function_arity, Fun}};
        {onterminate, undefined} ->
            ok;
        _ ->
            error
    end,
    case Validity of
        ok    ->
            do_update_server_opts(OtherOpts, State#state{server_opts = Opts#{OptKey => OptVal}});
        error ->
            {error, {bad_server_option, {OptKey, OptVal}}};
        {error, Reason} ->
            {error, {bad_server_option, Reason}}
    end.

%% init_uci_game/2
-spec init_uci_game(uci_game_opts(), state()) -> {ok, state()} | {error, init_uci_game_error(), state()}.
init_uci_game(Opts, State) ->
    Steps = [fen, disconnect, connect, uci_commands],
    init_uci_game(Steps, Opts, State).

%% init_uci_game/3
-spec init_uci_game([fen|disconnect|connect|uci_commands], uci_game_opts(), state()) -> {ok, state()} | {error, init_uci_game_error(), state()}.
init_uci_game([], _Opts, State) ->
    {ok, State};
init_uci_game([fen|Tail], Opts, State) ->
    Fen = maps:get(fen, Opts, binbo_fen:initial()),
    case binbo_game:new(Fen) of
        {ok, {Game, _GameStatus}} ->
            init_uci_game(Tail, Opts, State#state{game = Game});
        {error, Reason} ->
            {error, Reason, State}
    end;
init_uci_game([disconnect|Tail], Opts, State) ->
    {ok, State2} = uci_disconnect(State),
    init_uci_game(Tail, Opts, State2);
init_uci_game([connect|Tail], Opts, State) ->
    EnginePath = maps:get(engine_path, Opts, undefined),
    case uci_connect(State, EnginePath) of
        {ok, State2} ->
            init_uci_game(Tail, Opts, State2);
        {error, Reason} ->
            {error, {uci_connection_failed, Reason}, State}
    end;
init_uci_game([uci_commands|Tail], Opts, State) ->
    #state{uci_sockinfo = Port, game = Game} = State,
    {ok, Fen} = binbo_game:get_fen(Game),
    Command = [
        "uci", $\n,
        "ucinewgame", $\n,
        "position fen ", Fen
    ],
    _ = uci_send_command(Port, Command),
    init_uci_game(Tail, Opts, State).


%% uci_connect/1
-spec uci_connect(state(), engine_path()) -> {ok, state()} | {error, any()}.
uci_connect(State, EnginePath) ->
    case binbo_uci_connection:connect(EnginePath) of
        {ok, SocketInfo} ->
            {ok, State#state{uci_sockinfo = SocketInfo}};
        {error, Reason} ->
            {error, Reason}
    end.

%% uci_disconnect/1
-spec uci_disconnect(state()) -> {ok, state()}.
uci_disconnect(#state{uci_sockinfo = SocketInfo} = State) ->
    ok = binbo_uci_connection:disconnect(SocketInfo),
    {ok, State#state{uci_sockinfo = undefined}}.

%% uci_send_command/2
-spec uci_send_command(binbo_uci_connection:socket_info(), iodata()) -> ok.
uci_send_command(SocketInfo, Command) ->
    binbo_uci_connection:send_command(SocketInfo, Command).

%% maybe_perform_uci_handler/2
-spec maybe_perform_uci_handler(uci_handler(), binary()) -> term().
maybe_perform_uci_handler(Handler, Data) ->
    case Handler of
        undefined -> undefined;
        _ -> Handler(Data)
    end.

%% uci_play_bestmove/3
-spec uci_play_bestmove(pid(), binbo_uci_protocol:bestmove_opts(), bb_game()) -> uci_play_ret().
uci_play_bestmove(Pid, BestMoveOpts, Game) ->
    case uci_bestmove(Pid, BestMoveOpts) of
        {ok, BestMove} ->
            uci_play_update(Pid, BestMove, Game);
        {error, _} = Error ->
            Error
    end.

%% uci_play_update/3
-spec uci_play_update(pid(), sq_move(), bb_game()) -> uci_play_ret().
uci_play_update(Pid, BestMove, Game0) ->
    case binbo_game:move(sq, BestMove, Game0) of
        {ok, {Game, GameStatus}} ->
            ok = cast(Pid, {update_game_state, Game}), % update game in the process state
            ok = uci_send_game_position(Pid, Game), % change the internal engine's position
            {ok, GameStatus, BestMove};
        {error, _} = Error ->
            Error
    end.

%% uci_send_game_position/2
-spec uci_send_game_position(pid(), bb_game()) -> ok.
uci_send_game_position(Pid, Game) ->
    {ok, Fen} = binbo_game:get_fen(Game),
    uci_command_cast(Pid, <<"position fen ", Fen/bits>>).

%% handle_uci_packet/2
-spec handle_uci_packet(binary(), state()) -> state().
handle_uci_packet(Data, #state{uci_wait_prefix = Prefix} = State0) when is_binary(Prefix) ->
    #state{uci_wait_prefix_size = PrefixSize, uci_wait_prefix_handler = PrefixHandler} = State0,
    NewState = case PrefixHandler(Data, Prefix, PrefixSize) of
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
    _ = maybe_perform_uci_handler(State0#state.uci_handler, Data),
    NewState;
handle_uci_packet(Data, State0) ->
    _ = maybe_perform_uci_handler(State0#state.uci_handler, Data),
    State0.

%% handle_onterminate/2
-spec handle_onterminate(Reason :: any(), state()) -> term().
handle_onterminate(Reason, #state{server_opts = #{onterminate := {Fun, Arg}}} = State) ->
    Pid = self(),
    #state{game = Game} = State,
    % Callback function may take a long time, longer than 'shutdown' value in the supervisor's child spec.
    % That's why we have to spawn the other process executing it avoiding unexpected exit.
    spawn(fun() ->
        Fun(Pid, Reason, Game, Arg)
    end);
handle_onterminate(_Reason, _State) ->
    ok.
