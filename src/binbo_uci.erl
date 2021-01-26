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

-module(binbo_uci).

-export([open_port/1]).
-export([send_command/2]).
-export([command_spec_uci/0, command_spec_bestmove/2]).
-export([simple_prefix_handler/3]).
-export([bestmove_prefix_handler/3]).
-export([default_handler/1]).
-export([bestmove_search_time/1]).

%%%------------------------------------------------------------------------------
%%%   Macros
%%%------------------------------------------------------------------------------

-define(DEFAULT_UCI_MOVETIME, 1000).

%%%------------------------------------------------------------------------------
%%%   Types
%%%------------------------------------------------------------------------------
-type engine_path() :: binary() | string().
-type bestmove_opt_key() :: depth | wtime | btime | winc | binc | movestogo | nodes | movetime.
-type bestmove_opts() :: #{
	depth		=> pos_integer(),		% depth <x> (search x plies only)
	wtime		=> non_neg_integer(),	% wtime <x> (white has x msec left on the clock)
	btime		=> non_neg_integer(),	% btime <x> (black has x msec left on the clock)
	winc		=> pos_integer(),		% winc <x> (white increment per move in mseconds if x > 0)
	binc		=> pos_integer(),		% binc <x> (black increment per move in mseconds if x > 0)
	movestogo	=> pos_integer(),		% movestogo <x> (there are x moves to the next time control, this will only be sent if x > 0, if you don't get this and get the wtime and btime it's sudden death)
	nodes		=> pos_integer(),		% nodes <x> (search x nodes only)
	movetime	=> pos_integer()		% movetime <x> (search exactly x mseconds)
}.
-type command_spec() :: {binary(), binary(), fun()}.

-export_type([engine_path/0]).
-export_type([command_spec/0]).
-export_type([bestmove_opts/0]).

%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% open_port/1
-spec open_port(engine_path()) -> {ok, port()} | {error, any()}.
open_port(EnginePath) ->
	try erlang:open_port({spawn_executable, EnginePath}, [binary, stream]) of
		Port -> {ok, Port}
	catch
		_:Reason -> {error, Reason}
	end.

%% send_command/2
-spec send_command(port(), iodata()) -> ok.
send_command(Port, Command) ->
	_ = erlang:port_command(Port, [Command, $\n]),
	ok.

%% command_spec_uci/0
-spec command_spec_uci() -> command_spec().
command_spec_uci() ->
	{<<"uci">>, <<"uciok">>, fun ?MODULE:simple_prefix_handler/3}.

%% command_spec_bestmove/1
-spec command_spec_bestmove(bestmove_opts(), pos_integer() | undefined) -> command_spec().
command_spec_bestmove(Opts, Movetime0) ->
	% The behaviour of searching best move changed since Stockfish 12.
	% We have to set 'movetime' option explicitly to avoid infinite search.
	Movetime = case is_integer(Movetime0) of
		true  -> Movetime0;
		false -> ?DEFAULT_UCI_MOVETIME
	end,
	Command = get_bestmove_cmd(Opts#{movetime => Movetime}),
	{Command, <<"bestmove ">>, fun ?MODULE:bestmove_prefix_handler/3}.

%% simple_prefix_handler/3
-spec simple_prefix_handler(binary(), binary(), pos_integer()) -> reply_ok | skip.
simple_prefix_handler(Data, Prefix, PrefixSize) ->
	Binaries = split_data(Data),
	case prefix_match_any(Binaries, Prefix, PrefixSize) of
		{match, _}   -> reply_ok;
		nomatch -> skip
	end.

%% simple_prefix_handler/3
-spec bestmove_prefix_handler(binary(), binary(), pos_integer()) -> {reply, binary()} | skip.
bestmove_prefix_handler(Data, Prefix, PrefixSize) ->
	Binaries = split_data(Data),
	case prefix_match_any(Binaries, Prefix, PrefixSize) of
		{match, B} -> {reply, parse_bestmove(B)};
		nomatch    -> skip
	end.

%% default_handler/1
-spec default_handler(binary()) -> ok.
default_handler(Data) ->
	_ = io:format("~n--- UCI LOG BEGIN ---~n~s--- UCI LOG END ---~n", [Data]),
	ok.

%% bestmove_search_time/2
-spec bestmove_search_time(bestmove_opts()) -> pos_integer() | undefined.
bestmove_search_time(Opts) ->
	case Opts of
		#{movetime := Movetime} when is_integer(Movetime) andalso (Movetime > 0) ->
			Movetime;
		_ ->
			undefined
	end.

%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

%% prefix_match_any/3
-spec prefix_match_any([binary()], binary(), pos_integer()) -> {match, binary()} | nomatch.
prefix_match_any([], _, _) ->
	nomatch;
prefix_match_any([B|Tail], Prefix, PrefixSize) ->
	case B of
		<<Prefix:PrefixSize/binary, _/bits>> ->
			{match, B};
		<<_/bits>> ->
			prefix_match_any(Tail, Prefix, PrefixSize)
	end.

%% split_data/1
-spec split_data(binary()) -> [binary()].
split_data(Data) ->
	uef_bin:split(Data, <<"\n">>).

%% get_bestmove_cmd/1
-spec get_bestmove_cmd(bestmove_opts()) -> iodata().
get_bestmove_cmd(Opts) ->
	Keys = [depth, wtime, btime, winc, binc, movestogo, nodes, movetime],
	get_bestmove_cmd(<<"go">>, Opts, Keys).

%% get_bestmove_cmd/3
-spec get_bestmove_cmd(iodata(), bestmove_opts(), [bestmove_opt_key()]) -> iodata().
get_bestmove_cmd(Cmd, _, []) ->
	Cmd;
get_bestmove_cmd(Cmd, Opts, [Key|Tail]) ->
	Cmd2 = case Opts of
		#{Key := Val} -> [Cmd, $\s, to_iodata(Key), $\s, to_iodata(Val)];
		_ -> Cmd
	end,
	get_bestmove_cmd(Cmd2, Opts, Tail).

%% parse_bestmove/1
-spec parse_bestmove(binary()) -> binary().
parse_bestmove(B) ->
	[_, Move|_] = uef_bin:split(B, <<$\s>>),
	Move.

%% to_iodata/1
-spec to_iodata(list() | binary() | pos_integer() | atom()) -> list() | binary().
to_iodata(Arg) when is_list(Arg) -> Arg;
to_iodata(Arg) when is_binary(Arg) -> Arg;
to_iodata(Arg) when is_integer(Arg) -> erlang:integer_to_binary(Arg);
to_iodata(Arg) when is_atom(Arg) -> erlang:atom_to_binary(Arg, latin1).
