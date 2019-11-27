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

-module(binbo_uci).

-export([open_port/1]).
-export([send_command/2]).
-export([command_spec_uci/0, command_spec_bestmove/1]).
-export([simple_prefix_handler/3]).
-export([bestmove_prefix_handler/3]).
-export([default_handler/1]).

%%%------------------------------------------------------------------------------
%%%   Types
%%%------------------------------------------------------------------------------
-type engine_path() :: binary() | string().
-type bestmove_opt_key() :: depth | wtime | btime | winc | binc.
-type bestmove_opt_val() :: string() | binary() | non_neg_integer().
-type bestmove_opts() :: #{
	bestmove_opt_key() => bestmove_opt_val()
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
	try erlang:open_port({spawn_executable, EnginePath}, [binary, stream, exit_status]) of
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
-spec command_spec_bestmove(bestmove_opts()) -> command_spec().
command_spec_bestmove(Opts) ->
	Command = get_bestmove_cmd(Opts),
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
	Keys = [depth, wtime, btime, winc, binc],
	get_bestmove_cmd(<<"go">>, Opts, Keys).

%% get_bestmove_cmd/3
-spec get_bestmove_cmd(iodata(), bestmove_opts(), [bestmove_opt_key()]) -> iodata().
get_bestmove_cmd(Cmd, _, []) ->
	Cmd;
get_bestmove_cmd(Cmd, Opts, [Key|Tail]) ->
	Cmd2 = case Opts of
		#{Key := Val} -> [Cmd, $\s, to_iodata(Val)];
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
