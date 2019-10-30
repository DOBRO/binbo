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
-export([command_spec_uci/0]).
-export([simple_prefix_handler/3]).
-export([default_logger/1]).

%%%------------------------------------------------------------------------------
%%%   Types
%%%------------------------------------------------------------------------------
-type engine_path() :: binary() | string().


-export_type([engine_path/0]).

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
%% @todo Add spec
send_command(Port, Command) ->
	erlang:port_command(Port, [Command, $\n]).

%% command_spec_uci/0
%% @todo Add spec
command_spec_uci() ->
	{<<"uci">>, <<"uciok">>, fun ?MODULE:simple_prefix_handler/3}.

%% simple_prefix_handler/3
%% @todo Add spec
simple_prefix_handler(Data, Prefix, PrefixSize) ->
	Binaries = split_data(Data),
	case prefix_match_any(Binaries, Prefix, PrefixSize) of
		match   -> reply_ok;
		nomatch -> skip
	end.

-spec default_logger(binary()) -> ok.
default_logger(Data) ->
	_ = io:format("--- UCI LOG BEGIN---~n~s---UCI LOG END---~n", [Data]),
	ok.


%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

%% @todo Add spec
prefix_match_any([], _, _) ->
	nomatch;
prefix_match_any([B|Tail], Prefix, PrefixSize) ->
	case B of
		<<Prefix:PrefixSize/binary, _/bits>> ->
			match;
		<<_/bits>> ->
			prefix_match_any(Tail, Prefix, PrefixSize)
	end.

%% @todo Add spec
split_data(Data) ->
	uef_bin:split(Data, <<"\n">>).
