%% Copyright (c) 2019-2022, Sergei Semichev <chessvegas@chessvegas.com>. All Rights Reserved.
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

-module(binbo_uci_connection).

-export([connect/1, connect/3, disconnect/1]).
-export([send_command/2]).

%%%------------------------------------------------------------------------------
%%%   Types
%%%------------------------------------------------------------------------------
-type tcp_host() :: inet:socket_address() | inet:hostname().
-type tcp_port() :: inet:port_number().
-type tcp_socket() :: inet:socket().

-type socket_info() :: {erlang, port()} | {gen_tcp, tcp_socket()}.

-export_type([tcp_host/0, tcp_port/0]).
-export_type([socket_info/0]).


%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% connect/1
-spec connect(binary() | string()) -> {ok, {erlang, port()}} | {error, any()}.
connect(EnginePath) when is_list(EnginePath) orelse is_binary(EnginePath) ->
    try erlang:open_port({spawn_executable, EnginePath}, [binary, stream]) of
        Port -> {ok, {erlang, Port}}
    catch
        _:Reason -> {error, Reason}
    end.

%% connect/3
-spec connect(tcp_host(), tcp_port(), timeout()) -> {ok, {gen_tcp, tcp_socket()}} | {error, timeout | inet:posix()}.
connect(Host, Port, Timeout) ->
    Opts = [binary, {active, true}, {packet, raw}],
    case gen_tcp:connect(Host, Port, Opts, Timeout) of
        {ok, Socket} ->
            {ok, {gen_tcp, Socket}};
        {error, Reason} ->
            {error, Reason}
    end.

%% disconnect/1
-spec disconnect(socket_info() | undefined) -> ok.
disconnect({erlang, Port} = SocketInfo) ->
    _ = case erlang:port_info(Port, id) of
        undefined ->
            undefined;
        _ ->
            try
                _ = send_command(SocketInfo, "quit"),
                erlang:port_close(Port)
            catch
                _:_ -> ok % we don't care
            end
    end,
    ok;
disconnect({gen_tcp, Socket} = SocketInfo) ->
    _ = send_command(SocketInfo, "quit"),
    _ = gen_tcp:close(Socket),
    ok;
disconnect(undefined) ->
    ok.

%% send_command/2
-spec send_command(socket_info(), iodata()) -> ok.
send_command(SocketInfo, Command) ->
    send(SocketInfo, [Command, $\n]).


%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

%% send/2
-spec send(socket_info(), iodata()) -> ok.
send(SocketInfo, IoData) ->
    _ = case SocketInfo of
        {erlang, Port} ->
            erlang:port_command(Port, IoData);
        {gen_tcp, Socket} ->
            gen_tcp:send(Socket, IoData)
    end,
    ok.
