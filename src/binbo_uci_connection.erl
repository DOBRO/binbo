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

-export([connect/1, disconnect/1]).
-export([send_command/2]).

%%%------------------------------------------------------------------------------
%%%   Types
%%%------------------------------------------------------------------------------
-type socket_info() :: {erlang, port()}.
-type engine_path() :: binary() | string().

-export_type([socket_info/0, engine_path/0]).


%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% connect/1
-spec connect(engine_path()) -> {ok, socket_info()} | {error, any()}.
connect(EnginePath) when is_list(EnginePath) orelse is_binary(EnginePath) ->
    try erlang:open_port({spawn_executable, EnginePath}, [binary, stream]) of
        Port -> {ok, {erlang, Port}}
    catch
        _:Reason -> {error, Reason}
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
send({erlang, Port}, IoData) ->
    _ = erlang:port_command(Port, IoData),
    ok.
