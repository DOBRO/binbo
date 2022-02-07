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

-module(uci_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").
-include("binbo_test_lib.hrl").

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([uci_test/1]).


%% all/0
all() -> [uci_test].

%% init_per_suite/1
init_per_suite(Config) ->
    EnginePath = os:getenv("UCI_ENGINE_PATH"),
    case validate_engine_path(EnginePath) of
        ok ->
            {ok, _} = binbo:start(),
            [{engine_path, EnginePath} | Config];
        {error, Reason} ->
            {skip, {Reason, EnginePath}}
    end.

%% end_per_suite/1
end_per_suite(_Config) ->
    ok = binbo:stop(),
    ok.

%% uci_test/1
uci_test(Config) ->
    EnginePath = ?value(engine_path, Config),
    InitialFen = binbo_fen:initial(),

    % Start new process for the game
    {ok, Pid} = binbo:new_server(),

    % Start new game with engine (initial FEN)
    {ok, continue} = binbo:new_uci_game(Pid, #{engine_path => EnginePath}),

    % Start new game with engine (given FEN)
    {ok, continue} = binbo:new_uci_game(Pid, #{engine_path => EnginePath, fen => InitialFen}),

    % Best move
    {ok, BestMove1} = binbo:uci_bestmove(Pid),
    {ok, BestMove2} = binbo:uci_bestmove(Pid, #{}),
    {ok, BestMove3} = binbo:uci_bestmove(Pid, #{movetime => 100}),
    true = erlang:is_binary(BestMove1),
    true = erlang:is_binary(BestMove2),
    true = erlang:is_binary(BestMove3),

    % Play
    {ok, continue, EngineMove1} = binbo:uci_play(Pid, #{movetime => 100}, <<"e2e4">>),
    {ok, continue, EngineMove2} = binbo:uci_play(Pid, #{}),
    true = erlang:is_binary(EngineMove1),
    true = erlang:is_binary(EngineMove2),

    % Change position back to initial, make moves and sync
    {ok, continue} = binbo:uci_set_position(Pid, InitialFen),
    {ok, continue} = binbo:move(Pid, <<"e2e4">>),
    {ok, continue} = binbo:move(Pid, <<"e7e5">>),
    ok = binbo:uci_sync_position(Pid),

    % Set UCI mode on
    ok = binbo:uci_mode(Pid),

    % Set default message handler
    ok = binbo:set_uci_handler(Pid, default),
    % Send command
    ok = binbo:uci_command_call(Pid, "uci"),

    % Set custom message handler
    Self = erlang:self(),
    ok = binbo:set_uci_handler(Pid, fun(Msg) -> Self ! Msg end),
    ok = binbo:uci_command_cast(Pid, "uci"),
    ok = binbo:set_uci_handler(Pid, undefined),

    % Stop the game process
    ok = binbo:stop_server(Pid),
    ok.



%% validate_engine_path/1
validate_engine_path([_|_] = EnginePath) ->
    case file:read_file_info(EnginePath) of
        {ok, #file_info{type = regular}} ->
            ok;
        {ok, _} ->
            {error, not_regular_file};
        {error, Reason} ->
            {error, Reason}
    end;
validate_engine_path(_) ->
    {error, no_egine_path_provided}.
