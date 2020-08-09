%% Copyright (c) 2019-2020, Sergei Semichev <chessvegas@chessvegas.com>. All Rights Reserved.
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

-module(binbo_fen_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("binbo_test_lib.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-export([
	test_initial_fen/1,
	test_legal_string_fens/1,
	test_bad_fens/1
]).

%% all/0
all() -> [{group, fen_test_group}].

%% groups/0

groups() ->
	[{fen_test_group, [parallel], [
		test_initial_fen,
		test_legal_string_fens,
		test_bad_fens
	]}].

%% init_per_suite/1
init_per_suite(Config) ->
	ok = binbo_test_lib:all_group_testcases_exported(?MODULE),
	{ok, _} = binbo:start(),
	Config.

%% end_per_suite/1
end_per_suite(_Config) ->
	ok.

%% init_per_testcase/2
init_per_testcase(_TestCase, Config) ->
	{ok, Pid} = binbo:new_server(),
	[{pid, Pid} | Config].

%% end_per_testcase/2
end_per_testcase(_TestCase, Config) ->
	Pid = get_pid(Config),
	ok = binbo:stop_server(Pid),
	ok.


%%%------------------------------------------------------------------------------
%%%   Testcases
%%%------------------------------------------------------------------------------

%% test_initial_fen/1
test_initial_fen(Config) ->
	Pid = get_pid(Config),
	InitialFen = <<"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1">>,
	true = (InitialFen =:= binbo_fen:initial()),
	{ok,continue} = binbo:new_game(Pid, initial),
	{ok, InitialFen} = binbo:get_fen(Pid),
	ok.

%% test_legal_string_fens/1
test_legal_string_fens(Config) ->
	Pid = get_pid(Config),
	{ok,continue} = binbo:new_game(Pid, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"),
	ok.

%% test_bad_fens/1
test_bad_fens(Config) ->
	Pid = get_pid(Config),
	% Emty FEN
	{error,empty_fen} = binbo:new_game(Pid, <<>>),
	{error,empty_fen} = binbo:new_game(Pid, []),
	% Not valid binary, nor valid string
	{error,bad_data_type} = binbo:new_game(Pid, 3),
	{error,too_few_parts} = binbo:new_game(Pid, <<"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR">>),
	{error, invalid_fen_string} = binbo:new_game(Pid, [fen]),
	% Wrong active color
	{error, {invalid_active_color, <<"side">>}} = binbo:new_game(Pid, <<"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR side KQkq - 0 1">>),
	% @todo: add other possible wrong cases
	ok.



%%%------------------------------------------------------------------------------
%%%   Internal helpers
%%%------------------------------------------------------------------------------

%% get_pid/1
get_pid(Config) ->
	?value(pid, Config).
