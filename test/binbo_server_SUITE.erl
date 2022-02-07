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

-module(binbo_server_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("binbo_test_lib.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-export([
	start_server_without_options/1,
	start_server_with_empty_options/1,
	test_bad_server_options/1,
	start_server_with_idle_timeout/1,
	get_set_server_options/1,
	test_idle_timeout/1,
	onterminate_stop_server_undefined_game/1,
	onterminate_stop_server_initial_game/1,
	onterminate_idle_timeout_initial_game/1,
	onterminate_reset/1,
	onterminate_bad_fun/1
]).

%% all/0
all() -> [{group, game_servers}].

%% groups/0

groups() ->
	[{game_servers, [parallel], [
		start_server_without_options,
		start_server_with_empty_options,
		test_bad_server_options,
		start_server_with_idle_timeout,
		get_set_server_options,
		test_idle_timeout,
		onterminate_stop_server_undefined_game,
		onterminate_stop_server_initial_game,
		onterminate_idle_timeout_initial_game,
		onterminate_reset,
		onterminate_bad_fun
	]}].

%% init_per_suite/1
init_per_suite(Config) ->
	ok = binbo_test_lib:all_group_testcases_exported(?MODULE),
	{ok, _} = binbo:start(),
	DefaultServerOpts = #{
		idle_timeout => infinity
	},
	[
		% 'extra_sleep_millis' - adds some extra (small) time to wait before checking that process is stopped.
		% Occasionally, if this value is too small, tests not passed on some CI due to stripped-down resources of containers.
		% @todo: try to find a workaround
		{extra_sleep_millis, 100},
		{default_server_opts, DefaultServerOpts}
		| Config
	].


%% end_per_suite/1
end_per_suite(_Config) ->
	ok.

%% init_per_testcase/2
init_per_testcase(_TestCase, Config) ->
	Config.

%% end_per_testcase/2
end_per_testcase(_TestCase, _Config) ->
	ok.


%%%------------------------------------------------------------------------------
%%%   Testcases
%%%------------------------------------------------------------------------------

%% start_server_without_options/1
start_server_without_options(_Config) ->
	{ok, Pid} = binbo:new_server(),
	{error, {bad_game, undefined}} = binbo:print_board(Pid),
	{ok, Opts} = binbo:get_server_options(Pid),
	true = erlang:is_map(Opts),
	0 = maps:size(Opts),
	ok = binbo:stop_server(Pid),
	ok.

%% start_server_with_empty_options/1
start_server_with_empty_options(_Config) ->
	{ok, Pid} = binbo:new_server(#{}),
	{ok, Opts} = binbo:get_server_options(Pid),
	true = erlang:is_map(Opts),
	0 = maps:size(Opts),
	ok = binbo:stop_server(Pid),
	ok.

%% test_bad_server_options/1
test_bad_server_options(_Config) ->
	% start with bad options
	{error, {bad_server_options_type, _}} = binbo:new_server(0),
	{error, {bad_server_options_type, _}} = binbo:new_server(1.1),
	{error, {bad_server_options_type, _}} = binbo:new_server(ok),
	{error, {bad_server_options_type, _}} = binbo:new_server([]),
	{error, {bad_server_options_type, _}} = binbo:new_server({a, 1}),
	{error, {bad_server_options_type, _}} = binbo:new_server(<<"">>),
	{error, {bad_server_option, {unknown_option, value}}} = binbo:new_server(#{unknown_option => value}),
	% set bad options
	{ok, Pid} = binbo:new_server(),
	{error, {bad_server_options_type, _}} = binbo:set_server_options(Pid, 0),
	{error, {bad_server_options_type, _}} = binbo:set_server_options(Pid, 1.1),
	{error, {bad_server_options_type, _}} = binbo:set_server_options(Pid, ok),
	{error, {bad_server_options_type, _}} = binbo:set_server_options(Pid, []),
	{error, {bad_server_options_type, _}} = binbo:set_server_options(Pid, {a, 1}),
	{error, {bad_server_options_type, _}} = binbo:set_server_options(Pid, <<"">>),
	{error, {bad_server_option, {unknown_option, value}}} = binbo:set_server_options(Pid, #{unknown_option => value}),
	ok.

%% start_server_with_idle_timeout/1
start_server_with_idle_timeout(Config) ->
	IdleTimeout = 500,
	{ok, Pid} = binbo:new_server(#{idle_timeout => IdleTimeout}),
	timer:sleep(IdleTimeout + ?value(extra_sleep_millis, Config)),
	false = erlang:is_process_alive(Pid),
	ok.

%% get_set_server_options/1
get_set_server_options(Config) ->
	Opts1 = ?value(default_server_opts, Config),
	{ok, Pid} = binbo:new_server(Opts1),

	% Get options
	{ok, Opts2} = binbo:get_server_options(Pid),
	true = lists:all(fun(Key) ->
		maps:get(Key, Opts1) =:= maps:get(Key, Opts2)
	end, maps:keys(Opts1)),

	% Set/get options
	Opts3 = #{
		idle_timeout => 9999
	},
	ok = binbo:set_server_options(Pid, Opts3),
	{ok, Opts4} = binbo:get_server_options(Pid),
	true = lists:all(fun(Key) ->
		maps:get(Key, Opts3) =:= maps:get(Key, Opts4)
	end, maps:keys(Opts3)),

	ok = binbo:stop_server(Pid),
	ok.

%% test_idle_timeout/1
test_idle_timeout(Config) ->
	NewIdleTimeout = 300,
	% integer idle_timeout
	{ok, Pid} = binbo:new_server(),
	ok = binbo:set_server_options(Pid, #{
		idle_timeout => NewIdleTimeout
	}),
	timer:sleep(NewIdleTimeout div 2),
	true = erlang:is_process_alive(Pid),
	Pid ! timeout,
	timer:sleep(NewIdleTimeout div 2),
	true = erlang:is_process_alive(Pid), % still alive!
	Pid ! test_message,
	timer:sleep(NewIdleTimeout + ?value(extra_sleep_millis, Config)),
	false = erlang:is_process_alive(Pid), % should be dead here

	% infinity idle_timeout
	{ok, Pid2} = binbo:new_server(),
	ok = binbo:set_server_options(Pid2, #{
		idle_timeout => NewIdleTimeout
	}),
	timer:sleep(NewIdleTimeout div 2),
	true = erlang:is_process_alive(Pid2), % still alive!
	ok = binbo:set_server_options(Pid2, #{
		idle_timeout => infinity
	}),
	timer:sleep(NewIdleTimeout * 2),
	true = erlang:is_process_alive(Pid2), % still alive!
	ok = binbo:stop_server(Pid2),
	ok.


%% onterminate_stop_server_undefined_game/1
onterminate_stop_server_undefined_game(_Config) ->
	Parent = self(),
	Ref = erlang:make_ref(),
	{ok, GamePid} = binbo:new_server(#{
		onterminate => {fun onterminate_callback/4, {Parent, Ref}}
	}),
	ok = binbo:stop_server(GamePid), % stop server
	ok = receive
		{GamePid, Reason, Game, {Parent, Ref}} ->
			normal = Reason,
			undefined = Game,
			ok
	after
		5000 ->
			{error, message_not_received}
	end,
	ok.

%% onterminate_stop_server_initial_game/1
onterminate_stop_server_initial_game(_Config) ->
	Parent = self(),
	Ref = erlang:make_ref(),
	{ok, GamePid} = binbo:new_server(#{
		onterminate => {fun onterminate_callback/4, {Parent, Ref}}
	}),
	{ok, continue} = binbo:new_game(GamePid),
	{ok, Fen0} = binbo:get_fen(GamePid),
	ok = binbo:stop_server(GamePid), % stop server
	ok = receive
		{GamePid, Reason, Game, {Parent, Ref}} ->
			normal = Reason,
			true = erlang:is_map(Game),
			{ok, Fen} = binbo_game:get_fen(Game),
			true = (Fen0 =:= Fen),
			ok
	after
		5000 ->
			{error, message_not_received}
	end,
	ok.

%% onterminate_idle_timeout_initial_game/1
onterminate_idle_timeout_initial_game(Config) ->
	Parent = self(),
	Ref = erlang:make_ref(),
	IdleTimeout = 300,
	{ok, GamePid} = binbo:new_server(#{
		idle_timeout => IdleTimeout,
		onterminate  => {fun onterminate_callback/4, {Parent, Ref}}
	}),
	{ok, continue} = binbo:new_game(GamePid),
	timer:sleep(IdleTimeout + ?value(extra_sleep_millis, Config)),
	false = erlang:is_process_alive(GamePid),
	ok = receive
		{GamePid, Reason, _Game, {Parent, Ref}} ->
			{shutdown, {idle_timeout_reached, IdleTimeout}} = Reason,
			ok
	after
		5000 ->
			{error, message_not_received}
	end,
	ok.

%% onterminate_reset/1
onterminate_reset(_Config) ->
	Parent = self(),
	Ref = erlang:make_ref(),
	{ok, GamePid} = binbo:new_server(#{
		onterminate => {fun onterminate_callback/4, {Parent, Ref}}
	}),
	{ok, continue} = binbo:new_game(GamePid),
	ok = binbo:set_server_options(GamePid, #{onterminate => undefined}),
	ok = binbo:stop_server(GamePid), % stop server
	ok = receive
		{GamePid, _Reason, _Game, {Parent, Ref}} ->
			{error, message_should_not_be_received}
	after
		2000 ->
			ok
	end,
	ok.

%% onterminate_bad_fun/1
onterminate_bad_fun(_Config) ->
	{error, _} = binbo:new_server(#{onterminate => not_fun}),
	{ok, GamePid} = binbo:new_server(),
	{error, _} = binbo:set_server_options(GamePid, #{
		onterminate => not_fun
	}),
	ok = binbo:stop_server(GamePid),
	ok.


%%%------------------------------------------------------------------------------
%%%   Internal helpers
%%%------------------------------------------------------------------------------

%% onterminate_callback/4
onterminate_callback(GamePid, Reason, Game, {Parent, Ref}) ->
	Parent ! {GamePid, Reason, Game, {Parent, Ref}},
	ok.
