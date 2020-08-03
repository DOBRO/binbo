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
	test_idle_timeout/1
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
		test_idle_timeout
	]}].

%% init_per_suite/1
init_per_suite(Config) ->
	{ok, _} = binbo:start(),
	DefaultServerOpts = #{
		idle_timeout => infinity
	},
	[
		% 'extra_sleep_millis' - adds some extra (small) time to wait before checking that process is stopped.
		% Occasionally, if this value is too small, tests not passed on some CI due to stripped-down resources of containers.
		{extra_sleep_millis, 10},
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
