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

-module(binbo_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([start_child/1]).

-define(SUPERVISOR, ?MODULE).

%% start_link/0
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% init/1
-spec init(Args :: term()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(_Args) ->
    SupFlags = #{
        strategy    => simple_one_for_one,
        intensity   => 0,
        period      => 1
    },
    ChildSpec = #{
        id => binbo_server,
        start => {binbo_server, start_link, []},
        restart => temporary,
        shutdown => 2000,
        type => worker,
        modules => [binbo_server]
    },
    {ok,{SupFlags, [ChildSpec]}}.


%% start_child/1
-spec start_child(Opts :: binbo_server:server_opts()) -> {ok, pid()} | {error, term()}.
start_child(Opts) ->
    supervisor:start_child(?SUPERVISOR, [Opts]).
