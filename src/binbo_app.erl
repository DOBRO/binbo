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

-module(binbo_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

%%%------------------------------------------------------------------------------
%%%   Types
%%%------------------------------------------------------------------------------
-record(app_state, {
    attack_mods = undefined :: undefined | [module()],
    hash_mods   = undefined :: undefined | [module()]
}).

-type state() :: #app_state{}.

%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% start/2
-spec start(_, _) -> {ok, pid(), state()}.
start(_Type, _Args) ->
    AttackMods = binbo_attacks:init(), % init persistent attacks first
    HashMods = binbo_hash:init(),
    State = #app_state{attack_mods = AttackMods, hash_mods = HashMods},
    {ok, Pid} = binbo_sup:start_link(),
    {ok, Pid, State}.

%% stop/1
-spec stop(state()) -> ok.
stop(#app_state{attack_mods = AttackMods, hash_mods = HashMods}) ->
    ok = unload_dynamic_mods(AttackMods),
    ok = unload_dynamic_mods(HashMods),
    ok.


%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

%% unload_dynamic_mods/1
-spec unload_dynamic_mods(undefined | [module()]) -> ok.
unload_dynamic_mods([_|_] = DynMods) ->
    lists:foreach(fun(Mod) ->
        binbo_global:delete(Mod)
    end, DynMods);
unload_dynamic_mods(_) ->
    ok.
