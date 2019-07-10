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

%%%------------------------------------------------------------------------------
%%%	Inspired by mochiglobal module:
%%%	https://github.com/mochi/mochiweb/blob/master/src/mochiglobal.erl
%%%
%%% Slightly patched for performance
%%%------------------------------------------------------------------------------

-module(binbo_global).
-export([get/1, put/2, delete/1]).

%%%------------------------------------------------------------------------------
%%%   Types
%%%------------------------------------------------------------------------------

-type value() :: any().

%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% get/1
-spec get(module()) -> value().
get(Mod) ->
	Mod:value().

%% put/2
-spec put(module(), value()) -> ok.
put(Mod, V) ->
	Bin = compile(Mod, V),
	code:purge(Mod),
	Filename = atom_to_list(Mod) ++ ".erl",
	{module, Mod} = code:load_binary(Mod, Filename, Bin),
	ok.

%% delete/1
-spec delete(module()) -> boolean().
delete(Mod) ->
	code:purge(Mod),
	code:delete(Mod).


%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

%% compile/2
-spec compile(module(), value()) -> binary().
compile(Module, Value) ->
	{ok, Module, Bin} = compile:forms(forms(Module, Value),
									  [verbose, report_errors]),
	Bin.


%% forms/2
-spec forms(module(), value()) -> [erl_syntax:syntaxTree()].
forms(Module, Value) ->
	[erl_syntax:revert(X) || X <- term_to_abstract(Module, value, Value)].


%% term_to_abstract/3
-spec term_to_abstract(atom(), value, value()) -> [erl_syntax:syntaxTree()].
term_to_abstract(Module, Getter, Value) ->
	[
		%% -module(Module).
		erl_syntax:attribute(
			erl_syntax:atom(module),
			[erl_syntax:atom(Module)]
		),
		%% -export([Getter/0]).
		erl_syntax:attribute(
			erl_syntax:atom(export),
			[erl_syntax:list(
				[erl_syntax:arity_qualifier(
					erl_syntax:atom(Getter),
					erl_syntax:integer(0))
				])
			]
		),
		%% Getter() -> Value.
		erl_syntax:function(
			erl_syntax:atom(Getter),
			[erl_syntax:clause([], none, [erl_syntax:abstract(Value)])]
		)
	].
