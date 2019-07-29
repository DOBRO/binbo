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

-module(binbo_pgn).

-export([parse/1]).

%%%------------------------------------------------------------------------------
%%%   Types
%%%------------------------------------------------------------------------------

% -type pgn() :: binary().

%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% parse/1
parse(<<>>) ->
	{error, empty_pgn};
parse(Pgn) when is_binary(Pgn) ->
	Steps = [delete_headers, replace_newlines, delete_comments, delete_ravs, delete_movenums, delete_nags],
	parse(Steps, Pgn);
parse(_) ->
	{error, invalid_data_type}.


%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

%% parse/2
parse([], Pgn) ->
	Pgn;
parse([Step | Tail], Pgn) ->
	Pgn2 = case Step of
		delete_headers -> delete_headers(Pgn);
		replace_newlines -> replace_newlines(Pgn);
		delete_comments -> delete_comments(Pgn);
		delete_ravs -> delete_ravs(Pgn);
		delete_movenums -> delete_movenums(Pgn);
		delete_nags -> delete_nags(Pgn)
	end,
	parse(Tail, Pgn2).


%% delete_headers/1
delete_headers(Pgn) ->
	re:replace(Pgn, <<".*\\]">>, <<>>, [{return, binary}, dotall, never_utf]).

%% replace_newlines/1
replace_newlines(Pgn) ->
	re:replace(Pgn, <<"\\R+">>, <<$\s>>, [{return, binary}, global, bsr_anycrlf, never_utf]).

%% delete_comments/1
delete_comments(Pgn) ->
	re:replace(Pgn, <<"(\\{[^}]+\\})+?">>, <<>>, [{return, binary}, global, never_utf]).


%% delete_ravs/1
%% Deletes Recursive Annotation Variations (RAVs)
%% https://chess.stackexchange.com/questions/18214/valid-pgn-variations
delete_ravs(Pgn) ->
	{ok, MP} = re:compile(<<"(\\([^\\(\\)]+\\))+?">>, [never_utf]),
	delete_ravs(Pgn, MP).

%% delete_ravs/2
delete_ravs(Pgn, MP) ->
	case re:run(Pgn, MP, [{capture, none}]) of
		match ->
			Pgn2 = re:replace(Pgn, MP, <<>>, [{return, binary}, global]),
			delete_ravs(Pgn2, MP);
		_ ->
			Pgn
	end.


%% delete_movenums/1
delete_movenums(Pgn) ->
	re:replace(Pgn, <<"\\d+\\.+">>, <<>>, [{return, binary}, global, never_utf]).

%% delete_nags/1
%% Deletes Numeric Annotation Glyphs (NAGs)
%% https://en.wikipedia.org/wiki/Numeric_Annotation_Glyphs
delete_nags(Pgn) ->
	re:replace(Pgn, <<"\\$\\d+">>, <<>>, [{return, binary}, global, never_utf]).
