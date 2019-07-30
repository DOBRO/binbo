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

-export([get_moves/1]).

%%%------------------------------------------------------------------------------
%%%   Types
%%%------------------------------------------------------------------------------

% -type pgn() :: binary().

%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% get_moves/1
get_moves(<<>>) ->
	{error, empty_pgn};
get_moves(Pgn) when is_binary(Pgn) ->
	Steps = [delete_headers, replace_newlines, delete_comments, delete_ravs, delete_movenums, delete_nags],
	get_moves(Steps, Pgn);
get_moves(_) ->
	{error, invalid_data_type}.


%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

%% get_moves/2
get_moves([], Pgn) ->
	uef_bin:split(Pgn, <<$\s>>, 'trim_all');
get_moves([Step | Tail], Pgn) ->
	Pgn2 = case Step of
		delete_headers   -> delete_headers(Pgn);
		replace_newlines -> replace_newlines(Pgn);
		delete_comments  -> delete_comments(Pgn);
		delete_ravs      -> delete_ravs(Pgn);
		delete_movenums  -> delete_movenums(Pgn);
		delete_nags      -> delete_nags(Pgn)
	end,
	get_moves(Tail, Pgn2).


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
	% This PCRE pattern solves the nested parentheses problem.
	% As a reference see 'Recursive Patterns' section in OTP docs for 're' module:
	% http://erlang.org/doc/man/re.html#recursive-patterns
	re:replace(Pgn, <<"\\(([^()]++|(?R))*\\)">>, <<>>, [{return, binary}, global, never_utf]).

%% delete_movenums/1
delete_movenums(Pgn) ->
	re:replace(Pgn, <<"\\d+\\.+">>, <<>>, [{return, binary}, global, never_utf]).

%% delete_nags/1
%% Deletes Numeric Annotation Glyphs (NAGs)
%% https://en.wikipedia.org/wiki/Numeric_Annotation_Glyphs
delete_nags(Pgn) ->
	re:replace(Pgn, <<"\\$\\d+">>, <<>>, [{return, binary}, global, never_utf]).
