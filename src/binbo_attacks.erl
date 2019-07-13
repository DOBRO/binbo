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

-module(binbo_attacks).

-export([init/0]).
-export([
	pawn_attacks_bb/2,
	knight_attacks_bb/1,
	bishop_attacks_bb/2,
	rook_attacks_bb/2,
	queen_attacks_bb/2,
	king_attacks_bb/1
]).

%%%------------------------------------------------------------------------------
%%%   Includes
%%%------------------------------------------------------------------------------

-include("binbo_board.hrl").
-include("binbo_global.hrl").

%%%------------------------------------------------------------------------------
%%%   Types
%%%------------------------------------------------------------------------------

-type color() :: binbo_board:color().
-type bb() :: binbo_bb:bb().
-type sq_idx() :: binbo_board:square_index().
-type occupied() :: bb().


%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% init/0
-spec init() -> [module()].
init() ->
	% Pawn attacks
	WPmod = init_pawn_attacks(?WHITE),
	BPmod = init_pawn_attacks(?BLACK),
	% Knight attacks
	Nmod = init_knight_attacks(),
	% Bishop, Rook and, hence, Queen attacks
	{Bmod1, Bmod2} = binbo_magic:init_bishop_attacks(),
	{Rmod1, Rmod2} = binbo_magic:init_rook_attacks(),
	% King attacks
	Kmod = init_king_attacks(),
	[Bmod1, Bmod2, Rmod1, Rmod2, Nmod, Kmod, WPmod, BPmod].

%% pawn_attacks_bb/2
-spec pawn_attacks_bb(sq_idx(), color()) -> bb().
pawn_attacks_bb(FromIdx, Color) ->
	Mod = pawn_static_module(Color),
	persistent_attacks_bb(FromIdx, Mod).

%% knight_attacks_bb/1
-spec knight_attacks_bb(sq_idx()) -> bb().
knight_attacks_bb(FromIdx) ->
	persistent_attacks_bb(FromIdx, ?GLOBAL_KNIGHT_ATTACKS_MOD).

%% bishop_attacks_bb/2
-spec bishop_attacks_bb(sq_idx(), occupied()) -> bb().
bishop_attacks_bb(FromIdx, Occupied) ->
	binbo_magic:bishop_attacks_bb(FromIdx, Occupied).

%% rook_attacks_bb/2
-spec rook_attacks_bb(sq_idx(), occupied()) -> bb().
rook_attacks_bb(FromIdx, Occupied) ->
	binbo_magic:rook_attacks_bb(FromIdx, Occupied).

%% queen_attacks_bb/2
-spec queen_attacks_bb(sq_idx(), occupied()) -> bb().
queen_attacks_bb(FromIdx, Occupied) ->
	binbo_magic:queen_attacks_bb(FromIdx, Occupied).

%% king_attacks_bb/1
-spec king_attacks_bb(sq_idx()) -> bb().
king_attacks_bb(FromIdx) ->
	persistent_attacks_bb(FromIdx, ?GLOBAL_KING_ATTACKS_MOD).


%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

%% init_knight_attacks/0
-spec init_knight_attacks() -> module().
init_knight_attacks() ->
	init_kn_attacks(?KNIGHT).

%% init_king_attacks/0
-spec init_king_attacks() -> module().
init_king_attacks() ->
	init_kn_attacks(?KING).


%% init_kn_attacks/1
-spec init_kn_attacks(?KNIGHT | ?KING) -> module().
init_kn_attacks(Ptype) ->
	{AttacksKey, AttackFun} = case Ptype of
		?KING   -> {?GLOBAL_KING_ATTACKS_MOD,   fun binbo_bb:king_attacks_bb/1};
		?KNIGHT -> {?GLOBAL_KNIGHT_ATTACKS_MOD, fun binbo_bb:knight_attacks_bb/1}
	end,
	AttacksTuple = lists:foldl(fun(SqIdx, Tuple) ->
		SqBB = ?SQUARE_BB(SqIdx),
		AttacksBB = AttackFun(SqBB),
		erlang:setelement(SqIdx + 1, Tuple, AttacksBB)
	end, binbo_board:board_tuple(0), binbo_board:index_list()),
	ok = binbo_global:put(AttacksKey, AttacksTuple),
	AttacksKey.


%% init_pawn_attacks/1
-spec init_pawn_attacks(color()) -> module().
init_pawn_attacks(Color) ->
	AttacksKey = pawn_static_module(Color),
	AttacksTuple = lists:foldl(fun(SqIdx, Tuple) ->
		SqBB = ?SQUARE_BB(SqIdx),
		AttacksBB = binbo_bb:pawn_attacks_bb(Color, SqBB),
		erlang:setelement(SqIdx + 1, Tuple, AttacksBB)
	end, binbo_board:board_tuple(0), binbo_board:index_list()),
	ok = binbo_global:put(AttacksKey, AttacksTuple),
	AttacksKey.


%% pawn_static_module/1
-spec pawn_static_module(color()) -> module().
pawn_static_module(?WHITE) -> ?GLOBAL_WHITE_PAWN_ATTACKS_MOD;
pawn_static_module(?BLACK) -> ?GLOBAL_BLACK_PAWN_ATTACKS_MOD.


%% persistent_attacks_bb/2
-spec persistent_attacks_bb(sq_idx(), module()) -> bb().
persistent_attacks_bb(SqIdx, PersistentKey) ->
	erlang:element(SqIdx + 1, binbo_global:get(PersistentKey)).
