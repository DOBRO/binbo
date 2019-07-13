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
%%%   Technique
%%%------------------------------------------------------------------------------
%%
%% Binbo uses the Magic Bitboards approach for a blazing fast move generation of sliding pieces
%% (rook, bishop, and queen). As a reference see https://www.chessprogramming.org/Magic_Bitboards
%% Good explanations can also be found here:
%% https://stackoverflow.com/questions/16925204/sliding-move-generation-using-magic-bitboard/30862064#30862064
%% and here:
%% http://vicki-chess.blogspot.com/2013/04/magics.html
%%
%% The main problem is to find the index which is then used to lookup legal moves
%% of sliding pieces in a preinitialized move database.
%% The formula for the index is:
%%  in C/C++:
%% ------------------------------------------------------------------
%%    magic_index = ((occupied & mask) * magic_number) >> shift);
%% ------------------------------------------------------------------
%%  in Erlang:
%% ------------------------------------------------------------------
%%    MagicIndex = (((Occupied band Mask) * MagicNumber) bsr Shift).
%% ------------------------------------------------------------------
%% where:
%% - Occupied is the bitboard of all pieces.
%% - Mask is the attack mask of a piece for a given square.
%% - MagicNumber is the magic number, see https://www.chessprogramming.org/Looking_for_Magics
%% - Shift = (64 - Bits), where Bits is the number of bits corresponding to attack mask of a given square.
%%
%% All values for magic numbers and shifts are precalculated before and stored in 'binbo_magic.hrl'.
%%
%% To be accurate, Binbo uses Fancy Magic Bitboards (https://www.chessprogramming.org/Magic_Bitboards#Fancy)
%% It means that all moves are stored in a table of its own (individual) size for each square.
%% In C/C++ such tables are actually two-dimensional arrays and any move can be accessed by
%% a simple lookup: move = global_move_table[square][magic_index].
%%
%% If detailed:
%% ------------------------------------------------------------------
%%    moves_from = global_move_table[square];
%%    move = moves_from[magic_index];
%% ------------------------------------------------------------------
%% The size of 'moves_from' table depends on piece and square where it is placed on.
%% For example,
%% - for rook on 'A1' the size of 'moves_from' is 4096 (2^12 = 4096, 12 bits requred for the attack mask),
%% - for bishop on 'A1' it is 64 (2^6 = 64, 6 bits requred for the attack mask).
%%
%% There are no two-dimensional arrays in Erlang, and no global variables which could help us
%% to get the fast access to the move tables from everywhere.
%%
%% So, how does Binbo beat this? Well, it's simple :).
%%
%% Erlang gives us the power of tuples and maps with their blazing fast lookup of elements/values by their index/key.
%% Since the number of squares on the chessboard is the constant value (it's always 64, right?),
%% our 'global_move_table' can be constructed as a tuple of 64 elements, and each element of this tuple
%% is a map containing the key-value association as 'MagicIndex => Moves'.
%%
%% If detailed, for moves:
%% ------------------------------------------------------------------
%%    GlobalMovesTable = { MoveMap1, ..., MoveMap64 },
%%  where:
%%    MoveMap1  = #{
%%      MagicIndex_1_1 => Moves_1_1,
%%      ...
%%      MagicIndex_1_K => Moves_1_K
%%    },
%%    MoveMap64 = #{
%%      MagicIndex_64_1 => Moves_64_1, ...
%%      ...
%%      MagicIndex_64_N => Moves_64_N
%%    },
%% ------------------------------------------------------------------
%%
%%  and then we lookup legal moves from a square, say, 'E4' (29th element of the tuple):
%% ------------------------------------------------------------------
%%    E4 = 29,
%%    MoveMapE4   = erlang:element(E4, GlobalMovesTable),
%%    MovesFromE4 = maps:get(MagicIndex, MovesMapE4).
%% ------------------------------------------------------------------
%%
%% To calculate magic index we also need the attack mask for a given square.
%% Every attack mask generated is stored in a tuple of 64 elements:
%% ------------------------------------------------------------------
%%    GlobalMaskTable = {Mask1, Mask2, ..., Mask64},
%%  where:
%%    Mask1, Mask2, ..., Mask64 are bitboards (integers)
%% ------------------------------------------------------------------
%%
%% Now, if we need to get all moves from 'E4':
%% ------------------------------------------------------------------
%%    E4 = 29,
%%    Mask = erlang:element(E4, GlobalMaskTable),
%%    MagicIndex = ((Occupied band Mask) * MagicNumber) bsr Shift,
%%    MoveMapE4   = erlang:element(E4, GlobalMovesTable),
%%    MovesFromE4 = maps:get(MagicIndex, MovesMapE4).
%% ------------------------------------------------------------------
%%
%% Next, no global variables? We make them global!
%% How do we get the fastest access to the move tables and to the atack masks from everywhere?
%% ETS? No! Using ETS as a storage for static terms we get the overhead due to extra copying
%% of data during lookup.
%%
%% And now we are coming to the fastest solution.
%% When Binbo starts up, all move tables are initialized.
%% Once these tables (tuples, actually) initialized, they are injected into dynamically generated
%% modules compiled at Binbo start. Then, to get the values, we just call a getter function
%% (binbo_global:get/1) with the argument as the name of the corresponding dynamic module.
%%
%% This awesome trick is used in MochiWeb library, see module 'mochiglobal':
%% https://github.com/mochi/mochiweb/blob/master/src/mochiglobal.erl
%%
%% Since OTP 21.2 there is module 'persistent_term':
%% http://erlang.org/doc/man/persistent_term.html
%%
%% Using 'persistent_term' for storing static data is also a good idea.
%% But it doesn't seem to be a better way for the following reason with respect to dynamic modules.
%% When Binbo stops, it gets them unloaded as they are not necessary anymore.
%% It should do the similar things for 'persistent_term' data, say, delete all unused terms to free memory.
%% In this case we run into the issue regarding scanning the heaps in all processes.
%% So, using 'global' dynamic modules with large static data seems to be more reasonable.
%%
%% That's it! Thanks for reading :)
%%
%%%------------------------------------------------------------------------------

-module(binbo_magic).

-export([init_bishop_attacks/0, init_rook_attacks/0]).
-export([bishop_attacks_bb/2, rook_attacks_bb/2, queen_attacks_bb/2]).

%%%------------------------------------------------------------------------------
%%%   Includes
%%%------------------------------------------------------------------------------

-include("binbo_global.hrl").
-include("binbo_magic.hrl").

%%%------------------------------------------------------------------------------
%%%   Types
%%%------------------------------------------------------------------------------

-type bb() :: binbo_bb:bb().
-type sq_idx() :: binbo_board:square_index().
-type magic_number() :: pos_integer().
-type magic_shift() :: pos_integer().
-type magic_index() :: non_neg_integer().
-type attack_map() :: #{magic_index() => bb()}.
-type attack_fun() :: fun().
-type magic_index_fun() :: fun().


%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% bishop_attacks_bb/2
-spec bishop_attacks_bb(sq_idx(), bb()) -> bb().
bishop_attacks_bb(SqIdx, Occupied) ->
	TupleIndex = SqIdx + 1,
	Mask = erlang:element(TupleIndex, binbo_global:get(?GLOBAL_MAGIC_BISHOP_MASK_MOD)),
	MagicIndex = magic_bishop_index(SqIdx, Occupied, Mask),
	Attacks = erlang:element(TupleIndex, binbo_global:get(?GLOBAL_MAGIC_BISHOP_ATTACKS_MOD)),
	maps:get(MagicIndex, Attacks).


%% rook_attacks_bb/2
-spec rook_attacks_bb(sq_idx(), bb()) -> bb().
rook_attacks_bb(SqIdx, Occupied) ->
	TupleIndex = SqIdx + 1,
	Mask = erlang:element(TupleIndex, binbo_global:get(?GLOBAL_MAGIC_ROOK_MASK_MOD)),
	MagicIndex = magic_rook_index(SqIdx, Occupied, Mask),
	Attacks = erlang:element(TupleIndex, binbo_global:get(?GLOBAL_MAGIC_ROOK_ATTACKS_MOD)),
	maps:get(MagicIndex, Attacks).


%% queen_attacks_bb/2
-spec queen_attacks_bb(sq_idx(), bb()) -> bb().
queen_attacks_bb(SqIdx, Occupied) ->
	bishop_attacks_bb(SqIdx, Occupied)
	bor
	rook_attacks_bb(SqIdx, Occupied).


%% init_bishop_attacks/0
-spec init_bishop_attacks() -> {module(), module()}.
init_bishop_attacks() ->
	init_br_attacks(bishop).

%% init_rook_attacks/0
-spec init_rook_attacks() -> {module(), module()}.
init_rook_attacks() ->
	init_br_attacks(rook).

%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------


%% init_br_attacks/1
-spec init_br_attacks(bishop | rook) -> {module(), module()}.
init_br_attacks(Ptype) ->
	{MaskKey, AttacksKey, AttackFun, MagicIndexFun} = case Ptype of
		bishop -> {
			?GLOBAL_MAGIC_BISHOP_MASK_MOD,
			?GLOBAL_MAGIC_BISHOP_ATTACKS_MOD,
			fun binbo_bb:bishop_attacks_bb/2,
			fun magic_bishop_index/3
		};
		rook   -> {
			?GLOBAL_MAGIC_ROOK_MASK_MOD,
			?GLOBAL_MAGIC_ROOK_ATTACKS_MOD,
			fun binbo_bb:rook_attacks_bb/2,
			fun magic_rook_index/3
		}
	end,
	AttackMask0 = gen_attack_mask(AttackFun),
	Attacks0 = gen_attacks(AttackMask0, AttackFun, MagicIndexFun),
	ok = binbo_global:put(MaskKey, AttackMask0),
	ok = binbo_global:put(AttacksKey, Attacks0),
	{MaskKey, AttacksKey}.


%% gen_attack_mask/1
-spec gen_attack_mask(attack_fun()) -> {bb()}.
gen_attack_mask(AttackFun) ->
	EmptyBB = binbo_bb:empty_bb(),
	EmptyTuple = binbo_board:board_tuple(0),
	lists:foldl(fun(SqIdx, Tuple) ->
		EdgesBB = binbo_bb:edges_bb(SqIdx),
		Mask = binbo_bb:bb_not(AttackFun(SqIdx, EmptyBB), EdgesBB),
		erlang:setelement(SqIdx + 1, Tuple, Mask)
	end, EmptyTuple, binbo_board:index_list()).


%% gen_attacks/3
-spec gen_attacks({bb()}, attack_fun(), magic_index_fun()) -> {attack_map()}.
gen_attacks(AttackMask0, AttackFun, MagicIndexFun) ->
	EmptyBB = binbo_bb:empty_bb(),
	EmptyTuple = binbo_board:board_tuple(0),
	lists:foldl(fun(SqIdx, AttackTuple) ->
		TupleIndex = SqIdx + 1,
		Mask = erlang:element(TupleIndex, AttackMask0),
		AttackMap = attack_map(SqIdx, AttackFun, MagicIndexFun, Mask, EmptyBB, #{}),
		erlang:setelement(TupleIndex, AttackTuple, AttackMap)
	end, EmptyTuple, binbo_board:index_list()).


%% attack_map/6
-spec attack_map(sq_idx(), attack_fun(), magic_index_fun(), bb(), bb(), attack_map()) -> attack_map().
attack_map(SqIdx, AttackFun, MagicIndexFun, Mask, Occupied, AttackMap) ->
	AttacksBB = AttackFun(SqIdx, Occupied),
	MagicIndex = MagicIndexFun(SqIdx, Occupied, Mask),
	AttackMap2 = AttackMap#{MagicIndex => AttacksBB},
	Occupied2 = (Occupied - Mask) band Mask, % next occupancy
	case (Occupied2 > 0) of
		true  -> attack_map(SqIdx, AttackFun, MagicIndexFun, Mask, Occupied2, AttackMap2);
		false -> AttackMap2
	end.


%% magic_index/3
-spec magic_index(bb(), bb(), magic_number(), magic_shift()) -> magic_index().
magic_index(Occupied, Mask, MagicNumber, Shift) ->
	(((Occupied band Mask) * MagicNumber) bsr Shift).


%% magic_bishop_number/1
-spec magic_bishop_number(sq_idx()) -> magic_number().
magic_bishop_number(SqIdx) ->
	erlang:element(SqIdx + 1, ?MAGIC_BISHOP_NUMBER).

%% magic_bishop_shift/1
-spec magic_bishop_shift(sq_idx()) -> magic_shift().
magic_bishop_shift(SqIdx) ->
	erlang:element(SqIdx + 1, ?MAGIC_BISHOP_SHIFT).

%% magic_rook_number/1
-spec magic_rook_number(sq_idx()) -> magic_number().
magic_rook_number(SqIdx) ->
	erlang:element(SqIdx + 1, ?MAGIC_ROOK_NUMBER).

%% magic_rook_shift/1
-spec magic_rook_shift(sq_idx()) -> magic_shift().
magic_rook_shift(SqIdx) ->
	erlang:element(SqIdx + 1, ?MAGIC_ROOK_SHIFT).

%% magic_bishop_index/3
-spec magic_bishop_index(sq_idx(), bb(), bb()) -> magic_index().
magic_bishop_index(SqIdx, Occupied, Mask) ->
	magic_index(Occupied, Mask, magic_bishop_number(SqIdx), magic_bishop_shift(SqIdx)).

%% magic_rook_index/3
-spec magic_rook_index(sq_idx(), bb(), bb()) -> magic_index().
magic_rook_index(SqIdx, Occupied, Mask) ->
	magic_index(Occupied, Mask, magic_rook_number(SqIdx), magic_rook_shift(SqIdx)).
