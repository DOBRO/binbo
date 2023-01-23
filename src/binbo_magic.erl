%% Copyright (c) 2019-2023, Sergei Semichev <chessvegas@chessvegas.com>. All Rights Reserved.
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
    #{MagicIndex := BB} = Attacks,
    BB.


%% rook_attacks_bb/2
-spec rook_attacks_bb(sq_idx(), bb()) -> bb().
rook_attacks_bb(SqIdx, Occupied) ->
    TupleIndex = SqIdx + 1,
    Mask = erlang:element(TupleIndex, binbo_global:get(?GLOBAL_MAGIC_ROOK_MASK_MOD)),
    MagicIndex = magic_rook_index(SqIdx, Occupied, Mask),
    Attacks = erlang:element(TupleIndex, binbo_global:get(?GLOBAL_MAGIC_ROOK_ATTACKS_MOD)),
    #{MagicIndex := BB} = Attacks,
    BB.


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
