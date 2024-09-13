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

-module(binbo_bb_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("binbo_test_lib.hrl").
-include("binbo_board.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([to_index_test/1, to_index_list_test/1]).

%% all/0
all() -> [{group, all_tests}].

groups() -> [{all_tests, [parallel], [
    to_index_test, to_index_list_test
]}].

%% init_per_suite/1
init_per_suite(Config) ->
    ok = binbo_test_lib:all_group_testcases_exported(?MODULE),
    Config.

%% end_per_suite/1
end_per_suite(_Config) ->
    ok.

%% to_index_test/1
to_index_test(_Config) ->
    List = binbo_board:index_list(),
    lists:foreach(fun(Idx) ->
        SqBB = ?SQUARE_BB(Idx),
        true = (binbo_bb:to_index(SqBB) =:= exact_sqbb_to_idx(SqBB))
    end, List).

%% to_index_list_test/1
to_index_list_test(_Config) ->
    BoardIndexList = binbo_board:index_list(),
    List_64_1 = binbo_bb:to_index_list(?ALL_SQUARES_BB),
    64 = erlang:length(List_64_1),
    true = (lists:reverse(List_64_1) =:= BoardIndexList),
    ok = lists:foreach(fun(Idx) ->
        SqBB = ?SQUARE_BB(Idx),
        [Idx] = binbo_bb:to_index_list(SqBB)
    end, BoardIndexList),
    [] = binbo_bb:to_index_list(?EMPTY_BB),
    ok.


%% exact_sqbb_to_idx/1
exact_sqbb_to_idx(SqBB) ->
    case SqBB of
        1 -> 0;
        2 -> 1;
        4 -> 2;
        8 -> 3;
        16 -> 4;
        32 -> 5;
        64 -> 6;
        128 -> 7;
        256 -> 8;
        512 -> 9;
        1024 -> 10;
        2048 -> 11;
        4096 -> 12;
        8192 -> 13;
        16384 -> 14;
        32768 -> 15;
        65536 -> 16;
        131072 -> 17;
        262144 -> 18;
        524288 -> 19;
        1048576 -> 20;
        2097152 -> 21;
        4194304 -> 22;
        8388608 -> 23;
        16777216 -> 24;
        33554432 -> 25;
        67108864 -> 26;
        134217728 -> 27;
        268435456 -> 28;
        536870912 -> 29;
        1073741824 -> 30;
        2147483648 -> 31;
        4294967296 -> 32;
        8589934592 -> 33;
        17179869184 -> 34;
        34359738368 -> 35;
        68719476736 -> 36;
        137438953472 -> 37;
        274877906944 -> 38;
        549755813888 -> 39;
        1099511627776 -> 40;
        2199023255552 -> 41;
        4398046511104 -> 42;
        8796093022208 -> 43;
        17592186044416 -> 44;
        35184372088832 -> 45;
        70368744177664 -> 46;
        140737488355328 -> 47;
        281474976710656 -> 48;
        562949953421312 -> 49;
        1125899906842624 -> 50;
        2251799813685248 -> 51;
        4503599627370496 -> 52;
        9007199254740992 -> 53;
        18014398509481984 -> 54;
        36028797018963968 -> 55;
        72057594037927936 -> 56;
        144115188075855872 -> 57;
        288230376151711744 -> 58;
        576460752303423488 -> 59;
        1152921504606846976 -> 60;
        2305843009213693952 -> 61;
        4611686018427387904 -> 62;
        9223372036854775808 -> 63
    end.
