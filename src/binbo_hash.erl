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

-module(binbo_hash).

-export([init/0, init/1]).
-export([piece_hash/2, enpa_hash/1, side_hash/1, castling_hash/1]).

%%%------------------------------------------------------------------------------
%%%   Includes
%%%------------------------------------------------------------------------------

-include("binbo_global.hrl").

-define(MAX_RANDOM_NUMBER, 9223372036854775808). % (1 bsl 63)

%%%------------------------------------------------------------------------------
%%%   Types
%%%------------------------------------------------------------------------------

-type max_random() :: pos_integer().
-type hash() :: pos_integer().
-type piece() :: binbo_board:piece().
-type color() :: binbo_board:color().
-type sq_idx() :: binbo_board:square_index().
-type file() :: binbo_board:file().
-type castling() :: binbo_position:castling().
-type piece_hash_tuple() :: {
	hash(), hash(), hash(), hash(), hash(), hash(), hash(), hash(),
	hash(), hash(), hash(), hash(), hash(), hash(), hash(), hash(),
	hash(), hash(), hash(), hash(), hash(), hash(), hash(), hash(),
	hash(), hash(), hash(), hash(), hash(), hash(), hash(), hash(),
	hash(), hash(), hash(), hash(), hash(), hash(), hash(), hash(),
	hash(), hash(), hash(), hash(), hash(), hash(), hash(), hash(),
	hash(), hash(), hash(), hash(), hash(), hash(), hash(), hash(),
	hash(), hash(), hash(), hash(), hash(), hash(), hash(), hash()
}.
-type pieces_hash_map() :: #{
	piece() => piece_hash_tuple()
}.
-type enpassant_hash_map() :: #{
	file() => hash()
}.
-type side_hash_map() :: #{
	color() => hash()
}.
-type castling_hash_map() :: #{
	castling() => hash()
}.

-export_type([hash/0]).

%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% init/0
-spec init() -> [module()].
init() ->
	init(?MAX_RANDOM_NUMBER).

%% init/1
-spec init(max_random()) -> [module()].
init(MaxRandom) ->
	_ = crypto:rand_seed(),
	PiecesMod = init_pieces_hash_map(MaxRandom),
	EnpaMod = init_enpassant_hash_map(MaxRandom),
	SideMod = init_side_hash_map(MaxRandom),
	CastlingMod = init_castling_hash_map(MaxRandom),
	% Here we check random numbers whether they are unique or not.
	% If not, it fails with exception.
	_ = check_randoms(),
	[PiecesMod, EnpaMod, SideMod, CastlingMod].

%% piece_hash/2
-spec piece_hash(piece(), sq_idx()) -> hash().
piece_hash(Piece, SqIdx) ->
	Map = binbo_global:get(?GLOBAL_HASH_PIECE_MOD),
	Tuple = maps:get(Piece, Map),
	erlang:element(SqIdx + 1, Tuple).

%% enpa_hash/1
-spec enpa_hash(file()) -> hash().
enpa_hash(File) ->
	Map = binbo_global:get(?GLOBAL_HASH_ENPASSANT_MOD),
	maps:get(File, Map).

%% side_hash/1
-spec side_hash(color()) -> hash().
side_hash(Side) ->
	Map = binbo_global:get(?GLOBAL_HASH_SIDE_MOD),
	maps:get(Side, Map).

%% castling_hash/1
-spec castling_hash(castling()) -> hash().
castling_hash(Castling) ->
	Map = binbo_global:get(?GLOBAL_HASH_CASTLING_MOD),
	maps:get(Castling, Map).

%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

%% random/1
-spec random(max_random()) -> hash().
random(MaxRandom) ->
	rand:uniform(MaxRandom).

%% init_pieces_hash_map/1
-spec init_pieces_hash_map(max_random()) -> ?GLOBAL_HASH_PIECE_MOD.
init_pieces_hash_map(MaxRandom) ->
	Mod = ?GLOBAL_HASH_PIECE_MOD,
	PiecesHashMap = pieces_hash_map(MaxRandom),
	ok = binbo_global:put(Mod, PiecesHashMap),
	Mod.

%% pieces_hash_map/1
-spec pieces_hash_map(max_random()) -> pieces_hash_map().
pieces_hash_map(MaxRandom) ->
	BoardPieces = binbo_board:pieces(),
	lists:foldl(fun(Piece, MapAcc) ->
		HashTuple = piece_hash_tuple(MaxRandom),
		MapAcc#{Piece => HashTuple}
	end, #{}, BoardPieces).

%% piece_hash_tuple/1
-spec piece_hash_tuple(max_random()) -> piece_hash_tuple().
piece_hash_tuple(MaxRandom) ->
	lists:foldl(fun(SqIdx, TupleAcc) ->
		Random = random(MaxRandom),
		erlang:setelement(SqIdx + 1, TupleAcc, Random)
	end, binbo_board:board_tuple(0), binbo_board:index_list()).


%% init_enpassant_hash_map/1
-spec init_enpassant_hash_map(max_random()) -> ?GLOBAL_HASH_ENPASSANT_MOD.
init_enpassant_hash_map(MaxRandom) ->
	Mod = ?GLOBAL_HASH_ENPASSANT_MOD,
	EnpaHashMap = enpassant_hash_map(MaxRandom),
	ok = binbo_global:put(Mod, EnpaHashMap),
	Mod.

%% enpassant_hash_map/1
-spec enpassant_hash_map(max_random()) -> enpassant_hash_map().
enpassant_hash_map(MaxRandom) ->
	lists:foldl(fun(File, MapAcc) ->
		Random = random(MaxRandom),
		MapAcc#{File => Random}
	end, #{}, binbo_board:file_list()).


%% init_side_hash_map/1
-spec init_side_hash_map(max_random()) -> ?GLOBAL_HASH_SIDE_MOD.
init_side_hash_map(MaxRandom) ->
	Mod = ?GLOBAL_HASH_SIDE_MOD,
	SideHashMap = side_hash_map(MaxRandom),
	ok = binbo_global:put(Mod, SideHashMap),
	Mod.

%% side_hash_map/1
-spec side_hash_map(max_random()) -> side_hash_map().
side_hash_map(MaxRandom) ->
	lists:foldl(fun(Side, MapAcc) ->
		Random = random(MaxRandom),
		MapAcc#{Side => Random}
	end, #{}, binbo_board:side_list()).


%% init_castling_hash_map/1
-spec init_castling_hash_map(max_random()) -> ?GLOBAL_HASH_CASTLING_MOD.
init_castling_hash_map(MaxRandom) ->
	Mod = ?GLOBAL_HASH_CASTLING_MOD,
	HashMap = castling_hash_map(MaxRandom),
	ok = binbo_global:put(Mod, HashMap),
	Mod.


%% castling_hash_map/1
-spec castling_hash_map(max_random()) -> castling_hash_map().
castling_hash_map(MaxRandom) ->
	lists:foldl(fun(Castling, MapAcc) ->
		Random = random(MaxRandom),
		MapAcc#{Castling => Random}
	end, #{}, binbo_board:castling_list()).



%% check_randoms/0
-spec check_randoms() -> ok | no_return().
check_randoms() ->
	check_randoms([piece, enpa, side, castling], #{}).

%% check_randoms/2
-spec check_randoms([piece|enpa|side|castling], #{hash() => 0}) -> ok | no_return().
check_randoms([], _Map) ->
	ok;
check_randoms([piece | Tail], Map0) ->
	Map = lists:foldl(fun(Piece, MapAcc) ->
		lists:foldl(fun(SqIdx, MapAcc2) ->
			PieceHash = piece_hash(Piece, SqIdx),
			case maps:find(PieceHash, MapAcc2) of
				{ok, _} ->
					erlang:error({duplicated_piece_hash, PieceHash});
				error ->
					MapAcc2#{PieceHash => 0}
			end
		end, MapAcc, binbo_board:index_list())
	end, Map0, binbo_board:pieces()),
	check_randoms(Tail, Map);
check_randoms([enpa | Tail], Map0) ->
	Map = lists:foldl(fun(File, MapAcc) ->
		EnpaHash = enpa_hash(File),
		case maps:find(EnpaHash, MapAcc) of
			{ok, _} ->
				erlang:error({duplicated_enpa_hash, EnpaHash});
			error ->
				MapAcc#{EnpaHash => 0}
		end
	end, Map0, binbo_board:file_list()),
	check_randoms(Tail, Map);
check_randoms([side | Tail], Map0) ->
	Map = lists:foldl(fun(Side, MapAcc) ->
		SideHash = side_hash(Side),
		case maps:find(SideHash, MapAcc) of
			{ok, _} ->
				erlang:error({duplicated_side_hash, SideHash});
			error ->
				MapAcc#{SideHash => 0}
		end
	end, Map0, binbo_board:side_list()),
	check_randoms(Tail, Map);
check_randoms([castling | Tail], Map0) ->
	Map = lists:foldl(fun(Castling, MapAcc) ->
		CastlingHash = castling_hash(Castling),
		case maps:find(CastlingHash, MapAcc) of
			{ok, _} ->
				erlang:error({duplicated_castling_hash, CastlingHash});
			error ->
				MapAcc#{CastlingHash => 0}
		end
	end, Map0, binbo_board:castling_list()),
	check_randoms(Tail, Map).
