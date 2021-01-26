%% Copyright (c) 2019-2021, Sergei Semichev <chessvegas@chessvegas.com>. All Rights Reserved.
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

-module(pgn_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("binbo_test_lib.hrl").


-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([
    load_pgn_nightmare/1,
    load_pgn_empty/1,
    load_pgn_not_binary/1,
    load_pgn_no_moves/1,
    load_pgn_no_result/1,
    load_pgn_from_file/1,
    load_pgn_with_queen_castling/1
    ]).

%% all/0
all() -> [{group, pgn_loading}].

%% groups/0
groups() ->
	[{pgn_loading, [parallel], [
		load_pgn_nightmare,
		load_pgn_empty,
		load_pgn_not_binary,
		load_pgn_no_moves,
		load_pgn_no_result,
		load_pgn_from_file,
        load_pgn_with_queen_castling
	]}].

%% init_per_suite/1
init_per_suite(Config) ->
	ok = binbo_test_lib:all_group_testcases_exported(?MODULE),
	{ok, _} = binbo:start(),
	Config.

%% end_per_suite/1
end_per_suite(_Config) ->
	ok.

%% init_per_testcase/2
init_per_testcase(_TestCase, Config) ->
	{ok, Pid} = binbo:new_server(),
	[{pid, Pid} | Config].

%% end_per_testcase/2
end_per_testcase(_TestCase, Config) ->
	Pid = get_pid(Config),
	ok = binbo:stop_server(Pid),
	ok.

%% get_pid/1
get_pid(Config) ->
	?value(pid, Config).

%% load_pgn_empty/1
load_pgn_empty(Config) ->
	Pid = get_pid(Config),
	Pgn = <<>>,
	{error,empty_pgn} = binbo:load_pgn(Pid, Pgn),
	ok.

%% load_pgn_not_binary/1
load_pgn_not_binary(Config) ->
	Pid = get_pid(Config),
	Pgn = " not binary PGN ",
	{error,invalid_pgn_datatype} = binbo:load_pgn(Pid, Pgn),
	ok.

%% load_pgn_no_moves/1
load_pgn_no_moves(Config) ->
	Pid = get_pid(Config),
	Pgn1 = <<"     ">>,
	Pgn2 = <<
	"[Event \"Sicilian Schev. (90m+30s)/40+(15m+30s)\"]\n",
	"[Black \"White\"]\n",
	"[Date \"2017.7.13\"]\n",
	"[Result \"0-1\"]\n",
	"[White \"Rated game\"]\n"
	>>,
	Pgn3 = <<"*">>,
	{ok, continue} = binbo:load_pgn(Pid, Pgn1),
	{ok, continue} = binbo:load_pgn(Pid, Pgn2),
	{ok, continue} = binbo:load_pgn(Pid, Pgn3),
	ok.

%% load_pgn_no_result/1
load_pgn_no_result(Config) ->
	Pid = get_pid(Config),
	Pgn = pgn_no_result(),
	{ok,continue} = binbo:load_pgn(Pid, Pgn),
	ok.

%% load_pgn_nightmare/1
load_pgn_nightmare(Config) ->
	Pid = get_pid(Config),
	Pgn = pgn_nightmare(),
	{ok, checkmate} = binbo:load_pgn(Pid, Pgn),
	ok.

%% load_pgn_from_file/1
load_pgn_from_file(Config) ->
	Pid = get_pid(Config),
	DataDir = ?value(data_dir, Config), % ./pgn_SUITE_data/
	Filename = filename:join(DataDir, "simple.pgn"),
	{ok, _} = binbo:load_pgn_file(Pid, Filename),
	ok.

%% load_pgn_with_queen_castling/1
load_pgn_with_queen_castling(Config) ->
	Pid = get_pid(Config),
	Pgn = pgn_with_queen_castling(),
	{ok, continue} = binbo:load_pgn(Pid, Pgn),
	ok.

%%%------------------------------------------------------------------------------
%%%   PGNs
%%%------------------------------------------------------------------------------

%% pgn_no_result/0
pgn_no_result() ->
<<"
[Event \"IBM Kasparov vs. Deep Blue Rematch\"]
[Site \"New York, NY USA\"]
[Date \"1997.05.11\"]
[Round \"6\"]
[White \"Deep Blue\"]
[Black \"Kasparov, Garry\"]
[Opening \"Caro-Kann: 4...Nd7\"]
[ECO \"B17\"]
[Result \"1-0\"]

1.e4 c6 2.d4 d5 3.Nc3 dxe4 4.Nxe4 Nd7 5.Ng5 Ngf6 6.Bd3 e6 7.N1f3 h6
8.Nxe6 Qe7 9.O-O fxe6 10.Bg6+ Kd8 {Каспаров встряхнул головой}
11.Bf4 b5 12.a4 Bb7 13.Re1 Nd5 14.Bg3 Kc8 15.axb5 cxb5
">>.

%% pgn_nightmare/0
pgn_nightmare() ->
<<"
[Event \"Sicilian Schev. (90m+30s)/40+(15m+30s)\"]
[Black \"White\"]
[Date \"2017.7.13\"]
[Result \"0-1\"]
[White \"Rated game\"]


    { B84: Sicilian Scheveningen: 6 Be2 a6, lines without early Be3 }
1.e4
    { 0 }
\t\t c5 \t\t {% !!! INCLUDE TABS FOR TEST !!! %}
    { 37 }
2.Nf3
    { 0 }
d6
    { 2 }
3.d4
    { 0 }
cxd4
    { 2 }
4.Nxd4
    { 0 }
Nf6
    { 4 }
5.Nc3
    { 0 }
a6
    { 23 }
6.Be2
    { 0 }
e6
    { 19 }
7.O-O
    { 0 }
Be7
    { 14 }
8.f4
    { 0.22/17 0 last book move }
e5
    { 0.22/17 237 }
    ( 8...b5
        ( 8...Qb6
            ( 8...Qc7 9.a3
                ( 9.Qe1 Nbd7 10.a4
                    ( 10.Bf3 h5 11.Kh1 g6 12.Be3 e5 13.Nb3 b6 14.fxe5 dxe5 15.Nd5 Nxd5 16.exd5 Bb7 17.Rc1 f5 18.c4 a5 19.Qg3 Kf7 20.Be4 Nf6 21.Bb1 h4 22.Qh3 Kg7 23.c5 Nxd5 24.cxb6 Qd7 { Zuckerman,B (2450)-Benko,P (2480) New York 1976 1-0 (38) }  )
10...b6 11.Bf3 Bb7 12.Kh1 Rd8 13.Be3 O-O 14.Qg3 Nc5 15.f5 e5 16.Bh6 Ne8 17.Nb3 Nd7 18.Rad1 Kh8 19.Be3 Nef6 20.Qf2 Rfe8 21.Rfe1 Bf8 22.Bg5 h6 23.Bh4 Rc8 { Polgar,J (2630)-Kasparov,G (2805) Linares 1994 0-1 (46) }  )
9...O-O 10.Be3 b5 11.Bf3 Bb7 12.Qe2 Nbd7 13.Rad1 Rac8 14.g4 Nc5 15.Qg2 Rfe8 16.g5 Nfd7 17.h4 Bf8 18.h5 e5 19.Nde2 Nb6 20.f5 Nca4 21.g6 Nc4 22.gxh7+ Kxh7 23.Bf2 { Paoli,E (2300)-Ribli,Z (2505) Kecskemet 1972 0-1 (47) }  )
9.Kh1 Nbd7 10.a4 O-O 11.a5 Qc7 12.Bf3 Rb8 13.g4 b5 14.g5 Ne8 15.Be3 Bb7 16.Bg2 b4 17.Na4 Nc5 18.Nxc5 dxc5 19.Nb3 c4 20.Bb6 Qc8 21.Nd4 Nd6 22.Qe2 Re8 23.Rad1 { Toth,A (2180)-Biro,G (2210) Hungary 1995 0-1 (38) }  )

        ( 8...Qc7 9.a3
            ( 9.Qe1 Nbd7 10.a4
                ( 10.Bf3 h5 11.Kh1 g6 12.Be3 e5 13.Nb3 b6 14.fxe5 dxe5 15.Nd5 Nxd5 16.exd5 Bb7 17.Rc1 f5 18.c4 a5 19.Qg3 Kf7 20.Be4 Nf6 21.Bb1 h4 22.Qh3 Kg7 23.c5 Nxd5 24.cxb6 Qd7 { Zuckerman,B (2450)-Benko,P (2480) New York 1976 1-0 (38) }  )
10...b6 11.Bf3 Bb7 12.Kh1 Rd8 13.Be3 O-O 14.Qg3 Nc5 15.f5 e5 16.Bh6 Ne8 17.Nb3 Nd7 18.Rad1 Kh8 19.Be3 Nef6 20.Qf2 Rfe8 21.Rfe1 Bf8 22.Bg5 h6 23.Bh4 Rc8 { Polgar,J (2630)-Kasparov,G (2805) Linares 1994 0-1 (46) }  )
9...O-O 10.Be3 b5 11.Bf3 Bb7 12.Qe2 Nbd7 13.Rad1 Rac8 14.g4 Nc5 15.Qg2 Rfe8 16.g5 Nfd7 17.h4 Bf8 18.h5 e5 19.Nde2 Nb6 20.f5 Nca4 21.g6 Nc4 22.gxh7+ Kxh7 23.Bf2 { Paoli,E (2300)-Ribli,Z (2505) Kecskemet 1972 0-1 (47) }  )
9.Bf3 Ra7 10.Be3 Rd7 11.Qe1 O-O 12.Qg3 b4 13.Nd1 Bb7 14.Nf2 Kh8 15.Rad1 Qa5 16.Ra1 d5 17.e5 Ne4 18.Bxe4 dxe4 19.f5 Rxd4 20.Bxd4 exf5 21.c3 Nc6 22.Nd1 g6 23.Ne3 { Bueno Perez,L (2200)-Soler,A (2345) Cienfuegos 1976 1/2-1/2 (44) }  )

    ( 8...Qb6
        ( 8...Qc7 9.a3
            ( 9.Qe1 Nbd7 10.a4
                ( 10.Bf3 h5 11.Kh1 g6 12.Be3 e5 13.Nb3 b6 14.fxe5 dxe5 15.Nd5 Nxd5 16.exd5 Bb7 17.Rc1 f5 18.c4 a5 19.Qg3 Kf7 20.Be4 Nf6 21.Bb1 h4 22.Qh3 Kg7 23.c5 Nxd5 24.cxb6 Qd7 { Zuckerman,B (2450)-Benko,P (2480) New York 1976 1-0 (38) }  )
10...b6 11.Bf3 Bb7 12.Kh1 Rd8 13.Be3 O-O 14.Qg3 Nc5 15.f5 e5 16.Bh6 Ne8 17.Nb3 Nd7 18.Rad1 Kh8 19.Be3 Nef6 20.Qf2 Rfe8 21.Rfe1 Bf8 22.Bg5 h6 23.Bh4 Rc8 { Polgar,J (2630)-Kasparov,G (2805) Linares 1994 0-1 (46) }  )
9...O-O 10.Be3 b5 11.Bf3 Bb7 12.Qe2 Nbd7 13.Rad1 Rac8 14.g4 Nc5 15.Qg2 Rfe8 16.g5 Nfd7 17.h4 Bf8 18.h5 e5 19.Nde2 Nb6 20.f5 Nca4 21.g6 Nc4 22.gxh7+ Kxh7 23.Bf2 { Paoli,E (2300)-Ribli,Z (2505) Kecskemet 1972 0-1 (47) }  )
9.Kh1 Nbd7 10.a4 O-O 11.a5 Qc7 12.Bf3 Rb8 13.g4 b5 14.g5 Ne8 15.Be3 Bb7 16.Bg2 b4 17.Na4 Nc5 18.Nxc5 dxc5 19.Nb3 c4 20.Bb6 Qc8 21.Nd4 Nd6 22.Qe2 Re8 23.Rad1 { Toth,A (2180)-Biro,G (2210) Hungary 1995 0-1 (38) }  )

    ( 8...Qc7 9.a3
        ( 9.Qe1 Nbd7 10.a4
            ( 10.Bf3 h5 11.Kh1 g6 12.Be3 e5 13.Nb3 b6 14.fxe5 dxe5 15.Nd5 Nxd5 16.exd5 Bb7 17.Rc1 f5 18.c4 a5 19.Qg3 Kf7 20.Be4 Nf6 21.Bb1 h4 22.Qh3 Kg7 23.c5 Nxd5 24.cxb6 Qd7 { Zuckerman,B (2450)-Benko,P (2480) New York 1976 1-0 (38) }  )
10...b6 11.Bf3 Bb7 12.Kh1 Rd8 13.Be3 O-O 14.Qg3 Nc5 15.f5 e5 16.Bh6 Ne8 17.Nb3 Nd7 18.Rad1 Kh8 19.Be3 Nef6 20.Qf2 Rfe8 21.Rfe1 Bf8 22.Bg5 h6 23.Bh4 Rc8 { Polgar,J (2630)-Kasparov,G (2805) Linares 1994 0-1 (46) }  )
9...O-O 10.Be3 b5 11.Bf3 Bb7 12.Qe2 Nbd7 13.Rad1 Rac8 14.g4 Nc5 15.Qg2 Rfe8 16.g5 Nfd7 17.h4 Bf8 18.h5 e5 19.Nde2 Nb6 20.f5 Nca4 21.g6 Nc4 22.gxh7+ Kxh7 23.Bf2 { Paoli,E (2300)-Ribli,Z (2505) Kecskemet 1972 0-1 (47) }  )
9.fxe5
    { 0.14/17 0 }
    ( 9.Nf5 { Fritz 15: } Bxf5 10.exf5 Nc6 11.Qd3 O-O 12.Bf3 Re8 13.Be3 Rb8 14.fxe5 Nxe5 15.Qe2 Qd7 16.Bd5 Rbc8 17.Rad1 Qc7 { 0.22/17 }  )
9...dxe5
    { 0.14/17 12 }
10.Nf5
    { 0.11/17 0 White threatens to win material: Nf5xg7 }
    ( 10.Nb3 { Fritz 15: } O-O 11.Be3 Nc6 12.Qxd8 Rxd8 13.Bb6 Rd7 14.Rad1 Nb4 15.Rc1 Nc6 16.Rfd1 Rxd1+ 17.Rxd1 Nb4 18.Bc5 Bxc5+ 19.Nxc5 Kf8 20.N3a4 Ke7 21.Nb6 Rb8 22.c3 Nxa2 { 0.14/17 }  )
10...O-O
    { 0.73/17 139 }
    ( 10...Qxd1
        ( 10...Qxd1 { Fritz 15: } 11.Rxd1 Bxf5 12.exf5 Nc6 13.Bg5 Nd4 14.Bd3 Rd8 15.Re1 Nc6 16.Rad1 Bb4 17.Kf2 Bxc3 18.bxc3 Ke7 19.Kf3 h6 20.Be3 { 0.11/17 }  )
11.Rxd1 Bxf5 12.exf5 $11  )

    ( 10...Qxd1 { Fritz 15: } 11.Rxd1 Bxf5 12.exf5 Nc6 13.Bg5 Nd4 14.Bd3 Rd8 15.Re1 Nc6 16.Rad1 Bb4 17.Kf2 Bxc3 18.bxc3 Ke7 19.Kf3 h6 20.Be3 { 0.11/17 }  )
11.Qxd8
    { 0.31/17 0 White has a mate threat }
    ( 11.Nxe7+
        ( 11.Nxe7+ { Fritz 15: } Qxe7 12.Bg5 Qc5+ 13.Kh1 Nbd7 14.Qd3 Qc6 15.Nd5 Nxd5 16.Qxd5 Qxd5 17.exd5 e4 18.Kg1 Ne5 19.Kf2 Re8 20.h3 Bd7 21.Ke3 f6 22.Bf4 Rac8 23.c3 { 0.73/17 }  )
11...Qxe7 12.Bg5 Qc5+ 13.Kh1 Nbd7 $16  )

    ( 11.Nxe7+ { Fritz 15: } Qxe7 12.Bg5 Qc5+ 13.Kh1 Nbd7 14.Qd3 Qc6 15.Nd5 Nxd5 16.Qxd5 Qxd5 17.exd5 e4 18.Kg1 Ne5 19.Kf2 Re8 20.h3 Bd7 21.Ke3 f6 22.Bf4 Rac8 23.c3 { 0.73/17 }  )
11...Bxd8
    { 0.31/17 33 }
12.Be3
    { 0.18/17 0 }
    ( 12.Nd5 { Fritz 15: } Bxf5 13.Nxf6+ Bxf6 14.Rxf5 Nd7 15.Be3 Rac8 16.c3 Nc5 17.Bxc5 Rxc5 18.Kf2 Rd8 19.Rd1 Rxd1 20.Bxd1 Rc6 21.Bb3 Rd6 22.Bd5 Rb6 { 0.31/17 }  )
12...Bxf5
    { 0.18/17 60 }
13.exf5
    { 0.06/17 0 White has the pair of bishops }
    ( 13.Rxf5 { Fritz 15: } Nc6 14.Bd3 Rc8 15.h3 Nd7 16.Kh2 Nd4 17.Rf2 Nc5 18.Nd5 Nxd3 19.cxd3 Ne6 20.Kg3 f6 21.Kg4 Kf7 22.h4 Re8 23.Raf1 { 0.18/17 }  )
13...Nc6
    { 0.06/17 80 }
14.Rad1
    { 0.06/17 0 }
Re8
    { 0.06/17 112 Black intends e4. White has an active position }
15.Bf3
    { -0.05/17 0 White has a very active position }
    ( 15.Nd5 { Fritz 15: } Nd4 16.Bxd4 exd4 17.Nxf6+ Bxf6 18.Bf3 Bg5 19.Rfe1 Be3+ 20.Kh1 Rac8 21.Re2 b5 22.g4 g6 23.fxg6 fxg6 24.Bb7 Rc5 25.b4 Rce5 26.Kg2 R8e6 27.Bf3 Re8 { 0.06/17 }  )
15...Rc8
    { 0.85/17 186 }
    ( 15...e4
        ( 15...e4 { Fritz 15: } 16.Be2 Rc8 17.a3 Na5 18.Rfe1 Be7 19.Bd4 Nc6 20.Be3 Ne5 21.Bd4 Bc5 22.Bxc5 Rxc5 23.Kf2 Nc6 24.g4 e3+ 25.Kg3 g6 26.g5 Ne4+ 27.Nxe4 Rxe4 28.Bd3 { -0.05/17 }  )
16.Be2 Rc8 $11  )

    ( 15...e4 { Fritz 15: } 16.Be2 Rc8 17.a3 Na5 18.Rfe1 Be7 19.Bd4 Nc6 20.Be3 Ne5 21.Bd4 Bc5 22.Bxc5 Rxc5 23.Kf2 Nc6 24.g4 e3+ 25.Kg3 g6 26.g5 Ne4+ 27.Nxe4 Rxe4 28.Bd3 { -0.05/17 }  )
16.Rf2
    { -0.75/17 0 Black has a cramped position. hands over the advantage to the opponent }
    ( 16.Ne4 $142
        ( 16.Ne4 { Fritz 15: } Be7 17.Nxf6+ Bxf6 18.Be4 b5 19.c3 Na5 20.b3 Nc6 21.Rd3 h6 22.Rfd1 Red8 23.Rxd8+ Nxd8 24.Bb6 Nc6 25.Kf2 { 0.85/17 }  )
16...Be7 17.Nxf6+ Bxf6 18.Be4 $16  )

    ( 16.Ne4 { Fritz 15: } Be7 17.Nxf6+ Bxf6 18.Be4 b5 19.c3 Na5 20.b3 Nc6 21.Rd3 h6 22.Rfd1 Red8 23.Rxd8+ Nxd8 24.Bb6 Nc6 25.Kf2 { 0.85/17 }  )
16...e4
    { -0.75/17 55 Black threatens to win material: e4xf3 }
17.Be2
    { -0.75/17 0 }
h5
    { -0.14/17 53 }
    ( 17...Nb4 $17
        ( 17...Nb4 { Fritz 15: } 18.Ba7 Ba5 { ....! } 19.Bd4 Nxc2 20.Bxf6 gxf6 21.Nd5 Rc6 22.Bg4 Nb4 23.Nxb4 Bxb4 24.Re2 Bc5+ 25.Kf1 Re5 26.Rd7 h5 27.Bh3 b5 28.a3 a5 29.g3 { -0.75/17 }  )
 )

    ( 17...Nb4 { Fritz 15: } 18.Ba7 Ba5 { ....! } 19.Bd4 Nxc2 20.Bxf6 gxf6 21.Nd5 Rc6 22.Bg4 Nb4 23.Nxb4 Bxb4 24.Re2 Bc5+ 25.Kf1 Re5 26.Rd7 h5 27.Bh3 b5 28.a3 a5 29.g3 { -0.75/17 }  )
18.b3
    { -2.84/17 0 }
    ( 18.a3 { would hold out }
        ( 18.a3 { Fritz 15: } Ne5 19.Bd4 Bc7 20.g3 Bd6 21.h3 Bc5 22.Bxc5 Rxc5 23.Rf4 Nc6 24.Kf2 e3+ 25.Kf3 Ne5+ 26.Kg2 Rec8 27.Rh4 Nc6 28.Rf4 { -0.14/17 }  )
 )

    ( 18.a3 { Fritz 15: } Ne5 19.Bd4 Bc7 20.g3 Bd6 21.h3 Bc5 22.Bxc5 Rxc5 23.Rf4 Nc6 24.Kf2 e3+ 25.Kf3 Ne5+ 26.Kg2 Rec8 27.Rh4 Nc6 28.Rf4 { -0.14/17 }  )
18...Nb4
    { -2.84/17 166 }
19.Bc4
    { -2.97/17 0 }
Ng4
    { -1.94/17 146 }
    ( 19...b5 { seems even better }
        ( 19...b5 { Fritz 15: } 20.Nxb5 axb5 21.Bxb5 Re5 22.a4 Rd5 23.Rff1 Nxc2 24.Ba7 Be7 25.Rxd5 Nxd5 26.g3 Rc7 27.Bf2 Nc3 28.Rc1 e3 29.Be1 Nxb5 30.axb5 Rc5 31.b6 Nxe1 { -2.97/17 }  )
20.Nxb5 axb5 21.Bxb5 Rf8 $19  )

    ( 19...b5 { Fritz 15: } 20.Nxb5 axb5 21.Bxb5 Re5 22.a4 Rd5 23.Rff1 Nxc2 24.Ba7 Be7 25.Rxd5 Nxd5 26.g3 Rc7 27.Bf2 Nc3 28.Rc1 e3 29.Be1 Nxb5 30.axb5 Rc5 31.b6 Nxe1 { -2.97/17 }  )
20.Re2
    { -4.38/17 0 an oversight. But White was lost anyway. }
    ( 20.Rd7 $142
        ( 20.Rd7 { Fritz 15: } Nxf2 21.Kxf2 Re7 22.Rxe7 Bxe7 23.a4 b5 24.axb5 axb5 25.Nxb5 Nxc2 26.Nd4 Nb4 27.Nb5 Nd3+ 28.Ke2 Ne5 29.Na7 Rc7 30.Nb5 Rb7 31.Bd4 Nxc4 32.bxc4 Rb8 33.Na7 Bg5 34.c5 Rd8 { -1.94/17 }  )
20...Nxf2 21.Kxf2 $19  )

    ( 20.Rd7 { Fritz 15: } Nxf2 21.Kxf2 Re7 22.Rxe7 Bxe7 23.a4 b5 24.axb5 axb5 25.Nxb5 Nxc2 26.Nd4 Nb4 27.Nb5 Nd3+ 28.Ke2 Ne5 29.Na7 Rc7 30.Nb5 Rb7 31.Bd4 Nxc4 32.bxc4 Rb8 33.Na7 Bg5 34.c5 Rd8 { -1.94/17 }  )
20...Nxe3
    { -4.38/17 231 }
21.Rxe3
    { -4.38/17 0 }
Bb6
    { -4.38/17 2 }
22.Rde1
    { -6.55/17 0 }
    ( 22.Kf2 { there is nothing better in the position }
        ( 22.Kf2 { Fritz 15: } Bxe3+ 23.Kxe3 b5 24.Nd5 Nxc2+ 25.Kd2 Nd4 26.Ke1 Nxf5 27.Be2 Rc2 28.Nb4 Rb2 29.Rd2 Rxd2 30.Kxd2 Rd8+ 31.Ke1 e3 32.Nc2 g6 33.Bf3 Kg7 34.Be4 Kf6 35.Bxf5 Kxf5 36.Nxe3+ Ke4 37.Nc2 { -4.38/17 }  )
22...Bxe3+ 23.Kxe3 $19  )

    ( 22.Kf2 { Fritz 15: } Bxe3+ 23.Kxe3 b5 24.Nd5 Nxc2+ 25.Kd2 Nd4 26.Ke1 Nxf5 27.Be2 Rc2 28.Nb4 Rb2 29.Rd2 Rxd2 30.Kxd2 Rd8+ 31.Ke1 e3 32.Nc2 g6 33.Bf3 Kg7 34.Be4 Kf6 35.Bxf5 Kxf5 36.Nxe3+ Ke4 37.Nc2 { -4.38/17 }  )
22...Nxc2
    { -6.55/17 166 }
23.Bxf7+
    { -14.36/17 0 }
    ( 23.Nd5 { the only chance to get some counterplay }
        ( 23.Nd5 { Fritz 15: } Bxe3+ 24.Rxe3 Nxe3 25.Nxe3 Red8 26.a4 Rd2 27.Nf1 Rb2 28.Bd5 Rc5 29.Bxe4 Rxb3 30.f6 gxf6 31.Bf3 Rb2 32.h3 Ra5 33.Bd1 Re5 34.Bf3 Kg7 35.h4 Kg6 { -6.55/17 }  )
23...Bxe3+ 24.Rxe3 Nxe3 25.Nxe3 $19  )

    ( 23.Nd5 { Fritz 15: } Bxe3+ 24.Rxe3 Nxe3 25.Nxe3 Red8 26.a4 Rd2 27.Nf1 Rb2 28.Bd5 Rc5 29.Bxe4 Rxb3 30.f6 gxf6 31.Bf3 Rb2 32.h3 Ra5 33.Bd1 Re5 34.Bf3 Kg7 35.h4 Kg6 { -6.55/17 }  )
23...Kxf7
    { -14.36/17 7 }
24.Nd5
    { -14.36/17 0 }
Bxe3+
    { -14.36/17 40 }
25.Rxe3
    { -14.36/17 0 }
Nxe3
    { -14.36/17 2 }
26.Nxe3
    { -14.36/17 0 }
Red8
    { -14.36/17 79 }
27.Kf2
    { -14.74/17 0 }
Rd2+
    { -14.74/17 39 }
28.Kg3
    { -14.74/17 0 }
Rc3
    { -14.74/17 55 }
29.Kf4
    { -14.74/17 0 }
Rxa2
    { -14.74/17 264 }
    ( 29...Rf2+ 30.Kxe4 { 5 } Re2 { 1 } 31.Kd5 { 2 } Rexe3 { 3 } 32.h3 { 3 }  )
30.Kxe4
    { -#9/17 0 }
    ( 30.Nd5 { Fritz 15: } Rxb3 31.Kxe4 Rxg2 32.Kd4 Rxh2 33.Kc4 Ra3 34.Kd4 Rh4+ 35.Ke5 Rb3 36.Kd6 Rb5 37.Ne3 Re4 38.Nc2 Rc4 39.Ne3 Rc6+ 40.Kd7 Rb3 41.Nd5 Rd3 42.Kd8 Rxd5# { -14.74/17 }  )
30...Re2
    { -#8/17 20 }
31.b4
    { -#7/17 0 }
Rexe3+
    { -#6/17 58 }
32.Kf4
    { -#6/17 0 }
    ( 32.Kd5 { cannot change what is in store for ? } Kf6 33.h3 Red3+ 34.Ke4 Rg3 35.Kd5 b5 36.Ke4 Rc4+ 37.Kd5 Rd3#  )
32...Kf6
    { -#5/17 21 }
33.h3
    { -#4/17 0 }
    ( 33.g3 { doesn't improve anything } b5 34.g4 Rf3+ 35.Ke4 hxg4 36.Kd5 Rc4 37.h4 Rd3#  )
33...Rg3
    { -#3/17 21 }
34.Ke4
    { -#3/17 0 }
    ( 34.b5 { does not improve anything } axb5 35.Ke4 Rc4+ 36.Kd5 Rd3#  )
34...Rc4+
    { -#2/7 42 }
35.Kd5
    { -#2/17 0 }
Rxb4
    { -#4/17 14 }
    ( 35...b5 { ....! } 36.Kd6 Rd3#  )
36.Kc5
    { -#4/17 0 }
a5
    { -#3/17 16 }
37.Kd6
    { -#3/17 0 }
Rc4
    { -#2/17 36 }
38.Kd7
    { -#2/17 0 }
    ( 38.Kd5 { doesn't do any good } b5 39.Kd6 Rd3#  )
38...Rd3+
    { -#1/17 39 }
39.Ke8
    { -#1/17 0 }
Re4+
    { -#1/17 13 }
    ( 39...Rc8#  )
40.Kf8
    { -#1/17 0 }
Rd8#
    { 2 }
0-1
">>.


pgn_with_queen_castling() ->
<<"
1. d4 Nf6 2. c4 Nc6 3. Nc3 e5 4. d5 Ne7 5. e4 Ng6 6. Be3 Bb4 7. f3 Qe7 8. Bd3
c6 9. Nge2 cxd5 10. cxd5 Nh5 11. g3 O-O 12. a3 Bc5 13. Bxc5 Qxc5 14. b4 Qe3 15.
Qd2 Qxf3 16. O-O-O d6 17. Rdf1 Qg4 18. Nb5 Qd7 19. Qg5 Nf6 20. Rxf6 gxf6 21.
Qxf6 Rd8 22. h4 Qe7 23. Qf3 Bd7 24. Nbc3 a5 25. b5 Rac8 26. Kb2 Rf8 27. g4 f6
28. Ng3 Nf4 29. Nf5 Bxf5 30. exf5 Kh8 31. Re1 Qg7 32. Rg1 Rc5 33. Ne4 Rxd5 34.
Bc4 Rd4 35. Ba2 Qc7 36. g5 fxg5 37. hxg5 Rxf5 38. Qb3 d5 39. Nf6 Nd3+ 40. Kb1
Rf2 41. Nxd5 Rxd5 42. Qxd5 Qc2+ 0-1
">>.
