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

-record(move_info, {
    from_idx = undefined :: undefined | binbo_board:square_index(),
    to_idx = undefined :: undefined | binbo_board:square_index(),
    from_bb = undefined :: undefined | binbo_bb:sq_bb(),
    to_bb = undefined :: undefined | binbo_bb:sq_bb(),
    piece = undefined :: undefined | binbo_board:piece(), % moving piece
    pcolor = undefined :: undefined | binbo_board:color(), % color of moving piece
    ptype = undefined :: undefined | binbo_board:piece_type(), % type of moving piece
    captured = 0 :: binbo_board:empty_square() | binbo_board:piece(), % captured piece
    % 'captured_idx' is the index of square where piece has been captured.
    % It may differ from the index of target square due to en-passant capture.
    captured_idx = undefined :: undefined | binbo_board:square_index(),
    promo = undefined :: undefined | binbo_move:promo_type(),
    castling = 0 :: binbo_move:castling_flag(),
    % 'is_check' is 'true' when the opposite king is in check after move
    is_check = false :: boolean(),
    % 'has_valid_moves' is 'false' when the opposite side has no valid moves (checkmate or stalemate).
    has_valid_moves = undefined :: undefined | boolean()
}).
