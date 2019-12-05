# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

- UCI protocol support added.
- UCI protocol related functions:
  - `binbo:new_uci_game/2`;
  - `binbo:uci_command_call/2`;
  - `binbo:uci_command_cast/2`;
  - `binbo:uci_mode/1`;
  - `binbo:uci_bestmove/1`;
  - `binbo:set_uci_handler/2`;
  - `binbo:uci_play/2,3`;
  - `binbo:uci_set_position/2`.
- New function `binbo:side_to_move/1`.

## [1.1.2] - 2019-09-26

- Performance optimization in game state update.
- Some binary optimizations for FEN/SAN parsing.
- Dependency `uef-lib` updated to 2.5.1.
- More perft tests added.

## [1.1.1] - 2019-08-16

- Optimized move generation using bitboards instead of lists of square indices.

## [1.1.0] - 2019-08-14

- Improved performance of all legal moves generation.
- All perft tests are passed much faster.
- New function `binbo_board:int_move/2`.
- New function `binbo_board:int_move/3`.
- New function `binbo_board:int_move_from/1`.
- New function `binbo_board:int_move_to/1`.
- New function `binbo_move:validate_int_move/2`.
- Count all legal moves with `binbo:all_legal_moves(Pid, count)`.

## [1.0.2] - 2019-08-08

- Fixed: Castling was allowed with enemy piece after capturing a friendly rook. The bug was found during testing Position 5 at depth 3 from [Perft Results](https://www.chessprogramming.org/Perft_Results).
- Fixed: Wrong castling allowed by FEN was not properly validated.
- Passes all perft tests now.

## [1.0.1] - 2019-08-06

- Fixed: `binbo:all_legal_moves/1,2` did not include possible pawn promotions. It now returns a list of legal moves where each element is a tuple `{From, To}` or `{From, To, Promo}`.

## [1.0.0] - 2019-08-05

- API of `binbo_movegen` module has been changed regarding function `all_valid_moves/*`. It now returns a list of legal moves where each element is a tuple `{From, To}`.
- New function `binbo:all_legal_moves/1`.
- New function `binbo:all_legal_moves/2`.

## [0.3.0] - 2019-08-01

- Support for loading PGN from file.
- New function `binbo:load_pgn_file/2`.

## [0.2.0] - 2019-07-30

- Support for PGN loading.
- New function `binbo:load_pgn/2`.
- New function `binbo:san_move/2`.

## [0.1.0] - 2019-07-22

- Initial release.
