# Changelog

All notable changes to this project will be documented in this file.

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
