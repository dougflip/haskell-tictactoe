# Haskell TicTacToe

A basic implementation of TicTacToe which could be exposed as a CLI app
or web service to power a web front end.

## Quick Start

- `stack build --fast --file-watch --haddock-deps` - builds code and documentation and watches for file changes
- `stack test --fast --file-watch` - runs the tests and watches for file changes

## Using GHCID

1. `stack build ghcid` - builds the correct version of ghcid for current ghc
2. `stack exec -- ghcid -c "stack ghci haskell-tictactoe"` - starts ghcid and watches for changes
