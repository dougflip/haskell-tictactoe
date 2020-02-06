# Haskell TicTacToe

A basic implementation of TicTacToe.

The goal is to have one module of business logic that is shared between a

- CLI app
- Web app
- Native app

Hoping to learn about

- building multiple executables
- writing a Haskell web server
- writing a Haskell native GUI app (even if it is very simple)

## Quick Start

- `stack build --fast --file-watch --haddock-deps` - builds code and documentation and watches for file changes
- `stack test --fast --file-watch` - runs the tests and watches for file changes

## Using GHCID

1. `stack build ghcid` - builds the correct version of ghcid for current ghc
2. `stack exec -- ghcid -c "stack ghci haskell-tictactoe"` - starts ghcid and watches for changes
