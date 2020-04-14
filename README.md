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

## Running the Tests

- `stack test`
- `stack test --fast --file-watch` - watch for changes and re-run tests

## Using GHCID

`stack build ghcid` - builds the correct version of ghcid for current ghc

- `ghcid --command 'stack ghci --test --main-is haskell-tictactoe:haskell-tictactoe-test' --test ':main' --warnings` - compiles the library code and if successful runs the tests
- `stack exec -- ghcid -c "stack ghci haskell-tictactoe"` - compiles the project

## Play via CLI

- `stack exec haskell-tictactoe-cli`
