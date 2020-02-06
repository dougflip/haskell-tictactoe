module TestUtils where

import           Data.List.NonEmpty (NonEmpty ((:|)))
import           TicTacToeCore

isError :: MoveResult -> Bool
isError (Error _) = True
isError _         = False

cellToString :: Cell -> String
cellToString (Occupied x) = show x
cellToString Empty        = "_"

gameStatusString :: GameStatus -> String
gameStatusString (NextMove move) = show move
gameStatusString (Winner move)   = "Winner " ++ show move
gameStatusString Tie             = "Tie"

toGameString :: MoveResult -> String
toGameString (Error msg) = msg
toGameString (Ok (Game board status)) = statusString ++ ": " ++ boardString
  where
    statusString = gameStatusString status
    boardString = concatMap cellToString board

playOneTurn :: MoveResult -> CellNumber -> MoveResult
playOneTurn err@(Error _) _ = err
playOneTurn (Ok game) cell  = playMove cell game

playGame :: Move -> NonEmpty CellNumber -> MoveResult
playGame move (x :| xs) = foldl playOneTurn initialResult xs
  where
    initialResult = playMove x $ newGame move
