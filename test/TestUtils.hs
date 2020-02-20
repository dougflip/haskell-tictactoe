module TestUtils where

import           Data.List.NonEmpty (NonEmpty ((:|)))
import           TicTacToeCore

isError :: MoveResult -> Bool
isError (Error _ _) = True
isError _           = False

cellToString :: Cell -> String
cellToString (Occupied x) = show x
cellToString Empty        = "_"

playOneTurn :: MoveResult -> CellNumber -> MoveResult
playOneTurn err@(Error _ _) _ = err
playOneTurn (Ok game) cell =
  case game of
    (InProgress inProgressGame) -> playMove cell inProgressGame
    completeGame@(Complete _)   -> Ok $ completeGame

playGame :: Move -> NonEmpty CellNumber -> MoveResult
playGame move (x :| xs) = foldl playOneTurn initialResult xs
  where
    initialResult = playMove x $ newGame move

toBoardString :: Board -> String
toBoardString = concatMap cellToString

completedGameString :: CompletedGame -> String
completedGameString (Winner board move) =
  "Winner " ++ show move ++ ": " ++ (toBoardString board)
completedGameString (Tie board) = "Tie: " ++ (toBoardString board)

inProgressGameString :: InProgressGame -> String
inProgressGameString (InProgressGame board move) =
  show move ++ ": " ++ (toBoardString board)

toGameString :: MoveResult -> String
toGameString (Error _ msg) = msg
toGameString (Ok gameResult) =
  case gameResult of
    (InProgress game) -> inProgressGameString game
    (Complete game)   -> completedGameString game
