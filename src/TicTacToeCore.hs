module TicTacToeCore
  ( PlayerPiece(X, O)
  , newGameBoard
  , applyMove
  , isGameComplete
  , GameBoard
  , PlayerMovePosition
  ) where

import           Data.List  (any, nub)
import           Data.Maybe (isJust)

data PlayerPiece
  = X
  | O
  deriving (Show, Eq)

type GameBoardSquare = Maybe PlayerPiece

data GameBoard =
  GameBoard
    { square1 :: GameBoardSquare
    , square2 :: GameBoardSquare
    , square3 :: GameBoardSquare
    , square4 :: GameBoardSquare
    , square5 :: GameBoardSquare
    , square6 :: GameBoardSquare
    , square7 :: GameBoardSquare
    , square8 :: GameBoardSquare
    , square9 :: GameBoardSquare
    }
  deriving (Show, Eq)

type PlayerMovePosition = Int

newGameBoard :: GameBoard
newGameBoard =
  GameBoard
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

-- Can probably use lenses to reduce this boiler plate.
-- Its also possible that using a record is a poor data model for this.
applyMove :: PlayerPiece -> PlayerMovePosition -> GameBoard -> GameBoard
applyMove piece 1 board = board {square1 = Just piece}
applyMove piece 2 board = board {square2 = Just piece}
applyMove piece 3 board = board {square3 = Just piece}
applyMove piece 4 board = board {square4 = Just piece}
applyMove piece 5 board = board {square5 = Just piece}
applyMove piece 6 board = board {square6 = Just piece}
applyMove piece 7 board = board {square7 = Just piece}
applyMove piece 8 board = board {square8 = Just piece}
applyMove piece 9 board = board {square9 = Just piece}
-- can we make our "PlayerMovePosition" enforce it is a number 1-9 instead of needing to error here?
applyMove _ position _ =
  error ("Position of " ++ show position ++ " is out of bounds!")

winningPatterns :: [[GameBoard -> Maybe PlayerPiece]]
winningPatterns = [[square1, square2, square3]]

isWinningPattern :: GameBoard -> [(GameBoard -> Maybe PlayerPiece)] -> Bool
isWinningPattern board positions =
  areAllPlayerPieces board positions && areAllTheSamePlayerPiece board positions
  where
    areAllPlayerPieces board positions = all isJust $ fmap ($ board) positions
    areAllTheSamePlayerPiece board positions =
      length (nub $ fmap ($ board) positions) == 1

isGameComplete :: GameBoard -> Bool
isGameComplete board = any (isWinningPattern board) winningPatterns
