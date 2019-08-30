module TicTacToeCore
  ( PlayerPiece(..)
  , PlayerMovePosition(..)
  , GameBoard
  , newGameBoard
  , applyMove
  , isGameComplete
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

data PlayerMovePosition
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine

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
applyMove piece One board   = board {square1 = Just piece}
applyMove piece Two board   = board {square2 = Just piece}
applyMove piece Three board = board {square3 = Just piece}
applyMove piece Four board  = board {square4 = Just piece}
applyMove piece Five board  = board {square5 = Just piece}
applyMove piece Six board   = board {square6 = Just piece}
applyMove piece Seven board = board {square7 = Just piece}
applyMove piece Eight board = board {square8 = Just piece}
applyMove piece Nine board  = board {square9 = Just piece}

winningPatterns :: [[GameBoard -> Maybe PlayerPiece]]
winningPatterns =
  [ [square1, square2, square3]
  , [square4, square5, square6]
  , [square7, square8, square9]
  , [square1, square4, square7]
  , [square2, square5, square8]
  , [square3, square6, square9]
  , [square1, square5, square9]
  , [square3, square5, square7]
  ]

isWinningPattern :: GameBoard -> [(GameBoard -> Maybe PlayerPiece)] -> Bool
isWinningPattern board positions =
  areAllPlayerPieces board positions && areAllTheSamePlayerPiece board positions
  where
    areAllPlayerPieces board positions = all isJust $ fmap ($ board) positions
    areAllTheSamePlayerPiece board positions =
      length (nub $ fmap ($ board) positions) == 1

isGameComplete :: GameBoard -> Bool
isGameComplete board = any (isWinningPattern board) winningPatterns
