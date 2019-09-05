module TicTacToeCore where

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

{- Tracks the current board and the status of the game.
   Currently, `gameStatus` just stores which PlayerPiece has the next turn.
   Eventually, I'd like this to be one of two things:
     1. If the game is still in progress, then the PlayerPiece
     2. If the game has finished then that data plus who won.
   It's possible that what I actually want is 2 different game constructors:
   ```
   data Game = InProgressGame PlayerPiece | FinishedGame PlayerPiece
   ```
   But I'll need to play around with that
-}
data Game =
  Game
    { gameBoard  :: GameBoard
    , gameStatus :: PlayerPiece
    }

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

emptyGameBoard :: GameBoard
emptyGameBoard =
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

newGame :: Game
newGame = Game emptyGameBoard X

-- Can probably use lenses to reduce some boilerplate?
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

{- This is the function that should be exposed.
   The idea is that the "Game" itself records which piece has the next move
   TODO: We still need to enforce that you can't play a position that is already played.
-}
applyTurn :: PlayerMovePosition -> Game -> Game
applyTurn piece (Game board X) = Game (applyMove X piece board) O
applyTurn piece (Game board O) = Game (applyMove O piece board) X

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
  areAllPlayerPieces && areAllTheSamePlayerPiece
  where
    areAllPlayerPieces = all isJust $ fmap ($ board) positions
    areAllTheSamePlayerPiece = length (nub $ fmap ($ board) positions) == 1

isGameComplete :: Game -> Bool
isGameComplete (Game board _) = any (isWinningPattern board) winningPatterns
