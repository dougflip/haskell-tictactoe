-- Inspiration from
-- https://dev.to/nt591/writing-a-tictactoe-game-in-haskell-545e
module TicTacToeCore where

import           Text.Read (readEither)

data Move
  = X
  | O
  deriving (Show, Eq)

data Cell
  = Occupied Move
  | Empty
  deriving (Show, Eq)

type CellNumber = Int

type CellIndex = Int

type Board = [Cell]

{-|
  Models a game that can still have further moves applied to it.
-}
data InProgressGame =
  InProgressGame Board Move
  deriving (Show, Eq)

{-|
  Models a game that has been played to completion
-}
data CompletedGame
  = Winner Board Move
  | Tie Board
  deriving (Show, Eq)

{-|
  The result of applying a successful move to an in progress game.
-}
data GameResult
  = InProgress InProgressGame
  | Complete CompletedGame
  deriving (Show, Eq)

{-|
  The result of _attempting_ to apply a move to an in progress game.
-}
data MoveResult
  = Error InProgressGame String
  | Ok GameResult
  deriving (Show, Eq)

{-|
  Models a CellIndex that has been validated against the given game.
  In other words, we have proven that this move is valid against this board.
  Hidden behind a smart constructor.
-}
data ValidMove =
  ValidMove CellIndex InProgressGame

emptyBoard :: Board
emptyBoard = take 9 $ repeat Empty

nextMove :: Move -> Move
nextMove X = O
nextMove O = X

isCellInRange :: CellIndex -> Bool
isCellInRange x = x >= 0 && x <= 8

-- assumes the cell is valid since this is all internal
-- if we expose this function then we can make a
-- ValidMove type with a smart constructor
isCellEmpty :: CellIndex -> Board -> Bool
isCellEmpty x board = board !! x == Empty

updateBoard :: Move -> CellIndex -> Board -> Board
updateBoard move x board =
  take x board ++ [Occupied move] ++ (drop (x + 1) board)

winningPatterns :: [[CellIndex]]
winningPatterns =
  [ [0, 1, 2]
  , [3, 4, 5]
  , [6, 7, 8]
  , [0, 3, 6]
  , [1, 4, 7]
  , [2, 5, 8]
  , [0, 4, 8]
  , [2, 4, 6]
  ]

isOccupied :: Cell -> Bool
isOccupied (Occupied _) = True
isOccupied _            = False

allEqual :: Eq a => [a] -> Bool
allEqual []       = True
allEqual [_]      = True
allEqual (x:y:ys) = x == y && allEqual (y : ys)

-- determine if _one_ winning pattern is present
isWinningPattern :: Board -> [CellNumber] -> Bool
isWinningPattern board xs = all isOccupied pieces && allEqual pieces
  where
    pieces = map (board !!) xs

-- search all winning patterns to see if we have a winner
isWinner :: Board -> Bool
isWinner board = any (isWinningPattern board) winningPatterns

newGame :: Move -> InProgressGame
newGame move = InProgressGame emptyBoard move

{-|
  Updates the game with the provided move.
  This is called once we know we have a valid move
  so this will always succeed in updating the board.
-}
updateGame :: Move -> CellIndex -> Board -> GameResult
updateGame move x board = result
  where
    newBoard = updateBoard move x board
    result
      | isWinner newBoard = Complete $ Winner newBoard move
      | all isOccupied newBoard = Complete $ Tie newBoard -- TODO: could figure out Tie earlier with some more work
      | otherwise = InProgress $ InProgressGame newBoard (nextMove move)

{-|
  This is the main function used to play the game.
  You ask to play a particular cell and this function _attempts_ to apply that move.
  The result can be either a validation error or a successful move.
-}
playMove :: CellNumber -> InProgressGame -> MoveResult
playMove cell game@(InProgressGame board move)
  | not $ isCellInRange cellIndex =
    Error game ("Cell " ++ show cell ++ " is not valid")
  | not $ isCellEmpty cellIndex board =
    Error game ("Cell" ++ show cell ++ " is already occupied")
  | otherwise = Ok $ updateGame move cellIndex board
  where
    cellIndex = cell - 1

playMove' :: ValidMove -> GameResult
playMove' (ValidMove cell (InProgressGame board move)) = result
  where
    newBoard = updateBoard move cell board
    result
      | isWinner newBoard = Complete $ Winner newBoard move
      | all isOccupied newBoard = Complete $ Tie newBoard -- TODO: could figure out Tie earlier with some more work
      | otherwise = InProgress $ InProgressGame newBoard (nextMove move)

{-|
  Validates that a given `CellNumber` is valid for a given `InProgressGame`.
  This allows the `playMove` function to _know_ that validation has been handled.
-}
mkValidMove :: InProgressGame -> CellNumber -> Either String ValidMove
mkValidMove game@(InProgressGame board _) cell
  | not $ isCellInRange cellIndex =
    Left $ "Cell " ++ show cell ++ " is not valid"
  | not $ isCellEmpty cellIndex board =
    Left $ "Cell" ++ show cell ++ " is already occupied"
  | otherwise = Right $ ValidMove cellIndex game
  where
    cellIndex = cell - 1

{-|
  Helper to parse a String and determine if it is a valid move.
  If the string parses to an Int it is sent to `mkValidMove`.
-}
parseValidMove :: InProgressGame -> String -> Either String ValidMove
parseValidMove game cellString = do
  case readEither cellString of
    Left _          -> Left "You must provide an integer from 1 to 9"
    Right cellIndex -> mkValidMove game cellIndex
