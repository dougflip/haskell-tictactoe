-- Inspiration from
-- https://dev.to/nt591/writing-a-tictactoe-game-in-haskell-545e
module TicTacToeCore where

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

data GameStatus
  = NextMove Move
  | Winner Move
  | Tie
  deriving (Show, Eq)

data Game =
  Game
    { gameBoard  :: Board
    , gameStatus :: GameStatus
    }
  deriving (Show, Eq)

data MoveResult
  = Error String Game
  | Ok Game
  deriving (Show, Eq)

emptyBoard :: Board
emptyBoard = take 9 $ repeat Empty

newGame :: Move -> Game
newGame move = Game emptyBoard (NextMove move)

nextMove :: Move -> Move
nextMove X = O
nextMove O = X

isCellValid :: CellIndex -> Bool
isCellValid x = x >= 0 && x <= 8

-- assumes the cell is valid since this is all internal
-- if we expose this function then we can make a
-- ValidCell type with a smart constructor
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

-- Need to actually update the cell with the an `Occupied Move` instance
updateGame :: Move -> CellIndex -> Board -> Game
updateGame move x board = Game newBoard newStatus
  where
    newBoard = updateBoard move x board
    newStatus
      | isWinner newBoard = Winner move
      | all isOccupied newBoard = Tie -- TODO: could figure out Tie earlier with some more work
      | otherwise = NextMove $ nextMove move

-- doesn't take a "Move" because the Game records who goes next
playMove :: CellNumber -> Game -> MoveResult
playMove _ winner@(Game _ (Winner _)) = Ok winner
playMove _ tie@(Game _ Tie) = Ok tie
playMove x g@(Game board (NextMove move))
  | not $ isCellValid cellIndex = Error ("Cell " ++ show x ++ " is not valid") g
  | not $ isCellEmpty cellIndex board =
    Error ("Cell" ++ show x ++ " is already occupied") g
  | otherwise = Ok $ updateGame move cellIndex board
  where
    cellIndex = x - 1
