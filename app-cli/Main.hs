module Main where

import           Data.List           (intercalate)
import           System.Console.ANSI (SGR (Reset), clearScreen,
                                      setCursorPosition, setSGR)
import           TicTacToeCore

resetScreen :: IO ()
resetScreen = setSGR [Reset] >> clearScreen >> setCursorPosition 0 0

-- TODO: Maybe these "render" functions go in Core or a Utils?
renderCell :: Cell -> String
renderCell (Occupied move) = show move
renderCell Empty           = " "

renderRow :: [Cell] -> String
renderRow row = intercalate " | " $ fmap renderCell row

dividingLine :: String
dividingLine = "----------"

-- TODO: Maybe this could just return a string?
renderBoard :: [Cell] -> IO ()
renderBoard board = do
  putStrLn $ renderRow firstRow
  putStrLn dividingLine
  putStrLn $ renderRow secondRow
  putStrLn dividingLine
  putStrLn $ renderRow thirdRow
  where
    firstRow = take 3 board
    secondRow = drop 3 . take 6 $ board
    thirdRow = drop 6 board

askForCell :: InProgressGame -> IO ValidMove
askForCell game = do
  putStrLn "Which cell would you like to play (1-9)?"
  rawCell <- getLine
  case parseValidMove game rawCell of
    Left msg -> do
      putStrLn msg
      askForCell game
    Right valid -> pure valid

handleInProgressGame :: InProgressGame -> IO GameResult
handleInProgressGame game@(InProgressGame board move) = do
  resetScreen
  renderBoard board
  putStrLn $ "Player " ++ show move ++ " is up"
  validMove <- askForCell game
  pure $ playMove' validMove

gameLoop :: GameResult -> IO CompletedGame
gameLoop gameResult =
  case gameResult of
    (InProgress game) -> do
      result <- handleInProgressGame game
      gameLoop result
    (Complete completedGame) -> pure completedGame

main :: IO ()
main = do
  s <- gameLoop $ InProgress $ newGame X
  resetScreen
  case s of
    (Winner board move) -> do
      putStrLn $ "Player " ++ show move ++ " has won the game!"
      putStrLn "Here is the final board"
      renderBoard board
    (Tie board) -> do
      putStrLn "You tied!"
      putStrLn "Here is the final board"
      renderBoard board
