module Main where

import           Data.List           (intercalate)
import           System.Console.ANSI (SGR (Reset), clearScreen,
                                      setCursorPosition, setSGR)
import           Text.Read           (readMaybe)
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

-- TODO: Should the main `playMove` function take an already validated
-- cell number? Then Core could provide a validation function?
-- readEither >>= validateCell?
askForCell :: IO Int
askForCell = do
  putStrLn "Which cell would you like to play (1-9)?"
  s <- getLine
  case readMaybe s of
    Nothing -> do
      putStrLn "You must provide an integer bewteen 1-9."
      askForCell
    Just i -> return i

gameLoop :: MoveResult -> IO CompletedGame
gameLoop (Error game msg) = do
  putStrLn msg
  cell <- askForCell
  gameLoop $ playMove cell game
gameLoop (Ok gameResult) = do
  case gameResult of
    (InProgress progressGame@(InProgressGame board move)) -> do
      resetScreen
      renderBoard board
      putStrLn $ "Player " ++ show move ++ " is up"
      cell <- askForCell
      gameLoop $ playMove cell progressGame
    (Complete completedGame) -> pure completedGame

main :: IO ()
main = do
  s <- gameLoop $ Ok $ InProgress $ newGame X
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
