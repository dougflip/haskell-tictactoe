module Main where

import           Data.List           (intercalate)
import           System.Console.ANSI (SGR (Reset), clearScreen,
                                      setCursorPosition, setSGR)
import           Text.Read           (readMaybe)
import           TicTacToeCore

resetScreen :: IO ()
resetScreen = setSGR [Reset] >> clearScreen >> setCursorPosition 0 0

renderCell :: Cell -> String
renderCell (Occupied move) = show move
renderCell Empty           = " "

renderRow :: [Cell] -> String
renderRow row = intercalate " | " $ fmap renderCell row

dividingLine :: String
dividingLine = "----------"

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

askForCell :: IO Int
askForCell = do
  putStrLn "Which cell would you like to play (1-9)?"
  s <- getLine
  case readMaybe s of
    Nothing -> do
      putStrLn "You must provide an integer bewteen 1-9."
      askForCell
    Just i -> return i

-- TODO: Should this just run until we hit a Winner or Tie case?
-- maybe handle the "complete" cases in main?
-- Maybe even consider exporing `Error | InProgress | Complete`?
gameLoop :: MoveResult -> IO String
gameLoop (Error msg game) = do
  putStrLn msg
  gameLoop $ Ok game
gameLoop (Ok game) = do
  resetScreen -- TODO: This reset is great, but it overwrites the previous error message
  let board = gameBoard game
  renderBoard board
  case game of
    (Game _ (Winner move)) -> return (show move ++ " has won the game!")
    (Game _ (Tie)) -> return ("The game has ended in a tie")
    (Game _ (NextMove move)) -> do
      putStrLn $ "Player " ++ show move ++ " is up"
      cell <- askForCell
      gameLoop $ playMove cell game

main :: IO ()
main = do
  s <- gameLoop $ Ok $ newGame X
  putStrLn $ show s
