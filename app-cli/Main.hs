module Main where

import           Data.List     (intercalate)
import           Text.Read     (readMaybe)
import           TicTacToeCore

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

gameLoop :: MoveResult -> IO String
gameLoop (Error msg game) = do
  putStrLn msg
  gameLoop $ Ok game
-- these constructors could probably be combined
-- with a String as the outcome
gameLoop (Ok (winner@(Game _ (Winner move)))) =
  return (show move ++ " has won the game!")
gameLoop (Ok (tie@(Game _ (Tie)))) = return ("The game has ended in a tie")
gameLoop nextMove@(Ok game@(Game board (NextMove move))) = do
  renderBoard board
  putStrLn $ "Player " ++ show move ++ " is up"
  cell <- askForCell
  gameLoop $ playMove cell game

main :: IO ()
main = do
  s <- gameLoop $ Ok $ newGame X
  putStrLn $ show s
