module Main where

import           RenderUtils         (renderBoard)
import           System.Console.ANSI (SGR (Reset), clearScreen,
                                      setCursorPosition, setSGR)
import           TicTacToeCore       (CompletedGame (Tie, Winner),
                                      GameResult (Complete, InProgress),
                                      InProgressGame (InProgressGame), Move (X),
                                      MoveResult (Error, Ok),
                                      ValidPlay (ValidPlay), newGame,
                                      parseCellNumber, parseValidPlay, playMove,
                                      playMove')

resetScreen :: IO ()
resetScreen = setSGR [Reset] >> clearScreen >> setCursorPosition 0 0

-- TODO: Should the main `playMove` function take an already validated
-- cell number? Then Core could provide a validation function?
-- readEither >>= validateCell?
askForCell :: InProgressGame -> IO ValidPlay
askForCell game = do
  putStrLn "Which cell would you like to play (1-9)?"
  s <- getLine
  case parseValidPlay s game of
    Left msg -> do
      putStrLn msg
      askForCell game
    Right validPlay -> return validPlay

handleInProgressGame :: InProgressGame -> IO MoveResult
handleInProgressGame game@(InProgressGame board move) = do
  resetScreen
  putStrLn $ renderBoard board
  putStrLn $ "Player " ++ show move ++ " is up"
  validPlay <- askForCell game
  pure $ playMove' validPlay

gameLoop :: MoveResult -> IO CompletedGame
gameLoop (Error game msg) = do
  putStrLn msg
  validPlay <- askForCell game
  gameLoop $ playMove' validPlay
gameLoop (Ok gameResult) = do
  case gameResult of
    (InProgress game) -> do
      result <- handleInProgressGame game
      gameLoop result
    (Complete completedGame) -> pure completedGame

main :: IO ()
main = do
  s <- gameLoop $ Ok $ InProgress $ newGame X
  resetScreen
  case s of
    (Winner board move) -> do
      putStrLn $ "Player " ++ show move ++ " has won the game!"
      putStrLn "Here is the final board"
      putStrLn $ renderBoard board
    (Tie board) -> do
      putStrLn "You tied!"
      putStrLn "Here is the final board"
      putStrLn $ renderBoard board
