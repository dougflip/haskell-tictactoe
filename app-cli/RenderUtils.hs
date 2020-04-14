module RenderUtils
  ( renderBoard
  ) where

import           Data.List     (intercalate)
import           TicTacToeCore (Cell (Empty, Occupied))

renderCell :: Cell -> String
renderCell (Occupied move) = show move
renderCell Empty           = " "

renderRow :: [Cell] -> String
renderRow row = intercalate " | " $ fmap renderCell row

dividingLine :: String
dividingLine = "----------"

{-|
  Renders the board as a visual string.
  Intended for output to the CLI for playing the game.
-}
renderBoard :: [Cell] -> String
renderBoard board = do
  intercalate
    "\n"
    [ renderRow firstRow
    , dividingLine
    , renderRow secondRow
    , dividingLine
    , renderRow thirdRow
    ]
  where
    firstRow = take 3 board
    secondRow = drop 3 . take 6 $ board
    thirdRow = drop 6 board
