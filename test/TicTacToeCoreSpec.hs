module TicTacToeCoreSpec
  ( spec
  ) where

import           Test.Hspec
import           TicTacToeCore (GameBoard, PlayerMovePosition,
                                PlayerPiece (O, X), applyMove, isGameComplete,
                                newGameBoard)

applyMultipleMoves ::
     GameBoard -> [(PlayerPiece, PlayerMovePosition)] -> GameBoard
applyMultipleMoves startBoard moves =
  foldr
    (\(piece, position) board -> applyMove piece position board)
    startBoard
    moves

spec :: Spec
spec = do
  describe "TicTacToeCore" $ do
    it "verifies a new game board is not a winner" $ do
      isGameComplete newGameBoard `shouldBe` False
    it "recognizes a winner" $ do
      isGameComplete (applyMultipleMoves newGameBoard [(X, 1), (X, 2), (X, 3)]) `shouldBe`
        True
