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

newGame :: [(PlayerPiece, PlayerMovePosition)] -> GameBoard
newGame = applyMultipleMoves newGameBoard

spec :: Spec
spec = do
  describe "TicTacToeCore" $ do
    describe "non winning patterns" $ do
      it "verifies a new game board is not a winner" $ do
        isGameComplete newGameBoard `shouldBe` False
      it
        "verifies a winning pattern is NOT a winner if the players are different" $ do
        isGameComplete (newGame [(X, 1), (O, 2), (X, 3)]) `shouldBe` False
    describe "winning patterns" $ do
      it "a 1,2,3 winner" $ do
        isGameComplete (newGame [(X, 1), (O, 4), (X, 2), (O, 5), (X, 3)]) `shouldBe`
          True
      it "a 4,5,6 winner" $ do
        isGameComplete (newGame [(X, 4), (O, 1), (X, 5), (O, 2), (X, 6)]) `shouldBe`
          True
      it "a 7,8,9 winner" $ do
        isGameComplete (newGame [(X, 7), (O, 1), (X, 8), (O, 2), (X, 9)]) `shouldBe`
          True
      it "a 1,4,7 winner" $ do
        isGameComplete (newGame [(X, 1), (O, 5), (X, 4), (O, 2), (X, 7)]) `shouldBe`
          True
      it "a 2,5,8 winner" $ do
        isGameComplete (newGame [(X, 2), (O, 1), (X, 5), (O, 6), (X, 8)]) `shouldBe`
          True
      it "a 3,6,9 winner" $ do
        isGameComplete (newGame [(X, 3), (O, 1), (X, 6), (O, 5), (X, 9)]) `shouldBe`
          True
      it "a 1,5,9 winner" $ do
        isGameComplete (newGame [(X, 1), (O, 2), (X, 5), (O, 6), (X, 9)]) `shouldBe`
          True
      it "a 3,5,7 winner" $ do
        isGameComplete (newGame [(X, 3), (O, 2), (X, 5), (O, 6), (X, 7)]) `shouldBe`
          True
