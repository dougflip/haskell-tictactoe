module TicTacToeCoreSpec
  ( spec
  ) where

import           Test.Hspec
import           TicTacToeCore (GameBoard, PlayerMovePosition (..),
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
        isGameComplete (newGame [(X, One), (O, Two), (X, Three)]) `shouldBe`
          False
    describe "winning patterns" $ do
      it "a 1,2,3 winner" $ do
        isGameComplete
          (newGame [(X, One), (O, Four), (X, Two), (O, Five), (X, Three)]) `shouldBe`
          True
      it "a 4,5,6 winner" $ do
        isGameComplete
          (newGame [(X, Four), (O, One), (X, Five), (O, Two), (X, Six)]) `shouldBe`
          True
      it "a 7,8,9 winner" $ do
        isGameComplete
          (newGame [(X, Seven), (O, One), (X, Eight), (O, Two), (X, Nine)]) `shouldBe`
          True
      it "a 1,4,7 winner" $ do
        isGameComplete
          (newGame [(X, One), (O, Five), (X, Four), (O, Two), (X, Seven)]) `shouldBe`
          True
      it "a 2,5,8 winner" $ do
        isGameComplete
          (newGame [(X, Two), (O, One), (X, Five), (O, Six), (X, Eight)]) `shouldBe`
          True
      it "a 3,6,9 winner" $ do
        isGameComplete
          (newGame [(X, Three), (O, One), (X, Six), (O, Five), (X, Nine)]) `shouldBe`
          True
      it "a 1,5,9 winner" $ do
        isGameComplete
          (newGame [(X, One), (O, Two), (X, Five), (O, Six), (X, Nine)]) `shouldBe`
          True
      it "a 3,5,7 winner" $ do
        isGameComplete
          (newGame [(X, Three), (O, Two), (X, Five), (O, Six), (X, Seven)]) `shouldBe`
          True
