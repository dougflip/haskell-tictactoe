module TicTacToeCoreSpec
  ( spec
  ) where

import           Test.Hspec
import           TicTacToeCore (Game, PlayerMovePosition (..), PlayerPiece (..),
                                applyTurn, isGameComplete, newGame)

playGame :: [PlayerMovePosition] -> Game
playGame moves = foldr (\position game -> applyTurn position game) newGame moves

spec :: Spec
spec = do
  describe "TicTacToeCore" $ do
    describe "non winning patterns" $ do
      it "verifies a new game board is not a winner" $ do
        isGameComplete (playGame []) `shouldBe` False
      it
        "verifies a winning pattern is NOT a winner if the players are different" $ do
        isGameComplete (playGame [One, Two, Three]) `shouldBe` False
    describe "winning patterns" $ do
      it "a 1,2,3 winner" $ do
        isGameComplete (playGame [One, Four, Two, Five, Three]) `shouldBe` True
      it "a 4,5,6 winner" $ do
        isGameComplete (playGame [Four, One, Five, Two, Six]) `shouldBe` True
      it "a 7,8,9 winner" $ do
        isGameComplete (playGame [Seven, One, Eight, Two, Nine]) `shouldBe` True
      it "a 1,4,7 winner" $ do
        isGameComplete (playGame [One, Five, Four, Two, Seven]) `shouldBe` True
      it "a 2,5,8 winner" $ do
        isGameComplete (playGame [Two, One, Five, Six, Eight]) `shouldBe` True
      it "a 3,6,9 winner" $ do
        isGameComplete (playGame [Three, One, Six, Five, Nine]) `shouldBe` True
      it "a 1,5,9 winner" $ do
        isGameComplete (playGame [One, Two, Five, Six, Nine]) `shouldBe` True
      it "a 3,5,7 winner" $ do
        isGameComplete (playGame [Three, Two, Five, Six, Seven]) `shouldBe` True
      it "O can be a winner" $ do
        isGameComplete (playGame [Nine, One, Eight, Two, Four, Three]) `shouldBe`
          True
