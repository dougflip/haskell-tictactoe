module TicTacToeCoreSpec
  ( spec
  ) where

import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Test.Hspec
import           TestUtils          (isError, playGame, toGameString)
import           TicTacToeCore

spec :: Spec
spec = do
  describe "TicTacToeCore" $ do
    describe "basic single turn" $ do
      it "allows a single X move and switches turns" $ do
        toGameString (playGame X (pure 5)) `shouldBe` "O: ____X____"
      it "allows a single O move and switches turns" $ do
        toGameString (playGame O (pure 3)) `shouldBe` "X: __O______"
    describe "winning patterns" $ do
      it "wins with 1, 2, 3" $ do
        let result = playGame X (1 :| [4, 2, 5, 3])
        toGameString result `shouldBe` "Winner X: XXXOO____"
      it "wins with 4, 5, 6" $ do
        let result = playGame O (4 :| [1, 5, 2, 6])
        toGameString result `shouldBe` "Winner O: XX_OOO___"
      it "wins with 7, 8, 9" $ do
        let result = playGame X (7 :| [1, 8, 2, 9])
        toGameString result `shouldBe` "Winner X: OO____XXX"
      it "wins with 1, 4, 7" $ do
        let result = playGame X (1 :| [5, 4, 2, 7])
        toGameString result `shouldBe` "Winner X: XO_XO_X__"
      it "wins with 2, 5, 8" $ do
        let result = playGame X (2 :| [1, 5, 4, 8])
        toGameString result `shouldBe` "Winner X: OX_OX__X_"
      it "wins with 3, 6, 9" $ do
        let result = playGame X (3 :| [2, 6, 5, 9])
        toGameString result `shouldBe` "Winner X: _OX_OX__X"
      it "wins with 1, 5, 9" $ do
        let result = playGame X (1 :| [2, 5, 3, 9])
        toGameString result `shouldBe` "Winner X: XOO_X___X"
      it "wins with 3, 5, 7" $ do
        let result = playGame X (3 :| [1, 5, 2, 7])
        toGameString result `shouldBe` "Winner X: OOX_X_X__"
    describe "play until a tie" $ do
      it "fills all available spaces" $ do
        let result = playGame X (1 :| [3, 2, 4, 5, 8, 6, 9, 7])
        toGameString result `shouldBe` "Tie: XXOOXXXOO"
    describe "statuses that are completed" $ do
      it "maintains a winner" $ do
        let result = playGame X (3 :| [1, 5, 2, 7, 6, 9])
        toGameString result `shouldBe` "Winner X: OOX_X_X__"
      it "maintains a tie" $ do
        let result = playGame X (1 :| [3, 2, 4, 5, 8, 6, 9, 7, 1, 2, 3])
        toGameString result `shouldBe` "Tie: XXOOXXXOO"
    describe "errors" $ do
      it "protects against a cell value that is too low" $ do
        playGame X (pure 0) `shouldSatisfy` isError
      it "protects against a cell value that is too high" $ do
        playGame X (pure 10) `shouldSatisfy` isError
      it "alerts when a space is already occupied" $ do
        let result = playGame X (1 :| [1])
        result `shouldSatisfy` isError
