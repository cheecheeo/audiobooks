module AudiobookSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "Prelude" $ do
    describe "read" $ do
      it "can parse integers" $ do
        read "10" `shouldBe` (10 :: Int)

    describe "head" $ do
      it "returns the first element of a list" $ do
        head [23 ..] `shouldBe` 23
