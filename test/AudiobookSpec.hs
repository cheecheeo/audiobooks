module AudiobookSpec where

import qualified Data.Either
import qualified System.Process

import Audiobook

import Test.Hspec

import Debug.Trace

spec :: Spec
spec = do
  describe "Audiobook" $ do
    describe "andThenProcess" $ do
      it "can run pwd then date" $ do
        let pwd = System.Process.shell "pwd"
        let date = System.Process.shell "date"
        (traceShowId <$> (andThenProcess pwd date)) >>= (`shouldSatisfy` Data.Either.isRight)


  describe "Prelude" $ do
    describe "read" $ do
      it "can parse integers" $ do
        read "10" `shouldBe` (10 :: Int)

    describe "head" $ do
      it "returns the first element of a list" $ do
        head [23 ..] `shouldBe` 23
