module Main where

import qualified Test.DocTest

main :: IO ()
main = putStrLn "hello from Main.main" >> Test.DocTest.doctest ["app/"]
