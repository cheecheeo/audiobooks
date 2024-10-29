module Main where

import qualified Control.Monad
import Path ((</>))
import qualified Path
import qualified Path.IO

import Audiobook

main :: IO ()
main = do
    putStrLn "hello from Main.main"
    -- let audioFilesDirectory = (</>) <$> Path.IO.getCurrentDir <*> (Path.parseRelDir "./audio_files")
    -- Control.Monad.void . evalIO . makeM4bFromDir =<< audioFilesDirectory
