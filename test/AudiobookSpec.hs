{-# LANGUAGE GADTs #-}
module AudiobookSpec where

import Control.Monad.Operational
  (ProgramViewT(Return, (:>>=)))
import qualified Control.Monad.Operational
import qualified Data.Either
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty
import Path (Path, Abs, File, (</>))
import qualified Path
import qualified Path.IO
import System.Process(CreateProcess(..), StdStream(..), CmdSpec(..))
import qualified System.Process
import Test.QuickCheck (Arbitrary)
import qualified Test.QuickCheck

import Audiobook

import Test.Hspec
import Test.Hspec.QuickCheck

import Debug.Trace

newtype FileString = FileString { getFile :: String }
  deriving (Show)

instance Arbitrary FileString where
  arbitrary = do
    let parts = Test.QuickCheck.elements ["foo", "bar", "baz", "fee", "fie", "foe", "fum"]
    let exts = Test.QuickCheck.elements ["c", "ogg", "h", "wav", "m4b", "mp3", "opus"]
    p1 <- parts
    p2 <- parts
    p3 <- parts
    ext <- exts
    return . FileString $ ("/" ++ p1 ++ "/" ++ p2 ++ "/" ++ p3 ++ "." ++ ext)

spec :: Spec
spec = do
  describe "view of makeM4a" $ do
    it "will have an outfile with m4a for input files of length 1" $ do
      audioFile <- Path.parseAbsFile "/foo/bar/baz.ogg"
      outFile <- Path.parseAbsFile "/foo/bar/fee.ogg"
      outFile1 <- Path.parseAbsFile "/foo/bar/fee.m4a"
      let infiles = audioFile :| []
      maybe
        (expectationFailure "Nothing in makeM4a test")
        (\makeM4aJ -> case Control.Monad.Operational.view makeM4aJ of
          MakeM4a infiles outFile2 :>>= k -> outFile2 `shouldBe` outFile1)
        (makeM4a infiles outFile)
    it "will have an outfile with m4a for input files of length 3" $ do
      audioFile1 <- Path.parseAbsFile "/foo/bar/baz.ogg"
      audioFile2 <- Path.parseAbsFile "/foo/bar/bat.ogg"
      audioFile3 <- Path.parseAbsFile "/foo/bar/bax.ogg"
      outFile <- Path.parseAbsFile "/foo/bar/fee.ogg"
      outFile1 <- Path.parseAbsFile "/foo/bar/fee.m4a"
      let infiles = audioFile1 :| [audioFile2, audioFile3]
      maybe
        (expectationFailure "Nothing in makeM4a test")
        (\makeM4aJ -> case Control.Monad.Operational.view makeM4aJ of
          MakeM4a infiles outFile2 :>>= k -> outFile2 `shouldBe` outFile1)
        (makeM4a infiles outFile)
    prop "will have an outfile with m4a for input files of arbitrary length" $
      \(f, fs, outF) -> do
        outFile <- Path.parseAbsFile . getFile $ outF
        infiles <- traverse (Path.parseAbsFile . getFile) (f :| fs)
        maybe
          (expectationFailure "Nothing in makeM4a test")
          (\makeM4aJ -> case Control.Monad.Operational.view makeM4aJ of
            MakeM4a infiles outFile1 :>>= k -> (Path.fileExtension outFile1) `shouldBe` (Just ".m4a"))
          (makeM4a infiles outFile)

  describe "ffmpegConcatCommand" $ do
    it "will create a process for one file" $ do
      audioFile <- Path.parseAbsFile "/foo/bar/baz.ogg"
      outFile <- Path.parseAbsFile "/foo/bar/fee.m4a"
      -- print $ ffmpegConcatCommand (audioFile :| []) outFile
      ffmpegConcatCommand (audioFile :| []) outFile `shouldReturn` CreateProcess {cmdspec = ShellCommand "ffmpeg -i \"/foo/bar/baz.ogg\" -filter_complex \"[0:0]concat=n=1:v=0:a=1[outa]\" -map \"[outa]\" -acodec alac \"/foo/bar/fee.m4a\"", cwd = Nothing, env = Nothing, std_in = Inherit, std_out = Inherit, std_err = Inherit, close_fds = False, create_group = False, delegate_ctlc = False, detach_console = False, create_new_console = False, new_session = False, child_group = Nothing, child_user = Nothing, use_process_jobs = False}
    -- TODO don't need the tempdir here, not doing any writing, just generating CreateProcesses
    it "will create a process for audio_files" $ Path.IO.withSystemTempDir "AudiobookSpec" $ (\dir -> do
      outFile <- Path.parseAbsFile "/foo/bar/out.m4a"
      audioFilesDirectory <- (</>) <$> Path.IO.getCurrentDir <*> (Path.parseRelDir "./audio_files")
      (_subdirs, audioFiles) <- Path.IO.listDir audioFilesDirectory
      maybe
        (expectationFailure "empty audioFiles")
        (\fs -> ffmpegConcatCommand fs outFile `shouldReturn` CreateProcess {cmdspec = ShellCommand "ffmpeg -i \"/home/chee1/packages/audiobooks/audio_files/Columbia-dx1536-cax10357.ogg\" -i \"/home/chee1/packages/audiobooks/audio_files/NordwindSonne.wav\" -i \"/home/chee1/packages/audiobooks/audio_files/Handel_-_messiah_-_02_comfort_ye.ogg\" -i \"/home/chee1/packages/audiobooks/audio_files/Handel_-_messiah_-_44_hallelujah.ogg\" -filter_complex \"[0:0][1:0][2:0][3:0]concat=n=4:v=0:a=1[outa]\" -map \"[outa]\" -acodec alac \"/foo/bar/out.m4a\"", cwd = Nothing, env = Nothing, std_in = Inherit, std_out = Inherit, std_err = Inherit, close_fds = False, create_group = False, delegate_ctlc = False, detach_console = False, create_new_console = False, new_session = False, child_group = Nothing, child_user = Nothing, use_process_jobs = False})
        (Data.List.NonEmpty.nonEmpty audioFiles)
      -- print $ ffmpegConcatCommand (audioFile :| []) outFile
      --ffmpegConcatCommand (audioFile :| []) outFile `shouldBe` CreateProcess {cmdspec = ShellCommand "ffmpeg -i \"/foo/bar/baz.ogg\" -filter_complex \"[0:0]concat=n=1:v=0:a=1[outa]\" -map \"[outa]\" -acodec alac \"/foo/bar/fee.m4a\"", cwd = Nothing, env = Nothing, std_in = Inherit, std_out = Inherit, std_err = Inherit, close_fds = False, create_group = False, delegate_ctlc = False, detach_console = False, create_new_console = False, new_session = False, child_group = Nothing, child_user = Nothing, use_process_jobs = False}
      )

  describe "makeM4bFromDir" $ do
    it "can make a file from ./audio_files" $ do
      audioFilesDirectory <- (</>) <$> Path.IO.getCurrentDir <*> (Path.parseRelDir "./audio_files")
      -- (evalIO . makeM4bFromDir $ audioFilesDirectory) `shouldReturn` True
      pending
    it "can make a file from <a single audio file>" $ do
      pending
  describe "andThenProcess" $ do
    it "can run pwd then date" $ do
      let pwd = System.Process.shell "pwd"
      let date = System.Process.shell "date"
      andThenProcess pwd date >>= (`shouldSatisfy` Data.Either.isRight)
      -- (traceShowId <$> (andThenProcess pwd date)) >>= (`shouldSatisfy` Data.Either.isRight)
      -- andThenProcess pwd date `shouldReturn` Right (ProcessExitCode {exitCode = (ExitSuccess,"/home/chee1/packages/audiobooks\n","")},ProcessExitCode {exitCode = (ExitSuccess,"Thu Oct 24 01:51:25 PM PDT 2024\n","")})
