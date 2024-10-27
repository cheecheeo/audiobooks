module AudiobookSpec where

import qualified Data.Either
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty
import Path ((</>))
import qualified Path
import qualified Path.IO
import System.Process(CreateProcess(..), StdStream(..), CmdSpec(..))
import qualified System.Process

import Audiobook

import Test.Hspec

import Debug.Trace

spec :: Spec
spec = do
  describe "ffmpegConcatCommand" $ do
    it "will create a process for one file" $ do
      audioFile <- Path.parseAbsFile "/foo/bar/baz.ogg"
      outFile <- Path.parseAbsFile "/foo/bar/fee.m4a"
      -- print $ ffmpegConcatCommand (audioFile :| []) outFile
      ffmpegConcatCommand (audioFile :| []) outFile `shouldBe` CreateProcess {cmdspec = ShellCommand "ffmpeg -i \"/foo/bar/baz.ogg\" -filter_complex \"[0:0]concat=n=1:v=0:a=1[outa]\" -map \"[outa]\" -acodec alac \"/foo/bar/fee.m4a\"", cwd = Nothing, env = Nothing, std_in = Inherit, std_out = Inherit, std_err = Inherit, close_fds = False, create_group = False, delegate_ctlc = False, detach_console = False, create_new_console = False, new_session = False, child_group = Nothing, child_user = Nothing, use_process_jobs = False}
    -- TODO don't need the tempdir here, not doing any writing, just generating CreateProcesses
    it "will create a process for audio_files" $ Path.IO.withSystemTempDir "AudiobookSpec" $ (\dir -> do
      outFile <- Path.parseAbsFile "/foo/bar/out.m4a"
      audioFilesDirectory <- (</>) <$> Path.IO.getCurrentDir <*> (Path.parseRelDir "./audio_files")
      (_subdirs, audioFiles) <- Path.IO.listDir audioFilesDirectory
      maybe
        (expectationFailure "empty audioFiles")
        (\fs -> ffmpegConcatCommand fs outFile `shouldBe` CreateProcess {cmdspec = ShellCommand "ffmpeg -i \"/home/chee1/packages/audiobooks/audio_files/Columbia-dx1536-cax10357.ogg\" -i \"/home/chee1/packages/audiobooks/audio_files/NordwindSonne.wav\" -i \"/home/chee1/packages/audiobooks/audio_files/Handel_-_messiah_-_02_comfort_ye.ogg\" -i \"/home/chee1/packages/audiobooks/audio_files/Handel_-_messiah_-_44_hallelujah.ogg\" -filter_complex \"[0:0][1:0][2:0][3:0]concat=n=4:v=0:a=1[outa]\" -map \"[outa]\" -acodec alac \"/foo/bar/out.m4a\"", cwd = Nothing, env = Nothing, std_in = Inherit, std_out = Inherit, std_err = Inherit, close_fds = False, create_group = False, delegate_ctlc = False, detach_console = False, create_new_console = False, new_session = False, child_group = Nothing, child_user = Nothing, use_process_jobs = False})
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
