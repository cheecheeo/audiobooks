{-# LANGUAGE GADTs, LambdaCase, PartialTypeSignatures #-}
module Audiobook where

import qualified Data.Bool
import qualified Data.List
import qualified Data.Semigroup as Semigroup
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
-- import qualified Control.Exception as CException

-- import qualified Control.Monad as CMonad
import Control.Monad.Catch (MonadThrow)
import System.Process (CreateProcess)
import qualified System.Process as Process
import qualified System.Exit
import qualified System.IO
import qualified System.IO.Error
import qualified System.FilePath
import Path (Path, Dir, Abs, File, (</>))
import qualified Path
import qualified Path.IO

import Control.Monad.Operational
    ( singleton,
      view,
      Program,
      ProgramView,
      ProgramViewT(Return, (:>>=)) )

type AudiobookP a = Program AudiobookI a
data AudiobookI a where
  DownloadFromYoutube :: String -> AudiobookI FilePath -- URL and either a file of the download or a directory of the download
  -- MakeM4bFromDir :: Path absOrRel File -> Path Abs File -> AudiobookI Bool -- explicitly a list of input files and output file, success or no?
  MakeM4a :: NonEmpty (Path Abs File) -> Path Abs File -> AudiobookI Bool -- explicitly a list of input files and output file, success or no?
  ListFiles :: Path absOrRel Dir -> AudiobookI [Path Abs File] -- list all the files in a directory
  -- The distinction between these two operations may not be relevant, it might be worthwhile
  -- to just use CopyFile only, see "Rename discussion" in the MakeM4a case of evalIO
  RenameFile :: Path absOrRel1 File -> Path absOrRel2 File -> AudiobookI Bool
  CopyFile :: Path absOrRel1 File -> Path absOrRel2 File -> AudiobookI Bool
  -- UpgradeWithScoop :: AudiobookI ()
  -- UpgradeWithWinget :: AudiobookI ()
  GetYoutubeURL :: AudiobookI String
  GetAudioDirectory :: AudiobookI (NonEmpty String)
  GetDirectory :: AudiobookI FilePath

getYoutubeURL :: AudiobookP String
getYoutubeURL = singleton GetYoutubeURL

downloadFromYoutube :: String -> AudiobookP FilePath
downloadFromYoutube = singleton . DownloadFromYoutube

makeM4a :: NonEmpty (Path Abs File) -> Path Abs File -> AudiobookP Bool
makeM4a infiles = singleton . MakeM4a infiles

listFiles :: Path absOrRel Dir -> AudiobookP [Path Abs File]
listFiles p = Data.List.sort <$> (singleton . ListFiles $ p)

renameFile :: Path absOrRel1 File -> Path absOrRel2 File -> AudiobookP Bool
renameFile fp1 = singleton . RenameFile fp1

copyFile :: Path absOrRel1 File -> Path absOrRel2 File -> AudiobookP Bool
copyFile fp1 = singleton . CopyFile fp1

simpleAudiobookP :: AudiobookP FilePath
simpleAudiobookP =
  do url <- getYoutubeURL
     downloadFromYoutube url

makeM4bFromDir :: Path Abs Dir -> AudiobookP Bool
makeM4bFromDir dir = do
  files <- listFiles dir
  maybe
    (pure False)
    (\filename ->
      maybe
        (pure False)
        (\fs -> do
          makeM4aSuccess <- makeM4a fs filename
          Data.Bool.bool
            (pure False)
            (maybe
              (pure False)
              (renameFile filename)
              (Path.replaceExtension ".m4b" filename))
            makeM4aSuccess) . NE.nonEmpty $ files) . filenameWithExtensionFromDir dir $ ".m4a"

-- | Drop the trailing path separator "/" on Unix from a Path
-- >>> dropTrailingPathSeparator =<< (Path.parseRelDir "foo/bar")
-- "foo/bar"
-- >>> dropTrailingPathSeparator =<< (Path.parseRelDir "bar/")
-- "bar"
dropTrailingPathSeparator :: Path absOrRel Dir -> FilePath
dropTrailingPathSeparator = System.FilePath.dropTrailingPathSeparator . Path.toFilePath

-- | Create a path based on the directory and the given extension
-- >>> Path.parseAbsDir "/home/john/bar/" >>= (\dir -> filenameWithExtensionFromDir dir ".mp4")
-- "/home/john/bar.mp4"
filenameWithExtensionFromDir :: (MonadThrow m) => Path Abs Dir -> String -> m (Path Abs File)
filenameWithExtensionFromDir dir fileExtension =
  Path.addExtension fileExtension =<< (Path.parseAbsFile (dropTrailingPathSeparator dir))

-- | Given an input audio file, create an m4a file and a CreateProcess to convert the input file into
-- an ALAC m4a file.
-- >>> import Control.Monad
-- >>> Path.fileExtension =<< (Path.parseRelFile "./foo.mp3")
-- ".mp3"
-- >>> join (ffmpegAlacCommand <$> (Path.parseAbsDir "/home/foobar/") <*> (Path.parseRelFile "./foo.mp3"))
-- ("/home/foobar/foo.m4a",...ShellCommand "ffmpeg -i \"foo.mp3\" -acodec alac \"/home/foobar/foo.m4a\"", ...
ffmpegAlacCommand :: (MonadThrow m) => Path Abs Dir -> Path absOrRel File -> m (Path Abs File, CreateProcess)
ffmpegAlacCommand dir filename = do
  m4aFile <- Path.replaceExtension ".m4a" (Path.filename filename)
  let filenameWithDir = dir </> m4aFile
  return (filenameWithDir, Process.shell ("ffmpeg -i " ++ show filename ++ " -acodec alac " ++ show filenameWithDir))

-- | Interacalate for NonEmpty.
-- >>> import Data.List.NonEmpty (NonEmpty(..))
-- >>> let x = "hello" :| ["world", "bar"]
-- >>> intercalatishNonEmpty "|" x
-- "hello|world|bar"
intercalatishNonEmpty :: (Semigroup a) => a -> NonEmpty a -> a
intercalatishNonEmpty x = Semigroup.sconcat . NE.intersperse x

-- | Take a NonEmpty list of alac files and create an ffmpeg CreateProcess to combine them all into one file.
-- TODO: The shell command may not need to be quoted???
-- >>> import Data.List.NonEmpty (NonEmpty(..))
-- >>> let infiles = do {infile1 <- Path.parseRelFile "./foo.m4a"; infile2 <- Path.parseRelFile "./bar.m4a"; return $ infile1 :| [infile2]}
-- >>> let outfile = Path.parseRelFile "./out.m4a"
-- >>> do {i <- infiles; o <- outfile; return . Process.cmdspec $ (ffmpegM4bCommand i o)}
-- ShellCommand "ffmpeg -i 'concat:\"foo.m4a\"|\"bar.m4a\"' -c copy \"out.m4a\""
ffmpegM4bCommand :: NonEmpty (Path absOrRel File) -> Path absOrRel File -> CreateProcess
ffmpegM4bCommand alacFiles outfile = Process.shell processString
  where processString =
          concat ["ffmpeg -i 'concat:",
                  (intercalatishNonEmpty "|" . fmap show $ alacFiles),
                  "\' -c copy ",
                  (show outfile)]
   -- "concat:input/Bible/Crossway/ESV-Audio-Bible/file1.mp3|input/Bible/Crossway/ESV-Audio-Bible/file2.mp3|input/Bible/Crossway/ESV-Audio-Bible/file3.mp3"
   -- ffmpeg -i "concat:input/Bible/Crossway/ESV-Audio-Bible/file1.mp3|input/Bible/Crossway/ESV-Audio-Bible/file2.mp3|input/Bible/Crossway/ESV-Audio-Bible/file3.mp3" -c copy output.mp3

data ProcessExitCode =
 ProcessExitCode {
    exitCode :: (System.Exit.ExitCode, String, String)
  } deriving (Show)

-- | Call readCreateProcessWithExitCode on both processes, only if the first succeeds
-- >>> let pwd = Process.shell "pwd"
-- >>> let date = Process.shell "date"
-- >>> andThenProcess pwd date
-- Right (ProcessExitCode ...ExitSuccess...ProcessExitCode ...ExitSuccess...
andThenProcess
  :: CreateProcess ->
     CreateProcess ->
     IO (Either ProcessExitCode (ProcessExitCode, ProcessExitCode))
andThenProcess p1 p2 = do
  p1PEC@(exCode, _, _) <- Process.readCreateProcessWithExitCode p1 ""
  Data.Bool.bool
    (pure . Left . ProcessExitCode $ p1PEC)
    ((\p2PEC -> pure . Right $ (ProcessExitCode p1PEC, ProcessExitCode p2PEC))
      =<< Process.readCreateProcessWithExitCode p2 "")
    (exCode == System.Exit.ExitSuccess)

-- | Mock readCreateProcessWithExitCode
-- >>> mockRCPWEC (Process.shell "ffmpeg -i foo.mp3") "test stdin"
-- I would execute: ShellCommand "ffmpeg -i foo.mp3" with stdin: test stdin
-- ProcessExitCode {exitCode = (ExitSuccess,"mock standard output","mock standard error")}
mockRCPWEC :: CreateProcess -> String -> IO ProcessExitCode
mockRCPWEC cProc stdin = do
  putStrLn $ "I would execute: " ++ (show . Process.cmdspec $ cProc) ++ " with stdin: " ++ stdin
  pure . ProcessExitCode $ (System.Exit.ExitSuccess, "mock standard output", "mock standard error")

processSuccess :: ProcessExitCode -> Bool
processSuccess = (\(ec, _, _) -> ec == System.Exit.ExitSuccess) . exitCode

logError :: String -> IO ()
logError = System.IO.hPutStrLn System.IO.stderr

-- makem4b test case
-- | Evaluate an AudiobookP (program) in Haskell
-- >>> import Path ((</>))
-- >>> System.Process.readCreateProcessWithExitCode (Process.shell "ghci -e '40 + 2'") ""
-- (ExitSuccess,"42\n","")
-- >>> evalIO . listFiles =<< (Path.parseRelDir "./audio_files")
-- ...Columbia-dx1536-cax10357.ogg...Handel_-_messiah_-_02_comfort_ye.ogg...Handel_-_messiah_-_44_hallelujah.ogg...NordwindSonne.wav...
-- >>> traverse print =<< (evalIO . listFiles =<< (Path.parseRelDir "./audio_files"))
-- ...Columbia-dx1536-cax10357.ogg...
-- ...Handel_-_messiah_-_02_comfort_ye.ogg...
-- ...Handel_-_messiah_-_44_hallelujah.ogg...
-- ...NordwindSonne.wav...
-- [(),(),(),()]
-- >>> let audioFilesDirectory = (</>) <$> Path.IO.getCurrentDir <*> (Path.parseRelDir "./audio_files")
-- >>> audioFilesDirectory
-- .../audio_files...
-- >>> evalIO . makeM4bFromDir =<< audioFilesDirectory
-- ...ffmpeg -i ...Columbia-dx1536-cax10357.ogg...Columbia-dx1536-cax10357.m4a...
-- ...ffmpeg -i ...Handel_-_messiah_-_02_comfort_ye.ogg...Handel_-_messiah_-_02_comfort_ye.m4a...
-- ...ffmpeg -i ...Handel_-_messiah_-_44_hallelujah.ogg...Handel_-_messiah_-_44_hallelujah.m4a...
-- ...ffmpeg -i ...NordwindSonne.wav...NordwindSonne.m4a...
-- ...ffmpeg -i 'concat:...Columbia-dx1536-cax10357.m4a...Handel_-_messiah_-_02_comfort_ye.m4a...Handel_-_messiah_-_44_hallelujah.m4a...NordwindSonne.m4a... -c copy ...audio_files.m4a...
-- ...audio_files.m4a...copyFile:atomicCopyFileContents:withReplacementFile:copyFileToHandle:...
-- MakeM4a failed,...
-- False
evalIO :: AudiobookP a -> IO a
evalIO = evalHS . view
  where
    evalHS :: ProgramView AudiobookI a -> IO a
    evalHS = \case
      -- parse URL and ensure a valid youtube.com URL?
      GetYoutubeURL :>>= is -> (getLine >>= (\line -> evalIO (is line)))
      ListFiles fp :>>= is ->
        (Path.IO.listDir fp >>= (\(dirs, files) ->
           if dirs /= []
             then (logError ("non-empty list of directories found in directory: " ++ show fp ++ " directories: " ++ show dirs) >> evalIO (is files))
             else evalIO (is files))) -- ensure that fst of tuple is []
      MakeM4a infiles outfile :>>= is -> Path.IO.withSystemTempDir "AudiobookTempDir" (\tempDir -> do
        outFileM4aExists <- Path.IO.doesFileExist outfile
        Data.Bool.bool (do
            -- convert each infile to alac
            (files, processes) <- fmap NE.unzip . traverse (ffmpegAlacCommand tempDir) $ infiles
            executedProcesses <- traverse (\process -> mockRCPWEC process "") processes
            Data.Bool.bool
              (logError ("ffmpegAlacCommand failed: " ++ (show executedProcesses)) >> evalIO (is False))
              (do
                -- create the alac file in the temporary directory
                let tempOutfilename = tempDir </> (Path.filename outfile)
                executedM4bCommandProcess <- mockRCPWEC (ffmpegM4bCommand files tempOutfilename) ""
                -- move the alac file from the temporary directory to destination
                -- Rename discussion. We need to copyFile rather than renameFile because the temporary
                -- directory filesystem is often a different filesystem than the rest of the filesystem.
                -- https://search.brave.com/search?q=unsupported+operation+(Invalid+cross-device+link+wsl&source=desktop
                copySuccess <- evalIO $ copyFile tempOutfilename outfile
                Data.Bool.bool
                  (logError ("MakeM4a failed, executedM4bCommandProcess or copyFile: " ++ (show executedM4bCommandProcess)) >> evalIO (is False))
                  (evalIO (is True))
                  (processSuccess executedM4bCommandProcess && copySuccess))
              (and . fmap processSuccess $ executedProcesses))
          (logError ("file already exists. outfile: " ++ (show outfile)) >> evalIO (is False))
          outFileM4aExists)
      RenameFile p1 p2 :>>= is -> do
        p2Exists <- Path.IO.doesFileExist p2
        Data.Bool.bool
          (System.IO.Error.catchIOError
            (Path.IO.renameFile p1 p2
              >> evalIO (is True))
            (\e -> (logError .  show $ e)
              >> evalIO (is False)))
          (evalIO (is False))
          p2Exists
      CopyFile p1 p2 :>>= is -> do
        p2Exists <- Path.IO.doesFileExist p2
        Data.Bool.bool
          (System.IO.Error.catchIOError
            (Path.IO.copyFile p1 p2
              >> evalIO (is True))
            (\e -> (logError .  show $ e)
              >> evalIO (is False)))
          (evalIO (is False))
          p2Exists
      Return x -> return x

-- generate a shell script that when run will execute the expected instructions
generateShell :: AudiobookP a -> (() -> (a, String))
generateShell = undefined

-- TODO:
-- test to see if we even combine it first
--
-- write some unit tests and integration tests in hspec
-- https://hspec.github.io/
--
-- TTS libraries to check out (in order)
-- https://github.com/idiap/coqui-ai-TTS https://coqui-tts.readthedocs.io/en/stable/tutorial_for_nervous_beginners.html
-- https://gspeech.io/
-- https://cloud.google.com/text-to-speech
--
-- stat the directory, if it's a single file, rename accordingly, if it's multiple files return the directory
-- then combine them into an audiobook
-- algorithm - common prefix treat as Title (strip punctuation (non-letters)), common substrings treat as author, or just something to append to title
-- example to write a test for:
{-
[nix-shell:~/packages/audiobooks]$ ll *m4a
-rw-r--r-- 1 chee1 chee1 68313041 Mar  9  2024 'Christianity and Liberalism： Christ - J. Gresham Machen (Chapter 5 of 7) [14VVtubP08o].m4a'
-rw-r--r-- 1 chee1 chee1 68559066 Aug 25 18:14 'Christianity and Liberalism： Doctrine - J. Gresham Machen (Chapter 2 of 7) [OOXcncVQRB4].m4a'
-rw-r--r-- 1 chee1 chee1 26273734 Nov 24  2013 'Christianity and Liberalism： God and Man - J. Gresham Machen (Chapter 3 of 7) [hLvYFC8xWZ8].m4a'
-rw-r--r-- 1 chee1 chee1 25828355 Mar 22  2014 'Christianity and Liberalism - J. Gresham Machen (Intro ⧸ Chapter 1 of 7) [z_rBPUFf6q0].m4a'
-rw-r--r-- 1 chee1 chee1 77290163 Sep 10  2013 'Christianity and Liberalism： Salvation - J. Gresham Machen (Chapter 6 of 7) [ssAHVCeLaEs].m4a'
-rw-r--r-- 1 chee1 chee1 18530350 Sep 13 06:05 'Christianity and Liberalism： The Bible - J. Gresham Machen (Chapter 4 of 7) [T-Jg-VYtX5Y].m4a'
-rw-r--r-- 1 chee1 chee1 44284696 Apr  4  2024 'Christianity and Liberalism： The Church - J. Gresham Machen (Chapter 7 of 7) [99CP77eX9Wk].m4a'
-}
--
-- CI with github
-- command line interface
-- download audiobooks from youtube: https://github.com/ytdl-org/youtube-dl?tab=readme-ov-file
-- make a tempdir
-- cd tempdir
-- yt-dlp -f "ba" https://youtube.com/playlist?list=PL62C4D2D718F07779&si=drIVnM7aqSq7-Klg
-- convert text to audiobook. book: https://etc.usf.edu/lit2go/68/fairy-tales-and-other-traditional-stories/5085/the-golden-goose/
-- windows text to voice bindings ^
-- use playwright from Haskell/Operational
-- download NET, ESV bible
-- download progress bar
-- upload m4bs to plex
-- transfer m4bs from plex to android
-- call unision from Haskell
-- upgrade from haskell: scoop, winget
-- nix-home-manager
-- flake
-- DONE:
-- download from youtube: https://search.nixos.org/packages?channel=unstable&show=yt-dlp&from=0&size=50&sort=relevance&type=packages&query=yt-dlp
-- operational monad
-- find best audiobook app on android
-- sirin: https://play.google.com/store/apps/details?id=com.sirin.android&hl=en_US&pli=1
-- make mp3s into m4b
-- ffmpeg -i file.mp3 -acodec alac file.m4a -- converting to alac first seems legit, chapters are nice in vlc and audio is clean
-- then mv file.m4a file.m4b
-- figure out how to make an audiobook from 3 mp3s first
-- ffmpeg -i "concat:input/Bible/Crossway/ESV-Audio-Bible/file1.mp3|input/Bible/Crossway/ESV-Audio-Bible/file2.mp3|input/Bible/Crossway/ESV-Audio-Bible/file3.mp3" -c copy output.mp3
-- https://gist.github.com/butuzov/fa7d456ebc3ec0493c0a10b73800bf42
