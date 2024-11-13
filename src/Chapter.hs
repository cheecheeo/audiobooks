module Chapter where

import Data.Text (Text)
import qualified Data.Text

data Chapter = Chapter {
  title :: !Text,
  start :: !Int,
  end   :: !Int
}

data File = File {
  filename :: !Text,
  duration :: !Int
}

chaptersData :: [Chapter] -> Text
chaptersData = undefined

fileToChapter :: File -> Chapter
fileToChapter = undefined

filesToChapters :: [File] -> [Chapter]
filesToChapters = undefined

-- flac chapter file format
{-
;FFMETADATA1

[CHAPTER]
TIMEBASE=1/1000
START=0
END=180000
title=Introduction

[CHAPTER]
TIMEBASE=1/1000
START=180000
END=360000
title=Main Topic

[CHAPTER]
TIMEBASE=1/1000
START=360000
END=540000
title=Conclusion

#!/bin/bash

echo ";FFMETADATA1" > chapters.txt
start=0

for file in input*.flac; do
  duration=$(ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 "$file")
  duration=${duration%.*}  # Remove decimal part
  end=$((start + duration))

  echo "[CHAPTER]" >> chapters.txt
  echo "TIMEBASE=1/1" >> chapters.txt
  echo "START=$start" >> chapters.txt
  echo "END=$end" >> chapters.txt
  echo "title=${file%.*}" >> chapters.txt
  echo "" >> chapters.txt

  start=$end
done

ffmpeg -i input.flac -i chapters.txt -map_metadata 1 -c copy output.flac
-}
