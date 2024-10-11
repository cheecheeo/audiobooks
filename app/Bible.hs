{-# LANGUAGE GADTs, LambdaCase, PartialTypeSignatures #-}
module Bible where

import qualified Data.Bool
import Data.List.NonEmpty (NonEmpty(..))

{-
Old Testament:
GENESIS (Gen 1:1 - 50:26)
EXODUS (Ex 1:1 - 40:38)
LEVITICUS (Lev 1:1 - 27:34)
NUMBERS (Num 1:1 - 36:13)
DEUTERONOMY (Deu 1:1 - 34:12)
JOSHUA (Jos 1:1 - 24:33)
JUDGES (Jdg 1:1 - 21:25)
RUTH (Ruth 1:1 - 4:22)
1 SAMUEL (1Sa 1:1 - 31:13)
2 SAMUEL (2Sa 1:1 - 24:25)
1 KINGS (1Ki 1:1 - 22:53)
2 KINGS (2Ki 1:1 - 25:30)
1 CHRONICLES (1Ch 1:1 - 29:30)
2 CHRONICLES (2Ch 1:1 - 36:23)
EZRA (Ezr 1:1 - 10:44)
NEHEMIAH (Neh 1:1 - 13:31)
ESTHER (Est 1:1 - 10:3)
JOB (Job 1:1 - 42:17)
PSALMS (Ps 1:1 - 150:6)
PROVERBS (Pr 1:1 - 31:31)
ECCLESIASTES (Ec 1:1 - 12:14)
SONG OF SOLOMON (So 1:1 - 8:14)
ISAIAH (Isa 1:1 - 66:24)
JEREMIAH (Jer 1:1 - 52:34)
LAMENTATIONS (La 1:1 - 5:22)
EZEKIEL (Eze 1:1 - 48:35)
DANIEL (Da 1:1 - 12:13)
HOSEA (Ho 1:1 - 14:9)
JOEL (Jo 1:1 - 3:21)
AMOS (Am 1:1 - 9:15)
OBADIAH (Ob 1:1 - 1:21)
JONAH (Jon 1:1 - 4:11)
MICAH (Mi 1:1 - 7:20)
NAHUM (Na 1:1 - 3:19)
HABAKKUK (Hab 1:1 - 3:19)
ZEPHANIAH (Zep 1:1 - 3:20)
HAGGAI (Hag 1:1 - 2:19)
ZECHARIAH (Zec 1:1 - 14:21)
MALACHI (Mal 1:1 - 4:6)

New Testament:
MATTHEW (Mt 1:1 - 28:20)
MARK (Mk 1:1 - 16:20)
LUKE (Lk 1:1 - 24:53)
JOHN (Jn 1:1 - 21:25)
ACTS (Ac 1:1 - 28:31)
ROMANS (Ro 1:1 - 16:27)
1 CORINTHIANS (1Co 1:1 - 16:24)
2 CORINTHIANS (2Co 1:1 - 13:14)
GALATIANS (Ga 1:1 - 6:18)
EPHESIANS (Eph 1:1 - 6:24)
PHILIPPIANS (Php 1:1 - 4:23)
COLOSSIANS (Col 1:1 - 4:18)
1 THESSALONIANS (1Th 1:1 - 5:28)
2 THESSALONIANS (2Th 1:1 - 3:18)
1 TIMOTHY (1Ti 1:1 - 6:21)
2 TIMOTHY (2Ti 1:1 - 4:22)
TITUS (Tit 1:1 - 3:15)
PHILEMON (Phm 1:1 - 25)
HEBREWS (Heb 1:1 - 13:25)
JAMES (Jas 1:1 - 5:20)
1 PETER (1Pe 1:1 - 5:14)
2 PETER (2Pe 1:1 - 3:18)
1 JOHN (1Jn 1:1 - 5:21)
2 JOHN (2Jn 1:1 - 13)
3 JOHN (3Jn 1:1 - 15)
JUDE (Jude 1:1 - 25)
REVELATION (Rev 1:1 - 22:21)
-}
data Book =
    Genesis
    | Exodus
    | Leviticus
    | Numbers
    | Deuteronomy
    | Joshua
    | Judges
    | Ruth
    | FirstSamuel
    | SecondSamuel
    | FirstKings
    | SecondKings
    | FirstChronicles
    | SecondChronicles
    | Ezra
    | Nehemiah
    | Esther
    | Job
    | Psalms
    | Proverbs
    | Ecclesiastes
    | SongOfSolomon
    | Isaiah
    | Jeremiah
    | Lamentations
    | Ezekiel
    | Daniel
    | Hosea
    | Joel
    | Amos
    | Obadiah
    | Jonah
    | Micah
    | Nahum
    | Habakkuk
    | Zephaniah
    | Haggai
    | Zechariah
    | Malachi
    | Matthew
    | Mark
    | Luke
    | John
    | Acts
    | Romans
    | FirstCorinthians
    | SecondCorinthians
    | Galatians
    | Ephesians
    | Philippians
    | Colossians
    | FirstThessalonians
    | SecondThessalonians
    | FirstTimothy
    | SecondTimothy
    | Titus
    | Philemon
    | Hebrews
    | James
    | FirstPeter
    | SecondPeter
    | FirstJohn
    | SecondJohn
    | ThirdJohn
    | Jude
    | Revelation
    deriving (Enum, Show)

{-
 - AI generated code
bibleBooks :: [(String, Int)]
bibleBooks = [
    -- Old Testament
    ("Genesis", 50),
    ("Exodus", 40),
    ("Leviticus", 27),
    ("Numbers", 36),
    ("Deuteronomy", 34),
    ("Joshua", 24),
    ("Judges", 21),
    ("Ruth", 4),
    ("1 Samuel", 31),
    ("2 Samuel", 24),
    ("1 Kings", 22),
    ("2 Kings", 25),
    ("1 Chronicles", 29),
    ("2 Chronicles", 36),
    ("Ezra", 10),
    ("Nehemiah", 13),
    ("Esther", 10),
    ("Job", 42),
    ("Psalms", 150),
    ("Proverbs", 31),
    ("Ecclesiastes", 12),
    ("Song of Solomon", 8),
    ("Isaiah", 66),
    ("Jeremiah", 52),
    ("Lamentations", 5),
    ("Ezekiel", 48),
    ("Daniel", 12),
    ("Hosea", 14),
    ("Joel", 3),
    ("Amos", 9),
    ("Obadiah", 1),
    ("Jonah", 4),
    ("Micah", 7),
    ("Nahum", 3),
    ("Habakkuk", 3),
    ("Zephaniah", 3),
    ("Haggai", 2),
    ("Zechariah", 14),
    ("Malachi", 4),
    -- New Testament
    ("Matthew", 28),
    ("Mark", 16),
    ("Luke", 24),
    ("John", 21),
    ("Acts", 28),
    ("Romans", 16),
    ("1 Corinthians", 16),
    ("2 Corinthians", 13),
    ("Galatians", 6),
    ("Ephesians", 6),
    ("Philippians", 4),
    ("Colossians", 4),
    ("1 Thessalonians", 5),
    ("2 Thessalonians", 3),
    ("1 Timothy", 6),
    ("2 Timothy", 4),
    ("Titus", 3),
    ("Philemon", 1),
    ("Hebrews", 13),
    ("James", 5),
    ("1 Peter", 5),
    ("2 Peter", 3),
    ("1 John", 5),
    ("2 John", 1),
    ("3 John", 1),
    ("Jude", 1),
    ("Revelation", 22)
  ]
-}

-- | The total number of chapters from each book of the Bible
chapters :: Book -> Int
chapters = \case
  Genesis -> 50
  Exodus -> 40
  Leviticus -> 27
  Numbers -> 36
  Deuteronomy -> 34
  Joshua -> 24
  Judges -> 21
  Ruth -> 4
  FirstSamuel -> 31
  SecondSamuel -> 24
  FirstKings -> 22
  SecondKings -> 25
  FirstChronicles -> 29
  SecondChronicles -> 36
  Ezra -> 10
  Nehemiah -> 13
  Esther -> 10
  Job -> 42
  Psalms -> 150
  Proverbs -> 31
  Ecclesiastes -> 12
  SongOfSolomon -> 8
  Isaiah -> 66
  Jeremiah -> 52
  Lamentations -> 5
  Ezekiel -> 48
  Daniel -> 12
  Hosea -> 14
  Joel -> 3
  Amos -> 9
  Obadiah -> 1
  Jonah -> 4
  Micah -> 7
  Nahum -> 3
  Habakkuk -> 3
  Zephaniah -> 3
  Haggai -> 2
  Zechariah -> 14
  Malachi -> 4
  Matthew -> 28
  Mark -> 16
  Luke -> 24
  John -> 21
  Acts -> 28
  Romans -> 16
  FirstCorinthians -> 16
  SecondCorinthians -> 13
  Galatians -> 6
  Ephesians -> 6
  Philippians -> 4
  Colossians -> 4
  FirstThessalonians -> 5
  SecondThessalonians -> 3
  FirstTimothy -> 6
  SecondTimothy -> 4
  Titus -> 3
  Philemon -> 1
  Hebrews -> 13
  James -> 5
  FirstPeter -> 5
  SecondPeter -> 3
  FirstJohn -> 5
  SecondJohn -> 1
  ThirdJohn -> 1
  Jude -> 1
  Revelation -> 22

chaptersList :: Book -> NonEmpty Int
chaptersList b = 1 :| (Data.Bool.bool [] ([2 .. chapters b]) (chapters b > 1))

bookOfTheBible :: Book -> Int
bookOfTheBible = succ . fromEnum

display :: Book -> String
display = \case
  FirstSamuel         -> "1Samuel"
  SecondSamuel        -> "2Samuel"
  FirstKings          -> "1Kings"
  SecondKings         -> "2Kings"
  FirstChronicles     -> "1Chronicles"
  SecondChronicles    -> "2Chronicles"
  FirstCorinthians    -> "1Corinthians"
  SecondCorinthians   -> "2Corinthians"
  FirstThessalonians  -> "1Thessalonians"
  SecondThessalonians -> "2Thessalonians"
  FirstTimothy        -> "1Timothy"
  SecondTimothy       -> "2Timothy"
  FirstPeter          -> "1Peter"
  SecondPeter         -> "2Peter"
  FirstJohn           -> "1John"
  SecondJohn          -> "2John"
  ThirdJohn           -> "3John"
  book                -> show book

-- | Take a number of zeros to pad and a number and return a string with `n` padded by `zeros` zeros.
-- >>> zeroPad (-3) 10
-- "10"
-- >>> zeroPad 3 10
-- "010"
-- >>> zeroPad 3 9
-- "009"
-- >>> zeroPad 2 9
-- "09"
-- >>> zeroPad 2 10
-- "10"
-- >>> zeroPad 2 (-10)
-- "-10"
-- >>> zeroPad 2 (-9)
-- "-09"
-- >>> zeroPad 3 (-9)
-- "-009"
zeroPad :: Int -> Int -> String
zeroPad zeros n =
  let predZero = pred zeros
      expnt = max (predZero) 0
  in Data.Bool.bool
      ("-" ++ (zeroPad zeros (negate n)))
      (Data.Bool.bool
        (show n)
        ("0" ++ (zeroPad (predZero) n))
        (n < 10 ^ expnt))
      (n >= 0)

-- | Take a book and an int and zeropod the int appropriately
-- >>> zeroPadBook Psalms 1
-- "001"
-- >>> zeroPadBook Jude 1
-- "01"
-- >>> zeroPadBook John 6
-- "06"
-- >>> zeroPadBook John 12
-- "12"
zeroPadBook :: Book -> Int -> String
zeroPadBook = \case
  Psalms -> zeroPad 3
  _      -> zeroPad 2

books :: [Book]
books = [Genesis .. Revelation]

-- | Book count, name, and zero-padded chapter number as a String
-- >>> bookWithIndexAndChapters Jude
-- (65,"Jude","01") :| []
-- >>> bookWithIndexAndChapters Jonah
-- (32,"Jonah","01") :| [(32,"Jonah","02"),(32,"Jonah","03"),(32,"Jonah","04")]
-- >>> bookWithIndexAndChapters Psalms
-- ...(19,"Psalms","055")...
bookWithIndexAndChapters :: Book -> NonEmpty (Int, String, String)
bookWithIndexAndChapters b = do
  c <- chaptersList b
  return (bookOfTheBible b, display b, zeroPadBook b c)
