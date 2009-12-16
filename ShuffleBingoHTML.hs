#!/usr/bin/env runhaskell


import System.IO (stdin, stderr, stdout)
import Control.Applicative
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as Bytes (hPutStr)
import qualified Data.ByteString.Lazy.Char8 as Bytes
import qualified Data.Char as Char
import System.Environment
import System.IO
import System.Exit

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar




n                            =  24


usage name                   =  unlines
 [ "USAGE: " ++ name ++ " count < some-lines"
 , ""
 , " Shuffles lines into groups of "
      ++ show n ++ " unique lines, then puts them into"
 , " an HTML table that is 5x5 and has an empty square in the middle."
 , " You specify $count to get only that many files in the output. The"
 , " is a streaming TAR archive."
 ]

 
main                         =  do
  name                      <-  getProgName
  go name


go name                      =  do
  parsed                    <-  digitize <$> getArgs
  case parsed of
    Left s                  ->  fail s
    Right count             ->  do
      choices               <-  Set.fromList . no_empty . Bytes.lines <$> b_in
      if Set.size choices < n
        then  fail "Not enough lines to choose from."
        else  do
          tar_out (name_nums choices) (render <$> block_out count choices)
 where
  tar_out names              =  (Bytes.hPutStr . Tar.write . tars dir names)
  Right dir                  =  toTarPath True "shuffle-bingo-html-output"
  name_nums how_many         =  name <$> [0..(how_many-1)]
   where
    name num                 =  p
     where
      Right p                =  toTarPath
                                 (dir ++ "/" ++ printf (digits ++ ".html") num)
    digits                   =  "%0" ++ (show . length . show) how_many ++ "d"
  no_empty                   =  filter (not . Bytes.all Char.isSpace)
  fail s                     =  do
    hPutStrLn stderr s
    hPutStrLn stderr ""
    hPutStrLn stderr (usage name)
    exitFailure
  b_in                       =  Bytes.hGetContents stdin
  digitize [s]               =  case reads s of
    [(i,"")]                ->  if i > 0 then Right i else Left "Non-positive."
    _                       ->  Left "Argument error."
  digitize _                 =  Left "Wrong number of args."


block_out choose choices     =  (combinations choose . Set.toList) choices
 where
  combinations 0 _           =  [ [] ]
  combinations i a           =  [ y:ys | y:b  <-  tails a
                                       , ys   <-  combinations (i-1) b ]


render                       =  Bytes.unlines


tars dir names contents      =  Tar.directoryEntry dir :
  [ Tar.fileEntry name content | name <- names | content <- contents ]

