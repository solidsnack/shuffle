#!/usr/bin/env runhaskell


import Data.Word
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as Bytes (hPutStr)
import qualified Data.ByteString.Lazy.Char8 as Bytes
import qualified Data.Char as Char
import Control.Applicative
import System.Environment
import System.IO
import System.Exit
import Text.Printf
import System.Posix.Time as Posix

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
 , " output is a streaming TAR archive."
 ]

 
main                         =  do
  name                      <-  getProgName
  args                      <-  getArgs
  time                      <-  floor . realToFrac <$> Posix.epochTime
  if List.any (`elem` args) ["-h","-?","--help"]
    then  hPutStrLn stdout (usage name) >> exitSuccess
    else  go name args time


go name args time            =  do
  case digitize args of
    Left s                  ->  fail s
    Right count             ->  do
      choices               <-  Set.fromList . no_empty . Bytes.lines <$> b_in
      if Set.size choices < n
        then  fail "Not enough lines to choose from."
        else  do
          (Bytes.hPutStr stdout . tar (name_nums count))
            (render <$> block_out count choices)
 where
  tar names contents         =  Tar.write (tars time dir' names contents)
  dir                        =  "shuffle-bingo-html-output"
  Right dir'                 =  Tar.toTarPath True "shuffle-bingo-html-output"
  name_nums how_many         =  name <$> [0..(how_many-1)]
   where
    name num                 =  p
     where
      Right p                =  Tar.toTarPath False
                                 (dir ++ "/" ++ printf (digits ++ ".html") num)
    digits                   =  "%0" ++ (show . length . show) how_many ++ "d"
  no_empty                   =  filter (not . Bytes.all Char.isSpace)
  fail s                     =  do
    hPutStrLn stderr s
    hPutStrLn stderr ""
    hPutStrLn stderr (usage name)
    exitFailure
  b_in                       =  Bytes.hGetContents stdin
  digitize                  ::  [String] -> Either String Word
  digitize [s]               =  case reads s of
    [(i,"")]                ->  if i > 0 then Right i else Left "Non-positive."
    _                       ->  Left "Argument error."
  digitize _                 =  Left "Wrong number of args."


block_out                   ::  Word -> Set.Set t -> [[t]]
block_out choose choices     =  (combinations choose . Set.toList) choices
 where
  combinations 0 _           =  [[]]
  combinations _ []          =  []
  combinations k (x:xs) = map (x:) (combinations (k-1) xs) ++ combinations k xs


render texts                 =  Bytes.unlines
  [ Bytes.pack "<table> <tbody>"
  , tr (pick [0..4])
  , tr (pick [5..9])
  , tr (pick [10..11] ++ [place_image] ++ pick [12..13])
  , tr (pick [14..18])
  , tr (pick [19..24])
  , Bytes.pack "</table> </tbody>"
  ]
 where
  tr elems                   =  Bytes.unlines ([Bytes.pack "<tr>"] ++
                                                 (td <$> elems)
                                            ++ [Bytes.pack "</tr>"])
  td text                    =  Bytes.unwords
                                  [Bytes.pack "<td>", text, Bytes.pack "</td>"]
  pick                       =  ((texts !!) <$>)
  place_image                =  Bytes.pack "<!-- Place image here. -->"


tars t dir names contents    =  t_set (Tar.directoryEntry dir) :
  [ t_set (Tar.fileEntry name content) | name <- names | content <- contents ]
 where
  t_set entry                =  entry {Tar.entryTime =  t}

