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




usage name                   =  unlines
 [ "USAGE: " ++ name ++ " n < some-lines"
 , ""
 , " Shuffles lines into groups of lines $n unique lines."
 ]


main                         =  do
  name                      <-  getProgName
  go name


go name                      =  do
  parsed                    <-  digitize <$> getArgs
  case parsed of
    Left s                  ->  fail s
    Right i                 ->  do
      choices               <-  Set.fromList . no_empty . Bytes.lines <$> b_in
      if Set.size choices < i
        then  fail "Not enough lines to choose from."
        else  do
          (Bytes.hPutStr stdout . Bytes.unlines
                                . (Bytes.unlines . take i <$>)
                                . List.permutations
                                . Set.toList) choices
 where
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


