#!/usr/bin/env stack
-- stack --resolver nightly-2017-11-14 script --package random-shuffle --package random --package directory --package time

import           Control.Monad         (forM_)
import           Data.List             (findIndex)
import           Data.Maybe            (mapMaybe)
import           Data.Time
import           System.Directory      (doesFileExist)
import           System.Environment    (getArgs)
import           System.IO
import           System.Random
import           System.Random.Shuffle

readUTF8File :: FilePath -> IO String
readUTF8File p = withFile p ReadMode $ \h -> do
  hSetEncoding h utf8
  contents <- hGetContents h
  foldr seq (pure contents) contents

writeUTF8File :: FilePath -> String -> IO ()
writeUTF8File p contents = withFile p WriteMode $ \h -> do
  hSetEncoding h utf8
  hPutStr h contents

appendUTF8File :: FilePath -> String -> IO ()
appendUTF8File p contents = withFile p AppendMode $ \h -> do
  hSetEncoding h utf8
  hPutStr h contents

parseLSV :: IO [String]
parseLSV = lines <$> readUTF8File "names.txt"

seedFile :: FilePath
seedFile = "seed_words.txt"

readSeed :: IO StdGen
readSeed = do
  exists <- doesFileExist seedFile
  if exists
    then read <$> readUTF8File seedFile
    else getStdGen

writeSeed :: StdGen -> IO ()
writeSeed = writeUTF8File seedFile . show

assignWords :: [String] -> [String] -> IO [(String, String)]
assignWords names words = do
  perm <- shuffleM [0..(length names - 1)]
  pure (zipWith (\i w -> (names !! i, w)) perm words)

main :: IO ()
main = do
  names <- parseLSV
  words <- parseLSV
  print words
  seed <- readSeed
  setStdGen seed
  assignment <- assignWords names words
  forM_ assignment $ \(name, word) ->
    appendUTF8File (name ++ ".txt") ("\n" ++ word ++ "\n")
  getStdGen >>= writeSeed
