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

type Permutation = [Int]

-- | Which edge must *not* be present in the permutation graph.
type Constraint = (Int, Int)

applyPermutation :: Permutation -> [a] -> [a]
applyPermutation perm xs = map (xs !!) perm

readUTF8File :: FilePath -> IO String
readUTF8File p = withFile p ReadMode $ \h -> do
  hSetEncoding h utf8
  contents <- hGetContents h
  foldr seq (pure contents) contents

writeUTF8File :: FilePath -> String -> IO ()
writeUTF8File p contents = withFile p WriteMode $ \h -> do
  hSetEncoding h utf8
  hPutStr h contents

parseNames :: [String] -> IO [String]
parseNames []    = words <$> readUTF8File "names.txt"
parseNames names = pure names

seedFile :: FilePath
seedFile = "seed.txt"

readSeed :: IO StdGen
readSeed = do
  exists <- doesFileExist seedFile
  if exists
    then read <$> readUTF8File seedFile
    else getStdGen

assignmentFile :: FilePath
assignmentFile = "assignment.txt"

readOldAssignment
  :: [String]
  -> IO [Constraint]
readOldAssignment names = do
  exists <- doesFileExist assignmentFile
  entries <- if exists
    then map read . lines <$> readUTF8File assignmentFile
    else pure ([] :: [(String, String)])
  let name2Idx name = findIndex (== name) names
  pure (mapMaybe (\(f,t) -> (,) <$> name2Idx f <*> name2Idx t) entries)

writeAssignment :: [(String, String)] -> IO ()
writeAssignment assignment = writeUTF8File assignmentFile $
  unlines $ map (\(from, to) -> "(\"" ++ from ++ "\", \"" ++ to ++ "\")") assignment

writeSeed :: StdGen -> IO ()
writeSeed = writeUTF8File seedFile . show

untilM :: Monad m => (a -> Bool) -> m a -> m a
untilM p act = do
  a <- act
  if p a then return a else untilM p act

assignNames :: [Constraint] -> [String] -> IO [(String, String)]
assignNames constraints names = do
  perm <- untilM (isValidPermutation constraints) (shuffleM [0..(length names - 1)])
  pure (zipWith (\n i -> (n, names !! i)) names perm)

isValidPermutation :: [Constraint] -> Permutation -> Bool
isValidPermutation constraints perm = cycleLengthOK && constraintsOK
  where
    -- | No cycle with length < minCycleLength
    cycleLengthOK :: Bool
    cycleLengthOK = not (any hasFixpoint (take (minCycleLength - 1) powers))
    minCycleLength :: Int
    minCycleLength = 3
    hasFixpoint :: Permutation -> Bool
    hasFixpoint = any (uncurry (==)) . zip [0..]
    powers :: [Permutation]
    powers = iterate (applyPermutation perm) perm
    -- | Permutation doesn't map any `from` to `to` in constraints
    constraintsOK :: Bool
    constraintsOK = not (any (\(from, to) -> to == perm !! from) constraints)

currentYear :: IO Int
currentYear = do
  now <- getCurrentTime
  let (year, month, day) = toGregorian $ utctDay now
  pure (fromIntegral year)

main :: IO ()
main = do
  names <- getArgs >>= parseNames
  putStrLn "Shuffling assignment for:"
  print names
  seed <- readSeed
  setStdGen seed
  -- We read the old perm.txt file to make sure that we don't shuffle a similar
  -- asignment.
  constraints <- readOldAssignment names
  assignment <- assignNames constraints names
  forM_ assignment $ \(from, to) ->
    writeUTF8File (from ++ ".txt") to
  writeAssignment assignment
  getStdGen >>= writeSeed
