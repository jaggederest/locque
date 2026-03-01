{-# LANGUAGE OverloadedStrings #-}
module SmythCount
  ( runCount
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

import SmythConfig (SmythConfig(..))
import Utils (listLqFiles)

data Count = Count
  { countFiles :: Int
  , countLines :: Int
  } deriving (Show)

instance Semigroup Count where
  Count files1 lines1 <> Count files2 lines2 =
    Count (files1 + files2) (lines1 + lines2)

instance Monoid Count where
  mempty = Count 0 0

runCount :: SmythConfig -> IO ()
runCount config = do
  let root = projectRoot config
      libDir = root </> libRoot config
      testDir = root </> testRoot config
  libCount <- countDir libDir
  testCount <- countDir testDir
  let total = libCount <> testCount
  putStrLn "Locque line count (.lq only)"
  printCount "lib" libCount
  printCount "test" testCount
  printCount "total" total

printCount :: String -> Count -> IO ()
printCount label cnt =
  putStrLn (label ++ ": " ++ show (countLines cnt) ++ " lines (" ++ show (countFiles cnt) ++ " files)")

countDir :: FilePath -> IO Count
countDir dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure mempty
    else do
      files <- listLqFiles dir
      counts <- mapM countFile files
      pure (mconcat counts)

countFile :: FilePath -> IO Count
countFile path = do
  contents <- TIO.readFile path
  let lineCount = length (T.lines contents)
  pure (Count 1 lineCount)
