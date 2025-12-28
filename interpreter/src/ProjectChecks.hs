{-# LANGUAGE OverloadedStrings #-}
module ProjectChecks
  ( requireLibTests
  ) where

import Control.Monad (forM, filterM)
import Data.Char (isUpper)
import Data.List (sort)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (die)
import System.FilePath ((</>), makeRelative, takeExtension)

requireLibTests :: FilePath -> IO ()
requireLibTests projectRoot = do
  let libDir = projectRoot </> "lib"
      testDir = projectRoot </> "test"
  libExists <- doesDirectoryExist libDir
  testExists <- doesDirectoryExist testDir
  if not libExists
    then die ("Missing lib directory: " ++ libDir)
    else if not testExists
      then die ("Missing test directory: " ++ testDir)
      else do
        libFiles <- collectLqFiles libDir
        testFiles <- collectLqFiles testDir
        let badLib = filter (hasUppercase libDir) libFiles
            badTest = filter (hasUppercase testDir) testFiles
        if not (null badLib) || not (null badTest)
          then die (formatUppercase libDir testDir badLib badTest)
          else do
            missing <- filterM (isMissingTest libDir testDir) libFiles
            if null missing
              then pure ()
              else die (formatMissing libDir missing)
  where
    isMissingTest libDir testDir libPath = do
      let rel = makeRelative libDir libPath
          testPath = testDir </> rel
      exists <- doesFileExist testPath
      pure (not exists)

    formatMissing libDir libPaths =
      let rels = sort (map (makeRelative libDir) libPaths)
      in unlines ("Missing test file(s) for lib modules:" : map ("  test/" ++) rels)

    hasUppercase root path =
      any isUpper (makeRelative root path)

    formatUppercase libDir testDir badLib badTest =
      let libRels = sort (map (makeRelative libDir) badLib)
          testRels = sort (map (makeRelative testDir) badTest)
          libLines = map ("  lib/" ++) libRels
          testLines = map ("  test/" ++) testRels
      in unlines (["Uppercase path(s) are not allowed:"] ++ libLines ++ testLines)

collectLqFiles :: FilePath -> IO [FilePath]
collectLqFiles root = do
  entries <- listDirectory root
  paths <- forM entries $ \name -> do
    let path = root </> name
    isDir <- doesDirectoryExist path
    if isDir
      then collectLqFiles path
      else pure [path]
  pure [path | path <- concat paths, takeExtension path == ".lq"]
