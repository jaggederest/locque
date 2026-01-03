{-# LANGUAGE OverloadedStrings #-}
module SmythFormat
  ( runFormat
  , runFormatCheck
  ) where

import System.Directory (doesFileExist, setCurrentDirectory)
import System.Environment (withArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))

import SmythConfig (SmythConfig(..))
import SmythRun (runFileNoExit)

runFormat :: SmythConfig -> [String] -> IO ()
runFormat config args = do
  ok <- runFormatCheck config args
  if ok then pure () else exitFailure

runFormatCheck :: SmythConfig -> [String] -> IO Bool
runFormatCheck config args = do
  let root = projectRoot config
      formatterPath = root </> libRoot config </> "tools" </> "formatter.lq"
      formatterArgs = if null args then [] else args
  exists <- doesFileExist formatterPath
  if not exists
    then do
      putStrLn ("Error: formatter not found: " ++ formatterPath)
      pure False
    else do
      setCurrentDirectory root
      withArgs formatterArgs (runFileNoExit config formatterPath)
