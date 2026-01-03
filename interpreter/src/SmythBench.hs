{-# LANGUAGE OverloadedStrings #-}
module SmythBench
  ( runBench
  ) where

import System.Directory (setCurrentDirectory)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>), isAbsolute)

import SmythConfig (SmythConfig(..))
import SmythRun (runFileNoExit)

runBench :: SmythConfig -> [String] -> IO ()
runBench config args = do
  setCurrentDirectory (projectRoot config)
  let files =
        case args of
          [] -> [projectRoot config </> testRoot config </> "bench.lq"]
          _  -> map (resolvePath (projectRoot config)) args
  results <- mapM (runFileNoExit config) files
  if and results then exitSuccess else exitFailure

resolvePath :: FilePath -> FilePath -> FilePath
resolvePath root path =
  if isAbsolute path then path else root </> path
