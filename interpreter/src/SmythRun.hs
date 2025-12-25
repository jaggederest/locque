{-# LANGUAGE OverloadedStrings #-}
module SmythRun
  ( runFile
  ) where

import qualified Data.Text.IO as TIO
import System.Exit (exitFailure, exitSuccess)
import Control.Exception (catch, SomeException)

import Parser (parseMExprFile)
import qualified TypeChecker as TC
import Eval (runModuleMain)
import SmythConfig (SmythConfig(..))

-- | Run a single file with type checking
runFile :: SmythConfig -> FilePath -> IO ()
runFile config file = do
  -- Parse
  contents <- TIO.readFile file
  case parseMExprFile file contents of
    Left parseErr -> do
      putStrLn $ "Parse error: " ++ parseErr
      exitFailure
    Right m -> do
      -- Type check
      tcResult <- TC.typeCheckModuleWithImports (projectRoot config) contents m
      case tcResult of
        Left tcErr -> do
          putStrLn $ "Type error: " ++ show tcErr
          exitFailure
        Right _ -> do
          -- Run
          _ <- (runModuleMain (projectRoot config) m `catch` handleRuntimeError)
          exitSuccess

-- | Handle runtime errors
handleRuntimeError :: SomeException -> IO Int
handleRuntimeError e = do
  putStrLn $ "Runtime error: " ++ show e
  exitFailure
