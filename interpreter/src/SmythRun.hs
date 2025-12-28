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
import DictPass (transformModuleWithEnvs)
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
      -- Type check (native typeclass support on original module)
      tcResult <- (TC.typeCheckModuleWithImports (projectRoot config) contents m `catch` handleTypeCheckError)
      case tcResult of
        Left tcErr -> do
          putStrLn $ "Type error: " ++ show tcErr
          exitFailure
        Right env -> do
          -- Transform: dictionary passing for evaluation
          m' <- transformModuleWithEnvs (projectRoot config) m
          -- Annotate: wrap expressions with inferred types
          case TC.annotateModule env m' of
            Left annotErr -> do
              putStrLn $ "Annotation error: " ++ show annotErr
              exitFailure
            Right annotatedM -> do
              -- Run
              _ <- (runModuleMain (projectRoot config) annotatedM `catch` handleRuntimeError)
              exitSuccess

-- | Handle type checking errors (including import loading failures)
handleTypeCheckError :: SomeException -> IO (Either TC.TypeError TC.TypeEnv)
handleTypeCheckError e = do
  putStrLn $ "Type check phase error: " ++ show e
  putStrLn $ "  (This may be a module import/loading issue)"
  exitFailure

-- | Handle runtime errors
handleRuntimeError :: SomeException -> IO Int
handleRuntimeError e = do
  let errMsg = show e
  -- Strip the "ghc-internal:GHC.Internal.Exception.ErrorCall:" prefix if present
  let cleanMsg = case break (== '\n') errMsg of
        (firstLine, rest) ->
          if "ErrorCall:" `elem` words firstLine
          then drop 1 rest  -- Skip the newline
          else errMsg
  putStrLn $ "Runtime error (evaluation phase): " ++ cleanMsg
  exitFailure
