{-# LANGUAGE OverloadedStrings #-}
module SmythRun
  ( runFile
  , runFileNoExit
  ) where

import qualified Data.Text.IO as TIO
import System.Exit (exitFailure, exitSuccess)
import Control.Exception (SomeException, try)

import Parser (parseMExprFile)
import AST (Module)
import qualified TypeChecker as TC
import Eval (ctorArityMap, runModuleMain)
import DictPass (transformModuleWithEnvs)
import SmythConfig (SmythConfig(..))

-- | Run a single file with type checking (no exit).
runFileNoExit :: SmythConfig -> FilePath -> IO Bool
runFileNoExit config file = do
  -- Parse
  contents <- TIO.readFile file
  case parseMExprFile file contents of
    Left parseErr -> do
      putStrLn $ "Parse error: " ++ parseErr
      pure False
    Right m -> do
      -- Type check (native typeclass support on original module)
      tcAttempt <- try (TC.typeCheckModuleWithImports (projectRoot config) contents m)
        :: IO (Either SomeException (Either TC.TypeError TC.TypeEnv))
      case tcAttempt of
        Left e -> do
          putStrLn $ "Type check phase error: " ++ show e
          putStrLn $ "  (This may be a module import/loading issue)"
          pure False
        Right (Left tcErr) -> do
          putStrLn $ "Type error: " ++ show tcErr
          pure False
        Right (Right env) -> do
          normalized <- TC.normalizeModuleWithImports (projectRoot config) contents m
          ctorArity <- case normalized of
            Left tcErr -> do
              putStrLn $ "Type error: " ++ show tcErr
              pure Nothing
            Right nm -> pure (Just (ctorArityMap nm))
          case ctorArity of
            Nothing -> pure False
            Just arity -> do
              -- Transform: dictionary passing for evaluation
              transformAttempt <- try (transformModuleWithEnvs (projectRoot config) m)
                :: IO (Either SomeException Module)
              case transformAttempt of
                Left e -> do
                  putStrLn $ "Transform error: " ++ show e
                  pure False
                Right m' -> do
                  -- Annotate: wrap expressions with inferred types
                  case TC.annotateModule env m' of
                    Left annotErr -> do
                      putStrLn $ "Annotation error: " ++ show annotErr
                      pure False
                    Right annotatedM -> do
                      runAttempt <- try (runModuleMain (projectRoot config) arity annotatedM)
                        :: IO (Either SomeException Int)
                      case runAttempt of
                        Left e -> do
                          let errMsg = show e
                          -- Strip the "ghc-internal:GHC.Internal.Exception.ErrorCall:" prefix if present
                          let cleanMsg = case break (== '\n') errMsg of
                                (firstLine, rest) ->
                                  if "ErrorCall:" `elem` words firstLine
                                  then drop 1 rest  -- Skip the newline
                                  else errMsg
                          putStrLn $ "Runtime error (evaluation phase): " ++ cleanMsg
                          pure False
                        Right _ -> pure True

-- | Run a single file with type checking and exit.
runFile :: SmythConfig -> FilePath -> IO ()
runFile config file = do
  ok <- runFileNoExit config file
  if ok then exitSuccess else exitFailure
