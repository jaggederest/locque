{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module SmythRun
  ( RunOptions(..)
  , defaultRunOptions
  , runFile
  , runFileWithOptions
  , runFileNoExit
  , runFileNoExitWithOptions
  , prepareModule
  ) where

import qualified Data.Text.IO as TIO
import System.Exit (exitFailure, exitSuccess)
import Control.Exception (SomeException, try, evaluate)
import System.Timeout (timeout)
import System.Posix.Process (getProcessID)

import Parser (parseMExprFile)
import AST (Module)
import CtorArity (CtorArityMap)
import qualified TypeChecker as TC
import Eval (ctorArityMap, runModuleMain)
import DictPass (transformModuleWithEnvs)
import Recursor (recursorDefs, insertRecursors)
import SmythConfig (SmythConfig(..))
import qualified RunCache as RC

data RunOptions = RunOptions
  { runPidFile :: Maybe FilePath
  , runTimeoutMs :: Maybe Int
  }

defaultRunOptions :: RunOptions
defaultRunOptions = RunOptions
  { runPidFile = Nothing
  , runTimeoutMs = Nothing
  }

-- | Prepare a type-checked module for execution: insert recursors,
-- annotate with types, and perform dictionary passing transformation.
-- Writes the run cache on success.
prepareModule
  :: FilePath        -- ^ project root
  -> FilePath        -- ^ source file path (for cache)
  -> String          -- ^ content digest (for cache)
  -> TC.TCEnv        -- ^ type checking environment
  -> Module          -- ^ original module (for recursor insertion)
  -> Module          -- ^ normalized module (for arity map)
  -> IO (Either String (CtorArityMap, Module))
prepareModule root file digest env original normalized = do
  let arity = ctorArityMap normalized
      recDefs = recursorDefs normalized
  case insertRecursors original recDefs of
    Left msg -> pure $ Left msg
    Right prepared ->
      case TC.annotateModule env prepared of
        Left annotErr -> pure $ Left (show annotErr)
        Right annotatedPrepared -> do
          m' <- transformModuleWithEnvs root annotatedPrepared
          let cacheEntry = RC.RunCache
                { RC.cacheVersion = RC.cacheVersionCurrent
                , RC.cacheDigest = digest
                , RC.cacheAnnotated = m'
                , RC.cacheCtorArity = arity
                }
          RC.writeRunCache root file cacheEntry
          pure $ Right (arity, m')

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
      let runAnnotated arity annotatedM = do
            runAttempt <- try (runModuleMain (projectRoot config) arity annotatedM)
              :: IO (Either SomeException Int)
            case runAttempt of
              Left e -> do
                let errMsg = show e
                let cleanMsg = case break (== '\n') errMsg of
                      (firstLine, rest) ->
                        if "ErrorCall:" `elem` words firstLine
                        then drop 1 rest
                        else errMsg
                putStrLn $ "Runtime error (evaluation phase): " ++ cleanMsg
                pure False
              Right _ -> pure True
      digestAttempt <- try @SomeException (TC.moduleDigestWithImports (projectRoot config) contents m)
      case digestAttempt of
        Left e -> do
          putStrLn $ "Type check phase error: " ++ show e
          putStrLn $ "  (This may be a module import/loading issue)"
          pure False
        Right (digest, importedEnv) -> do
          cached <- RC.readRunCache (projectRoot config) file digest
          case cached of
            Just entry ->
              runAnnotated (RC.cacheCtorArity entry) (RC.cacheAnnotated entry)
            Nothing -> do
              -- Type check (native typeclass support on original module)
              tcAttempt <- try (evaluate (TC.typeCheckAndNormalizeWithEnv importedEnv m))
                :: IO (Either SomeException (Either TC.TypeError (TC.TCEnv, Module)))
              case tcAttempt of
                Left e -> do
                  putStrLn $ "Type check phase error: " ++ show e
                  putStrLn $ "  (This may be a module import/loading issue)"
                  pure False
                Right (Left tcErr) -> do
                  putStrLn $ "Type error: " ++ show tcErr
                  pure False
                Right (Right (env, normalized)) -> do
                  prepResult <- try @SomeException
                    (prepareModule (projectRoot config) file digest env m normalized)
                  case prepResult of
                    Left e -> do
                      putStrLn $ "Transform error: " ++ show e
                      pure False
                    Right (Left msg) -> do
                      putStrLn $ "Transform error: " ++ msg
                      pure False
                    Right (Right (arity, m')) ->
                      runAnnotated arity m'

runFileNoExitWithOptions :: SmythConfig -> RunOptions -> FilePath -> IO Bool
runFileNoExitWithOptions config opts file = do
  let action = runFileNoExit config file
  withPidFile (runPidFile opts) $ do
    case runTimeoutMs opts of
      Nothing -> action
      Just ms -> do
        result <- timeout (ms * 1000) action
        case result of
          Nothing -> do
            putStrLn $ "Runtime error (evaluation phase): smyth run timed out after " ++ show ms ++ "ms"
            pure False
          Just ok -> pure ok

withPidFile :: Maybe FilePath -> IO a -> IO a
withPidFile Nothing action = action
withPidFile (Just path) action = do
  pid <- getProcessID
  writeFile path (show pid)
  action

-- | Run a single file with type checking and exit.
runFile :: SmythConfig -> FilePath -> IO ()
runFile config file = do
  ok <- runFileNoExit config file
  if ok then exitSuccess else exitFailure

runFileWithOptions :: SmythConfig -> RunOptions -> FilePath -> IO ()
runFileWithOptions config opts file = do
  ok <- runFileNoExitWithOptions config opts file
  if ok then exitSuccess else exitFailure
