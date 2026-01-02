{-# LANGUAGE OverloadedStrings #-}
module SmythTest
  ( runTests
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>), isAbsolute)
import System.Directory (setCurrentDirectory)
import Control.Exception (catch, SomeException, evaluate)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Char (toLower)
import Data.IORef (IORef, newIORef, modifyIORef', readIORef)
import System.Environment (lookupEnv)

import Parser (parseMExprFile)
import qualified TypeChecker as TC
import Eval (ctorArityMap, runModuleMain)
import DictPass (transformModuleWithEnvs)
import SmythConfig (SmythConfig(..))
import qualified RunCache as RC

data TestError = TestError
  { errorFile :: FilePath
  , errorLine :: Maybe Int
  , errorMsg  :: T.Text
  } deriving (Show)

data FailureKind
  = ParseFailure
  | TransformFailure
  | TypeFailure
  | AnnotFailure
  | RuntimeFailure
  deriving (Show)

data Failure = Failure
  { failureKind :: FailureKind
  , failureMsg  :: T.Text
  } deriving (Show)

failureMessage :: Failure -> T.Text
failureMessage (Failure kind msg) =
  case kind of
    ParseFailure -> "Parse error: " <> msg
    TransformFailure -> "Transform error: " <> msg
    TypeFailure -> "Type error: " <> msg
    AnnotFailure -> "Annotation error: " <> msg
    RuntimeFailure -> "Runtime error: " <> msg

-- | Run tests based on arguments
runTests :: SmythConfig -> [String] -> IO ()
runTests config args = do
  -- Change to project root so all paths are relative to it
  setCurrentDirectory (projectRoot config)
  timingEnv <- lookupEnv "LOCQUE_TIMING"
  let verbose = "--verbose" `elem` args
      slow = "--slow" `elem` args
      stageTiming = slow || isTimingEnabled timingEnv
      files = filter (\arg -> arg /= "--slow" && arg /= "--verbose") args
  case files of
    [] -> runAllTests config verbose stageTiming
    _  -> runSelectedTests config verbose stageTiming files

runAllTests :: SmythConfig -> Bool -> Bool -> IO ()
runAllTests config verbose stageTiming = do
  start <- getPOSIXTime
  let testFile = projectRoot config </> testRoot config </> "main.lq"
  (assertions, baseErrors) <- runPositiveTestsWithCounts config stageTiming [testFile]
  (expectedCount, expectedErrors) <- runExpectedFailures config
  let errors = baseErrors ++ expectedErrors
  end <- getPOSIXTime
  let elapsedUs = floor ((end - start) * 1000000) :: Integer
  if null errors
    then do
      if verbose
        then exitSuccess
        else do
          putStrLn $
            "✓ All tests passed (" ++ show assertions ++ " assertions, "
            ++ show expectedCount ++ " expected failures, "
            ++ show elapsedUs ++ "us)"
          exitSuccess
    else do
      if verbose
        then do
          mapM_ printError errors
          exitFailure
        else do
          putStrLn $ "✗ " ++ show (length errors) ++ " test(s) failed (" ++ show elapsedUs ++ "us)\n"
          mapM_ printError errors
          exitFailure

runPositiveTests :: SmythConfig -> Bool -> Bool -> [FilePath] -> IO ()
runPositiveTests config verbose stageTiming files = do
  (assertions, errors) <- runPositiveTestsWithCounts config stageTiming files
  if null errors
    then do
      if verbose
        then exitSuccess
        else do
          putStrLn $ "✓ All tests passed (" ++ show assertions ++ " assertions)"
          exitSuccess
    else do
      if verbose
        then do
          mapM_ printError errors
          exitFailure
        else do
          putStrLn $ "✗ " ++ show (length errors) ++ " test(s) failed\n"
          mapM_ printError errors
          exitFailure

runSelectedTests :: SmythConfig -> Bool -> Bool -> [FilePath] -> IO ()
runSelectedTests config verbose stageTiming files = do
  let root = projectRoot config
      errorMap =
        Map.fromList
          [ (resolvePath root path, (path, msg))
          | (path, msg) <- errorTests config
          ]
      (expectedEntries, positiveFiles) =
        foldr
          (\file (expectedAcc, positiveAcc) ->
              let resolved = resolvePath root file
              in case Map.lookup resolved errorMap of
                  Just entry -> (entry : expectedAcc, positiveAcc)
                  Nothing -> (expectedAcc, file : positiveAcc))
          ([], [])
          files
  (assertions, posErrors) <- runPositiveTestsWithCounts config stageTiming positiveFiles
  expectedResults <- mapM (runExpectedFailure root) expectedEntries
  let expectedErrors = catMaybes expectedResults
      expectedCount = length expectedEntries - length expectedErrors
      errors = posErrors ++ expectedErrors
  if null errors
    then do
      if verbose
        then exitSuccess
        else do
          putStrLn $
            "✓ All tests passed (" ++ show assertions ++ " assertions, "
            ++ show expectedCount ++ " expected failures)"
          exitSuccess
    else do
      if verbose
        then do
          mapM_ printError errors
          exitFailure
        else do
          putStrLn $ "✗ " ++ show (length errors) ++ " test(s) failed\n"
          mapM_ printError errors
          exitFailure

runPositiveTestsWithCounts :: SmythConfig -> Bool -> [FilePath] -> IO (Int, [TestError])
runPositiveTestsWithCounts config stageTiming files = do
  results <- mapM (runPositiveTest (projectRoot config) stageTiming) files
  let assertions = sum [n | Right n <- results]
      errors = [e | Left e <- results]
  pure (assertions, errors)

runPositiveTest :: FilePath -> Bool -> FilePath -> IO (Either TestError Int)
runPositiveTest projectRoot stageTiming file = do
  (outcome, timings) <-
    if stageTiming
      then runTestOutcomeTimed projectRoot file
      else do
        result <- runTestOutcome projectRoot file
        pure (result, [])
  if stageTiming
    then printStageTimings file timings
    else pure ()
  case outcome of
    Right count -> pure (Right count)
    Left failure ->
      pure $ Left (TestError file Nothing (failureMessage failure))

runExpectedFailures :: SmythConfig -> IO (Int, [TestError])
runExpectedFailures config = do
  let tests = errorTests config
  results <- mapM (runExpectedFailure (projectRoot config)) tests
  let errors = catMaybes results
      expectedCount = length tests - length errors
  pure (expectedCount, errors)

runExpectedFailure :: FilePath -> (FilePath, T.Text) -> IO (Maybe TestError)
runExpectedFailure projectRoot (path, expectedSubstring) = do
  let fullPath = resolvePath projectRoot path
  outcome <- runTestOutcome projectRoot fullPath
  case outcome of
    Right _ ->
      pure $ Just (TestError path Nothing "Expected failure, but test passed")
    Left failure -> do
      let actual = failureMessage failure
      if expectedSubstring `T.isInfixOf` actual
        then pure Nothing
        else pure $ Just (TestError path Nothing
              ("Expected error containing: " <> expectedSubstring <> "\nActual: " <> actual))

resolvePath :: FilePath -> FilePath -> FilePath
resolvePath root path =
  if isAbsolute path then path else root </> path

runTestOutcome :: FilePath -> FilePath -> IO (Either Failure Int)
runTestOutcome projectRoot file = do
  contents <- TIO.readFile file
  case parseMExprFile file contents of
    Left parseErr -> pure $ Left (Failure ParseFailure (T.pack parseErr))
    Right m -> do
      result <- (do
        (digest, importedEnv) <- TC.moduleDigestWithImports projectRoot contents m
        cached <- RC.readRunCache projectRoot file digest
        case cached of
          Just entry -> do
            count <- runModuleMain projectRoot (RC.cacheCtorArity entry) (RC.cacheAnnotated entry)
            pure (Right count)
          Nothing -> do
            let tcResult = TC.typeCheckAndNormalizeWithEnv importedEnv m
            case tcResult of
              Left tcErr -> pure $ Left (Failure TypeFailure (T.pack $ show tcErr))
              Right (env, normalized) -> do
                let arity = ctorArityMap normalized
                m' <- transformModuleWithEnvs projectRoot m
                case TC.annotateModule env m' of
                  Left annotErr -> pure $ Left (Failure AnnotFailure (T.pack $ show annotErr))
                  Right annotatedM -> do
                    let cacheEntry = RC.RunCache
                          { RC.cacheVersion = RC.cacheVersionCurrent
                          , RC.cacheDigest = digest
                          , RC.cacheAnnotated = annotatedM
                          , RC.cacheCtorArity = arity
                          }
                    RC.writeRunCache projectRoot file cacheEntry
                    count <- runModuleMain projectRoot arity annotatedM
                    pure (Right count)
        ) `catch` handleTestException
      pure result

data StageTiming = StageTiming
  { stageName :: String
  , stageMicros :: Integer
  } deriving (Show)

runTestOutcomeTimed :: FilePath -> FilePath -> IO (Either Failure Int, [StageTiming])
runTestOutcomeTimed projectRoot file = do
  timingsRef <- newIORef []
  let record name action = do
        (result, micros) <- timeAction action
        modifyIORef' timingsRef (\acc -> acc ++ [StageTiming name micros])
        pure result
      recordZero name =
        modifyIORef' timingsRef (\acc -> acc ++ [StageTiming name 0])
  outcome <- (do
    contents <- record "read" (TIO.readFile file)
    parsed <- record "parse" (evaluate (parseMExprFile file contents))
    case parsed of
      Left parseErr -> pure $ Left (Failure ParseFailure (T.pack parseErr))
      Right m -> do
        (digest, importedEnv) <- record "digest" (TC.moduleDigestWithImports projectRoot contents m)
        cached <- record "cache-read" (RC.readRunCache projectRoot file digest)
        case cached of
          Just entry -> do
            recordZero "typecheck"
            recordZero "normalize"
            recordZero "transform"
            recordZero "annotate"
            recordZero "cache-write"
            count <- record "run" (runModuleMain projectRoot (RC.cacheCtorArity entry) (RC.cacheAnnotated entry))
            pure (Right count)
          Nothing -> do
            tcResult <- record "typecheck" (evaluate (TC.typeCheckAndNormalizeWithEnv importedEnv m))
            recordZero "normalize"
            case tcResult of
              Left tcErr -> pure $ Left (Failure TypeFailure (T.pack $ show tcErr))
              Right (env, normalized) -> do
                let arity = ctorArityMap normalized
                m' <- record "transform" (transformModuleWithEnvs projectRoot m)
                annotRes <- record "annotate" (evaluate (TC.annotateModule env m'))
                case annotRes of
                  Left annotErr -> pure $ Left (Failure AnnotFailure (T.pack $ show annotErr))
                  Right annotatedM -> do
                    record "cache-write" $ do
                      let cacheEntry = RC.RunCache
                            { RC.cacheVersion = RC.cacheVersionCurrent
                            , RC.cacheDigest = digest
                            , RC.cacheAnnotated = annotatedM
                            , RC.cacheCtorArity = arity
                            }
                      RC.writeRunCache projectRoot file cacheEntry
                    count <- record "run" (runModuleMain projectRoot arity annotatedM)
                    pure (Right count)
    ) `catch` handleTestException
  timings <- readIORef timingsRef
  pure (outcome, timings)

timeAction :: IO a -> IO (a, Integer)
timeAction action = do
  start <- getPOSIXTime
  result <- action
  end <- getPOSIXTime
  let elapsedUs = floor ((end - start) * 1000000) :: Integer
  pure (result, elapsedUs)

printStageTimings :: FilePath -> [StageTiming] -> IO ()
printStageTimings file timings = do
  putStrLn $ "Stage timings (" ++ file ++ "):"
  mapM_ (\(StageTiming name micros) ->
    putStrLn ("  " ++ name ++ " " ++ show micros ++ "us")) timings

isTimingEnabled :: Maybe String -> Bool
isTimingEnabled = maybe False matches
  where
    matches raw =
      let val = map toLower raw
      in val == "1" || val == "true" || val == "yes" || val == "on"

handleTestException :: SomeException -> IO (Either Failure Int)
handleTestException e =
  pure $ Left (Failure RuntimeFailure (T.pack $ show e))

-- | Print a test error
printError :: TestError -> IO ()
printError (TestError file maybeLine msg) = do
  case maybeLine of
    Just line -> putStrLn $ file ++ ":" ++ show line
    Nothing   -> putStrLn file
  TIO.putStrLn $ "  " <> msg
