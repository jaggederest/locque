{-# LANGUAGE OverloadedStrings #-}
module SmythTest
  ( runTests
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (catMaybes)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>), isAbsolute)
import System.Directory (setCurrentDirectory)
import Control.Exception (catch, SomeException)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Parser (parseMExprFile)
import qualified TypeChecker as TC
import Eval (runModuleMain)
import DictPass (transformModuleWithEnvs)
import SmythConfig (SmythConfig(..))

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
  case args of
    []    -> runAllTests config
    files -> runPositiveTests config files

runAllTests :: SmythConfig -> IO ()
runAllTests config = do
  start <- getPOSIXTime
  let testFile = projectRoot config </> testRoot config </> "main.lq"
  (assertions, baseErrors) <- runPositiveTestsWithCounts config [testFile]
  (expectedCount, expectedErrors) <- runExpectedFailures config
  let errors = baseErrors ++ expectedErrors
  end <- getPOSIXTime
  let elapsedUs = floor ((end - start) * 1000000) :: Integer
  if null errors
    then do
      putStrLn $
        "✓ All tests passed (" ++ show assertions ++ " assertions, "
        ++ show expectedCount ++ " expected failures, "
        ++ show elapsedUs ++ "us)"
      exitSuccess
    else do
      putStrLn $ "✗ " ++ show (length errors) ++ " test(s) failed (" ++ show elapsedUs ++ "us)\n"
      mapM_ printError errors
      exitFailure

runPositiveTests :: SmythConfig -> [FilePath] -> IO ()
runPositiveTests config files = do
  (assertions, errors) <- runPositiveTestsWithCounts config files
  if null errors
    then do
      putStrLn $ "✓ All tests passed (" ++ show assertions ++ " assertions)"
      exitSuccess
    else do
      putStrLn $ "✗ " ++ show (length errors) ++ " test(s) failed\n"
      mapM_ printError errors
      exitFailure

runPositiveTestsWithCounts :: SmythConfig -> [FilePath] -> IO (Int, [TestError])
runPositiveTestsWithCounts config files = do
  results <- mapM (runPositiveTest (projectRoot config)) files
  let assertions = sum [n | Right n <- results]
      errors = [e | Left e <- results]
  pure (assertions, errors)

runPositiveTest :: FilePath -> FilePath -> IO (Either TestError Int)
runPositiveTest projectRoot file = do
  outcome <- runTestOutcome projectRoot file
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
        tcResult <- TC.typeCheckModuleWithImports projectRoot contents m
        case tcResult of
          Left tcErr -> pure $ Left (Failure TypeFailure (T.pack $ show tcErr))
          Right env -> do
            m' <- transformModuleWithEnvs projectRoot m
            case TC.annotateModule env m' of
              Left annotErr -> pure $ Left (Failure AnnotFailure (T.pack $ show annotErr))
              Right annotatedM -> do
                count <- runModuleMain projectRoot annotatedM
                pure (Right count)
        ) `catch` handleTestException
      pure result

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
