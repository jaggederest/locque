{-# LANGUAGE OverloadedStrings #-}
module SmythTest
  ( runTests
  , TestResult(..)
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import Control.Exception (catch, SomeException)

import Parser (parseMExprFile)
import qualified TypeChecker as TC
import Eval (runModuleMain)
import SmythConfig (SmythConfig(..))

data TestResult
  = AllPassed Int      -- Number of assertions
  | Failed [TestError]
  | TypeCheckFailed T.Text
  deriving (Show)

data TestError = TestError
  { errorFile :: FilePath
  , errorLine :: Maybe Int
  , errorMsg  :: T.Text
  } deriving (Show)

-- | Run tests based on arguments
runTests :: SmythConfig -> [String] -> IO ()
runTests config args = case args of
  []       -> runTestMain config  -- Run test/main.lq
  files    -> runTestFiles config files

-- | Run test/main.lq
runTestMain :: SmythConfig -> IO ()
runTestMain config = do
  let testFile = projectRoot config </> testRoot config </> "main.lq"
  runTestFile config testFile

-- | Run specific test files
runTestFiles :: SmythConfig -> [FilePath] -> IO ()
runTestFiles config files = do
  mapM_ (runTestFile config) files

-- | Run a single test file with type checking
runTestFile :: SmythConfig -> FilePath -> IO ()
runTestFile config file = do
  result <- runTest (projectRoot config) file
  case result of
    AllPassed n -> do
      putStrLn $ "✓ All tests passed (" ++ show n ++ " assertions)"
      exitSuccess
    TypeCheckFailed err -> do
      TIO.putStrLn $ "✗ Type error:\n" <> err
      exitFailure
    Failed errors -> do
      putStrLn $ "✗ " ++ show (length errors) ++ " assertion(s) failed\n"
      mapM_ printError errors
      exitFailure

-- | Run a test file and capture result
runTest :: FilePath -> FilePath -> IO TestResult
runTest projectRoot file = do
  -- Parse
  contents <- TIO.readFile file
  case parseMExprFile file contents of
    Left parseErr -> pure $ Failed [TestError file Nothing (T.pack parseErr)]
    Right m -> do
      -- Type check
      tcResult <- TC.typeCheckModuleWithImports projectRoot m
      case tcResult of
        Left tcErr -> pure $ TypeCheckFailed (T.pack $ show tcErr)
        Right _ -> do
          -- Run and capture assertion count
          result <- (do
            count <- runModuleMain projectRoot m
            pure (AllPassed count)
            ) `catch` handleRuntimeError file
          pure result

-- | Handle runtime errors (failed assertions)
handleRuntimeError :: FilePath -> SomeException -> IO TestResult
handleRuntimeError file e =
  pure $ Failed [TestError file Nothing (T.pack $ show e)]

-- | Print a test error
printError :: TestError -> IO ()
printError (TestError file maybeLine msg) = do
  case maybeLine of
    Just line -> putStrLn $ file ++ ":" ++ show line
    Nothing   -> putStrLn file
  TIO.putStrLn $ "  " <> msg
