module SmythCompileTest
  ( runCompileTest
  ) where

import Control.Monad (unless, when)
import Data.List (isPrefixOf)
import System.Directory (setCurrentDirectory)
import System.Environment (getExecutablePath, lookupEnv, setEnv)
import System.Exit (ExitCode(..), exitFailure, exitSuccess)
import System.FilePath ((</>), takeDirectory)
import System.IO (hPutStr, stderr)
import System.Process (proc, readCreateProcessWithExitCode)

import SmythCompile (defaultCompileOutPath)
import SmythConfig (SmythConfig(..))

data CompileTestOptions = CompileTestOptions
  { compileTestOutPath :: Maybe FilePath
  } deriving (Eq, Show)

defaultCompileTestOptions :: CompileTestOptions
defaultCompileTestOptions = CompileTestOptions
  { compileTestOutPath = Nothing
  }

parseCompileTestArgs :: [String] -> Either String CompileTestOptions
parseCompileTestArgs args = go args defaultCompileTestOptions
  where
    go remaining opts = case remaining of
      [] -> Right opts
      "--out" : path : rest ->
        go rest opts { compileTestOutPath = Just path }
      unknown : _ ->
        Left ("Error: unknown option for 'smyth compile-test': " ++ unknown)

data ProcResult = ProcResult
  { procExit :: ExitCode
  , procOut :: String
  , procErr :: String
  } deriving (Eq, Show)

runProcessCapture :: FilePath -> [String] -> IO ProcResult
runProcessCapture path args = do
  (exitCode, out, err) <- readCreateProcessWithExitCode (proc path args) ""
  pure (ProcResult exitCode out err)

runCompileTest :: SmythConfig -> [String] -> IO ()
runCompileTest config args = do
  ensureSelfOnPath
  setCurrentDirectory (projectRoot config)
  let (optArgs, extraArgs) = break (== "--") args
  case parseCompileTestArgs optArgs of
    Left err -> do
      putStrLn err
      putStrLn "Usage: smyth compile-test [--out <path>] [-- <args>]"
      exitFailure
    Right opts -> do
      let runArgs = case extraArgs of
            [] -> []
            ("--" : rest) -> rest
            _ -> []
      let testFile = projectRoot config </> testRoot config </> "main.lq"
      let outPath = maybe (defaultCompileOutPath (projectRoot config) testFile)
            id
            (compileTestOutPath opts)
      exePath <- getExecutablePath
      compileResult <- runProcessCapture exePath ["compile", "--out", outPath, testFile]
      case compileResult of
        ProcResult ExitSuccess _ _ -> pure ()
        ProcResult (ExitFailure code) out err -> do
          putStrLn ("smyth compile failed (" ++ show code ++ "):")
          unless (null out) (putStr out)
          unless (null err) (hPutStr stderr err)
          exitFailure
      let runArgsWithSeparator =
            if null runArgs
              then ["run", testFile]
              else ["run", testFile, "--"] ++ runArgs
      interpResult <- runProcessCapture exePath runArgsWithSeparator
      compiledResult <- runProcessCapture outPath runArgs
      reportResults (normalizeOutput (elem "--verbose" runArgs)) interpResult compiledResult

ensureSelfOnPath :: IO ()
ensureSelfOnPath = do
  exePath <- getExecutablePath
  let exeDir = takeDirectory exePath
  existing <- lookupEnv "PATH"
  let newPath = case existing of
        Nothing -> exeDir
        Just path -> exeDir ++ ":" ++ path
  setEnv "PATH" newPath

reportResults :: (String -> String) -> ProcResult -> ProcResult -> IO ()
reportResults normalize interp compiled = do
  let interpOut = normalize (procOut interp)
      compiledOut = normalize (procOut compiled)
      interpErr = procErr interp
      compiledErr = procErr compiled
      exitMatch = procExit interp == procExit compiled
      outMatch = interpOut == compiledOut
      errMatch = interpErr == compiledErr
      success = procExit interp == ExitSuccess && procExit compiled == ExitSuccess
  if success && outMatch && errMatch
    then do
      putStrLn "compile-test: OK (outputs match)"
      exitSuccess
    else do
      putStrLn "compile-test: FAIL"
      when (not success || not exitMatch) $ do
        putStrLn ("Interpreter exit: " ++ show (procExit interp))
        putStrLn ("Compiled exit: " ++ show (procExit compiled))
      when (not outMatch) $ do
        putStrLn "Interpreter stdout:"
        printOutput (procOut interp)
        putStrLn "Compiled stdout:"
        printOutput (procOut compiled)
      when (not errMatch) $ do
        putStrLn "Interpreter stderr:"
        printOutput (procErr interp)
        putStrLn "Compiled stderr:"
        printOutput (procErr compiled)
      exitFailure

printOutput :: String -> IO ()
printOutput output =
  if null output
    then putStrLn "<empty>"
    else putStrLn output

normalizeOutput :: Bool -> String -> String
normalizeOutput False output = output
normalizeOutput True output =
  unlines (filter (not . isTimingLine) (lines output))

isTimingLine :: String -> Bool
isTimingLine line = "timing " `isPrefixOf` line
