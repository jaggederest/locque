{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Environment (getArgs, withArgs)
import System.Exit (exitFailure)

import SmythConfig (findSmythfile, loadSmythConfig)
import SmythBench (runBench)
import SmythCount (runCount)
import SmythDependencies (runDependencies)
import SmythDump (runDump)
import SmythFormat (runFormat)
import SmythTest (runTests)
import SmythRun (RunOptions(..), defaultRunOptions, runFileWithOptions)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("test" : testArgs) -> runTestCommand testArgs
    ("run" : runArgs)   -> runRunCommand runArgs
    ("bench" : benchArgs) -> runBenchCommand benchArgs
    ("count" : countArgs) -> runCountCommand countArgs
    ("dependencies" : depArgs) -> runDependenciesCommand depArgs
    ("dump" : dumpArgs) -> runDumpCommand dumpArgs
    ("format" : formatArgs) -> runFormatCommand formatArgs
    ("--help" : _)      -> printHelp
    []                  -> printHelp
    _                   -> do
      putStrLn "Unknown command. Use 'smyth --help' for usage."
      exitFailure

runTestCommand :: [String] -> IO ()
runTestCommand args = do
  -- Find project root
  maybeRoot <- findSmythfile
  case maybeRoot of
    Nothing -> do
      putStrLn "Error: No Smythfile.lq found (searched up from current directory)"
      exitFailure
    Just root -> do
      -- Load configuration
      config <- loadSmythConfig root
      runTests config args

runRunCommand :: [String] -> IO ()
runRunCommand args = do
  case args of
    (file : rest) -> do
      -- Find project root
      maybeRoot <- findSmythfile
      case maybeRoot of
        Nothing -> do
          putStrLn "Error: No Smythfile.lq found (searched up from current directory)"
          exitFailure
        Just root -> do
          -- Load configuration
          config <- loadSmythConfig root
          let (optArgs, extraArgs) = break (== "--") rest
          case parseRunOptions optArgs of
            Left err -> do
              putStrLn err
              putStrLn "Usage: smyth run [--pid-file <path>] [--timeout <ms>] <file> -- <args>"
              exitFailure
            Right opts -> do
              case extraArgs of
                [] -> runFileWithOptions config opts file
                ("--" : runArgs) -> withArgs runArgs (runFileWithOptions config opts file)
                _ -> do
                  putStrLn "Error: extra arguments for 'smyth run' must follow '--'"
                  putStrLn "Usage: smyth run [--pid-file <path>] [--timeout <ms>] <file> -- <args>"
                  exitFailure
    _ -> do
      putStrLn "Error: 'smyth run' requires a file argument"
      putStrLn "Usage: smyth run [--pid-file <path>] [--timeout <ms>] <file> -- <args>"
      exitFailure

parseRunOptions :: [String] -> Either String RunOptions
parseRunOptions args = go args defaultRunOptions
  where
    go remaining opts = case remaining of
      [] -> Right opts
      "--pid-file" : path : rest -> go rest opts { runPidFile = Just path }
      "--timeout" : ms : rest ->
        case readMaybe ms of
          Nothing -> Left "Error: --timeout expects an integer (milliseconds)"
          Just val -> go rest opts { runTimeoutMs = Just val }
      unknown : _ -> Left ("Error: unknown option for 'smyth run': " ++ unknown)

runBenchCommand :: [String] -> IO ()
runBenchCommand args = do
  maybeRoot <- findSmythfile
  case maybeRoot of
    Nothing -> do
      putStrLn "Error: No Smythfile.lq found (searched up from current directory)"
      exitFailure
    Just root -> do
      config <- loadSmythConfig root
      runBench config args

runCountCommand :: [String] -> IO ()
runCountCommand args = do
  case args of
    [] -> do
      maybeRoot <- findSmythfile
      case maybeRoot of
        Nothing -> do
          putStrLn "Error: No Smythfile.lq found (searched up from current directory)"
          exitFailure
        Just root -> do
          config <- loadSmythConfig root
          runCount config
    _ -> do
      putStrLn "Error: 'smyth count' does not take arguments"
      putStrLn "Usage: smyth count"
      exitFailure

runDependenciesCommand :: [String] -> IO ()
runDependenciesCommand args = do
  maybeRoot <- findSmythfile
  case maybeRoot of
    Nothing -> do
      putStrLn "Error: No Smythfile.lq found (searched up from current directory)"
      exitFailure
    Just root -> do
      config <- loadSmythConfig root
      runDependencies config args

runFormatCommand :: [String] -> IO ()
runFormatCommand args = do
  maybeRoot <- findSmythfile
  case maybeRoot of
    Nothing -> do
      putStrLn "Error: No Smythfile.lq found (searched up from current directory)"
      exitFailure
    Just root -> do
      config <- loadSmythConfig root
      runFormat config args

runDumpCommand :: [String] -> IO ()
runDumpCommand args = do
  maybeRoot <- findSmythfile
  case maybeRoot of
    Nothing -> do
      putStrLn "Error: No Smythfile.lq found (searched up from current directory)"
      exitFailure
    Just root -> do
      config <- loadSmythConfig root
      runDump config args

printHelp :: IO ()
printHelp = do
  putStrLn "smyth - Locque build tool"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  smyth run [--pid-file <path>] [--timeout <ms>] <file> -- <args>    Type check and run a .lq/.lqs file"
  putStrLn "  smyth test [--slow] [--verbose] Run all tests (test/main.lq)"
  putStrLn "  smyth test <file>   Run specific test file"
  putStrLn "  smyth bench         Run benchmarks (test/bench.lq)"
  putStrLn "  smyth bench <file>  Run a specific benchmark file"
  putStrLn "  smyth count         Count .lq lines in lib/ and test/"
  putStrLn "  smyth dependencies  Print lib/ module dependencies as a tree"
  putStrLn "  smyth dump (core|normalized|elaborated|typed|typed-normalized|types|types-normalized) <file> [name]"
  putStrLn "  smyth dump --multi <file> <mode[:name]>... [-- <file> <mode[:name]>...]"
  putStrLn "  smyth format [path] Check .lq formatting in a file or directory"
  putStrLn "  smyth --help        Show this help"
  putStrLn ""
  putStrLn "Notes:"
  putStrLn "  All commands type-check before execution"
  putStrLn "  Project root found by searching for Smythfile.lq"
  putStrLn "  'smyth format' defaults to lib/, test/, and Smythfile.lq under the Smythfile.lq directory"
  putStrLn "  'smyth test --slow' prints the slowest suites from test/main.lq"
  putStrLn "  'smyth test --verbose' prints per-suite timing from test/main.lq"
  putStrLn "  LOCQUE_TIMING=true prints per-stage timings (also enabled by --slow)"
