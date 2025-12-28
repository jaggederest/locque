{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import SmythConfig (findSmythfile, loadSmythConfig)
import SmythTest (runTests)
import SmythRun (runFile)
import ProjectChecks (requireLibTests)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("test" : testArgs) -> runTestCommand testArgs
    ("run" : runArgs)   -> runRunCommand runArgs
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
      requireLibTests root
      -- Run tests
      runTests config args

runRunCommand :: [String] -> IO ()
runRunCommand args = do
  case args of
    [file] -> do
      -- Find project root
      maybeRoot <- findSmythfile
      case maybeRoot of
        Nothing -> do
          putStrLn "Error: No Smythfile.lq found (searched up from current directory)"
          exitFailure
        Just root -> do
          -- Load configuration
          config <- loadSmythConfig root
          requireLibTests root
          -- Run file
          runFile config file
    _ -> do
      putStrLn "Error: 'smyth run' requires exactly one file argument"
      putStrLn "Usage: smyth run <file>"
      exitFailure

printHelp :: IO ()
printHelp = do
  putStrLn "smyth - Locque build tool"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  smyth run <file>    Type check and run a .lq/.lqs file"
  putStrLn "  smyth test          Run all tests (test/main.lq)"
  putStrLn "  smyth test <file>   Run specific test file"
  putStrLn "  smyth --help        Show this help"
  putStrLn ""
  putStrLn "Notes:"
  putStrLn "  All commands type-check before execution"
  putStrLn "  Project root found by searching for Smythfile.lq"
