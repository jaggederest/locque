module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import SmythCompile (runCompile)
import SmythConfig (findSmythfile, loadSmythConfig)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("compile" : compileArgs) -> runCompileCommand compileArgs
    ["--help"] -> printHelp
    [] -> do
      printHelp
      exitFailure
    _ -> do
      putStrLn "Unknown command. Use 'locque-compiler --help' for usage."
      exitFailure

runCompileCommand :: [String] -> IO ()
runCompileCommand args = do
  maybeRoot <- findSmythfile
  case maybeRoot of
    Nothing -> do
      putStrLn "Error: No Smythfile.lq found (searched up from current directory)"
      exitFailure
    Just root -> do
      config <- loadSmythConfig root
      runCompile config args

printHelp :: IO ()
printHelp = do
  putStrLn "locque-compiler - Locque compiler driver"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  locque-compiler compile [--out <path>] [--debug] <file> [-- <args>]"
  putStrLn "  locque-compiler --help"
