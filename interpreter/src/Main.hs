module Main where

import qualified Data.Text.IO as T
import           System.Environment (getArgs)
import           System.Exit (die)
import           System.Directory (getCurrentDirectory)
import           System.FilePath (takeDirectory, (</>))

import           Eval
import           Parser
import           Validator

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> putStrLn usage
    ["--run-lqs", file] -> runLqs file
    ["--run-lq", file] -> die "M-expression (.lq) parsing not implemented yet."
    ["--emit-lqs", file, out] -> die "Conversion from .lq to .lqs not implemented yet."
    ["--validate", file] -> validateLqs file
    [] -> runLqs "../examples/00_hello_world.lqs"
    [file] -> runLqs file
    _ -> die usage

runLqs :: FilePath -> IO ()
runLqs file = do
  cwd <- getCurrentDirectory
  let projectRoot = takeDirectory cwd
  contents <- T.readFile file
  case parseModuleFile file contents of
    Left err -> die err
    Right m  -> runModuleMain projectRoot m

usage :: String
usage = unlines
  [ "locque-interpreter usage:"
  , "  locque-interpreter [--run-lqs <file>]"
  , "  locque-interpreter --run-lq <file>            (not yet implemented)"
  , "  locque-interpreter --emit-lqs <file> <out>    (not yet implemented)"
  , "  locque-interpreter --validate <file>          (validate S-expr module)"
  , "  locque-interpreter --help"
  , "Default: run ../examples/00_hello_world.lqs"
  ]

validateLqs :: FilePath -> IO ()
validateLqs file = do
  contents <- T.readFile file
  case checkParens file contents of
    Left e -> die e
    Right _ -> case parseModuleFile file contents of
      Left err -> die err
      Right m  -> case validateModule m of
        Left e  -> die e
        Right _ -> putStrLn "ok"
