module Main where

import qualified Data.Text.IO as T
import qualified Data.Map.Strict as Map
import           System.Environment (getArgs)
import           System.Exit (die)
import           System.Directory (getCurrentDirectory)
import           System.FilePath (takeDirectory, takeExtension)

import           Eval
import           Parser
import           Validator
import qualified TypeChecker as TC

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> putStrLn usage
    ["--run-lqs", file] -> runLqs True file
    ["--run-lq", file] -> runLq True file
    ["--run-lqs", "--skip-typecheck", file] -> runLqs False file
    ["--run-lq", "--skip-typecheck", file] -> runLq False file
    ["--skip-typecheck", "--run-lqs", file] -> runLqs False file
    ["--skip-typecheck", "--run-lq", file] -> runLq False file
    ["--typecheck", file] -> typecheckFile file
    ["--emit-lqs", file, out] -> emitLqs file out
    ["--emit-lq", file, out] -> emitLq file out
    ["--validate", file] -> validateLqs file
    [] -> runLqs True "../examples/00_hello_world.lqs"
    [file] -> runLqs True file
    _ -> die usage

runLqs :: Bool -> FilePath -> IO ()
runLqs doTypeCheck file = do
  cwd <- getCurrentDirectory
  let projectRoot = takeDirectory cwd
  contents <- T.readFile file
  case parseModuleFile file contents of
    Left err -> die err
    Right m  -> do
      -- Type check before execution (if enabled)
      if doTypeCheck
        then do
          typeResult <- TC.typeCheckModuleWithImports projectRoot m
          case typeResult of
            Left tcErr -> die ("Type error: " ++ show tcErr)
            Right _env -> do _ <- runModuleMain projectRoot m; pure ()
        else do _ <- runModuleMain projectRoot m; pure ()

runLq :: Bool -> FilePath -> IO ()
runLq doTypeCheck file = do
  cwd <- getCurrentDirectory
  let projectRoot = takeDirectory cwd
  contents <- T.readFile file
  case parseMExprFile file contents of
    Left err -> die err
    Right m  -> do
      -- Type check before execution (if enabled)
      if doTypeCheck
        then do
          typeResult <- TC.typeCheckModuleWithImports projectRoot m
          case typeResult of
            Left tcErr -> die ("Type error: " ++ show tcErr)
            Right _env -> do _ <- runModuleMain projectRoot m; pure ()
        else do _ <- runModuleMain projectRoot m; pure ()

usage :: String
usage = unlines
  [ "locque-interpreter usage:"
  , "  locque-interpreter [--run-lqs <file>]         (run with type checking)"
  , "  locque-interpreter --run-lq <file>            (run with type checking)"
  , "  locque-interpreter --skip-typecheck --run-lq <file>  (skip type checking)"
  , "  locque-interpreter --typecheck <file>         (type check only, don't run)"
  , "  locque-interpreter --emit-lqs <file> <out>"
  , "  locque-interpreter --emit-lq <file> <out>"
  , "  locque-interpreter --validate <file>          (validate S-expr module)"
  , "  locque-interpreter --help"
  , "Default: run ../examples/00_hello_world.lqs (with type checking)"
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

typecheckFile :: FilePath -> IO ()
typecheckFile file = do
  cwd <- getCurrentDirectory
  let projectRoot = takeDirectory cwd
  contents <- T.readFile file
  let parseResult = if takeExtension file == ".lq"
                    then parseMExprFile file contents
                    else do
                      _ <- checkParens file contents
                      parseModuleFile file contents
  case parseResult of
    Left err -> die err
    Right m  -> do
      result <- TC.typeCheckModuleWithImports projectRoot m
      case result of
        Left tcErr -> die ("Type error: " ++ show tcErr)
        Right env  -> do
          putStrLn "✓ Type check passed"
          putStrLn ("  Definitions checked: " ++ show (Map.size env))

emitLqs :: FilePath -> FilePath -> IO ()
emitLqs file out = do
  contents <- T.readFile file
  case parseMExprFile file contents of
    Left err -> die err
    Right m  -> T.writeFile out (moduleToSExprText m)

emitLq :: FilePath -> FilePath -> IO ()
emitLq file out = do
  contents <- T.readFile file
  case checkParens file contents of
    Left e -> die e
    Right _ -> case parseModuleFile file contents of
      Left err -> die err
      Right m  -> T.writeFile out (moduleToMExprText m)
