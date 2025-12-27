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
    ["run", file] -> runFile file
    ["typecheck", file] -> typecheckFile file
    ["emit-lqs", file, out] -> emitLqs file out
    ["emit-lq", file, out] -> emitLq file out
    ["validate", file] -> validateLqs file
    _ -> die usage

runFile :: FilePath -> IO ()
runFile file = do
  cwd <- getCurrentDirectory
  let projectRoot = takeDirectory cwd
  contents <- T.readFile file
  case parseAny file contents of
    Left err -> die err
    Right m  -> do
      typeResult <- TC.typeCheckModuleWithImports projectRoot contents m
      case typeResult of
        Left tcErr -> die ("Type error: " ++ show tcErr)
        Right _env -> do _ <- runModuleMain projectRoot m; pure ()

usage :: String
usage = unlines
  [ "locque-interpreter usage:"
  , "  locque-interpreter run <file>                 (type check + run .lq/.lqs)"
  , "  locque-interpreter typecheck <file>           (type check only)"
  , "  locque-interpreter emit-lqs <in.lq> <out.lqs> (M-expr -> S-expr)"
  , "  locque-interpreter emit-lq <in.lqs> <out.lq>  (S-expr -> M-expr)"
  , "  locque-interpreter validate <file.lqs>        (paren/structural check)"
  , "  locque-interpreter --help"
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
  case parseAny file contents of
    Left err -> die err
    Right m  -> do
      result <- TC.typeCheckModuleWithImports projectRoot contents m
      case result of
        Left tcErr -> die ("Type error: " ++ show tcErr)
        Right env  -> do
          putStrLn "✓ Type check passed"
          putStrLn ("  Definitions checked: " ++ show (Map.size env))

parseAny :: FilePath -> T.Text -> Either String Module
parseAny file contents =
  case takeExtension file of
    ".lq"  -> parseMExprFile file contents
    ".lqs" -> do
      _ <- checkParens file contents
      parseModuleFile file contents
    _      -> Left ("Unsupported file extension (expected .lq or .lqs): " ++ file)

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
