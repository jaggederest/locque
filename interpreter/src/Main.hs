module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import           System.Environment (getArgs)
import           System.Exit (die)
import           System.Directory (getCurrentDirectory)

import           Eval (ctorArityMap, runModuleMain)
import           Parser (parseMExprFile, parseModuleFile, moduleToSExprText, moduleToMExprText)
import           Validator (validateModule, checkParens)
import qualified TypeChecker as TC
import           DictPass (transformModuleWithEnvs)
import           Recursor (recursorDefs, insertRecursors)
import           SmythConfig (SmythConfig(..), findSmythfile, loadSmythConfig)
import           SmythDump (parseAny, dumpMode)

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
    ["dump", mode, file] -> doDump mode file Nothing
    ["dump", mode, file, name] -> doDump mode file (Just (T.pack name))
    _ -> die usage

runFile :: FilePath -> IO ()
runFile file = do
  projectRoot <- getProjectRoot
  contents <- TIO.readFile file
  case parseAny file contents of
    Left err -> die err
    Right m  -> do
      typeResult <- TC.typeCheckAndNormalizeWithImports projectRoot file contents m
      case typeResult of
        Left tcErr -> die ("Type error: " ++ show tcErr)
        Right (_env, normalized) -> do
          let ctorArity = ctorArityMap normalized
              recDefs = recursorDefs normalized
          prepared <- case insertRecursors m recDefs of
            Left msg -> die ("Transform error: " ++ msg)
            Right prepared -> pure prepared
          m' <- transformModuleWithEnvs projectRoot prepared
          _ <- runModuleMain projectRoot ctorArity m'
          pure ()

usage :: String
usage = unlines
  [ "locque-interpreter usage:"
  , "  locque-interpreter run <file>                 (type check + run .lq/.lqs)"
  , "  locque-interpreter typecheck <file>           (type check only)"
  , "  locque-interpreter emit-lqs <in.lq> <out.lqs> (M-expr -> S-expr)"
  , "  locque-interpreter emit-lq <in.lqs> <out.lq>  (S-expr -> M-expr)"
  , "  locque-interpreter validate <file.lqs>        (paren/structural check)"
  , "  locque-interpreter dump (core|normalized|elaborated|typed|typed-normalized|types|types-normalized) <file> [name]"
  , "  locque-interpreter --help"
  ]

validateLqs :: FilePath -> IO ()
validateLqs file = do
  contents <- TIO.readFile file
  case checkParens file contents of
    Left e -> die e
    Right _ -> case parseModuleFile file contents of
      Left err -> die err
      Right m  -> case validateModule m of
        Left e  -> die e
        Right _ -> putStrLn "ok"

typecheckFile :: FilePath -> IO ()
typecheckFile file = do
  projectRoot <- getProjectRoot
  contents <- TIO.readFile file
  case parseAny file contents of
    Left err -> die err
    Right m  -> do
      result <- TC.typeCheckModuleWithImports projectRoot file contents m
      case result of
        Left tcErr -> die ("Type error: " ++ show tcErr)
        Right env  -> do
          putStrLn "✓ Type check passed"
          putStrLn ("  Definitions checked: " ++ show (Map.size env))

emitLqs :: FilePath -> FilePath -> IO ()
emitLqs file out = do
  contents <- TIO.readFile file
  case parseMExprFile file contents of
    Left err -> die err
    Right m  -> TIO.writeFile out (moduleToSExprText m)

emitLq :: FilePath -> FilePath -> IO ()
emitLq file out = do
  contents <- TIO.readFile file
  case checkParens file contents of
    Left e -> die e
    Right _ -> case parseModuleFile file contents of
      Left err -> die err
      Right m  -> TIO.writeFile out (moduleToMExprText m)

-- | Delegate dump to SmythDump.dumpMode
doDump :: String -> FilePath -> Maybe T.Text -> IO ()
doDump mode file selected = do
  config <- getConfig
  contents <- TIO.readFile file
  case parseAny file contents of
    Left err -> die err
    Right m -> dumpMode config file contents m (mode, selected)

getProjectRoot :: IO FilePath
getProjectRoot = do
  maybeRoot <- findSmythfile
  case maybeRoot of
    Just root -> pure root
    Nothing -> getCurrentDirectory

getConfig :: IO SmythConfig
getConfig = do
  root <- getProjectRoot
  loadSmythConfig root
