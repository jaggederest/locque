module Main where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import           System.Environment (getArgs)
import           System.Exit (die)
import           System.Directory (getCurrentDirectory)
import           System.FilePath (takeExtension)

import           AST (Module(..), defName)
import           Eval (ctorArityMap, runModuleMain)
import           Parser
import           Validator
import qualified Type as LT
import qualified TypeChecker as TC
import           DictPass (transformModuleWithEnvs)
import           Recursor (recursorDefs, insertRecursors)
import           SmythConfig (findSmythfile)

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
    ["dump", mode, file] -> dumpFile mode file Nothing
    ["dump", mode, file, name] -> dumpFile mode file (Just (T.pack name))
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

parseAny :: FilePath -> Text -> Either String Module
parseAny file contents =
  case takeExtension file of
    ".lq"  -> parseMExprFile file contents
    ".lqs" -> do
      _ <- checkParens file contents
      parseModuleFile file contents
    _      -> Left ("Unsupported file extension (expected .lq or .lqs): " ++ file)

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

dumpFile :: String -> FilePath -> Maybe Text -> IO ()
dumpFile mode file selected = do
  projectRoot <- getProjectRoot
  contents <- TIO.readFile file
  case parseAny file contents of
    Left err -> die err
    Right m -> do
      case mode of
        "core" -> do
          m' <- selectModule m selected
          TIO.putStrLn (moduleToSExprText m')
        "normalized" -> do
          result <- TC.normalizeModuleWithImports projectRoot file contents m
          case result of
            Left tcErr -> die ("Type error: " ++ show tcErr)
            Right normalized -> do
              m' <- selectModule normalized selected
              TIO.putStrLn (moduleToSExprText m')
        "elaborated" -> do
          typeResult <- TC.typeCheckModuleWithImports projectRoot file contents m
          case typeResult of
            Left tcErr -> die ("Type error: " ++ show tcErr)
            Right _env -> do
              elaborated <- transformModuleWithEnvs projectRoot m
              m' <- selectModule elaborated selected
              TIO.putStrLn (moduleToSExprText m')
        "typed" -> do
          typeResult <- TC.typeCheckAndNormalizeWithImports projectRoot file contents m
          case typeResult of
            Left tcErr -> die ("Type error: " ++ show tcErr)
            Right (env, _normalized) -> do
              case TC.annotateModule env m of
                Left annotErr -> die ("Annotation error: " ++ show annotErr)
                Right annotated -> do
                  m' <- selectModule annotated selected
                  TIO.putStrLn (moduleToSExprTextTyped m')
        "typed-normalized" -> do
          typeResult <- TC.typeCheckAndNormalizeWithImportsOpaqueRecur projectRoot file contents m
          case typeResult of
            Left tcErr -> die ("Type error: " ++ show tcErr)
            Right (env, normalized) -> do
              case TC.annotateModule env normalized of
                Left annotErr -> die ("Annotation error: " ++ show annotErr)
                Right annotated -> do
                  m' <- selectModule annotated selected
                  TIO.putStrLn (moduleToSExprTextTyped m')
        "types" -> do
          typeResult <- TC.typeCheckModuleWithImports projectRoot file contents m
          case typeResult of
            Left tcErr -> die ("Type error: " ++ show tcErr)
            Right env -> do
              m' <- selectModule m selected
              dumpTypes m' env
        "types-normalized" -> do
          typeResult <- TC.normalizeTypeEnvWithImports projectRoot file contents m
          case typeResult of
            Left tcErr -> die ("Type error: " ++ show tcErr)
            Right env -> do
              m' <- selectModule m selected
              dumpTypes m' env
        _ -> die usage

selectModule :: Module -> Maybe Text -> IO Module
selectModule m Nothing = pure m
selectModule m (Just name) =
  case filter (\defn -> defName defn == name) (modDefs m) of
    [] -> die ("Definition not found: " ++ T.unpack name)
    defs -> pure m { modDefs = defs }

dumpTypes :: Module -> TC.TypeEnv -> IO ()
dumpTypes m env = do
  let names = map defName (modDefs m)
      render name =
        case Map.lookup name env of
          Nothing -> die ("Type missing for definition: " ++ T.unpack name)
          Just ty ->
            pure (T.concat
              [ T.pack "(of-type "
              , name
              , T.pack " "
              , LT.typeToSExpr ty
              , T.pack ")"
              ])
  rendered <- mapM render names
  TIO.putStrLn (T.unlines rendered)

getProjectRoot :: IO FilePath
getProjectRoot = do
  maybeRoot <- findSmythfile
  case maybeRoot of
    Just root -> pure root
    Nothing -> getCurrentDirectory
