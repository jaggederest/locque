{-# LANGUAGE OverloadedStrings #-}
module SmythDump
  ( runDump
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (exitFailure)
import System.FilePath (takeExtension)

import AST (Module(..), defName)
import DictPass (transformModuleWithEnvs)
import Parser (parseMExprFile, parseModuleFile, moduleToSExprText)
import Validator (checkParens)
import qualified Type as LT
import qualified TypeChecker as TC
import SmythConfig (SmythConfig(..))

runDump :: SmythConfig -> [String] -> IO ()
runDump config args = case args of
  ("--multi" : rest) -> runDumpMulti config rest
  [mode, file] -> dumpFile config mode file Nothing
  [mode, file, name] -> dumpFile config mode file (Just (T.pack name))
  _ -> do
    putStrLn "Usage: smyth dump (core|normalized|elaborated|types|types-normalized|types-elaborated) <file> [name]"
    putStrLn "   or: smyth dump --multi <file> <mode[:name]>... [-- <file> <mode[:name]>...]"
    exitFailure

runDumpMulti :: SmythConfig -> [String] -> IO ()
runDumpMulti config args = do
  let groups = splitGroups args
  if null groups
    then do
      putStrLn "Usage: smyth dump --multi <file> <mode[:name]>... [-- <file> <mode[:name]>...]"
      exitFailure
    else mapM_ (dumpGroup config) groups

splitGroups :: [String] -> [[String]]
splitGroups args = reverse (finalize current acc)
  where
    (current, acc) = go args [] []
    go [] curr groups = (curr, groups)
    go ("--" : rest) curr groups = go rest [] (finalize curr groups)
    go (x : rest) curr groups = go rest (x : curr) groups
    finalize curr groups =
      if null curr then groups else reverse curr : groups

dumpGroup :: SmythConfig -> [String] -> IO ()
dumpGroup _ [] = do
  putStrLn "Usage: smyth dump --multi <file> <mode[:name]>... [-- <file> <mode[:name]>...]"
  exitFailure
dumpGroup config (file : specs) = do
  if null specs
    then do
      putStrLn "Usage: smyth dump --multi <file> <mode[:name]>... [-- <file> <mode[:name]>...]"
      exitFailure
    else dumpFileMulti config file (map parseSpec specs)

parseSpec :: String -> (String, Maybe T.Text)
parseSpec spec =
  case break (== ':') spec of
    (mode, "") -> (mode, Nothing)
    (mode, ':' : name) -> (mode, Just (T.pack name))
    _ -> (spec, Nothing)

dumpFileMulti :: SmythConfig -> FilePath -> [(String, Maybe T.Text)] -> IO ()
dumpFileMulti config file specs = do
  contents <- TIO.readFile file
  case parseAny file contents of
    Left err -> failWith err
    Right m ->
      mapM_ (dumpMode config file contents m) specs

dumpMode :: SmythConfig -> FilePath -> T.Text -> Module -> (String, Maybe T.Text) -> IO ()
dumpMode config file contents m (mode, selected) =
  case mode of
    "core" -> do
      m' <- selectModule m selected
      TIO.putStrLn (moduleToSExprText m')
    "normalized" -> do
      result <- TC.normalizeModuleWithImports (projectRoot config) contents m
      case result of
        Left tcErr -> failWith ("Type error: " ++ show tcErr)
        Right normalized -> do
          m' <- selectModule normalized selected
          TIO.putStrLn (moduleToSExprText m')
    "elaborated" -> do
      typeResult <- TC.typeCheckModuleWithImports (projectRoot config) contents m
      case typeResult of
        Left tcErr -> failWith ("Type error: " ++ show tcErr)
        Right _env -> do
          elaborated <- transformModuleWithEnvs (projectRoot config) m
          m' <- selectModule elaborated selected
          TIO.putStrLn (moduleToSExprText m')
    "types" -> do
      typeResult <- TC.typeCheckModuleWithImports (projectRoot config) contents m
      case typeResult of
        Left tcErr -> failWith ("Type error: " ++ show tcErr)
        Right env -> do
          m' <- selectModule m selected
          dumpTypes m' env
    "types-normalized" -> do
      typeResult <- TC.normalizeTypeEnvWithImports (projectRoot config) contents m
      case typeResult of
        Left tcErr -> failWith ("Type error: " ++ show tcErr)
        Right env -> do
          m' <- selectModule m selected
          dumpTypes m' env
    "types-elaborated" -> do
      elaborated <- transformModuleWithEnvs (projectRoot config) m
      typeResult <- TC.typeCheckModuleWithImports (projectRoot config) contents elaborated
      case typeResult of
        Left tcErr -> failWith ("Type error: " ++ show tcErr)
        Right env -> do
          m' <- selectModule elaborated selected
          dumpTypes m' env
    _ -> do
      putStrLn "Usage: smyth dump (core|normalized|elaborated|types|types-normalized|types-elaborated) <file> [name]"
      exitFailure

dumpFile :: SmythConfig -> String -> FilePath -> Maybe T.Text -> IO ()
dumpFile config mode file selected = do
  contents <- TIO.readFile file
  case parseAny file contents of
    Left err -> failWith err
    Right m ->
      case mode of
        "core" -> do
          m' <- selectModule m selected
          TIO.putStrLn (moduleToSExprText m')
        "normalized" -> do
          result <- TC.normalizeModuleWithImports (projectRoot config) contents m
          case result of
            Left tcErr -> failWith ("Type error: " ++ show tcErr)
            Right normalized -> do
              m' <- selectModule normalized selected
              TIO.putStrLn (moduleToSExprText m')
        "elaborated" -> do
          typeResult <- TC.typeCheckModuleWithImports (projectRoot config) contents m
          case typeResult of
            Left tcErr -> failWith ("Type error: " ++ show tcErr)
            Right _env -> do
              elaborated <- transformModuleWithEnvs (projectRoot config) m
              m' <- selectModule elaborated selected
              TIO.putStrLn (moduleToSExprText m')
        "types" -> do
          typeResult <- TC.typeCheckModuleWithImports (projectRoot config) contents m
          case typeResult of
            Left tcErr -> failWith ("Type error: " ++ show tcErr)
            Right env -> do
              m' <- selectModule m selected
              dumpTypes m' env
        "types-normalized" -> do
          typeResult <- TC.normalizeTypeEnvWithImports (projectRoot config) contents m
          case typeResult of
            Left tcErr -> failWith ("Type error: " ++ show tcErr)
            Right env -> do
              m' <- selectModule m selected
              dumpTypes m' env
        "types-elaborated" -> do
          elaborated <- transformModuleWithEnvs (projectRoot config) m
          typeResult <- TC.typeCheckModuleWithImports (projectRoot config) contents elaborated
          case typeResult of
            Left tcErr -> failWith ("Type error: " ++ show tcErr)
            Right env -> do
              m' <- selectModule elaborated selected
              dumpTypes m' env
        _ -> do
          putStrLn "Usage: smyth dump (core|normalized|elaborated|types|types-normalized|types-elaborated) <file> [name]"
          exitFailure

parseAny :: FilePath -> T.Text -> Either String Module
parseAny file contents =
  case takeExtension file of
    ".lq"  -> parseMExprFile file contents
    ".lqs" -> do
      _ <- checkParens file contents
      parseModuleFile file contents
    _      -> Left ("Unsupported file extension (expected .lq or .lqs): " ++ file)

selectModule :: Module -> Maybe T.Text -> IO Module
selectModule m Nothing = pure m
selectModule m (Just name) =
  case filter (\defn -> defName defn == name) (modDefs m) of
    [] -> failWith ("Definition not found: " ++ T.unpack name)
    defs -> pure m { modDefs = defs }

dumpTypes :: Module -> TC.TypeEnv -> IO ()
dumpTypes m env = do
  let names = map defName (modDefs m)
      render name =
        case Map.lookup name env of
          Nothing -> failWith ("Type missing for definition: " ++ T.unpack name)
          Just ty ->
            pure (T.concat
              [ "(of-type "
              , name
              , " "
              , LT.typeToSExpr ty
              , ")"
              ])
  rendered <- mapM render names
  TIO.putStrLn (T.unlines rendered)

failWith :: String -> IO a
failWith msg = do
  putStrLn msg
  exitFailure
