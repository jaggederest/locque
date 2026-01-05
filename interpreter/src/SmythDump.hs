{-# LANGUAGE OverloadedStrings #-}
module SmythDump
  ( runDump
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (exitFailure)
import System.FilePath (takeExtension)

import AST
import CompilerPipeline (loadAnnotatedModuleWithImports, loadCoreModule)
import DictPass (transformModuleWithEnvs)
import Parser (parseMExprFile, parseModuleFile, moduleToSExprText, moduleToSExprTextTyped)
import Validator (checkParens)
import qualified Type as LT
import qualified TypeChecker as TC
import SmythConfig (SmythConfig(..))
import StripTyped (stripRecursorsModule, stripTypedModule)
import qualified Locque.Compiler.Core as Core
import qualified Locque.Compiler.CoreErased as Erased
import Locque.Compiler.CorePretty (renderCoreModule)
import Locque.Compiler.CoreErasedPretty (renderErasedModule)
import Locque.Compiler.Erase (eraseModule)

runDump :: SmythConfig -> [String] -> IO ()
runDump config args = case args of
  ("--multi" : rest) -> runDumpMulti config rest
  [mode, file] -> dumpFile config mode file Nothing
  [mode, file, name] -> dumpFile config mode file (Just (T.pack name))
  _ -> do
    putStrLn "Usage: smyth dump (core|normalized|elaborated|elaborated-combined|typed|typed-normalized|typed-elaborated|types|types-normalized|types-elaborated|core-ir|erased-ir) <file> [name]"
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
      result <- TC.normalizeModuleWithImports (projectRoot config) file contents m
      case result of
        Left tcErr -> failWith ("Type error: " ++ show tcErr)
        Right normalized -> do
          m' <- selectModule normalized selected
          TIO.putStrLn (moduleToSExprText m')
    "elaborated" -> do
      typeResult <- TC.typeCheckModuleWithImports (projectRoot config) file contents m
      case typeResult of
        Left tcErr -> failWith ("Type error: " ++ show tcErr)
        Right _env -> do
          elaborated <- transformModuleWithEnvs (projectRoot config) m
          m' <- selectModule elaborated selected
          TIO.putStrLn (moduleToSExprText m')
    "elaborated-combined" -> do
      combinedResult <- loadAnnotatedModuleWithImports config file
      case combinedResult of
        Left err -> failWith err
        Right elaboratedCombined -> do
          let stripped = stripTypedModule elaboratedCombined
          m' <- selectModule stripped selected
          TIO.putStrLn (moduleToSExprText m')
    "typed" -> do
      typeResult <- TC.typeCheckAndNormalizeWithImports (projectRoot config) file contents m
      case typeResult of
        Left tcErr -> failWith ("Type error: " ++ show tcErr)
        Right (env, _normalized) -> do
          case TC.annotateModule env m of
            Left annotErr -> failWith ("Annotation error: " ++ show annotErr)
            Right annotated -> do
              m' <- selectModule annotated selected
              TIO.putStrLn (moduleToSExprTextTyped m')
    "typed-normalized" -> do
      typeResult <- TC.typeCheckAndNormalizeWithImports (projectRoot config) file contents m
      case typeResult of
        Left tcErr -> failWith ("Type error: " ++ show tcErr)
        Right (env, normalized) -> do
          case TC.annotateModule env normalized of
            Left annotErr -> failWith ("Annotation error: " ++ show annotErr)
            Right annotated -> do
              m' <- selectModule annotated selected
              TIO.putStrLn (moduleToSExprTextTyped m')
    "typed-elaborated" -> do
      elaborated <- transformModuleWithEnvs (projectRoot config) m
      combinedResult <- loadAnnotatedModuleWithImports config file
      case combinedResult of
        Left err -> failWith err
        Right elaboratedCombined -> do
          let stripped = stripRecursorsModule (stripTypedModule elaboratedCombined)
          case TC.typeCheckModuleFull stripped of
            Left tcErr -> failWith ("Type error: " ++ show tcErr)
            Right env -> do
              let envWithOpens = TC.processOpens (modOpens elaborated) env
              case TC.annotateModule envWithOpens elaborated of
                Left annotErr -> failWith ("Annotation error: " ++ show annotErr)
                Right annotated -> do
                  m' <- selectModule annotated selected
                  TIO.putStrLn (moduleToSExprTextTyped m')
    "types" -> do
      typeResult <- TC.typeCheckModuleWithImports (projectRoot config) file contents m
      case typeResult of
        Left tcErr -> failWith ("Type error: " ++ show tcErr)
        Right env -> do
          m' <- selectModule m selected
          dumpTypes m' env
    "types-normalized" -> do
      typeResult <- TC.normalizeTypeEnvWithImports (projectRoot config) file contents m
      case typeResult of
        Left tcErr -> failWith ("Type error: " ++ show tcErr)
        Right env -> do
          m' <- selectModule m selected
          dumpTypes m' env
    "types-elaborated" -> do
      elaborated <- transformModuleWithEnvs (projectRoot config) m
      combinedResult <- loadAnnotatedModuleWithImports config file
      case combinedResult of
        Left err -> failWith err
        Right elaboratedCombined -> do
          let stripped = stripRecursorsModule (stripTypedModule elaboratedCombined)
          case TC.typeCheckModuleFull stripped of
            Left tcErr -> failWith ("Type error: " ++ show tcErr)
            Right env -> do
              let envWithOpens = TC.processOpens (modOpens elaborated) env
              m' <- selectModule elaborated selected
              dumpTypes m' (TC.tcEnvTypes envWithOpens)
    "core-ir" -> do
      coreResult <- loadCoreModule config file
      case coreResult of
        Left err -> failWith err
        Right coreModule -> do
          core' <- selectCoreModule coreModule selected
          TIO.putStrLn (renderCoreModule core')
    "erased-ir" -> do
      coreResult <- loadCoreModule config file
      case coreResult of
        Left err -> failWith err
        Right coreModule -> do
          let erasedModule = eraseModule coreModule
          erased' <- selectErasedModule erasedModule selected
          TIO.putStrLn (renderErasedModule erased')
    _ -> do
      putStrLn "Usage: smyth dump (core|normalized|elaborated|elaborated-combined|typed|typed-normalized|typed-elaborated|types|types-normalized|types-elaborated|core-ir|erased-ir) <file> [name]"
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
          result <- TC.normalizeModuleWithImports (projectRoot config) file contents m
          case result of
            Left tcErr -> failWith ("Type error: " ++ show tcErr)
            Right normalized -> do
              m' <- selectModule normalized selected
              TIO.putStrLn (moduleToSExprText m')
        "elaborated" -> do
          typeResult <- TC.typeCheckModuleWithImports (projectRoot config) file contents m
          case typeResult of
            Left tcErr -> failWith ("Type error: " ++ show tcErr)
            Right _env -> do
              elaborated <- transformModuleWithEnvs (projectRoot config) m
              m' <- selectModule elaborated selected
              TIO.putStrLn (moduleToSExprText m')
        "elaborated-combined" -> do
          combinedResult <- loadAnnotatedModuleWithImports config file
          case combinedResult of
            Left err -> failWith err
            Right elaboratedCombined -> do
              let stripped = stripTypedModule elaboratedCombined
              m' <- selectModule stripped selected
              TIO.putStrLn (moduleToSExprText m')
        "typed" -> do
          typeResult <- TC.typeCheckAndNormalizeWithImports (projectRoot config) file contents m
          case typeResult of
            Left tcErr -> failWith ("Type error: " ++ show tcErr)
            Right (env, _normalized) -> do
              case TC.annotateModule env m of
                Left annotErr -> failWith ("Annotation error: " ++ show annotErr)
                Right annotated -> do
                  m' <- selectModule annotated selected
                  TIO.putStrLn (moduleToSExprTextTyped m')
        "typed-normalized" -> do
          typeResult <- TC.typeCheckAndNormalizeWithImports (projectRoot config) file contents m
          case typeResult of
            Left tcErr -> failWith ("Type error: " ++ show tcErr)
            Right (env, normalized) -> do
              case TC.annotateModule env normalized of
                Left annotErr -> failWith ("Annotation error: " ++ show annotErr)
                Right annotated -> do
                  m' <- selectModule annotated selected
                  TIO.putStrLn (moduleToSExprTextTyped m')
        "typed-elaborated" -> do
          elaborated <- transformModuleWithEnvs (projectRoot config) m
          combinedResult <- loadAnnotatedModuleWithImports config file
          case combinedResult of
            Left err -> failWith err
            Right elaboratedCombined -> do
              let stripped = stripRecursorsModule (stripTypedModule elaboratedCombined)
              case TC.typeCheckModuleFull stripped of
                Left tcErr -> failWith ("Type error: " ++ show tcErr)
                Right env -> do
                  let envWithOpens = TC.processOpens (modOpens elaborated) env
                  case TC.annotateModule envWithOpens elaborated of
                    Left annotErr -> failWith ("Annotation error: " ++ show annotErr)
                    Right annotated -> do
                      m' <- selectModule annotated selected
                      TIO.putStrLn (moduleToSExprTextTyped m')
        "types" -> do
          typeResult <- TC.typeCheckModuleWithImports (projectRoot config) file contents m
          case typeResult of
            Left tcErr -> failWith ("Type error: " ++ show tcErr)
            Right env -> do
              m' <- selectModule m selected
              dumpTypes m' env
        "types-normalized" -> do
          typeResult <- TC.normalizeTypeEnvWithImports (projectRoot config) file contents m
          case typeResult of
            Left tcErr -> failWith ("Type error: " ++ show tcErr)
            Right env -> do
              m' <- selectModule m selected
              dumpTypes m' env
        "types-elaborated" -> do
          elaborated <- transformModuleWithEnvs (projectRoot config) m
          combinedResult <- loadAnnotatedModuleWithImports config file
          case combinedResult of
            Left err -> failWith err
            Right elaboratedCombined -> do
              let stripped = stripRecursorsModule (stripTypedModule elaboratedCombined)
              case TC.typeCheckModuleFull stripped of
                Left tcErr -> failWith ("Type error: " ++ show tcErr)
                Right env -> do
                  let envWithOpens = TC.processOpens (modOpens elaborated) env
                  m' <- selectModule elaborated selected
                  dumpTypes m' (TC.tcEnvTypes envWithOpens)
        "core-ir" -> do
          coreResult <- loadCoreModule config file
          case coreResult of
            Left err -> failWith err
            Right coreModule -> do
              core' <- selectCoreModule coreModule selected
              TIO.putStrLn (renderCoreModule core')
        "erased-ir" -> do
          coreResult <- loadCoreModule config file
          case coreResult of
            Left err -> failWith err
            Right coreModule -> do
              let erasedModule = eraseModule coreModule
              erased' <- selectErasedModule erasedModule selected
              TIO.putStrLn (renderErasedModule erased')
        _ -> do
          putStrLn "Usage: smyth dump (core|normalized|elaborated|elaborated-combined|typed|typed-normalized|typed-elaborated|types|types-normalized|types-elaborated|core-ir|erased-ir) <file> [name]"
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

selectCoreModule :: Core.CoreModule -> Maybe T.Text -> IO Core.CoreModule
selectCoreModule m Nothing = pure m
selectCoreModule (Core.CoreModule name decls) (Just target) =
  case filter (\decl -> coreDeclName decl == target) decls of
    [] -> failWith ("Definition not found: " ++ T.unpack target)
    matches -> pure (Core.CoreModule name matches)

coreDeclName :: Core.CoreDecl -> T.Text
coreDeclName decl = case decl of
  Core.CoreDef name _ _ -> Core.unName name
  Core.CoreDefComp name _ _ -> Core.unName name
  Core.CoreData dataDecl -> Core.unName (Core.dataName dataDecl)

selectErasedModule :: Erased.ErasedModule -> Maybe T.Text -> IO Erased.ErasedModule
selectErasedModule m Nothing = pure m
selectErasedModule (Erased.ErasedModule name decls) (Just target) =
  case filter (\decl -> erasedDeclName decl == target) decls of
    [] -> failWith ("Definition not found: " ++ T.unpack target)
    matches -> pure (Erased.ErasedModule name matches)

erasedDeclName :: Erased.ErasedDecl -> T.Text
erasedDeclName decl = case decl of
  Erased.EDef name _ -> Core.unName name
  Erased.EDefComp name _ -> Core.unName name
  Erased.EData dataDecl -> Core.unName (Erased.erasedDataName dataDecl)

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
