{-# LANGUAGE TypeApplications #-}
module CompilerPipeline
  ( loadAnnotatedModule
  , loadCoreModule
  ) where

import Control.Exception (SomeException, try, evaluate)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath (takeExtension)

import AST (Module)
import CompilerLower (lowerModule)
import DictPass (transformModuleWithEnvs)
import Eval (ctorArityMap)
import Parser (parseMExprFile, parseModuleFile)
import Recursor (recursorDefs, insertRecursors)
import SmythConfig (SmythConfig(..))
import Validator (checkParens)
import qualified RunCache as RC
import qualified TypeChecker as TC

import qualified Locque.Compiler.Core as Core

loadCoreModule :: SmythConfig -> FilePath -> IO (Either String Core.CoreModule)
loadCoreModule config file = do
  annotated <- loadAnnotatedModule config file
  pure (fmap lowerModule annotated)

loadAnnotatedModule :: SmythConfig -> FilePath -> IO (Either String Module)
loadAnnotatedModule config file = do
  contents <- TIO.readFile file
  case parseAny file contents of
    Left err -> pure (Left err)
    Right m -> do
      digestAttempt <- try @SomeException (TC.moduleDigestWithImports (projectRoot config) contents m)
      case digestAttempt of
        Left err -> pure (Left ("Type check phase error: " ++ show err))
        Right (digest, importedEnv) -> do
          cached <- RC.readRunCache (projectRoot config) file digest
          case cached of
            Just entry -> pure (Right (RC.cacheAnnotated entry))
            Nothing -> do
              tcAttempt <- try (evaluate (TC.typeCheckAndNormalizeWithEnv importedEnv m))
                :: IO (Either SomeException (Either TC.TypeError (TC.TypeEnv, Module)))
              case tcAttempt of
                Left err -> pure (Left ("Type check phase error: " ++ show err))
                Right (Left tcErr) -> pure (Left ("Type error: " ++ show tcErr))
                Right (Right (env, normalized)) -> do
                  let arity = ctorArityMap normalized
                      recDefs = recursorDefs normalized
                  prepared <- case insertRecursors m recDefs of
                    Left msg -> pure (Left ("Transform error: " ++ msg))
                    Right ok -> pure (Right ok)
                  case prepared of
                    Left msg -> pure (Left msg)
                    Right preparedModule -> do
                      transformed <- transformModuleWithEnvs (projectRoot config) preparedModule
                      case TC.annotateModule env transformed of
                        Left annotErr -> pure (Left ("Annotation error: " ++ show annotErr))
                        Right annotatedM -> do
                          let entry = RC.RunCache
                                { RC.cacheVersion = RC.cacheVersionCurrent
                                , RC.cacheDigest = digest
                                , RC.cacheAnnotated = annotatedM
                                , RC.cacheCtorArity = arity
                                }
                          RC.writeRunCache (projectRoot config) file entry
                          pure (Right annotatedM)

parseAny :: FilePath -> T.Text -> Either String Module
parseAny file contents =
  case takeExtension file of
    ".lq"  -> parseMExprFile file contents
    ".lqs" -> do
      _ <- checkParens file contents
      parseModuleFile file contents
    _      -> Left ("Unsupported file extension (expected .lq or .lqs): " ++ file)
