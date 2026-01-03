{-# LANGUAGE OverloadedStrings #-}
module SmythEmit
  ( runEmit
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, takeExtension)

import AST (Module)
import DictPass (transformModuleWithEnvs)
import Parser (parseMExprFile, parseModuleFile)
import SmythConfig (SmythConfig(..))
import Validator (checkParens)
import qualified TypeChecker as TC

import Locque.Compiler.Codegen (emitModule)
import CompilerLower (lowerModule)
import Locque.Compiler.Emit (emitHsPath)

runEmit :: SmythConfig -> [String] -> IO ()
runEmit config args = do
  (outDir, file) <- parseArgs args
  contents <- TIO.readFile file
  case parseAny file contents of
    Left err -> failWith err
    Right m -> do
      typeResult <- TC.typeCheckModuleWithImports (projectRoot config) contents m
      case typeResult of
        Left tcErr -> failWith ("Type error: " ++ show tcErr)
        Right _env -> do
          elaborated <- transformModuleWithEnvs (projectRoot config) m
          let coreModule = lowerModule elaborated
              hsOutput = emitModule coreModule
              outPath = emitHsPath outDir file
          createDirectoryIfMissing True (takeDirectory outPath)
          TIO.writeFile outPath hsOutput
          putStrLn ("Wrote " ++ outPath)

parseArgs :: [String] -> IO (Maybe FilePath, FilePath)
parseArgs args =
  case args of
    [file] -> pure (Nothing, file)
    ["--out-dir", dir, file] -> pure (Just dir, file)
    _ -> do
      putStrLn "Usage: smyth emit-hs [--out-dir <dir>] <file>"
      exitFailure

parseAny :: FilePath -> T.Text -> Either String Module
parseAny file contents =
  case takeExtension file of
    ".lq"  -> parseMExprFile file contents
    ".lqs" -> do
      _ <- checkParens file contents
      parseModuleFile file contents
    _      -> Left ("Unsupported file extension (expected .lq or .lqs): " ++ file)

failWith :: String -> IO a
failWith msg = do
  putStrLn msg
  exitFailure
