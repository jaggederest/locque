{-# LANGUAGE OverloadedStrings #-}
module SmythEmit
  ( runEmit
  ) where

import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory)

import SmythConfig (SmythConfig(..))
import CompilerPipeline (loadCoreModule, loadCoreModuleWithDebug)

import Locque.Compiler.Codegen (EmitOptions(..), defaultEmitOptions, emitModuleWith)
import Locque.Compiler.Emit (emitHsPath)

runEmit :: SmythConfig -> [String] -> IO ()
runEmit config args = do
  (emitArgs, file) <- parseArgs args
  coreResult <-
    if emitDebug emitArgs
      then fmap (fmap (\(coreModule, debugInfo) -> (coreModule, Just debugInfo)))
        (loadCoreModuleWithDebug config file)
      else fmap (fmap (\coreModule -> (coreModule, Nothing)))
        (loadCoreModule config file)
  case coreResult of
    Left err -> failWith err
    Right (coreModule, maybeDebug) -> do
      let emitOptions = case maybeDebug of
            Nothing -> defaultEmitOptions
            Just debugInfo -> defaultEmitOptions { emitDebugInfo = debugInfo }
          hsOutput = emitModuleWith emitOptions coreModule
          outPath = emitHsPath (emitOutDir emitArgs) file
      createDirectoryIfMissing True (takeDirectory outPath)
      TIO.writeFile outPath hsOutput
      putStrLn ("Wrote " ++ outPath)

data EmitArgs = EmitArgs
  { emitOutDir :: Maybe FilePath
  , emitDebug :: Bool
  } deriving (Eq, Show)

defaultEmitArgs :: EmitArgs
defaultEmitArgs = EmitArgs
  { emitOutDir = Nothing
  , emitDebug = False
  }

parseArgs :: [String] -> IO (EmitArgs, FilePath)
parseArgs args = go args defaultEmitArgs
  where
    go remaining opts = case remaining of
      [] -> usage
      [file] -> pure (opts, file)
      "--out-dir" : dir : rest ->
        go rest opts { emitOutDir = Just dir }
      "--debug" : rest ->
        go rest opts { emitDebug = True }
      _ -> usage
    usage = do
      putStrLn "Usage: smyth emit-hs [--out-dir <dir>] [--debug] <file>"
      exitFailure

failWith :: String -> IO a
failWith msg = do
  putStrLn msg
  exitFailure
