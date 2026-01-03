{-# LANGUAGE OverloadedStrings #-}
module SmythEmit
  ( runEmit
  ) where

import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory)

import SmythConfig (SmythConfig(..))
import CompilerPipeline (loadCoreModule)

import Locque.Compiler.Codegen (emitModule)
import Locque.Compiler.Emit (emitHsPath)

runEmit :: SmythConfig -> [String] -> IO ()
runEmit config args = do
  (outDir, file) <- parseArgs args
  coreResult <- loadCoreModule config file
  case coreResult of
    Left err -> failWith err
    Right coreModule -> do
      let hsOutput = emitModule coreModule
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

failWith :: String -> IO a
failWith msg = do
  putStrLn msg
  exitFailure
