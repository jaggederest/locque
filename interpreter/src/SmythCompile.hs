{-# LANGUAGE OverloadedStrings #-}
module SmythCompile
  ( CompileOptions(..)
  , defaultCompileOutPath
  , parseCompileArgs
  , runCompile
  ) where

import Control.Monad (unless)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>), takeBaseName, takeDirectory)
import System.IO (hPutStr, stderr)
import System.Process (proc, readCreateProcessWithExitCode)

import CompilerPipeline (loadCoreModule)
import Locque.Compiler.Codegen (emitModule)
import SmythConfig (SmythConfig(..))

data CompileOptions = CompileOptions
  { compileOutPath :: Maybe FilePath
  } deriving (Eq, Show)

defaultCompileOutPath :: FilePath -> FilePath -> FilePath
defaultCompileOutPath root file =
  root </> "tmp" </> "locque" </> "bin" </> takeBaseName file

parseCompileArgs :: [String] -> Either String (CompileOptions, FilePath)
parseCompileArgs args = go args (CompileOptions Nothing)
  where
    go remaining opts = case remaining of
      [] -> Left "Error: 'smyth compile' requires a file argument"
      [file] -> Right (opts, file)
      "--out" : path : rest ->
        go rest opts { compileOutPath = Just path }
      unknown : _ ->
        Left ("Error: unknown option for 'smyth compile': " ++ unknown)

runCompile :: SmythConfig -> [String] -> IO ()
runCompile config args = do
  let (optArgs, extraArgs) = break (== "--") args
  case parseCompileArgs optArgs of
    Left err -> do
      putStrLn err
      putStrLn "Usage: smyth compile [--out <path>] <file> [-- <args>]"
      exitFailure
    Right (opts, file) -> do
      coreResult <- loadCoreModule config file
      case coreResult of
        Left err -> failWith err
        Right coreModule -> do
          let root = projectRoot config
              outPath = maybe (defaultCompileOutPath root file) id (compileOutPath opts)
              baseName = takeBaseName file
              genDir = root </> "tmp" </> "locque" </> "gen" </> baseName
              buildDir = root </> "tmp" </> "locque" </> "build" </> baseName
              genPath = genDir </> "LocqueGen.hs"
              mainPath = genDir </> "Main.hs"
              compilerSrc = root </> "compiler" </> "src"
              hsOutput = emitModule coreModule
          createDirectoryIfMissing True genDir
          createDirectoryIfMissing True buildDir
          createDirectoryIfMissing True (takeDirectory outPath)
          TIO.writeFile genPath hsOutput
          TIO.writeFile mainPath (wrapperSource "LocqueGen")
          compileResult <-
            readCreateProcessWithExitCode
              (proc "ghc"
                [ "-i" <> compilerSrc
                , "-i" <> genDir
                , "-odir"
                , buildDir
                , "-hidir"
                , buildDir
                , "-o"
                , outPath
                , mainPath
                , genPath
                ])
              ""
          case compileResult of
            (ExitFailure code, out, err) ->
              failWith ("ghc failed (" ++ show code ++ "): " ++ out ++ err)
            (ExitSuccess, out, err) -> do
              unless (null out) (putStr out)
              unless (null err) (hPutStr stderr err)
              putStrLn ("Wrote " ++ outPath)
              case extraArgs of
                [] -> pure ()
                ("--" : runArgs) -> runCompiled outPath runArgs
                _ -> pure ()

runCompiled :: FilePath -> [String] -> IO ()
runCompiled outPath runArgs = do
  (exitCode, out, err) <-
    readCreateProcessWithExitCode (proc outPath runArgs) ""
  unless (null out) (putStr out)
  unless (null err) (hPutStr stderr err)
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure code ->
      failWith ("compiled program failed (" ++ show code ++ ")")

wrapperSource :: T.Text -> T.Text
wrapperSource moduleName =
  T.unlines
    [ "{-# LANGUAGE OverloadedStrings #-}"
    , "module Main where"
    , "import qualified " <> moduleName <> " as L"
    , "import LocqueRuntime (runComp)"
    , "main :: IO ()"
    , "main = runComp L.main"
    ]

failWith :: String -> IO a
failWith msg = do
  putStrLn msg
  exitFailure
