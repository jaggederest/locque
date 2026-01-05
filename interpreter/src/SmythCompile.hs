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

import CompilerPipeline (loadCoreModule, loadCoreModuleWithDebug)
import ImportResolver (resolveCompilerSrc)
import Locque.Compiler.Codegen (EmitOptions(..), defaultEmitOptions, emitModuleWith)
import SmythConfig (SmythConfig(..))

data CompileOptions = CompileOptions
  { compileOutPath :: Maybe FilePath
  , compileDebug :: Bool
  } deriving (Eq, Show)

defaultCompileOutPath :: FilePath -> FilePath -> FilePath
defaultCompileOutPath root file =
  root </> "tmp" </> "locque" </> "bin" </> takeBaseName file

parseCompileArgs :: [String] -> Either String (CompileOptions, FilePath)
parseCompileArgs args = go args (CompileOptions Nothing False)
  where
    go remaining opts = case remaining of
      [] -> Left "Error: 'smyth compile' requires a file argument"
      [file] -> Right (opts, file)
      "--out" : path : rest ->
        go rest opts { compileOutPath = Just path }
      "--debug" : rest ->
        go rest opts { compileDebug = True }
      unknown : _ ->
        Left ("Error: unknown option for 'smyth compile': " ++ unknown)

runCompile :: SmythConfig -> [String] -> IO ()
runCompile config args = do
  let (optArgs, extraArgs) = break (== "--") args
  case parseCompileArgs optArgs of
    Left err -> do
      putStrLn err
      putStrLn "Usage: smyth compile [--out <path>] [--debug] <file> [-- <args>]"
      exitFailure
    Right (opts, file) -> do
      coreResult <-
        if compileDebug opts
          then fmap (fmap (\(coreModule, debugInfo) -> (coreModule, Just debugInfo)))
            (loadCoreModuleWithDebug config file)
          else fmap (fmap (\coreModule -> (coreModule, Nothing)))
            (loadCoreModule config file)
      case coreResult of
        Left err -> failWith err
        Right (coreModule, maybeDebug) -> do
          let root = projectRoot config
              outPath = maybe (defaultCompileOutPath root file) id (compileOutPath opts)
              baseName = takeBaseName file
              genDir = root </> "tmp" </> "locque" </> "gen" </> baseName
              buildDir = root </> "tmp" </> "locque" </> "build" </> baseName
              genPath = genDir </> "LocqueGen.hs"
              mainPath = genDir </> "Main.hs"
              emitOptions = case maybeDebug of
                Nothing -> defaultEmitOptions
                Just debugInfo -> defaultEmitOptions { emitDebugInfo = debugInfo }
              hsOutput = emitModuleWith emitOptions coreModule
          compilerSrc <- resolveCompilerSrc root
          createDirectoryIfMissing True genDir
          createDirectoryIfMissing True buildDir
          createDirectoryIfMissing True (takeDirectory outPath)
          TIO.writeFile genPath hsOutput
          TIO.writeFile mainPath (wrapperSource "LocqueGen")
          let packageFlags =
                [ "-package", "text"
                , "-package", "filepath"
                , "-package", "bytestring"
                , "-package", "containers"
                , "-package", "directory"
                , "-package", "process"
                , "-package", "time"
                , "-package", "unix"
                , "-package", "network"
                , "-package", "stm"
                ]
          let ghcArgs =
                [ "-i" <> compilerSrc
                , "-i" <> genDir
                , "-odir"
                , buildDir
                , "-hidir"
                , buildDir
                ]
                  <> packageFlags
                  <> [ "-o"
                     , outPath
                     , mainPath
                     , genPath
                     ]
          compileResult <-
            readCreateProcessWithExitCode
              (proc "ghc" ghcArgs)
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
    , "import Control.Monad (when)"
    , "import System.Environment (getArgs, withArgs)"
    , "import LocqueRuntime (runComp, assertionCountPrim)"
    , "main :: IO ()"
    , "main = do"
    , "  args <- getArgs"
    , "  let wantCount = \"--locque-assertions\" `elem` args"
    , "      filtered = filter (/= \"--locque-assertions\") args"
    , "  withArgs filtered (runComp L.main)"
    , "  when wantCount $ do"
    , "    count <- runComp assertionCountPrim"
    , "    putStrLn (\"LOCQUE_ASSERTIONS=\" ++ show count)"
    ]

failWith :: String -> IO a
failWith msg = do
  putStrLn msg
  exitFailure
