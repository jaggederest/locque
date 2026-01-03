{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (bracket, finally)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
  ( createDirectoryIfMissing
  , getCurrentDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory)
import System.IO
  ( hClose
  , hFlush
  , openTempFile
  , stdout
  )
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Process (readCreateProcessWithExitCode, proc)
import Test.Hspec

import CompilerLower (lowerModule)
import DictPass (transformModuleWithEnvs)
import Locque.Compiler.Codegen (emitModule)
import Parser (parseMExprFile)
import Recursor (recursorDefs, insertRecursors)
import SmythConfig (loadSmythConfig)
import SmythRun (runFileNoExit)
import SmythCompile (CompileOptions(..), defaultCompileOutPath, parseCompileArgs)
import qualified TypeChecker as TC
import qualified Locque.Compiler.Core as Core

main :: IO ()
main = hspec $ do
  describe "Compiler golden tests" $ do
    it "matches interpreter output for hello" $ do
      runGolden "test/compile/hello.lq"

    it "matches interpreter output for sequencing" $ do
      runGolden "test/compile/sequence.lq"

  describe "Compile argument helpers" $ do
    it "parses an output override" $ do
      parseCompileArgs ["--out", "bin/app", "test/compile/hello.lq"]
        `shouldBe` Right (CompileOptions (Just "bin/app"), "test/compile/hello.lq")

    it "computes the default output path" $ do
      defaultCompileOutPath "/repo" "test/compile/hello.lq"
        `shouldBe` "/repo/tmp/locque/bin/hello"

runGolden :: FilePath -> IO ()
runGolden relPath = do
  root <- projectRoot
  config <- loadSmythConfig root
  let absPath = root </> relPath
  (interpOut, ok) <- captureStdout (runFileNoExit config absPath)
  ok `shouldBe` True
  compiledOut <- compileAndRun root absPath
  compiledOut `shouldBe` interpOut

compileAndRun :: FilePath -> FilePath -> IO String
compileAndRun root file = do
  coreModule <- loadCoreModule root file
  let hsText = emitModule coreModule
  withTempDir (root </> "tmp" </> "locque" </> "gen") "golden" $ \dir -> do
    let genPath = dir </> "LocqueGen.hs"
        mainPath = dir </> "Main.hs"
        binPath = dir </> "locque-golden"
        buildDir = dir </> "build"
    createDirectoryIfMissing True buildDir
    TIO.writeFile genPath hsText
    TIO.writeFile mainPath (wrapperSource "LocqueGen")
    compileResult <-
      readCreateProcessWithExitCode
        (proc "ghc"
          [ "-i" <> (root </> "compiler" </> "src")
          , "-i" <> dir
          , "-odir"
          , buildDir
          , "-hidir"
          , buildDir
          , "-o"
          , binPath
          , mainPath
          , genPath
          ])
        ""
    case compileResult of
      (ExitSuccess, _out, _err) -> do
        (exitCode, out, err) <- readCreateProcessWithExitCode (proc binPath []) ""
        case exitCode of
          ExitSuccess -> pure out
          ExitFailure code ->
            fail ("compiled program failed (" ++ show code ++ "): " ++ err)
      (ExitFailure code, out, err) ->
        fail ("ghc failed (" ++ show code ++ "): " ++ out ++ err)

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

loadCoreModule :: FilePath -> FilePath -> IO Core.CoreModule
loadCoreModule root file = do
  contents <- TIO.readFile file
  case parseMExprFile file contents of
    Left parseErr -> fail ("Parse error: " ++ parseErr)
    Right m -> do
      digestResult <- TC.moduleDigestWithImports root contents m
      let (_digest, importedEnv) = digestResult
      case TC.typeCheckAndNormalizeWithEnv importedEnv m of
        Left tcErr -> fail ("Type error: " ++ show tcErr)
        Right (_env, normalized) -> do
          let recDefs = recursorDefs normalized
          prepared <-
            case insertRecursors m recDefs of
              Left msg -> fail ("Transform error: " ++ msg)
              Right ok -> pure ok
          transformed <- transformModuleWithEnvs root prepared
          pure (lowerModule transformed)

projectRoot :: IO FilePath
projectRoot = do
  cwd <- getCurrentDirectory
  pure (takeDirectory cwd)

captureStdout :: IO a -> IO (String, a)
captureStdout action =
  bracket (hDuplicate stdout) hClose $ \original ->
    bracket (openTempFile "/tmp" "locque_stdout") cleanup $ \(path, handle) -> do
      hDuplicateTo handle stdout
      hClose handle
      result <- action `finally` hDuplicateTo original stdout
      hFlush stdout
      out <- readFile path
      pure (out, result)
  where
    cleanup (path, handle) = do
      hClose handle
      removeFile path

withTempDir :: FilePath -> String -> (FilePath -> IO a) -> IO a
withTempDir base name action = do
  createDirectoryIfMissing True base
  (path, handle) <- openTempFile base name
  hClose handle
  removeFile path
  createDirectoryIfMissing True path
  action path `finally` removeDirectoryRecursive path
