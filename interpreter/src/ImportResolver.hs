{-# LANGUAGE OverloadedStrings #-}
module ImportResolver
  ( ImportScope(..)
  , ResolvedModule(..)
  , resolveModulePath
  ) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath ((</>), (<.>), splitSearchPath, takeDirectory)
import System.IO.Unsafe (unsafePerformIO)

import Utils (modNameToPath)

data ImportScope
  = ProjectScope
  | StdlibScope
  deriving (Eq, Show)

data ResolvedModule = ResolvedModule
  { resolvedPath :: FilePath
  , resolvedScope :: ImportScope
  } deriving (Eq, Show)

resolveModulePath
  :: FilePath
  -> FilePath
  -> ImportScope
  -> T.Text
  -> IO ResolvedModule
resolveModulePath projectRoot libRoot scope modName = do
  let modPathFull = modNameToPath modName
      (hasStdlibPrefix, rawName) = stripStdlibPrefix modName
      stdlibPathPrefix = "standard-library/"
      (hasStdlibPath, rawPath) = case List.stripPrefix stdlibPathPrefix modPathFull of
        Just rest -> (True, rest)
        Nothing -> (False, modPathFull)
      isStdlib = hasStdlibPrefix || hasStdlibPath
      modPath =
        if hasStdlibPrefix
          then modNameToPath rawName
          else rawPath
  if null modPath
    then error "standard-library import requires a module name"
    else do
      let isTest = "test/" `List.isPrefixOf` modPath
      stdlibPaths <- stdlibRoots projectRoot
      let roots =
            if isStdlib
              then map (\path -> (path, StdlibScope)) stdlibPaths
              else if isTest
                then [(projectRoot, ProjectScope)]
                else case scope of
                  StdlibScope -> map (\path -> (path, StdlibScope)) stdlibPaths
                  ProjectScope ->
                    (projectRoot </> libRoot, ProjectScope)
                      : map (\path -> (path, StdlibScope)) stdlibPaths
      resolved <- findModule modPath roots
      case resolved of
        Just (path, resolvedScope) -> pure (ResolvedModule path resolvedScope)
        Nothing ->
          error ("Module file not found for import: " ++ T.unpack modName)

stripStdlibPrefix :: T.Text -> (Bool, T.Text)
stripStdlibPrefix name =
  let prefix = "standard-library::"
  in if prefix `T.isPrefixOf` name
      then (True, T.drop (T.length prefix) name)
      else (False, name)

findModule :: FilePath -> [(FilePath, ImportScope)] -> IO (Maybe (FilePath, ImportScope))
findModule modPath roots = do
  matches <- catMaybes <$> mapM (findInRoot modPath) roots
  pure (List.find (const True) matches)

findInRoot :: FilePath -> (FilePath, ImportScope) -> IO (Maybe (FilePath, ImportScope))
findInRoot modPath (root, scope) = do
  let basePath = root </> modPath
      lqPath = basePath <.> "lq"
      lqsPath = basePath <.> "lqs"
  lqExists <- doesFileExist lqPath
  if lqExists
    then pure (Just (lqPath, scope))
    else do
      lqsExists <- doesFileExist lqsPath
      if lqsExists
        then pure (Just (lqsPath, scope))
        else pure Nothing

{-# NOINLINE stdlibRootsRef #-}
stdlibRootsRef :: IORef (Map.Map FilePath [FilePath])
stdlibRootsRef = unsafePerformIO (newIORef Map.empty)

stdlibRoots :: FilePath -> IO [FilePath]
stdlibRoots projectRoot = do
  cache <- readIORef stdlibRootsRef
  case Map.lookup projectRoot cache of
    Just roots -> pure roots
    Nothing -> do
      roots <- computeStdlibRoots projectRoot
      modifyIORef' stdlibRootsRef (Map.insert projectRoot roots)
      pure roots

computeStdlibRoots :: FilePath -> IO [FilePath]
computeStdlibRoots projectRoot = do
  env <- lookupEnv "LOCQUE_STDLIB"
  let envRoots = maybe [] splitSearchPath env
  exeRoot <- findStdlibRootFromExecutable
  repoRoot <- findStdlibRootFromRepo projectRoot
  let roots = dedupePaths (envRoots ++ maybeToList exeRoot ++ maybeToList repoRoot)
  pure roots
  where
    maybeToList Nothing = []
    maybeToList (Just path) = [path]

findStdlibRootFromExecutable :: IO (Maybe FilePath)
findStdlibRootFromExecutable = do
  exePath <- getExecutablePath
  findStdlibRoot (takeDirectory exePath)

findStdlibRootFromRepo :: FilePath -> IO (Maybe FilePath)
findStdlibRootFromRepo projectRoot = search projectRoot
  where
    search dir = do
      isRepo <- isLocqueRepoRoot dir
      if isRepo
        then pure (Just (dir </> "lib"))
        else do
          let parent = takeDirectory dir
          if parent == dir
            then pure Nothing
            else search parent

    isLocqueRepoRoot dir = do
      let libRoot = dir </> "lib"
          preludeLq = libRoot </> "prelude.lq"
          preludeLqs = libRoot </> "prelude.lqs"
          interpreterCabal = dir </> "interpreter" </> "locque-interpreter.cabal"
      hasPrelude <- or <$> mapM doesFileExist [preludeLq, preludeLqs]
      hasInterpreter <- doesFileExist interpreterCabal
      pure (hasPrelude && hasInterpreter)

findStdlibRoot :: FilePath -> IO (Maybe FilePath)
findStdlibRoot start = do
  let candidate = start </> "lib"
      preludeLq = candidate </> "prelude.lq"
      preludeLqs = candidate </> "prelude.lqs"
  exists <- or <$> mapM doesFileExist [preludeLq, preludeLqs]
  if exists
    then pure (Just candidate)
    else do
      let parent = takeDirectory start
      if parent == start
        then pure Nothing
        else findStdlibRoot parent

dedupePaths :: [FilePath] -> [FilePath]
dedupePaths = List.nub . filter (not . null)
