{-# LANGUAGE OverloadedStrings #-}
module Utils
  ( modNameToPath
  , qualifyName
  , resolvePath
  , listLqFiles
  ) where

import Control.Monad (forM)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), isAbsolute, takeExtension)

modNameToPath :: Text -> FilePath
modNameToPath modName =
  let withSlashes = T.replace "::" "/" modName
      lowercased = T.toLower withSlashes
  in T.unpack lowercased

qualifyName :: Text -> Text -> Text
qualifyName prefix name = prefix <> "::" <> name

-- | Resolve a path relative to a root directory (identity if already absolute)
resolvePath :: FilePath -> FilePath -> FilePath
resolvePath root path =
  if isAbsolute path then path else root </> path

-- | Recursively list all .lq files under a directory
listLqFiles :: FilePath -> IO [FilePath]
listLqFiles dir = do
  entries <- listDirectory dir
  paths <- forM entries $ \entry -> do
    let path = dir </> entry
    isDir <- doesDirectoryExist path
    if isDir
      then listLqFiles path
      else if takeExtension path == ".lq"
        then pure [path]
        else pure []
  pure (concat paths)
