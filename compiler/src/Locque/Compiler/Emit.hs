module Locque.Compiler.Emit
  ( emitHsPath
  ) where

import System.FilePath (takeBaseName, replaceExtension, (</>))

emitHsPath :: Maybe FilePath -> FilePath -> FilePath
emitHsPath maybeOutDir inputPath =
  case maybeOutDir of
    Nothing -> replaceExtension inputPath ".hs"
    Just outDir -> outDir </> takeBaseName inputPath <> ".hs"
