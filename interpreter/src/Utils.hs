{-# LANGUAGE OverloadedStrings #-}
module Utils
  ( modNameToPath
  , qualifyName
  ) where

import Data.Text (Text)
import qualified Data.Text as T

modNameToPath :: Text -> FilePath
modNameToPath modName =
  let withSlashes = T.replace "::" "/" modName
      lowercased = T.toLower withSlashes
  in T.unpack lowercased

qualifyName :: Text -> Text -> Text
qualifyName prefix name = prefix <> "::" <> name
