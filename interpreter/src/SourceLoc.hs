{-# LANGUAGE OverloadedStrings #-}
module SourceLoc
  ( SrcLoc(..)
  , SrcSpan(..)
  , noLoc
  , prettyLoc
  , extractSourceLine
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Source location (file, line, column)
data SrcLoc = SrcLoc
  { locFile   :: FilePath
  , locLine   :: Int
  , locColumn :: Int
  } | NoLoc  -- For generated/unknown locations
  deriving (Show, Eq)

-- | Source span (range) - for future use
data SrcSpan = SrcSpan SrcLoc SrcLoc
  deriving (Show, Eq)

-- | Placeholder for unknown locations
noLoc :: SrcLoc
noLoc = NoLoc

-- | Pretty-print a source location
prettyLoc :: SrcLoc -> Text
prettyLoc NoLoc = "<unknown>"
prettyLoc (SrcLoc file line col) =
  T.pack file <> ":" <> T.pack (show line) <> ":" <> T.pack (show col)

-- | Extract source line from file contents
extractSourceLine :: FilePath -> Text -> Int -> Maybe Text
extractSourceLine _file contents lineNum =
  let ls = T.lines contents
  in if lineNum > 0 && lineNum <= length ls
     then Just (ls !! (lineNum - 1))
     else Nothing
