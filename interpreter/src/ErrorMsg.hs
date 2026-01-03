{-# LANGUAGE OverloadedStrings #-}
module ErrorMsg
  ( ErrorMsg(..)
  , ErrorSuggestion(..)
  , ErrorContext(..)
  , buildErrorMsg
  , formatError
  , levenshteinDistance
  , findFuzzyMatches
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Array
import SourceLoc

-- | Structured error with context
data ErrorMsg = ErrorMsg
  { errLoc         :: SrcLoc
  , errMainMsg     :: Text
  , errContext     :: Maybe ErrorContext
  , errSuggestions :: [ErrorSuggestion]
  , errNote        :: Maybe Text
  } deriving (Show, Eq)

-- | Source code context
data ErrorContext = ErrorContext
  { ctxSourceLine :: Text
  , ctxHighlight  :: (Int, Int)  -- (start_col, end_col)
  } deriving (Show, Eq)

-- | Suggestions for fixes
data ErrorSuggestion
  = DidYouMean Text
  | InlineFix Text Text  -- (description, code_fix)
  | Hint Text
  deriving (Show, Eq)

-- | Build an error message (convenience constructor)
buildErrorMsg :: SrcLoc -> Text -> ErrorMsg
buildErrorMsg loc msg = ErrorMsg loc msg Nothing [] Nothing

-- | Format error for display (compact, LLM-friendly)
formatError :: ErrorMsg -> Text
formatError (ErrorMsg loc mainMsg mbCtx suggestions mbNote) =
  T.unlines $ filter (not . T.null)
    [ prettyLoc loc <> ": " <> mainMsg
    , formatContext mbCtx
    , formatSuggestions suggestions
    , maybe "" (\n -> "Note: " <> n) mbNote
    ]

-- | Format source code context with highlighting
formatContext :: Maybe ErrorContext -> Text
formatContext Nothing = ""
formatContext (Just (ErrorContext line (startCol, endCol))) =
  let indent = "  "
      markerLen = max 1 (endCol - startCol + 1)
      marker = T.replicate (startCol - 1) " " <> T.replicate markerLen "^"
  in T.unlines [indent <> line, indent <> marker]

-- | Format suggestions
formatSuggestions :: [ErrorSuggestion] -> Text
formatSuggestions [] = ""
formatSuggestions sgs = T.unlines (map formatSug sgs)
  where
    formatSug (DidYouMean name) = "  Did you mean: " <> name <> "?"
    formatSug (InlineFix desc fix) = "  " <> desc <> ": " <> fix
    formatSug (Hint txt) = "  Hint: " <> txt

-- | Levenshtein distance using dynamic programming (O(n*m))
levenshteinDistance :: Text -> Text -> Int
levenshteinDistance s1 s2 =
  let str1 = T.unpack s1
      str2 = T.unpack s2
      n = length str1
      m = length str2

      arrBounds = ((0, 0), (n, m))
      arr = listArray arrBounds [compute i j | (i, j) <- range arrBounds]

      compute 0 j = j
      compute i 0 = i
      compute i j =
        minimum [ arr ! (i-1, j) + 1                             -- deletion
                , arr ! (i, j-1) + 1                             -- insertion
                , arr ! (i-1, j-1) + if str1 !! (i-1) == str2 !! (j-1) then 0 else 1  -- substitution
                ]
  in arr ! (n, m)

-- | Find fuzzy matches (distance <= 2), return top 3
findFuzzyMatches :: Text -> [Text] -> [Text]
findFuzzyMatches target candidates =
  let withDist = [(name, levenshteinDistance target name) | name <- candidates]
      filtered = filter (\(_, d) -> d <= 2 && d > 0) withDist
      sorted = sortBy (comparing snd) filtered
  in take 3 (map fst sorted)
