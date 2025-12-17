module Validator
  ( validateModule
  , checkParens
  ) where

import           AST
import qualified Data.Text as T

validateModule :: Module -> Either String ()
validateModule (Module name _ defs) = do
  mapM_ checkDef defs
  if T.null name then Left "Empty module name" else Right ()

checkDef :: Definition -> Either String ()
checkDef (Definition _ n kind body) = do
  if T.null n then Left "Empty definition name" else Right ()
  case (kind, body) of
    (ValueDef, Left _)        -> Right ()
    (ComputationDef, Right _) -> Right ()
    _                         -> Left ("Kind/body mismatch in " ++ show n)

-- Simple parenthesis balance checker with position reporting
checkParens :: FilePath -> T.Text -> Either String ()
checkParens path txt = go (1 :: Int) (1 :: Int) (0 :: Int) (T.unpack txt)
  where
    go _ _ 0 [] = Right ()
    go line col depth [] =
      if depth > 0
        then Left (path ++ ":" ++ show line ++ ":" ++ show col ++ ": missing ')'")
        else Right ()
    go line col depth (c:cs)
      | c == '('  = go line (col+1) (depth+1) cs
      | c == ')'  =
          if depth == 0
            then Left (path ++ ":" ++ show line ++ ":" ++ show col ++ ": unmatched ')'")
            else go line (col+1) (depth-1) cs
      | c == '\n' = go (line+1) 1 depth cs
      | otherwise = go line (col+1) depth cs
