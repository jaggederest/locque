module Validator
  ( validateModule
  , checkParens
  ) where

import           AST
import qualified Data.Text as T
import qualified Data.Set as Set

validateModule :: Module -> Either String ()
validateModule (Module name _ defs) = do
  checkDuplicates defs
  mapM_ checkDef defs
  if T.null name then Left "Empty module name" else Right ()

checkDuplicates :: [Definition] -> Either String ()
checkDuplicates defs = go Set.empty defs
  where
    go _ [] = Right ()
    go seen (Definition _ n _ _ _ : rest) =
      if n `Set.member` seen
        then Left ("Duplicate definition name: " ++ T.unpack n)
        else go (Set.insert n seen) rest

checkDef :: Definition -> Either String ()
checkDef (Definition _ n kind _mType body) = do
  if T.null n then Left "Empty definition name" else Right ()
  case (kind, body) of
    (ValueDef, Left _)        -> Right ()
    (ComputationDef, Right _) -> Right ()
    _                         -> Left ("Kind/body mismatch in " ++ show n)

-- Simple parenthesis balance checker with position reporting
checkParens :: FilePath -> T.Text -> Either String ()
checkParens path txt = go (1 :: Int) (1 :: Int) [] False (T.unpack txt)
  where
    go _ _ [] False [] = Right ()
    go line col _ True [] =
      Left (path ++ ":" ++ show line ++ ":" ++ show col ++ ": unterminated string literal")
    go _ _ stack False [] =
      case stack of
        ((ol, oc):_) ->
          Left (path ++ ":" ++ show ol ++ ":" ++ show oc ++ ": missing ')' for '(' opened here")
    go line col stack inStr (c:cs)
      | inStr =
          case c of
            '"'  -> go line (col+1) stack False cs
            '\n' -> go (line+1) 1 stack True cs
            _    -> go line (col+1) stack True cs
      | c == '"' = go line (col+1) stack True cs
      | c == '('  = go line (col+1) ((line, col):stack) False cs
      | c == ')'  =
          case stack of
            [] -> Left (path ++ ":" ++ show line ++ ":" ++ show col ++ ": unmatched ')'")
            (_:rest) -> go line (col+1) rest False cs
      | c == '\n' = go (line+1) 1 stack False cs
      | otherwise = go line (col+1) stack False cs
