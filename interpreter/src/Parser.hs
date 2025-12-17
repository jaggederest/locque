{-# LANGUAGE OverloadedStrings #-}
module Parser
  ( parseModuleFile
  ) where

import           AST
import           Data.List (partition)
import           Data.Maybe (listToMaybe)
import           Data.Char (isAlphaNum, isLetter)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Void (Void)
import           Text.Megaparsec (Parsec, between, many, manyTill, optional, (<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

-- Basic S-expression representation

data SExpr
  = SAtom Text
  | SNum Integer
  | SStr Text
  | SList [SExpr]
  deriving (Show, Eq)

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space C.space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment ";"
    blockCmnt = L.skipBlockComment "#|" "|#"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

pSExpr :: Parser SExpr
pSExpr = lexeme (pList <|> pString <|> pNumber <|> pAtom)
  where
    pList = SList <$> between (symbol "(") (symbol ")") (many pSExpr)
    pString = do
      _ <- C.char '"'
      content <- manyTill L.charLiteral (C.char '"')
      pure $ SStr (T.pack content)
    pNumber = do
      n <- L.signed spaceConsumer L.decimal
      pure $ SNum n
    pAtom = do
      first <- M.satisfy (\c -> isLetter c || elem c ("_-+=*/<>:" :: String))
      rest <- many (M.satisfy (\c -> isAlphaNum c || elem c ("._-+=*/<>:" :: String)))
      pure $ SAtom (T.pack (first:rest))

parseModuleFile :: FilePath -> Text -> Either String Module
parseModuleFile path txt = case M.parse (spaceConsumer *> many pSExpr <* M.eof) path txt of
  Left err     -> Left (M.errorBundlePretty err)
  Right sexprs -> maybe (Left "No module found in file") Right (extractModule sexprs)

extractModule :: [SExpr] -> Maybe Module
extractModule sexprs = do
  let (imps, mods) = partitionImports sexprs
  msexpr <- case mods of
    []    -> listToMaybe sexprs  -- fallback: try first sexpr
    (m:_) -> Just m
  imports <- mapM fromImport imps
  m <- fromSExprModule msexpr
  pure m { modImports = imports }

partitionImports :: [SExpr] -> ([SExpr], [SExpr])
partitionImports = partition isImport
  where
    isImport (SList (SAtom "import":_)) = True
    isImport _ = False

fromSExprModule :: SExpr -> Maybe Module
fromSExprModule (SList (SAtom "module" : SAtom name : defs)) = do
  defs' <- mapM fromDef defs
  pure $ Module name [] defs'
fromSExprModule _ = Nothing

fromImport :: SExpr -> Maybe Import
fromImport (SList [SAtom "import", SAtom modName]) = Just (Import modName modName)
fromImport (SList [SAtom "import", SAtom modName, SAtom alias]) = Just (Import modName alias)
fromImport _ = Nothing

fromDef :: SExpr -> Maybe Definition
fromDef (SList [SAtom "def", SAtom tr, SAtom name, body]) = do
  transparency <- case tr of
    "transparent" -> Just Transparent
    "opaque"      -> Just Opaque
    _              -> Nothing
  case body of
    SList [SAtom "value", e] -> do
      e' <- fromExpr e
      pure $ Definition transparency name ValueDef (Left e')
    SList [SAtom "computation", c] -> do
      c' <- fromComp c
      pure $ Definition transparency name ComputationDef (Right c')
    _ -> Nothing
fromDef _ = Nothing

fromExpr :: SExpr -> Maybe Expr
fromExpr se = case se of
  SAtom "true"  -> Just (ELit (LBool True))
  SAtom "false" -> Just (ELit (LBool False))
  SAtom t -> Just (EVar t)
  SNum n  -> Just (ELit (LNat n))
  SStr s  -> Just (ELit (LString s))
  SList [] -> Nothing
  SList (SAtom "lambda" : SList params : body : []) -> do
    (names, finalBody) <- peel params body
    pure $ foldr ELam finalBody names
  SList (f:args) -> do
    f' <- fromExpr f
    args' <- mapM fromExpr args
    pure $ EApp f' args'

peel :: [SExpr] -> SExpr -> Maybe ([Text], Expr)
peel ps b = do
  names <- mapM grab ps
  b' <- fromExpr b
  pure (names, b')
  where
    grab (SAtom v)        = Just v
    grab (SList (SAtom v:_)) = Just v
    grab _                = Nothing

fromComp :: SExpr -> Maybe Comp
fromComp se = case se of
  SList [SAtom "return", e] -> CReturn <$> fromExpr e
  SList (SAtom "do" : rest) -> doBlock rest
  SList [SAtom "perform", SAtom "io", e] -> CPerform <$> fromExpr e
  SAtom t -> Just (CVar t)
  SList (SAtom "bind" : SAtom v : c1 : c2 : []) -> do
    c1' <- fromComp c1
    c2' <- fromComp c2
    pure $ CBind v c1' c2'
  SList (f:args) -> do
    f' <- fromExpr f
    args' <- mapM fromExpr args
    pure $ CReturn (EApp f' args')
  _ -> Nothing

-- Parse (do ...) block into nested binds/seqs

doBlock :: [SExpr] -> Maybe Comp
doBlock [] = Nothing
doBlock [one] = fromComp one
doBlock (stmt:rest) = case stmt of
  SList [SAtom "bind", SAtom v, c] -> do
    c' <- fromComp c
    rest' <- doBlock rest
    pure $ CBind v c' rest'
  _ -> do
    c' <- fromComp stmt
    rest' <- doBlock rest
    pure $ CSeq c' rest'
