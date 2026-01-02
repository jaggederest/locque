{-# LANGUAGE OverloadedStrings #-}

module Locque.Compiler.CoreParse
  ( parseCoreModule
  , parseCoreDecl
  , parseCoreValue
  , parseCoreComp
  , parseCoreType
  ) where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Control.Applicative (empty, (<|>))
import Text.Megaparsec (Parsec, between, eof, many, runParser, satisfy, some)
import Text.Megaparsec.Char (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

import Locque.Compiler.Core

parseCoreModule :: Text -> Either Text CoreModule
parseCoreModule = parseWith coreModuleFromSExpr

parseCoreDecl :: Text -> Either Text CoreDecl
parseCoreDecl = parseWith coreDeclFromSExpr

parseCoreValue :: Text -> Either Text CoreValue
parseCoreValue = parseWith coreValueFromSExpr

parseCoreComp :: Text -> Either Text CoreComp
parseCoreComp = parseWith coreCompFromSExpr

parseCoreType :: Text -> Either Text CoreType
parseCoreType = parseWith coreTypeFromSExpr

-- S-expr parsing

data Atom
  = AtomName Text
  | AtomString Text
  | AtomNatural Integer
  | AtomBool Bool
  deriving (Show, Eq)

data SExpr
  = SAtom Atom
  | SList [SExpr]
  deriving (Show, Eq)

type Parser = Parsec Void Text

parseWith :: (SExpr -> Either Text a) -> Text -> Either Text a
parseWith decode input =
  case runParser (spaces *> parseSExpr <* spaces <* eof) "<core>" input of
    Left err -> Left (T.pack (show err))
    Right expr -> decode expr

parseSExpr :: Parser SExpr
parseSExpr = spaces *> parseSExprRaw

parseSExprRaw :: Parser SExpr
parseSExprRaw = parseList <|> (SAtom <$> parseAtom)

parseList :: Parser SExpr
parseList = SList <$> between (char '(' *> spaces) (spaces *> char ')') (many parseSExpr)

parseAtom :: Parser Atom
parseAtom =
  parseString
    <|> parseBool
    <|> parseNatural
    <|> parseName

parseString :: Parser Atom
parseString = do
  _ <- char '"'
  content <- many stringChar
  _ <- char '"'
  pure (AtomString (T.pack content))

stringChar :: Parser Char
stringChar =
  (char '\\' *> escapeChar)
    <|> satisfy (\chr -> chr /= '"' && chr /= '\\')

escapeChar :: Parser Char
escapeChar =
  (char 'n' *> pure '\n')
    <|> (char 't' *> pure '\t')
    <|> (char 'r' *> pure '\r')
    <|> (char '"' *> pure '"')
    <|> (char '\\' *> pure '\\')

parseBool :: Parser Atom
parseBool =
  (AtomBool True <$ string "true")
    <|> (AtomBool False <$ string "false")

parseNatural :: Parser Atom
parseNatural = AtomNatural <$> L.decimal

parseName :: Parser Atom
parseName = AtomName . T.pack <$> some nameChar

nameChar :: Parser Char
nameChar =
  satisfy $ \chr ->
    not (isSpace chr) && chr /= '(' && chr /= ')' && chr /= '"'

spaces :: Parser ()
spaces = L.space space1 empty empty

-- Core decoding

coreModuleFromSExpr :: SExpr -> Either Text CoreModule
coreModuleFromSExpr expr =
  case expr of
    SList (SAtom (AtomName "module") : SAtom (AtomName name) : rest) -> do
      decls <- traverse coreDeclFromSExpr rest
      pure (CoreModule (Name name) decls)
    _ -> Left "expected module s-expr"

coreDeclFromSExpr :: SExpr -> Either Text CoreDecl
coreDeclFromSExpr expr =
  case expr of
    SList [SAtom (AtomName "def"), SAtom (AtomName name), tyExpr, valueExpr] ->
      CoreDef (Name name) <$> coreTypeFromSExpr tyExpr <*> coreValueFromSExpr valueExpr
    SList [SAtom (AtomName "def-comp"), SAtom (AtomName name), tyExpr, compExpr] ->
      CoreDefComp (Name name) <$> coreTypeFromSExpr tyExpr <*> coreCompFromSExpr compExpr
    SList (SAtom (AtomName "data") : SAtom (AtomName name) : rest) ->
      case rest of
        (paramsExpr : ctorExprs) -> do
          params <- parseParamList paramsExpr
          ctors <- traverse coreCtorFromSExpr ctorExprs
          pure (CoreData (CoreDataDecl (Name name) params ctors))
        _ -> Left "expected data params and ctors"
    _ -> Left "expected decl s-expr"

coreValueFromSExpr :: SExpr -> Either Text CoreValue
coreValueFromSExpr expr =
  case expr of
    SList [SAtom (AtomName "vvar"), SAtom (AtomName name)] ->
      Right (VVar (Name name))
    SList [SAtom (AtomName "vlit-natural"), SAtom (AtomNatural nat)] ->
      Right (VLit (LitNatural nat))
    SList [SAtom (AtomName "vlit-string"), SAtom (AtomString text)] ->
      Right (VLit (LitString text))
    SList [SAtom (AtomName "vlit-bool"), SAtom (AtomBool value)] ->
      Right (VLit (LitBoolean value))
    SList [SAtom (AtomName "vlit-unit")] ->
      Right (VLit LitUnit)
    SList [SAtom (AtomName "vlambda"), SAtom (AtomName name), tyExpr, compExpr] ->
      VLam (Name name) <$> coreTypeFromSExpr tyExpr <*> coreCompFromSExpr compExpr
    SList (SAtom (AtomName "vctor") : SAtom (AtomName name) : args) -> do
      values <- traverse coreValueFromSExpr args
      pure (VConstructor (Name name) values)
    SList [SAtom (AtomName "vcompute"), compExpr] ->
      VCompute <$> coreCompFromSExpr compExpr
    _ -> Left "expected value s-expr"

coreCompFromSExpr :: SExpr -> Either Text CoreComp
coreCompFromSExpr expr =
  case expr of
    SList [SAtom (AtomName "creturn"), valueExpr] ->
      CReturn <$> coreValueFromSExpr valueExpr
    SList [SAtom (AtomName "cbind"), SAtom (AtomName name), leftExpr, rightExpr] ->
      CBind (Name name) <$> coreCompFromSExpr leftExpr <*> coreCompFromSExpr rightExpr
    SList [SAtom (AtomName "cperform"), valueExpr] ->
      CPerform <$> coreValueFromSExpr valueExpr
    SList [SAtom (AtomName "capp"), fnExpr, argExpr] ->
      CApp <$> coreValueFromSExpr fnExpr <*> coreValueFromSExpr argExpr
    SList [SAtom (AtomName "clet"), SAtom (AtomName name), valueExpr, bodyExpr] ->
      CLet (Name name) <$> coreValueFromSExpr valueExpr <*> coreCompFromSExpr bodyExpr
    SList (SAtom (AtomName "cmatch") : valueExpr : caseExprs) -> do
      scrut <- coreValueFromSExpr valueExpr
      cases <- traverse coreCaseFromSExpr caseExprs
      pure (CMatch scrut cases)
    _ -> Left "expected comp s-expr"

coreTypeFromSExpr :: SExpr -> Either Text CoreType
coreTypeFromSExpr expr =
  case expr of
    SList [SAtom (AtomName "tvar"), SAtom (AtomName name)] ->
      Right (TyVar (Name name))
    SList (SAtom (AtomName "tcon") : SAtom (AtomName name) : args) ->
      TyCon (Name name) <$> traverse coreTypeFromSExpr args
    SList [SAtom (AtomName "tfun"), leftExpr, rightExpr] ->
      TyFun <$> coreTypeFromSExpr leftExpr <*> coreTypeFromSExpr rightExpr
    SList [SAtom (AtomName "tcomp"), innerExpr] ->
      TyComp <$> coreTypeFromSExpr innerExpr
    SList [SAtom (AtomName "tunit")] -> Right TyUnit
    SList [SAtom (AtomName "tboolean")] -> Right TyBoolean
    SList [SAtom (AtomName "tnatural")] -> Right TyNatural
    SList [SAtom (AtomName "tstring")] -> Right TyString
    SList [SAtom (AtomName "tcharacter")] -> Right TyCharacter
    _ -> Left "expected type s-expr"

coreCaseFromSExpr :: SExpr -> Either Text CoreCase
coreCaseFromSExpr expr =
  case expr of
    SList [SAtom (AtomName "ccase"), SAtom (AtomName ctor), bindersExpr, bodyExpr] -> do
      binders <- parseBinders bindersExpr
      body <- coreCompFromSExpr bodyExpr
      pure (CoreCase (Name ctor) binders body)
    _ -> Left "expected case s-expr"

coreCtorFromSExpr :: SExpr -> Either Text CoreCtor
coreCtorFromSExpr expr =
  case expr of
    SList (SAtom (AtomName "ctor") : SAtom (AtomName name) : fields) ->
      CoreCtor (Name name) <$> traverse coreTypeFromSExpr fields
    _ -> Left "expected ctor s-expr"

parseParamList :: SExpr -> Either Text [Name]
parseParamList expr =
  case expr of
    SList (SAtom (AtomName "params") : names) -> traverse parseNameAtom names
    _ -> Left "expected params s-expr"

parseBinders :: SExpr -> Either Text [Name]
parseBinders expr =
  case expr of
    SList names -> traverse parseNameAtom names
    _ -> Left "expected binders list"

parseNameAtom :: SExpr -> Either Text Name
parseNameAtom expr =
  case expr of
    SAtom (AtomName name) -> Right (Name name)
    _ -> Left "expected name"
