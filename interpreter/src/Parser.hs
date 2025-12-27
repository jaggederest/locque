{-# LANGUAGE OverloadedStrings #-}
module Parser
  ( parseModuleFile
  , parseMExprFile
  , moduleToSExprText
  , moduleToMExprText
  ) where

import           AST
import qualified Type as T
import           Data.Char (isAlphaNum, isLetter, isSpace, isLower, isAscii, isDigit)
import           Data.List (intercalate)
import           Data.Text (Text)
import qualified Data.Text as DT
import           Data.Void (Void)
import           Text.Megaparsec (Parsec, between, many, manyTill, optional, some, (<|>), notFollowedBy)
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

-- S-expr space consumer (Lisp-style comments)
spaceConsumer :: Parser ()
spaceConsumer = L.space C.space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment ";"
    blockCmnt = L.skipBlockComment "#|" "|#"

-- M-expr space consumer (Algol-style comments)
mExprSpaceConsumer :: Parser ()
mExprSpaceConsumer = L.space C.space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "#"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

mLexeme :: Parser a -> Parser a
mLexeme = L.lexeme mExprSpaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

mSymbol :: Text -> Parser Text
mSymbol = L.symbol mExprSpaceConsumer

pSExpr :: Parser SExpr
pSExpr = lexeme (pList <|> pString <|> pNumber <|> pAtom)
  where
    pList = SList <$> between (symbol "(") (symbol ")") (many pSExpr)
    pString = do
      _ <- C.char '"'
      content <- manyTill L.charLiteral (C.char '"')
      pure $ SStr (DT.pack content)
    pNumber = do
      n <- L.signed spaceConsumer L.decimal
      pure $ SNum n
    pAtom = do
      first <- M.satisfy (\c -> isLetter c || elem c ("_-+=*/<>:" :: String))
      rest <- many (M.satisfy (\c -> isAlphaNum c || elem c ("._-+=*/<>:" :: String)))
      pure $ SAtom (DT.pack (first:rest))

parseModuleFile :: FilePath -> Text -> Either String Module
parseModuleFile path rawTxt = do
  txt <- preprocessInput path rawTxt
  sexprs <- case M.parse (spaceConsumer *> many pSExpr <* spaceConsumer <* M.eof) path txt of
    Left err -> Left (whitespaceHelp <> M.errorBundlePretty err)
    Right xs -> Right xs
  moduleFromSExprs path sexprs

moduleFromSExprs :: FilePath -> [SExpr] -> Either String Module
moduleFromSExprs path sexprs = do
  let (pre, rest) = break isModuleForm sexprs
  modExpr <- case rest of
    []        -> Left (path ++ ": expected a `(module <Name> ...)` form")
    (m : more) -> case filter isModuleForm more of
      (extra:_) -> Left (path ++ ": multiple module forms; first is " ++ renderHead m ++ ", extra " ++ renderHead extra)
      []        -> Right m
  case filter (\s -> not (isImportForm s || isOpenForm s)) pre of
    (bad:_) -> Left (path ++ ": unexpected toplevel form " ++ renderHead bad ++ "; only imports and opens may precede the module")
    []      -> pure ()
  case drop 1 rest of
    (bad:_) -> Left (path ++ ": unexpected form after module; only a single module is allowed per file (saw " ++ renderHead bad ++ ")")
    []      -> pure ()
  let (importForms, openForms) = partitionImportsOpens pre
  imports <- mapM (fromImport path) importForms
  opens <- mapM (fromOpen path) openForms
  modVal <- fromSExprModule path modExpr
  pure modVal { modImports = imports, modOpens = opens }
  where
    partitionImportsOpens :: [SExpr] -> ([SExpr], [SExpr])
    partitionImportsOpens = foldr classify ([], [])
      where
        classify s (imps, ops)
          | isImportForm s = (s:imps, ops)
          | isOpenForm s = (imps, s:ops)
          | otherwise = (imps, ops)

isImportForm :: SExpr -> Bool
isImportForm (SList (SAtom "import" : _)) = True
isImportForm _ = False

isOpenForm :: SExpr -> Bool
isOpenForm (SList (SAtom "open" : _)) = True
isOpenForm _ = False

isModuleForm :: SExpr -> Bool
isModuleForm (SList (SAtom "module" : _)) = True
isModuleForm _ = False

fromSExprModule :: FilePath -> SExpr -> Either String Module
fromSExprModule path (SList (SAtom "module" : SAtom name : defs)) = do
  defs' <- mapM (fromDef path) defs
  pure $ Module name [] [] defs'  -- Empty imports and opens (filled by caller)
fromSExprModule path other =
  Left (path ++ ": expected `(module <Name> ...)`, found " ++ renderHead other)

fromImport :: FilePath -> SExpr -> Either String Import
fromImport _ (SList [SAtom "import", SAtom modName]) = Right (Import modName modName)
fromImport _ (SList [SAtom "import", SAtom modName, SAtom alias]) = Right (Import modName alias)
fromImport path other = Left (path ++ ": invalid import form " ++ renderHead other)

fromOpen :: FilePath -> SExpr -> Either String Open
fromOpen _ (SList (SAtom "open" : SAtom modAlias : names)) = do
  names' <- mapM extractName names
  pure (Open modAlias names')
  where
    extractName (SAtom n) = Right n
    extractName other = Left ("open statement expects atom names, found " ++ renderHead other)
fromOpen path other = Left (path ++ ": invalid open form " ++ renderHead other)

fromDef :: FilePath -> SExpr -> Either String Definition
fromDef path (SList [SAtom "def", SAtom tr, SAtom name, body]) = do
  transparency <- case tr of
    "transparent" -> Right Transparent
    "opaque"      -> Right Opaque
    _             -> Left (path ++ ": definition " ++ DT.unpack name ++ " must declare transparency (transparent|opaque)")
  case body of
    SList [SAtom "value", e] -> do
      e' <- fromExpr path e
      pure $ Definition transparency name ValueDef Nothing (ValueBody e')
    SList [SAtom "computation", c] -> do
      c' <- fromComp path c
      pure $ Definition transparency name ComputationDef Nothing (ComputationBody c')
    _ -> Left (path ++ ": definition " ++ DT.unpack name ++ " must specify (value ...) or (computation ...)")
fromDef path other = Left (path ++ ": invalid definition form " ++ renderHead other)

fromExpr :: FilePath -> SExpr -> Either String Expr
fromExpr _ (SAtom "true")  = Right (ELit (LBoolean True))
fromExpr _ (SAtom "false") = Right (ELit (LBoolean False))
fromExpr _ (SAtom t)       = Right (EVar t)
fromExpr _ (SNum n)        = Right (ELit (LNatural n))
fromExpr _ (SStr s)        = Right (ELit (LString s))
fromExpr path (SList [])   = Left (path ++ ": empty expression list")
fromExpr path (SList (f:args)) = do
  f' <- fromExpr path f
  args' <- mapM (fromExpr path) args
  pure $ mkApp f' args'

mkApp :: Expr -> [Expr] -> Expr
mkApp (EApp f existing) more = EApp f (existing ++ more)
mkApp f more                 = EApp f more

fromComp :: FilePath -> SExpr -> Either String Comp
fromComp path se = case se of
  SList [SAtom "return", e] -> CReturn <$> fromExpr path e
  SList [SAtom "perform", e] -> CPerform <$> fromExpr path e
  SAtom t -> Right (CVar t)
  SList (SAtom "bind" : SAtom v : c1 : c2 : []) -> do
    c1' <- fromComp path c1
    c2' <- fromComp path c2
    pure $ CBind v c1' c2'
  SList (f:args) -> do
    f' <- fromExpr path f
    args' <- mapM (fromExpr path) args
    pure $ CReturn (EApp f' args')
  _ -> Left (path ++ ": invalid computation form " ++ renderHead se)

renderHead :: SExpr -> String
renderHead sexpr = case sexpr of
  SAtom t     -> DT.unpack t
  SNum n      -> show n
  SStr s      -> show s
  SList []    -> "()"
  SList (h:_) -> "(" ++ headText h ++ " ...)"
  where
    headText (SAtom t) = DT.unpack t
    headText (SNum n)  = show n
    headText (SStr s)  = show s
    headText (SList xs) = "(" ++ intercalate " " (map headText xs) ++ ")"

--------------------------------------------------------------------------------
-- M-expression parser (surface syntax)

parseMExprFile :: FilePath -> DT.Text -> Either String Module
parseMExprFile path rawTxt = do
  txt <- preprocessInput path rawTxt
  case M.parse (mExprSpaceConsumer *> pMModule <* mExprSpaceConsumer <* M.eof) path txt of
    Left err -> Left (whitespaceHelp <> M.errorBundlePretty err)
    Right m  -> Right m

pMModule :: Parser Module
pMModule = do
  imports <- many pMImport
  opens <- many pMOpen
  keyword "module"
  name <- pModuleName
  keyword "contains"
  defs <- many pDefinition
  mExprSpaceConsumer
  keyword "end"
  let _ = if null opens then () else error $ "DEBUG pMModule: parsed " <> show (length opens) <> " opens"
  pure (Module name imports opens defs)

pMImport :: Parser Import
pMImport = do
  keyword "import"
  modName <- pModuleName
  alias <- optional (keyword "as" *> pModuleName)
  let chosen = maybe modName id alias
  pure (Import modName chosen)

pMOpen :: Parser Open
pMOpen = do  -- REMOVED M.try
  keyword "open"
  modAlias <- pIdentifier
  names <- between (mSymbol "(") (mSymbol ")") (pIdentifier `M.sepBy` mSymbol ",")
  pure (Open modAlias names)

pDefinition :: Parser Definition
pDefinition = mLexeme $ do
  keyword "define"
  tr <- (Transparent <$ keyword "transparent") <|> (Opaque <$ keyword "opaque")
  name <- pIdentifier
  keyword "as"
  kind <- (ValueDef <$ keyword "value")
      <|> (ComputationDef <$ keyword "computation")
      <|> (FamilyDef <$ keyword "family")
      <|> (TypeClassDef <$ keyword "typeclass")
      <|> (InstanceDef <$ keyword "instance")
  body <- case kind of
    ValueDef        -> ValueBody <$> pExpr
    ComputationDef  -> ComputationBody <$> pComp
    FamilyDef       -> FamilyBody <$> pTypeFamilyBody name
    TypeClassDef    -> ClassBody <$> pTypeClassBody
    InstanceDef     -> InstBody <$> pInstanceBody
  pure (Definition tr name kind Nothing body)

-- | Parse type family body:
-- family (Type -> Type -> Type) where
--   FamilyName (List a) b equals (() -> b)
--   FamilyName Bool b equals (() -> b)
pTypeFamilyBody :: Text -> Parser TypeFamilyBody
pTypeFamilyBody familyName = do
  kindSig <- pType  -- Parse kind signature like (Type -> Type -> Type)
  keyword "where"
  cases <- many (pTypeFamilyCase familyName)
  pure (TypeFamilyBody kindSig cases)

-- | Parse a single type family case:
-- FamilyName (List a) b equals (() -> b)
pTypeFamilyCase :: Text -> Parser TypeFamilyCase
pTypeFamilyCase familyName = M.try $ do
  -- Expect family name at start of each case
  caseName <- pIdentifier
  if caseName /= familyName
    then fail ("Expected family name " ++ DT.unpack familyName ++ " but got " ++ DT.unpack caseName)
    else pure ()
  -- Parse pattern types (everything before "equals")
  patterns <- manyTill pType (keyword "equals")
  -- Parse result type
  result <- pType
  pure (TypeFamilyCase patterns result)

-- | Parse type class body:
-- typeclass a where
--   method1 of-type Type1
--   method2 of-type Type2
pTypeClassBody :: Parser TypeClassBody
pTypeClassBody = do
  -- Parse the type parameter (lowercase identifier after "typeclass")
  param <- pIdentifier  -- e.g., "a" in "typeclass a where"
  keyword "where"
  methods <- many pMethodSig
  pure (TypeClassBody param methods)

-- | Parse method signature: name of-type Type
pMethodSig :: Parser (Text, T.Type)
pMethodSig = M.try $ do
  name <- pIdentifier
  keyword "of-type"
  ty <- pType
  pure (name, ty)

-- | Parse instance body:
-- instance ClassName Type where
--   method1 produce expr1
--   method2 produce expr2
pInstanceBody :: Parser InstanceBody
pInstanceBody = do
  className <- pIdentifier
  instType <- pType
  keyword "where"
  impls <- many pMethodImpl
  pure (InstanceBody className instType impls)

-- | Parse method implementation: name produce expr
pMethodImpl :: Parser (Text, Expr)
pMethodImpl = M.try $ do
  name <- pIdentifier
  keyword "produce"
  impl <- pExpr
  pure (name, impl)

-- Computations

pComp :: Parser Comp
pComp = pBind <|> pCompNonBind

pBind :: Parser Comp
pBind = M.try $ do
  keyword "bind"
  v <- pIdentifier
  keyword "from"
  first <- pCompNonBind
  keyword "then"
  rest <- pComp
  optional (keyword "end")
  pure (CBind v first rest)

pCompNonBind :: Parser Comp
pCompNonBind =
  pReturn
    <|> pPerform
    <|> compFromExpr <$> pExpr

pReturn :: Parser Comp
pReturn = do
  keyword "return"
  CReturn <$> pExpr

pPerform :: Parser Comp
pPerform = do
  keyword "perform"
  CPerform <$> pExpr

compFromExpr :: Expr -> Comp
compFromExpr (EVar t) = CVar t
compFromExpr e        = CReturn e

-- Expressions

pExpr :: Parser Expr
pExpr = pLet <|> pFunction <|> pApp

pLet :: Parser Expr
pLet = M.try $ do
  keyword "let"
  keyword "value"
  v <- pIdentifier
  keyword "be"
  val <- pExpr
  keyword "in"
  body <- pExpr
  optional (keyword "end")
  pure (EApp (ELam v Nothing body) [val])

pFunction :: Parser Expr
pFunction = M.try $ do
  keyword "function"
  params <- many paramWithType
  keyword "returns"
  _retTy <- pType
  body <- (keyword "as" *> pExpr) <|> (keyword "do" *> pExpr)
  optional (keyword "end")
  pure (foldr (\(n, _t) b -> ELam n Nothing b) body params)
  where
    paramWithType = do
      name <- pIdentifier
      ty <- pType
      pure (name, ty)

pApp :: Parser Expr
pApp = do
  atoms <- some pExprAtom
  case atoms of
    [a]      -> pure a
    (f:args) -> pure (EApp f args)
    []       -> fail "unreachable: some pExprAtom returned []"

pExprAtom :: Parser Expr
pExprAtom =
      parens pExpr
  <|> pLiteral
  <|> pLet
  <|> (EVar <$> pIdentifier)

pLiteral :: Parser Expr
pLiteral =
      (ELit (LBoolean True) <$ keyword "true")
  <|> (ELit (LBoolean False) <$ keyword "false")
  <|> (ELit . LNatural <$> lexeme (L.decimal))
  <|> pStringLit

pStringLit :: Parser Expr
pStringLit = lexeme $ do
  _ <- C.char '"'
  content <- manyTill L.charLiteral (C.char '"')
  pure (ELit (LString (DT.pack content)))

-- Type parsing

pType :: Parser T.Type
pType = pTypeAtom <|> pTypeCompound

pTypeAtom :: Parser T.Type
pTypeAtom =
      (T.TNatural <$ keyword "Natural")
  <|> (T.TString <$ keyword "String")
  <|> (T.TBoolean <$ keyword "Boolean")
  <|> (T.TUnit <$ keyword "Unit")
  <|> pTypeVar

pTypeVar :: Parser T.Type
pTypeVar = lexeme . M.try $ do
  first <- M.satisfy isLower
  rest <- many (M.satisfy (\c -> isAlphaNum c || c == '_'))
  let var = DT.pack (first:rest)
  if var `elem` ["nat", "string", "bool", "unit"]
     then fail ("lowercase type keyword " ++ DT.unpack var ++ " should be capitalized")
     else pure (T.TVar var)

pTypeCompound :: Parser T.Type
pTypeCompound = do
  _ <- mSymbol "("
  result <- pTypeForm
  _ <- mSymbol ")"
  pure result

pTypeForm :: Parser T.Type
pTypeForm =
      pListType
  <|> pPairType
  <|> pFunType
  <|> pCompType
  <|> pTypeFamilyApp  -- type family application: (FamilyName arg1 arg2 ...)
  <|> pType  -- nested type

pListType :: Parser T.Type
pListType = do
  keyword "List"
  T.TList <$> pType

pPairType :: Parser T.Type
pPairType = do
  keyword "Pair"
  t1 <- pType
  t2 <- pType
  pure (T.TPair t1 t2)

pFunType :: Parser T.Type
pFunType = do
  mSymbol "->"
  t1 <- pType
  t2 <- pType
  pure (T.TFun t1 t2)

pCompType :: Parser T.Type
pCompType = do
  keyword "Comp"
  T.TComp <$> pType

-- | Parse a type family application: (FamilyName arg1 arg2 ...)
-- Uppercase identifier followed by one or more type arguments
pTypeFamilyApp :: Parser T.Type
pTypeFamilyApp = M.try $ do
  name <- pUppercaseIdent
  args <- some pType  -- at least one argument
  pure (T.TFamilyApp name args)

-- | Parse an uppercase identifier (for type family names)
pUppercaseIdent :: Parser Text
pUppercaseIdent = mLexeme . M.try $ do
  first <- M.satisfy (\c -> isLetter c && isAscii c && not (isLower c))
  rest <- many (M.satisfy (\c -> (isAlphaNum c && isAscii c) || c == '_' || c == '-'))
  let ident = DT.pack (first:rest)
  -- Don't match reserved type constructors
  if ident `elem` ["Natural", "String", "Boolean", "Unit", "List", "Pair", "Comp"]
     then fail ("builtin type " ++ DT.unpack ident ++ " is not a type family")
     else pure ident

-- Identifiers and symbols

pIdentifier :: Parser Text
pIdentifier = mLexeme . M.try $ do
  first <- M.satisfy (\c -> (isLetter c && isAscii c) || c == '_')
  rest <- many (M.satisfy (\c -> (isAlphaNum c && isAscii c) || c == '_' || c == '-' || c == '.'))
  let ident = DT.pack (first:rest)
  if ident `elem` reservedWords
     then fail ("keyword " ++ DT.unpack ident ++ " cannot be used as an identifier")
     else pure ident

reservedWords :: [Text]
reservedWords =
  [ "module", "contains", "import", "open", "define", "transparent", "opaque"
  , "as", "value", "computation", "function", "of-type", "produce"
  , "return", "returns", "perform", "bind", "then", "from", "end", "exposing", "be", "in"
  , "family", "typeclass", "instance", "where", "equals"  -- Type classes
  ]

pModuleName :: Parser Text
pModuleName = mLexeme $ do
  first <- M.satisfy (\c -> (isLetter c && isAscii c) || c == '_')
  rest <- many (M.satisfy (\c -> (isAlphaNum c && isAscii c) || c == '_' || c == '-' || c == '/' || c == '.' || c == ':'))
  pure (DT.pack (first:rest))

-- M-expr keyword (uses M-expr space consumer)
keyword :: Text -> Parser ()
keyword t = mLexeme (C.string t *> notFollowedBy (M.satisfy isIdentChar))

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c `elem` ("._-:" :: String)

parens :: Parser a -> Parser a
parens = between (mSymbol "(") (mSymbol ")")

--------------------------------------------------------------------------------
-- Input preprocessing (whitespace normalization/validation)

preprocessInput :: FilePath -> DT.Text -> Either String DT.Text
preprocessInput path rawTxt = do
  let normalized = normalizeLineEndings rawTxt
  ensureTrailingNewline path normalized >>= pure . collapseDuplicateBlankLines

normalizeLineEndings :: DT.Text -> DT.Text
normalizeLineEndings = DT.replace "\r\n" "\n" . DT.replace "\r" "\n"

ensureTrailingNewline :: FilePath -> DT.Text -> Either String DT.Text
ensureTrailingNewline path txt
  | DT.null txt = Right txt
  | DT.isSuffixOf "\n" txt = Right txt
  | otherwise = Left (path ++ ": missing trailing newline; locque source requires a newline at EOF to avoid ambiguous whitespace during parsing.")

collapseDuplicateBlankLines :: DT.Text -> DT.Text
collapseDuplicateBlankLines =
  DT.unlines . foldr compress [] . DT.splitOn "\n"
  where
    compress line acc =
      case (DT.all isSpace line, acc) of
        (True, next:rest) | DT.all isSpace next -> "" : rest
        _ -> line : acc

whitespaceHelp :: String
whitespaceHelp = ""  -- Only report whitespace issues if they're problematic

--------------------------------------------------------------------------------
-- S-expression pretty-printer (AST -> S-text)

moduleToSExprText :: Module -> DT.Text
moduleToSExprText (Module name imports opens defs) =
  let importLines = map renderImport imports
      openLines = map renderOpen opens
      moduleLine = renderModule name defs
  in DT.unlines (importLines ++ openLines ++ [moduleLine])

renderImport :: Import -> DT.Text
renderImport (Import modName alias)
  | modName == alias = "(import " <> modName <> ")"
  | otherwise        = "(import " <> modName <> " " <> alias <> ")"

renderOpen :: Open -> DT.Text
renderOpen (Open modAlias names) =
  "(open " <> modAlias <> " " <> DT.unwords names <> ")"

renderModule :: Text -> [Definition] -> DT.Text
renderModule name defs =
  case defs of
    [] -> "(module " <> name <> ")"
    _  -> "(module " <> name <> " " <> DT.unwords (map renderDef defs) <> ")"

renderDef :: Definition -> DT.Text
renderDef (Definition tr name kind _mType body) =
  "(def " <> renderTransparency tr <> " " <> name <> " " <> renderBody kind body <> ")"
  where
    renderTransparency Transparent = "transparent"
    renderTransparency Opaque      = "opaque"

renderBody :: DefKind -> DefBody -> DT.Text
renderBody ValueDef (ValueBody e)             = "(value " <> renderExpr e <> ")"
renderBody ComputationDef (ComputationBody c) = "(computation " <> renderComp c <> ")"
renderBody _ _                                = "(value <invalid>)"

renderExpr :: Expr -> DT.Text
renderExpr expr = case expr of
  EVar t          -> t
  ELit lit        -> renderLit lit
  ELam v _mType b -> "(lambda (" <> v <> ") " <> renderExpr b <> ")"
  ELamMulti ps _mType b -> "(lambda (" <> DT.unwords ps <> ") " <> renderExpr b <> ")"
  EAnnot e _ty    -> renderExpr e  -- Ignore type annotations in rendering
  ETyped e _ty    -> renderExpr e  -- Ignore inferred type wrappers in rendering
  EApp f args     -> "(" <> DT.unwords (renderExpr f : map renderExpr args) <> ")"
  EDict className impls ->
    "(dict " <> className <> " " <>
    DT.intercalate " " [n <> " " <> renderExpr e | (n, e) <- impls] <> ")"
  EDictAccess d method -> "(dict-access " <> renderExpr d <> " " <> method <> ")"

renderComp :: Comp -> DT.Text
renderComp comp = case comp of
  CReturn e      -> "(return " <> renderExpr e <> ")"
  CBind v c1 c2  -> "(bind " <> v <> " " <> renderComp c1 <> " " <> renderComp c2 <> ")"
  CPerform e     -> "(perform io " <> renderExpr e <> ")"
  CVar t         -> t
  CSeq c1 c2     -> "(do " <> renderComp c1 <> " " <> renderComp c2 <> ")"

renderLit :: Literal -> DT.Text
renderLit lit = case lit of
  LNatural n    -> DT.pack (show n)
  LString s -> DT.pack (show (DT.unpack s))
  LBoolean b   -> if b then "true" else "false"

--------------------------------------------------------------------------------
-- AST -> M-expression pretty-printer

moduleToMExprText :: Module -> DT.Text
moduleToMExprText (Module name imports opens defs) =
  DT.unlines $
    map renderMImport imports
    ++ map renderMOpen opens
    ++ ["" | not (null imports && null opens)]
    ++ ["module " <> name <> " contains"]
    ++ concatMap renderMDef defs
    ++ ["end"]

renderMImport :: Import -> DT.Text
renderMImport (Import modName alias)
  | modName == alias = "import " <> modName
  | otherwise        = "import " <> modName <> " as " <> alias

renderMOpen :: Open -> DT.Text
renderMOpen (Open modAlias names) =
  "open " <> modAlias <> " (" <> DT.intercalate ", " names <> ")"

renderMDef :: Definition -> [DT.Text]
renderMDef (Definition tr name kind _mType body) =
  let header = "  define " <> renderMTransparency tr <> " " <> name <> " as " <> renderMKind kind
  in case body of
      ValueBody e       -> [header <> " " <> renderMExpr e]
      ComputationBody c -> header : renderMComp 4 c
      _                 -> [header <> " <invalid>"]

renderMTransparency :: Transparency -> DT.Text
renderMTransparency Transparent = "transparent"
renderMTransparency Opaque      = "opaque"

renderMKind :: DefKind -> DT.Text
renderMKind ValueDef       = "value"
renderMKind ComputationDef = "computation"
renderMKind FamilyDef      = "family"
renderMKind TypeClassDef   = "typeclass"
renderMKind InstanceDef    = "instance"

renderMComp :: Int -> Comp -> [DT.Text]
renderMComp indentLevel comp = case comp of
  CReturn e -> [indent indentLevel ("return " <> renderMExpr e)]
  CPerform e -> [indent indentLevel ("perform io " <> renderMExpr e)]
  CVar t -> [indent indentLevel t]
  CSeq c1 c2 ->
    [indent indentLevel "do"] ++ renderMComp (indentLevel+2) c1 ++ [indent indentLevel "then"] ++ renderMComp (indentLevel+2) c2
  CBind v c1 c2 ->
    [indent indentLevel ("bind " <> renderMCompInline c1 <> " as " <> v <> " then")] ++ renderMComp (indentLevel+2) c2

renderMCompInline :: Comp -> DT.Text
renderMCompInline c = DT.intercalate " " (renderMComp 0 c)

renderMExpr :: Expr -> DT.Text
renderMExpr expr = render expr False
  where
    render e inAtom = case e of
      EVar t      -> t
      ELit lit    -> renderLit lit
      ELam{}      -> wrapIf inAtom (renderLambdaChain e)
      ELamMulti{} -> wrapIf inAtom (renderLambdaChain e)
      EAnnot e' _ -> render e' inAtom  -- Ignore type annotations
      ETyped e' _ -> render e' inAtom  -- Ignore inferred types
      EApp f args ->
        case matchAsInspect f args of
          Just (scrut, cases) -> wrapIf inAtom (renderInspect scrut cases)
          Nothing ->
            let parts = renderAtom f : map renderAtom args
            in "(" <> DT.unwords parts <> ")"
      EDict className impls ->
        "(dict " <> className <> " " <>
        DT.intercalate " " [n <> " " <> render impl False | (n, impl) <- impls] <> ")"
      EDictAccess d method -> "(dict-access " <> render d False <> " " <> method <> ")"
    renderAtom e = render e True
    wrapIf True t  = "(" <> t <> ")"
    wrapIf False t = t

renderLambdaChain :: Expr -> DT.Text
renderLambdaChain e =
  let (params, body) = collectLams e
  in case params of
    [] -> "lambda -> " <> renderMExpr body  -- Zero-param
    [p] | p == "_unit" -> "lambda -> " <> renderMExpr body  -- Zero-param (internal representation)
    _ -> "lambda " <> DT.unwords params <> " -> " <> renderMExpr body  -- Single or multi-param

collectLams :: Expr -> ([Text], Expr)
collectLams = go []
  where
    go acc (ELam v _mType b)
      | v == "_unit" = (acc, b)  -- Zero-param, don't include _unit
      | otherwise = go (acc ++ [v]) b
    go acc (ELamMulti ps _mType b) = (acc ++ ps, b)
    go acc other = (acc, other)

matchAsInspect :: Expr -> [Expr] -> Maybe (Expr, [Expr])
matchAsInspect (EVar "match") (scrut:cases) = Just (scrut, cases)
matchAsInspect _ _ = Nothing

renderInspect :: Expr -> [Expr] -> DT.Text
renderInspect scrut cases =
  "inspect " <> renderMExpr scrut <> " with " <> DT.unwords (map renderCase cases) <> " end"
  where
    renderCase c =
      let (params, body) = collectLams c
          paramsTxt = if null params then "" else " " <> DT.unwords params
      in "case" <> paramsTxt <> " -> " <> renderMExpr body

indent :: Int -> DT.Text -> DT.Text
indent n t = DT.replicate n " " <> t
