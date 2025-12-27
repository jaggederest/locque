{-# LANGUAGE OverloadedStrings #-}
module Parser
  ( parseModuleFile
  , parseMExprFile
  , moduleToSExprText
  , moduleToMExprText
  ) where

import AST
import qualified Type as T
import Data.Char (isAlphaNum, isLetter, isSpace, isAscii)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as DT
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, many, manyTill, optional, some, (<|>), notFollowedBy)
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
      n <- L.decimal
      pure $ SNum n
    pAtom = do
      first <- M.satisfy (\c -> isLetter c || c == '_')
      rest <- many (M.satisfy (\c -> isAlphaNum c || c == '_' || c == '-' || c == ':' || c == '/'))
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
  pure $ Module name [] [] defs'
fromSExprModule path other =
  Left (path ++ ": expected `(module <Name> ...)`, found " ++ renderHead other)

fromImport :: FilePath -> SExpr -> Either String Import
fromImport _ (SList [SAtom "import", SAtom modName]) = Right (Import modName modName)
fromImport _ (SList [SAtom "import", SAtom modName, SAtom alias]) = Right (Import modName alias)
fromImport path other = Left (path ++ ": invalid import form " ++ renderHead other)

fromOpen :: FilePath -> SExpr -> Either String Open
fromOpen _ (SList (SAtom "open" : SAtom modAlias : rest)) =
  case rest of
    (SAtom "exposing" : names) -> do
      names' <- mapM extractName names
      pure (Open modAlias names')
    _ -> Left ("open expects exposing list, found " ++ renderHead (SList rest))
  where
    extractName (SAtom n) = Right n
    extractName other = Left ("open statement expects atom names, found " ++ renderHead other)
fromOpen path other = Left (path ++ ": invalid open form " ++ renderHead other)

fromDef :: FilePath -> SExpr -> Either String Definition
fromDef path (SList [SAtom "define", SAtom tr, SAtom name, body]) = do
  transparency <- case tr of
    "transparent" -> Right Transparent
    "opaque"      -> Right Opaque
    _             -> Left (path ++ ": definition " ++ DT.unpack name ++ " must declare transparency (transparent|opaque)")
  body' <- fromExpr path body
  pure $ Definition transparency name body'
fromDef path other = Left (path ++ ": invalid definition form " ++ renderHead other)

fromExpr :: FilePath -> SExpr -> Either String Expr
fromExpr _ (SAtom "true")  = Right (ELit (LBoolean True))
fromExpr _ (SAtom "false") = Right (ELit (LBoolean False))
fromExpr _ (SAtom "tt")    = Right (ELit LUnit)
fromExpr _ (SAtom "Natural") = Right (ETypeConst TCNatural)
fromExpr _ (SAtom "String") = Right (ETypeConst TCString)
fromExpr _ (SAtom "Boolean") = Right (ETypeConst TCBoolean)
fromExpr _ (SAtom "Unit") = Right (ETypeConst TCUnit)
fromExpr _ (SAtom "List") = Right (ETypeConst TCList)
fromExpr _ (SAtom "Pair") = Right (ETypeConst TCPair)
fromExpr _ (SAtom t)
  | Just n <- parseUniverseAtom t = Right (ETypeUniverse n)
  | otherwise = Right (EVar t)
fromExpr _ (SNum n)         = Right (ELit (LNatural n))
fromExpr _ (SStr s)         = Right (ELit (LString s))
fromExpr path (SList [])    = Left (path ++ ": empty expression list")
fromExpr path (SList (SAtom "function" : parts)) = fromFunction path parts
fromExpr path (SList [SAtom "compute", comp]) = ECompute <$> fromComp path comp
fromExpr path (SList [SAtom "let", SList [SAtom name, val], body]) = do
  val' <- fromExpr path val
  body' <- fromExpr path body
  pure (ELet name val' body')
fromExpr path (SList (SAtom "match" : scrut : cases)) = do
  (scrutExpr, scrutTy) <- fromOfType path scrut
  cases' <- mapM (fromMatchCase path) cases
  pure (EMatch scrutExpr scrutTy cases')
fromExpr path (SList [SAtom "of-type", e, ty]) = do
  e' <- fromExpr path e
  ty' <- fromType path ty
  pure (EAnnot e' ty')
fromExpr path (SList (f:args)) = do
  f' <- fromExpr path f
  args' <- mapM (fromExpr path) args
  pure $ mkApp f' args'

fromFunction :: FilePath -> [SExpr] -> Either String Expr
fromFunction path parts =
  case reverse parts of
    (bodyForm : retForm : paramFormsRev) -> do
      body <- fromFunctionBody path bodyForm
      retTy <- fromType path retForm
      params <- mapM (fromParam path) (reverse paramFormsRev)
      pure (EFunction params retTy body)
    _ -> Left (path ++ ": function expects params, return type, and body")

fromFunctionBody :: FilePath -> SExpr -> Either String FunctionBody
fromFunctionBody path (SList [SAtom "value", body]) =
  FunctionValue <$> fromExpr path body
fromFunctionBody path (SList [SAtom "compute", body]) =
  FunctionCompute <$> fromComp path body
fromFunctionBody path other =
  Left (path ++ ": function body must be (value <expr>) or (compute <comp>), found " ++ renderHead other)

fromParam :: FilePath -> SExpr -> Either String Param
fromParam path (SList [SAtom name, ty]) = do
  ty' <- fromType path ty
  pure (Param name ty')
fromParam path other =
  Left (path ++ ": function parameter must be (name Type), found " ++ renderHead other)

fromOfType :: FilePath -> SExpr -> Either String (Expr, Expr)
fromOfType path (SList [SAtom "of-type", e, ty]) = do
  e' <- fromExpr path e
  ty' <- fromType path ty
  pure (e', ty')
fromOfType path other =
  Left (path ++ ": match scrutinee must be (of-type <expr> <Type>), found " ++ renderHead other)

mkApp :: Expr -> [Expr] -> Expr
mkApp (EApp f existing) more = EApp f (existing ++ more)
mkApp f more                 = EApp f more

fromMatchCase :: FilePath -> SExpr -> Either String MatchCase
fromMatchCase path (SList [SAtom "empty-case", body]) =
  MatchEmpty <$> fromExpr path body
fromMatchCase path (SList [SAtom "cons-case", SList [SAtom h, hTy], SList [SAtom t, tTy], body]) = do
  hTy' <- fromType path hTy
  tTy' <- fromType path tTy
  body' <- fromExpr path body
  pure (MatchCons h hTy' t tTy' body')
fromMatchCase path (SList [SAtom "false-case", body]) =
  MatchFalse <$> fromExpr path body
fromMatchCase path (SList [SAtom "true-case", body]) =
  MatchTrue <$> fromExpr path body
fromMatchCase path (SList [SAtom "pair-case", SList [SAtom a, aTy], SList [SAtom b, bTy], body]) = do
  aTy' <- fromType path aTy
  bTy' <- fromType path bTy
  body' <- fromExpr path body
  pure (MatchPair a aTy' b bTy' body')
fromMatchCase path other =
  Left (path ++ ": invalid match case " ++ renderHead other)

fromComp :: FilePath -> SExpr -> Either String Comp
fromComp path se = case se of
  SList [SAtom "return", e] -> CReturn <$> fromExpr path e
  SList [SAtom "perform", e] -> CPerform <$> fromExpr path e
  SList [SAtom "bind", SList [SAtom v, c1], c2] -> do
    c1' <- fromComp path c1
    c2' <- fromComp path c2
    pure $ CBind v c1' c2'
  _ -> Left (path ++ ": invalid computation form " ++ renderHead se)

fromType :: FilePath -> SExpr -> Either String Expr
fromType path se = case se of
  SAtom "Natural" -> Right (ETypeConst TCNatural)
  SAtom "String" -> Right (ETypeConst TCString)
  SAtom "Boolean" -> Right (ETypeConst TCBoolean)
  SAtom "Unit" -> Right (ETypeConst TCUnit)
  SAtom "List" -> Right (ETypeConst TCList)
  SAtom "Pair" -> Right (ETypeConst TCPair)
  SAtom "true" -> Right (ELit (LBoolean True))
  SAtom "false" -> Right (ELit (LBoolean False))
  SAtom "tt" -> Right (ELit LUnit)
  SAtom t
    | Just n <- parseUniverseAtom t -> Right (ETypeUniverse n)
    | otherwise -> Right (EVar t)
  SNum n -> Right (ELit (LNatural n))
  SStr s -> Right (ELit (LString s))
  SList [] -> Left (path ++ ": empty type expression")
  SList [SAtom "computation", t] -> ECompType <$> fromType path t
  SList [SAtom "for-all", SList [SAtom v, a], b] ->
    EForAll v <$> fromType path a <*> fromType path b
  SList [SAtom "there-exists", SList [SAtom v, a], b] ->
    EThereExists v <$> fromType path a <*> fromType path b
  SList (f:args) -> do
    f' <- fromType path f
    args' <- mapM (fromType path) args
    pure (mkApp f' args')

parseUniverseAtom :: Text -> Maybe Int
parseUniverseAtom t =
  case DT.stripPrefix "Type" t of
    Just rest | DT.all isDigitChar rest && not (DT.null rest) ->
      Just (read (DT.unpack rest))
    _ -> Nothing
  where
    isDigitChar c = c >= '0' && c <= '9'

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
  keyword "end"
  pure (Module name imports opens defs)

pMImport :: Parser Import
pMImport = do
  keyword "import"
  modName <- pModuleName
  alias <- optional (keyword "as" *> pModuleName)
  let chosen = maybe modName id alias
  pure (Import modName chosen)

pMOpen :: Parser Open
pMOpen = do
  keyword "open"
  modAlias <- pIdentifier
  keyword "exposing"
  names <- manyTill pIdentifier (keyword "end")
  pure (Open modAlias names)

pDefinition :: Parser Definition
pDefinition = mLexeme $ do
  keyword "define"
  tr <- (Transparent <$ keyword "transparent") <|> (Opaque <$ keyword "opaque")
  name <- pIdentifier
  keyword "as"
  body <- pValue
  pure (Definition tr name body)

-- Computations

pComp :: Parser Comp
pComp = pBind <|> pReturn <|> pPerform

pBind :: Parser Comp
pBind = M.try $ do
  keyword "bind"
  v <- pIdentifier
  keyword "from"
  first <- pComp
  keyword "then"
  rest <- pComp
  keyword "end"
  pure (CBind v first rest)

pReturn :: Parser Comp
pReturn = do
  keyword "return"
  CReturn <$> pValue

pPerform :: Parser Comp
pPerform = do
  keyword "perform"
  CPerform <$> pValue

-- Expressions (values)

pValue :: Parser Expr
pValue = pLet <|> pMatch <|> pFunction <|> pCompute <|> pAnnot <|> pApp

pLet :: Parser Expr
pLet = M.try $ do
  keyword "let"
  keyword "value"
  v <- pIdentifier
  keyword "be"
  val <- pValue
  keyword "in"
  body <- pValue
  keyword "end"
  pure (ELet v val body)

pFunction :: Parser Expr
pFunction = M.try $ do
  keyword "function"
  params <- manyTill pParam (keyword "returns")
  retTy <- pType
  body <- (FunctionValue <$> (keyword "value" *> pValue))
      <|> (FunctionCompute <$> (keyword "compute" *> pComp))
  keyword "end"
  pure (EFunction params retTy body)
  where
    pParam = do
      name <- pIdentifier
      ty <- pTypeParam
      pure (Param name ty)

pCompute :: Parser Expr
pCompute = M.try $ do
  keyword "compute"
  comp <- pComp
  keyword "end"
  pure (ECompute comp)

pAnnot :: Parser Expr
pAnnot = M.try $ do
  keyword "of-type"
  e <- pValue
  ty <- pType
  pure (EAnnot e ty)

pMatch :: Parser Expr
pMatch = M.try $ do
  keyword "match"
  scrut <- pValue
  keyword "of-type"
  scrutTy <- pType
  cases <- manyTill pMatchCase (keyword "end")
  pure (EMatch scrut scrutTy cases)

pMatchCase :: Parser MatchCase
pMatchCase =
      pEmptyCase
  <|> pConsCase
  <|> pFalseCase
  <|> pTrueCase
  <|> pPairCase
  where
    pEmptyCase = M.try $ do
      keyword "empty-case"
      keyword "as"
      MatchEmpty <$> pValue
    pConsCase = M.try $ do
      keyword "cons-case"
      keyword "with"
      h <- pIdentifier
      hTy <- pTypeParam
      t <- pIdentifier
      tTy <- pTypeParam
      keyword "as"
      body <- pValue
      pure (MatchCons h hTy t tTy body)
    pFalseCase = M.try $ do
      keyword "false-case"
      keyword "as"
      MatchFalse <$> pValue
    pTrueCase = M.try $ do
      keyword "true-case"
      keyword "as"
      MatchTrue <$> pValue
    pPairCase = M.try $ do
      keyword "pair-case"
      keyword "with"
      a <- pIdentifier
      aTy <- pTypeParam
      b <- pIdentifier
      bTy <- pTypeParam
      keyword "as"
      body <- pValue
      pure (MatchPair a aTy b bTy body)

pApp :: Parser Expr
pApp = do
  atoms <- some pValueAtom
  case atoms of
    [a]      -> pure a
    (f:args) -> pure (EApp f args)
    []       -> fail "unreachable: some pValueAtom returned []"

pValueAtom :: Parser Expr
pValueAtom =
      parens pValue
  <|> pLiteral
  <|> pUniverse
  <|> pTypeConst
  <|> (EVar <$> pIdentifier)

pLiteral :: Parser Expr
pLiteral =
      (ELit (LBoolean True) <$ keyword "true")
  <|> (ELit (LBoolean False) <$ keyword "false")
  <|> (ELit LUnit <$ keyword "tt")
  <|> (ELit . LNatural <$> lexeme L.decimal)
  <|> pStringLit

pStringLit :: Parser Expr
pStringLit = lexeme $ do
  _ <- C.char '"'
  content <- manyTill L.charLiteral (C.char '"')
  pure (ELit (LString (DT.pack content)))

-- Type parsing

pType :: Parser Expr
pType = pForAll <|> pThereExists <|> pComputation <|> pTypeApp

pTypeParam :: Parser Expr
pTypeParam = parens pType <|> pTypeSimple

pForAll :: Parser Expr
pForAll = M.try $ do
  keyword "for-all"
  v <- pIdentifier
  keyword "as"
  dom <- pType
  keyword "to"
  cod <- pType
  pure (EForAll v dom cod)

pThereExists :: Parser Expr
pThereExists = M.try $ do
  keyword "there-exists"
  v <- pIdentifier
  keyword "as"
  dom <- pType
  keyword "in"
  cod <- pType
  pure (EThereExists v dom cod)

pComputation :: Parser Expr
pComputation = M.try $ do
  keyword "computation"
  ECompType <$> pType

pTypeApp :: Parser Expr
pTypeApp = do
  headTy <- pTypeAtom
  args <- many pTypeAtom
  case args of
    [] -> pure headTy
    _  -> pure (mkApp headTy args)

pTypeAtom :: Parser Expr
pTypeAtom =
      parens pType
  <|> pTypeSimple

pTypeSimple :: Parser Expr
pTypeSimple =
      pUniverse
  <|> pTypeConst
  <|> pLiteral
  <|> (EVar <$> pIdentifier)

pUniverse :: Parser Expr
pUniverse = mLexeme . M.try $ do
  _ <- C.string "Type"
  n <- L.decimal
  pure (ETypeUniverse n)

pTypeConst :: Parser Expr
pTypeConst =
      (ETypeConst TCNatural <$ keyword "Natural")
  <|> (ETypeConst TCString <$ keyword "String")
  <|> (ETypeConst TCBoolean <$ keyword "Boolean")
  <|> (ETypeConst TCUnit <$ keyword "Unit")
  <|> (ETypeConst TCList <$ keyword "List")
  <|> (ETypeConst TCPair <$ keyword "Pair")

-- Identifiers and symbols

pIdentifier :: Parser Text
pIdentifier = mLexeme . M.try $ do
  first <- M.satisfy (\c -> (isLetter c && isAscii c) || c == '_')
  rest <- many (M.satisfy (\c -> (isAlphaNum c && isAscii c) || c == '_' || c == '-' || c == ':'))
  let ident = DT.pack (first:rest)
  if ident `elem` reservedWords || isUniverseKeyword ident
     then fail ("keyword " ++ DT.unpack ident ++ " cannot be used as an identifier")
     else pure ident

isUniverseKeyword :: Text -> Bool
isUniverseKeyword t = case parseUniverseAtom t of
  Just _ -> True
  Nothing -> False

reservedWords :: [Text]
reservedWords =
  [ "module", "contains", "import", "open", "define", "transparent", "opaque"
  , "as", "function", "returns", "value", "compute", "let", "bind", "from", "then"
  , "perform", "return", "match", "of-type", "exposing", "end", "be", "in", "with"
  , "empty-case", "cons-case", "false-case", "true-case", "pair-case"
  , "for-all", "there-exists", "computation", "to"
  , "true", "false", "tt"
  , "Natural", "String", "Boolean", "Unit", "List", "Pair"
  ]

pModuleName :: Parser Text
pModuleName = mLexeme $ do
  first <- M.satisfy (\c -> (isAlphaNum c && isAscii c) || c == '_')
  rest <- many (M.satisfy (\c -> (isAlphaNum c && isAscii c) || c == '_' || c == '-' || c == '/' || c == ':'))
  pure (DT.pack (first:rest))

-- M-expr keyword (uses M-expr space consumer)
keyword :: Text -> Parser ()
keyword t = mLexeme (M.try (C.string t *> notFollowedBy (M.satisfy isIdentChar)))

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c `elem` ("_-:" :: String)

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
  "(open " <> modAlias <> " exposing " <> DT.unwords names <> ")"

renderModule :: Text -> [Definition] -> DT.Text
renderModule name defs =
  case defs of
    [] -> "(module " <> name <> ")"
    _  -> "(module " <> name <> " " <> DT.unwords (map renderDef defs) <> ")"

renderDef :: Definition -> DT.Text
renderDef (Definition tr name body) =
  "(define " <> renderTransparency tr <> " " <> name <> " " <> renderExpr body <> ")"
  where
    renderTransparency Transparent = "transparent"
    renderTransparency Opaque      = "opaque"

renderExpr :: Expr -> DT.Text
renderExpr expr = case expr of
  EVar t          -> t
  ELit lit        -> renderLit lit
  ETypeConst tc   -> T.typeConstName tc
  ETypeUniverse n -> "Type" <> DT.pack (show n)
  EForAll v dom cod ->
    "(for-all (" <> v <> " " <> T.typeToSExpr dom <> ") " <> T.typeToSExpr cod <> ")"
  EThereExists v dom cod ->
    "(there-exists (" <> v <> " " <> T.typeToSExpr dom <> ") " <> T.typeToSExpr cod <> ")"
  ECompType t -> "(computation " <> T.typeToSExpr t <> ")"
  EApp f args     -> "(" <> DT.unwords (renderExpr f : map renderExpr args) <> ")"
  EFunction params retTy body ->
    "(function " <> DT.unwords (map renderParam params)
      <> (if null params then "" else " ")
      <> T.typeToSExpr retTy <> " " <> renderFunctionBody body <> ")"
  ELet name val body ->
    "(let (" <> name <> " " <> renderExpr val <> ") " <> renderExpr body <> ")"
  ECompute comp -> "(compute " <> renderComp comp <> ")"
  EMatch scrut scrutTy cases ->
    "(match (of-type " <> renderExpr scrut <> " " <> T.typeToSExpr scrutTy <> ") "
      <> DT.unwords (map renderMatchCase cases) <> ")"
  EAnnot e ty -> "(of-type " <> renderExpr e <> " " <> T.typeToSExpr ty <> ")"
  ETyped e _ -> renderExpr e
  EDict className impls ->
    "(dict " <> className <> " " <>
    DT.intercalate " " [n <> " " <> renderExpr e | (n, e) <- impls] <> ")"
  EDictAccess d method -> "(dict-access " <> renderExpr d <> " " <> method <> ")"

renderParam :: Param -> DT.Text
renderParam (Param name ty) = "(" <> name <> " " <> T.typeToSExpr ty <> ")"

renderFunctionBody :: FunctionBody -> DT.Text
renderFunctionBody body = case body of
  FunctionValue e -> "(value " <> renderExpr e <> ")"
  FunctionCompute c -> "(compute " <> renderComp c <> ")"

renderMatchCase :: MatchCase -> DT.Text
renderMatchCase mc = case mc of
  MatchEmpty body -> "(empty-case " <> renderExpr body <> ")"
  MatchCons h hTy t tTy body ->
    "(cons-case (" <> h <> " " <> T.typeToSExpr hTy <> ") (" <> t <> " "
      <> T.typeToSExpr tTy <> ") " <> renderExpr body <> ")"
  MatchFalse body -> "(false-case " <> renderExpr body <> ")"
  MatchTrue body -> "(true-case " <> renderExpr body <> ")"
  MatchPair a aTy b bTy body ->
    "(pair-case (" <> a <> " " <> T.typeToSExpr aTy <> ") (" <> b <> " "
      <> T.typeToSExpr bTy <> ") " <> renderExpr body <> ")"

renderComp :: Comp -> DT.Text
renderComp comp = case comp of
  CReturn e      -> "(return " <> renderExpr e <> ")"
  CPerform e     -> "(perform " <> renderExpr e <> ")"
  CBind v c1 c2  -> "(bind (" <> v <> " " <> renderComp c1 <> ") " <> renderComp c2 <> ")"
  CSeq c1 c2     -> "(bind (_ " <> renderComp c1 <> ") " <> renderComp c2 <> ")"

renderLit :: Literal -> DT.Text
renderLit lit = case lit of
  LNatural n -> DT.pack (show n)
  LString s  -> DT.pack (show (DT.unpack s))
  LBoolean b -> if b then "true" else "false"
  LUnit      -> "tt"

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
  "open " <> modAlias <> " exposing " <> DT.unwords names <> " end"

renderMDef :: Definition -> [DT.Text]
renderMDef (Definition tr name body) =
  ["  define " <> renderMTransparency tr <> " " <> name <> " as " <> renderMExpr body]

renderMTransparency :: Transparency -> DT.Text
renderMTransparency Transparent = "transparent"
renderMTransparency Opaque      = "opaque"

renderMExpr :: Expr -> DT.Text
renderMExpr expr = case expr of
  EVar t      -> t
  ELit lit    -> renderLit lit
  ETypeConst tc -> T.typeConstName tc
  ETypeUniverse n -> "Type" <> DT.pack (show n)
  EForAll v dom cod ->
    "for-all " <> v <> " as " <> T.prettyTypeAtom dom <> " to " <> T.prettyType cod
  EThereExists v dom cod ->
    "there-exists " <> v <> " as " <> T.prettyTypeAtom dom <> " in " <> T.prettyType cod
  ECompType t -> "computation " <> T.prettyTypeAtom t
  EApp f args -> "(" <> DT.unwords (renderMExpr f : map renderMExpr args) <> ")"
  EFunction params retTy body ->
    "function " <> renderParams params <> " returns " <> T.prettyType retTy <> " "
      <> renderFunctionBodyM body <> " end"
  ELet name val body ->
    "let value " <> name <> " be " <> renderMExpr val <> " in " <> renderMExpr body <> " end"
  ECompute comp -> "compute " <> renderMComp comp <> " end"
  EMatch scrut scrutTy cases ->
    "match " <> renderMExpr scrut <> " of-type " <> T.prettyType scrutTy <> " "
      <> DT.unwords (map renderMatchCaseM cases) <> " end"
  EAnnot e ty -> "of-type " <> renderMExpr e <> " " <> T.prettyType ty
  ETyped e _ -> renderMExpr e
  EDict className impls ->
    "(dict " <> className <> " " <>
    DT.intercalate " " [n <> " " <> renderMExpr e | (n, e) <- impls] <> ")"
  EDictAccess d method -> "(dict-access " <> renderMExpr d <> " " <> method <> ")"

renderParams :: [Param] -> DT.Text
renderParams params = DT.unwords (map renderParamM params)

renderParamM :: Param -> DT.Text
renderParamM (Param name ty) = name <> " " <> T.prettyTypeAtom ty

renderFunctionBodyM :: FunctionBody -> DT.Text
renderFunctionBodyM body = case body of
  FunctionValue e -> "value " <> renderMExpr e
  FunctionCompute c -> "compute " <> renderMComp c

renderMatchCaseM :: MatchCase -> DT.Text
renderMatchCaseM mc = case mc of
  MatchEmpty body -> "empty-case as " <> renderMExpr body
  MatchCons h hTy t tTy body ->
    "cons-case with " <> h <> " " <> T.prettyTypeAtom hTy <> " "
      <> t <> " " <> T.prettyTypeAtom tTy <> " as " <> renderMExpr body
  MatchFalse body -> "false-case as " <> renderMExpr body
  MatchTrue body -> "true-case as " <> renderMExpr body
  MatchPair a aTy b bTy body ->
    "pair-case with " <> a <> " " <> T.prettyTypeAtom aTy <> " "
      <> b <> " " <> T.prettyTypeAtom bTy <> " as " <> renderMExpr body

renderMComp :: Comp -> DT.Text
renderMComp comp = case comp of
  CReturn e  -> "return " <> renderMExpr e
  CPerform e -> "perform " <> renderMExpr e
  CBind v c1 c2 ->
    "bind " <> v <> " from " <> renderMComp c1 <> " then " <> renderMComp c2 <> " end"
  CSeq c1 c2 ->
    "bind _ from " <> renderMComp c1 <> " then " <> renderMComp c2 <> " end"
