{-# LANGUAGE OverloadedStrings #-}
module Parser
  ( parseModuleFile
  , parseMExprFile
  , moduleToSExprText
  , moduleToMExprText
  ) where

import           AST
import           Data.Char (isAlphaNum, isLetter, isSpace)
import           Data.List (intercalate)
import           Data.Text (Text)
import qualified Data.Text as T
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
  case filter (not . isImportForm) pre of
    (bad:_) -> Left (path ++ ": unexpected toplevel form " ++ renderHead bad ++ "; only imports may precede the module")
    []      -> pure ()
  case drop 1 rest of
    (bad:_) -> Left (path ++ ": unexpected form after module; only a single module is allowed per file (saw " ++ renderHead bad ++ ")")
    []      -> pure ()
  imports <- mapM (fromImport path) pre
  modVal <- fromSExprModule path modExpr
  pure modVal { modImports = imports }

isImportForm :: SExpr -> Bool
isImportForm (SList (SAtom "import" : _)) = True
isImportForm _ = False

isModuleForm :: SExpr -> Bool
isModuleForm (SList (SAtom "module" : _)) = True
isModuleForm _ = False

fromSExprModule :: FilePath -> SExpr -> Either String Module
fromSExprModule path (SList (SAtom "module" : SAtom name : defs)) = do
  defs' <- mapM (fromDef path) defs
  pure $ Module name [] defs'
fromSExprModule path other =
  Left (path ++ ": expected `(module <Name> ...)`, found " ++ renderHead other)

fromImport :: FilePath -> SExpr -> Either String Import
fromImport _ (SList [SAtom "import", SAtom modName]) = Right (Import modName modName)
fromImport _ (SList [SAtom "import", SAtom modName, SAtom alias]) = Right (Import modName alias)
fromImport path other = Left (path ++ ": invalid import form " ++ renderHead other)

fromDef :: FilePath -> SExpr -> Either String Definition
fromDef path (SList [SAtom "def", SAtom tr, SAtom name, body]) = do
  transparency <- case tr of
    "transparent" -> Right Transparent
    "opaque"      -> Right Opaque
    _             -> Left (path ++ ": definition " ++ T.unpack name ++ " must declare transparency (transparent|opaque)")
  case body of
    SList [SAtom "value", e] -> do
      e' <- fromExpr path e
      pure $ Definition transparency name ValueDef (Left e')
    SList [SAtom "computation", c] -> do
      c' <- fromComp path c
      pure $ Definition transparency name ComputationDef (Right c')
    _ -> Left (path ++ ": definition " ++ T.unpack name ++ " must specify (value ...) or (computation ...)")
fromDef path other = Left (path ++ ": invalid definition form " ++ renderHead other)

fromExpr :: FilePath -> SExpr -> Either String Expr
fromExpr _ (SAtom "true")  = Right (ELit (LBool True))
fromExpr _ (SAtom "false") = Right (ELit (LBool False))
fromExpr _ (SAtom t)       = Right (EVar t)
fromExpr _ (SNum n)        = Right (ELit (LNat n))
fromExpr _ (SStr s)        = Right (ELit (LString s))
fromExpr path (SList [])   = Left (path ++ ": empty expression list")
fromExpr path (SList (SAtom "lambda" : SList params : body : [])) = do
  (names, finalBody) <- peel path params body
  pure $ foldr ELam finalBody names
fromExpr path (SList (SAtom "let" : SList binds : body : [])) = do
  binds' <- mapM (fromLetBind path) binds
  body' <- fromExpr path body
  pure (desugarLet binds' body')
fromExpr path (SList (f:args)) = do
  f' <- fromExpr path f
  args' <- mapM (fromExpr path) args
  pure $ mkApp f' args'

mkApp :: Expr -> [Expr] -> Expr
mkApp (EApp f existing) more = EApp f (existing ++ more)
mkApp f more                 = EApp f more

fromLetBind :: FilePath -> SExpr -> Either String (Text, Expr)
fromLetBind path (SList (SAtom v : parts)) = case parts of
  []      -> Left (path ++ ": let binding for " ++ T.unpack v ++ " must have a value")
  [expr]  -> do e <- fromExpr path expr; pure (v, e)
  exprs   -> do e <- fromExpr path (SList exprs); pure (v, e)
fromLetBind path other = Left (path ++ ": invalid let binding form " ++ renderHead other)

desugarLet :: [(Text, Expr)] -> Expr -> Expr
desugarLet binds body = foldr wrap body binds
  where
    wrap (v, val) acc = EApp (ELam v acc) [val]

peel :: FilePath -> [SExpr] -> SExpr -> Either String ([Text], Expr)
peel path ps b = do
  names <- mapM grab ps
  b' <- fromExpr path b
  pure (names, b')
  where
    grab (SAtom v)            = Right v
    grab (SList (SAtom v:_))  = Right v
    grab otherExpr            = Left (path ++ ": lambda parameters must be atoms, saw " ++ renderHead otherExpr)

fromComp :: FilePath -> SExpr -> Either String Comp
fromComp path se = case se of
  SList [SAtom "return", e] -> CReturn <$> fromExpr path e
  SList (SAtom "do" : rest) -> doBlock path rest
  SList [SAtom "perform", SAtom "io", e] -> CPerform <$> fromExpr path e
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

-- Parse (do ...) block into nested binds/seqs

doBlock :: FilePath -> [SExpr] -> Either String Comp
doBlock path [] = Left (path ++ ": empty do block")
doBlock path [one] = fromComp path one
doBlock path (stmt:rest) = case stmt of
  SList [SAtom "bind", SAtom v, c] -> do
    c' <- fromComp path c
    rest' <- doBlock path rest
    pure $ CBind v c' rest'
  _ -> do
    c' <- fromComp path stmt
    rest' <- doBlock path rest
    pure $ CSeq c' rest'

renderHead :: SExpr -> String
renderHead sexpr = case sexpr of
  SAtom t     -> T.unpack t
  SNum n      -> show n
  SStr s      -> show s
  SList []    -> "()"
  SList (h:_) -> "(" ++ headText h ++ " ...)"
  where
    headText (SAtom t) = T.unpack t
    headText (SNum n)  = show n
    headText (SStr s)  = show s
    headText (SList xs) = "(" ++ intercalate " " (map headText xs) ++ ")"

--------------------------------------------------------------------------------
-- M-expression parser (surface syntax)

parseMExprFile :: FilePath -> T.Text -> Either String Module
parseMExprFile path rawTxt = do
  txt <- preprocessInput path rawTxt
  case M.parse (spaceConsumer *> pMModule <* spaceConsumer <* M.eof) path txt of
    Left err -> Left (whitespaceHelp <> M.errorBundlePretty err)
    Right m  -> Right m

pMModule :: Parser Module
pMModule = do
  imports <- many pMImport
  keyword "module"
  name <- pModuleName
  keyword "contains"
  defs <- many pDefinition
  spaceConsumer
  keyword "end"
  pure (Module name imports defs)

pMImport :: Parser Import
pMImport = do
  keyword "import"
  modName <- pModuleName
  alias <- optional (keyword "as" *> pIdentifier)
  let chosen = maybe modName id alias
  pure (Import modName chosen)

pDefinition :: Parser Definition
pDefinition = lexeme $ do
  keyword "define"
  tr <- (Transparent <$ keyword "transparent") <|> (Opaque <$ keyword "opaque")
  name <- pIdentifier
  keyword "as"
  kind <- (ValueDef <$ keyword "value") <|> (ComputationDef <$ keyword "computation")
  body <- case kind of
    ValueDef        -> Left <$> pExpr
    ComputationDef  -> Right <$> pComp
  pure (Definition tr name kind body)

-- Computations

pComp :: Parser Comp
pComp = pBind <|> pCompNonBind

pBind :: Parser Comp
pBind = M.try $ do
  keyword "bind"
  v <- pIdentifier
  _ <- symbol "<-"
  first <- pCompNonBind
  keyword "then"
  rest <- pComp
  pure (CBind v first rest)

pCompNonBind :: Parser Comp
pCompNonBind =
  pReturn
    <|> pPerform
    <|> pSeq
    <|> compFromExpr <$> pExpr

pReturn :: Parser Comp
pReturn = do
  keyword "return"
  CReturn <$> pExpr

pPerform :: Parser Comp
pPerform = do
  keyword "perform"
  keyword "io"
  CPerform <$> pExpr

pSeq :: Parser Comp
pSeq = M.try $ do
  keyword "do"
  first <- pComp
  keyword "then"
  CSeq first <$> pComp

compFromExpr :: Expr -> Comp
compFromExpr (EVar t) = CVar t
compFromExpr e        = CReturn e

-- Expressions

pExpr :: Parser Expr
pExpr = pLet <|> pFunction <|> pLambda <|> pInspect <|> pApp

pLet :: Parser Expr
pLet = M.try $ do
  keyword "let"
  schemeBinds <- optional (M.try schemeBindings)
  case schemeBinds of
    Just binds -> do
      body <- pExpr
      pure (foldr (\(v,val) acc -> EApp (ELam v acc) [val]) body binds)
    Nothing -> do
      v <- pIdentifier
      _ <- symbol "="
      val <- pExpr
      keyword "in"
      body <- pExpr
      pure (EApp (ELam v body) [val])
  where
    schemeBindings = between (symbol "(") (symbol ")") (some oneBind)
    oneBind = between (symbol "(") (symbol ")") $ do
      v <- pIdentifier
      val <- pExpr
      pure (v, val)

pLambda :: Parser Expr
pLambda = do
  keyword "lambda"
  parenForm <|> arrowForm
  where
    parenForm = do
      params <- M.try (between (symbol "(") (symbol ")") (many pIdentifier))
      body <- pExpr
      pure (foldr ELam body params)
    arrowForm = do
      params <- many pIdentifier
      keyword "->"
      body <- pExpr
      pure (foldr ELam body params)

pFunction :: Parser Expr
pFunction = M.try $ do
  keyword "function"
  params <- some pIdentifier
  keyword "of-type"
  _ty <- pExpr  -- parsed and discarded; type not represented in the current AST
  keyword "produce"
  body <- pExpr
  pure (foldr ELam body params)

pInspect :: Parser Expr
pInspect = do
  keyword "inspect"
  scrut <- pExpr
  keyword "with"
  cases <- some pCase
  keyword "end"
  pure (EApp (EVar "match") (scrut : cases))

pCase :: Parser Expr
pCase = do
  keyword "case"
  args <- many pIdentifier
  keyword "->"
  body <- pExpr
  pure (foldr ELam body args)

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
      (ELit (LBool True) <$ keyword "true")
  <|> (ELit (LBool False) <$ keyword "false")
  <|> (ELit . LNat <$> lexeme (L.decimal))
  <|> pStringLit

pStringLit :: Parser Expr
pStringLit = lexeme $ do
  _ <- C.char '"'
  content <- manyTill L.charLiteral (C.char '"')
  pure (ELit (LString (T.pack content)))

-- Identifiers and symbols

pIdentifier :: Parser Text
pIdentifier = lexeme . M.try $ do
  first <- M.satisfy (\c -> isLetter c || c == '_')
  rest <- many (M.satisfy (\c -> isAlphaNum c || elem c ("._-:" :: String)))
  let ident = T.pack (first:rest)
  if ident `elem` reservedWords
     then fail ("keyword " ++ T.unpack ident ++ " cannot be used as an identifier")
     else pure ident

reservedWords :: [Text]
reservedWords =
  [ "module", "contains", "import", "define", "transparent", "opaque"
  , "as", "value", "computation", "function", "of-type", "produce"
  , "lambda", "let", "in", "inspect", "with", "case", "return"
  , "perform", "io", "bind", "then", "do", "end"
  ]

pModuleName :: Parser Text
pModuleName = lexeme $ do
  first <- M.satisfy isLetter
  rest <- many (M.satisfy (\c -> isAlphaNum c || c `elem` ("_:." :: String)))
  pure (T.pack (first:rest))

keyword :: Text -> Parser ()
keyword t = lexeme (C.string t *> notFollowedBy (M.satisfy isIdentChar))

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c `elem` ("._-:" :: String)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

--------------------------------------------------------------------------------
-- Input preprocessing (whitespace normalization/validation)

preprocessInput :: FilePath -> T.Text -> Either String T.Text
preprocessInput path rawTxt = do
  let normalized = normalizeLineEndings rawTxt
  ensureTrailingNewline path normalized >>= pure . collapseDuplicateBlankLines

normalizeLineEndings :: T.Text -> T.Text
normalizeLineEndings = T.replace "\r\n" "\n" . T.replace "\r" "\n"

ensureTrailingNewline :: FilePath -> T.Text -> Either String T.Text
ensureTrailingNewline path txt
  | T.null txt = Right txt
  | T.isSuffixOf "\n" txt = Right txt
  | otherwise = Left (path ++ ": missing trailing newline; locque source requires a newline at EOF to avoid ambiguous whitespace during parsing.")

collapseDuplicateBlankLines :: T.Text -> T.Text
collapseDuplicateBlankLines =
  T.unlines . foldr compress [] . T.splitOn "\n"
  where
    compress line acc =
      case (T.all isSpace line, acc) of
        (True, next:rest) | T.all isSpace next -> "" : rest
        _ -> line : acc

whitespaceHelp :: String
whitespaceHelp = "Note: whitespace preprocessing (newline required, no duplicate blank lines, normalized line endings) succeeded; parse error details follow:\n"

--------------------------------------------------------------------------------
-- S-expression pretty-printer (AST -> S-text)

moduleToSExprText :: Module -> T.Text
moduleToSExprText (Module name imports defs) =
  let importLines = map renderImport imports
      moduleLine = renderModule name defs
  in T.unlines (importLines ++ [moduleLine])

renderImport :: Import -> T.Text
renderImport (Import modName alias)
  | modName == alias = "(import " <> modName <> ")"
  | otherwise        = "(import " <> modName <> " " <> alias <> ")"

renderModule :: Text -> [Definition] -> T.Text
renderModule name defs =
  case defs of
    [] -> "(module " <> name <> ")"
    _  -> "(module " <> name <> " " <> T.unwords (map renderDef defs) <> ")"

renderDef :: Definition -> T.Text
renderDef (Definition tr name kind body) =
  "(def " <> renderTransparency tr <> " " <> name <> " " <> renderBody kind body <> ")"
  where
    renderTransparency Transparent = "transparent"
    renderTransparency Opaque      = "opaque"

renderBody :: DefKind -> Either Expr Comp -> T.Text
renderBody ValueDef (Left e)        = "(value " <> renderExpr e <> ")"
renderBody ComputationDef (Right c) = "(computation " <> renderComp c <> ")"
renderBody _ _ = "(value <invalid>)"

renderExpr :: Expr -> T.Text
renderExpr expr = case expr of
  EVar t      -> t
  ELit lit    -> renderLit lit
  ELam v b    -> "(lambda (" <> v <> ") " <> renderExpr b <> ")"
  EApp f args -> "(" <> T.unwords (renderExpr f : map renderExpr args) <> ")"

renderComp :: Comp -> T.Text
renderComp comp = case comp of
  CReturn e      -> "(return " <> renderExpr e <> ")"
  CBind v c1 c2  -> "(bind " <> v <> " " <> renderComp c1 <> " " <> renderComp c2 <> ")"
  CPerform e     -> "(perform io " <> renderExpr e <> ")"
  CVar t         -> t
  CSeq c1 c2     -> "(do " <> renderComp c1 <> " " <> renderComp c2 <> ")"

renderLit :: Literal -> T.Text
renderLit lit = case lit of
  LNat n    -> T.pack (show n)
  LString s -> T.pack (show (T.unpack s))
  LBool b   -> if b then "true" else "false"

--------------------------------------------------------------------------------
-- AST -> M-expression pretty-printer

moduleToMExprText :: Module -> T.Text
moduleToMExprText (Module name imports defs) =
  T.unlines $
    map renderMImport imports
    ++ ["" | not (null imports)]
    ++ ["module " <> name <> " contains"]
    ++ concatMap renderMDef defs
    ++ ["end"]

renderMImport :: Import -> T.Text
renderMImport (Import modName alias)
  | modName == alias = "import " <> modName
  | otherwise        = "import " <> modName <> " as " <> alias

renderMDef :: Definition -> [T.Text]
renderMDef (Definition tr name kind body) =
  let header = "  define " <> renderMTransparency tr <> " " <> name <> " as " <> renderMKind kind
  in case (kind, body) of
      (ValueDef, Left e)        -> [header <> " " <> renderMExpr e]
      (ComputationDef, Right c) -> header : renderMComp 4 c
      _                         -> [header <> " <invalid>"]

renderMTransparency :: Transparency -> T.Text
renderMTransparency Transparent = "transparent"
renderMTransparency Opaque      = "opaque"

renderMKind :: DefKind -> T.Text
renderMKind ValueDef       = "value"
renderMKind ComputationDef = "computation"

renderMComp :: Int -> Comp -> [T.Text]
renderMComp indentLevel comp = case comp of
  CReturn e -> [indent indentLevel ("return " <> renderMExpr e)]
  CPerform e -> [indent indentLevel ("perform io " <> renderMExpr e)]
  CVar t -> [indent indentLevel t]
  CSeq c1 c2 ->
    [indent indentLevel "do"] ++ renderMComp (indentLevel+2) c1 ++ [indent indentLevel "then"] ++ renderMComp (indentLevel+2) c2
  CBind v c1 c2 ->
    [indent indentLevel ("bind " <> v <> " <- " <> renderMCompInline c1 <> " then")] ++ renderMComp (indentLevel+2) c2

renderMCompInline :: Comp -> T.Text
renderMCompInline c = T.intercalate " " (renderMComp 0 c)

renderMExpr :: Expr -> T.Text
renderMExpr expr = render expr False
  where
    render e inAtom = case e of
      EVar t      -> t
      ELit lit    -> renderLit lit
      ELam{}      -> wrapIf inAtom (renderLambdaChain e)
      EApp f args ->
        case matchAsInspect f args of
          Just (scrut, cases) -> wrapIf inAtom (renderInspect scrut cases)
          Nothing ->
            let parts = renderAtom f : map renderAtom args
            in "(" <> T.unwords parts <> ")"
    renderAtom e = render e True
    wrapIf True t  = "(" <> t <> ")"
    wrapIf False t = t

renderLambdaChain :: Expr -> T.Text
renderLambdaChain e =
  let (params, body) = collectLams e
  in "lambda " <> T.unwords params <> " -> " <> renderMExpr body

collectLams :: Expr -> ([Text], Expr)
collectLams = go []
  where
    go acc (ELam v b) = go (acc ++ [v]) b
    go acc other      = (acc, other)

matchAsInspect :: Expr -> [Expr] -> Maybe (Expr, [Expr])
matchAsInspect (EVar "match") (scrut:cases) = Just (scrut, cases)
matchAsInspect _ _ = Nothing

renderInspect :: Expr -> [Expr] -> T.Text
renderInspect scrut cases =
  "inspect " <> renderMExpr scrut <> " with " <> T.unwords (map renderCase cases) <> " end"
  where
    renderCase c =
      let (params, body) = collectLams c
          paramsTxt = if null params then "" else " " <> T.unwords params
      in "case" <> paramsTxt <> " -> " <> renderMExpr body

indent :: Int -> T.Text -> T.Text
indent n t = T.replicate n " " <> t
