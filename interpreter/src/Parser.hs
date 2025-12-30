{-# LANGUAGE OverloadedStrings #-}
module Parser
  ( parseModuleFile
  , parseMExprFile
  , exprToSExprText
  , exprToMExprText
  , moduleToSExprText
  , moduleToMExprText
  ) where

import AST
import qualified Type as T
import Data.Char (isAlphaNum, isLetter, isSpace, isAscii)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as DT
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, many, manyTill, optional, sepBy, some, someTill, (<|>), notFollowedBy)
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
  validateImports path imports
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

validateImports :: FilePath -> [Import] -> Either String ()
validateImports path imports =
  case conflicts of
    (modName, aliases):_ ->
      Left (path ++ ": conflicting import aliases for module "
        ++ DT.unpack modName ++ " (" ++ renderAliases aliases ++ ")")
    [] -> Right ()
  where
    grouped = Map.fromListWith (++) [(impModule i, [impAlias i]) | i <- imports]
    conflicts =
      [ (modName, aliases)
      | (modName, aliases) <- Map.toList grouped
      , not (allSame aliases)
      ]

    allSame :: [Text] -> Bool
    allSame [] = True
    allSame (x:xs) = all (== x) xs

    renderAliases :: [Text] -> String
    renderAliases = intercalate ", " . map DT.unpack

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
fromExpr path (SList (SAtom "typeclass" : parts)) = fromTypeclass path parts
fromExpr path (SList (SAtom "instance" : parts)) = fromInstance path parts
fromExpr path (SList [SAtom "equal", ty, lhs, rhs]) = do
  ty' <- fromType path ty
  lhs' <- fromExpr path lhs
  rhs' <- fromExpr path rhs
  pure (EEqual ty' lhs' rhs')
fromExpr path (SList [SAtom "reflexive", ty, term]) = do
  ty' <- fromType path ty
  term' <- fromExpr path term
  pure (EReflexive ty' term')
fromExpr path (SList [SAtom "rewrite", family, proof, term]) = do
  family' <- fromExpr path family
  proof' <- fromExpr path proof
  term' <- fromExpr path term
  pure (ERewrite family' proof' term')
fromExpr path (SList [SAtom "pack", SList [SAtom v, dom], cod, witness, body]) = do
  dom' <- fromType path dom
  cod' <- fromType path cod
  witness' <- fromExpr path witness
  body' <- fromExpr path body
  pure (EPack v dom' cod' witness' body')
fromExpr path (SList [SAtom "unpack", packed, SList [SAtom x, SAtom y], body]) = do
  packed' <- fromExpr path packed
  body' <- fromExpr path body
  pure (EUnpack packed' x y body')
fromExpr path (SList [SAtom "lift", ty, from, to]) = do
  ty' <- fromType path ty
  fromLevel <- fromUniverse path "lift" from
  toLevel <- fromUniverse path "lift" to
  pure (ELift ty' fromLevel toLevel)
fromExpr path (SList [SAtom "up", ty, from, to, body]) = do
  ty' <- fromType path ty
  fromLevel <- fromUniverse path "up" from
  toLevel <- fromUniverse path "up" to
  body' <- fromExpr path body
  pure (EUp ty' fromLevel toLevel body')
fromExpr path (SList [SAtom "down", ty, from, to, body]) = do
  ty' <- fromType path ty
  fromLevel <- fromUniverse path "down" from
  toLevel <- fromUniverse path "down" to
  body' <- fromExpr path body
  pure (EDown ty' fromLevel toLevel body')
fromExpr path (SList (SAtom "data" : rest)) =
  fromData path rest
fromExpr path (SList [SAtom "compute", comp]) = ECompute <$> fromComp path comp
fromExpr path (SList [SAtom "let", SList [SAtom name, val], body]) = do
  val' <- fromExpr path val
  body' <- fromExpr path body
  pure (ELet name val' body')
fromExpr path (SList (SAtom "match" : scrut : rest)) = do
  (scrutExpr, scrutTy) <- fromOfType path scrut
  case rest of
    (SAtom name : retTy : cases) -> do
      retTy' <- fromType path retTy
      cases' <- mapM (fromMatchCase path) cases
      pure (EMatch scrutExpr scrutTy name retTy' cases')
    _ ->
      Left (path ++ ": match expects (of-type <expr> <Type>) <name> <Type> <cases...>, found "
        ++ renderHead (SList rest))
fromExpr path (SList [SAtom "of-type", e, ty]) = do
  e' <- fromExpr path e
  ty' <- fromType path ty
  pure (EAnnot e' ty')
fromExpr path (SList (SAtom "list" : elems)) = do
  elems' <- mapM (fromExpr path) elems
  pure (EListLiteral elems')
fromExpr path (SList (f:args)) = do
  f' <- fromExpr path f
  args' <- mapM (fromExpr path) args
  pure $ mkApp f' args'

fromFunction :: FilePath -> [SExpr] -> Either String Expr
fromFunction path parts =
  case reverse parts of
    (bodyForm : retForm : restRev) -> do
      let (constraints, paramFormsRev) =
            case restRev of
              (reqForm:paramsRev) | isRequiresForm reqForm ->
                case fromRequires path reqForm of
                  Left err -> (Left err, [])
                  Right cs -> (Right cs, paramsRev)
              _ -> (Right [], restRev)
      constraints' <- constraints
      body <- fromFunctionBody path bodyForm
      retTy <- fromType path retForm
      params <- mapM (fromParam path) (reverse paramFormsRev)
      pure (EFunction params constraints' retTy body)
    _ -> Left (path ++ ": function expects params, return type, and body")

fromFunctionBody :: FilePath -> SExpr -> Either String FunctionBody
fromFunctionBody path (SList [SAtom "value", body]) =
  FunctionValue <$> fromExpr path body
fromFunctionBody path (SList [SAtom "compute", body]) =
  FunctionCompute <$> fromComp path body
fromFunctionBody path other =
  Left (path ++ ": function body must be (value <expr>) or (compute <comp>), found " ++ renderHead other)

fromData :: FilePath -> [SExpr] -> Either String Expr
fromData path parts = do
  let (paramForms, rest) = span isParamForm parts
  params <- mapM (fromParam path) paramForms
  case rest of
    (universeExpr : caseForms) -> do
      level <- fromUniverse path "data" universeExpr
      cases <- mapM (fromDataCase path) caseForms
      pure (EData params (ETypeUniverse level) cases)
    _ ->
      Left (path ++ ": data expects (data <params...> <TypeN> <cases...>)")
  where
    isParamForm (SList [SAtom _, _]) = True
    isParamForm _ = False

fromDataCase :: FilePath -> SExpr -> Either String DataCase
fromDataCase path (SList [SAtom "case", SAtom name, ty]) = do
  ty' <- fromType path ty
  pure (DataCase name ty')
fromDataCase path other =
  Left (path ++ ": data case must be (case <Constructor> <Type>), found " ++ renderHead other)

fromParam :: FilePath -> SExpr -> Either String Param
fromParam path (SList [SAtom name, ty]) = do
  ty' <- fromType path ty
  pure (Param name ty')
fromParam path other =
  Left (path ++ ": function parameter must be (name Type), found " ++ renderHead other)

fromTypeclass :: FilePath -> [SExpr] -> Either String Expr
fromTypeclass path parts = case parts of
  (SList [SAtom param, SAtom "of-kind", kind] : methods) -> do
    kind' <- fromType path kind
    methods' <- mapM (fromClassMethod path) methods
    pure (ETypeClass param kind' methods')
  _ -> Left (path ++ ": typeclass expects (param of-kind Kind) and method declarations")

fromInstance :: FilePath -> [SExpr] -> Either String Expr
fromInstance path parts = case parts of
  (SAtom className : instTy : methods) -> do
    instTy' <- fromType path instTy
    methods' <- mapM (fromInstanceMethod path) methods
    pure (EInstance className instTy' methods')
  _ -> Left (path ++ ": instance expects class name, instance type, and methods")

fromClassMethod :: FilePath -> SExpr -> Either String (Text, Expr)
fromClassMethod path (SList [SAtom name, SAtom "of-type", ty]) =
  (name,) <$> fromType path ty
fromClassMethod path other =
  Left (path ++ ": typeclass method must be (name of-type Type), found " ++ renderHead other)

fromInstanceMethod :: FilePath -> SExpr -> Either String (Text, Expr)
fromInstanceMethod path (SList [SAtom name, body]) =
  (name,) <$> fromExpr path body
fromInstanceMethod path other =
  Left (path ++ ": instance method must be (name <expr>), found " ++ renderHead other)

isRequiresForm :: SExpr -> Bool
isRequiresForm (SList (SAtom "requires" : _)) = True
isRequiresForm _ = False

fromRequires :: FilePath -> SExpr -> Either String [Constraint]
fromRequires path (SList (SAtom "requires" : rest)) =
  mapM (fromConstraint path) rest
fromRequires path other =
  Left (path ++ ": requires expects a list of constraints, found " ++ renderHead other)

fromConstraint :: FilePath -> SExpr -> Either String Constraint
fromConstraint path (SList [SAtom className, ty]) =
  Constraint className <$> fromType path ty
fromConstraint path other =
  Left (path ++ ": constraint must be (Class Type), found " ++ renderHead other)

fromOfType :: FilePath -> SExpr -> Either String (Expr, Expr)
fromOfType path (SList [SAtom "of-type", e, ty]) = do
  e' <- fromExpr path e
  ty' <- fromType path ty
  pure (e', ty')
fromOfType path other =
  Left (path ++ ": match scrutinee must be (of-type <expr> <Type>), found " ++ renderHead other)

mkApp :: Expr -> [Expr] -> Expr
mkApp f [] = f
mkApp (EApp f existing) more = EApp f (existing ++ more)
mkApp f more                 = EApp f more

fromMatchCase :: FilePath -> SExpr -> Either String MatchCase
fromMatchCase path (SList (SAtom "case" : rest)) =
  case rest of
    (SAtom ctor : more) ->
      case reverse more of
        [] -> Left (path ++ ": case expects body")
        (bodyExpr : revBinders) -> do
          binders <- mapM (fromParam path) (reverse revBinders)
          body <- fromExpr path bodyExpr
          pure (MatchCase ctor binders body)
    _ -> Left (path ++ ": case expects (case <Constructor> <binders...> <body>)")
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
  SList [SAtom "lift", ty, from, to] -> do
    ty' <- fromType path ty
    fromLevel <- fromUniverse path "lift" from
    toLevel <- fromUniverse path "lift" to
    pure (ELift ty' fromLevel toLevel)
  SList [SAtom "for-all", SList [SAtom v, a], b] ->
    EForAll v <$> fromType path a <*> fromType path b
  SList [SAtom "there-exists", SList [SAtom v, a], b] ->
    EThereExists v <$> fromType path a <*> fromType path b
  SList [SAtom "equal", ty, lhs, rhs] -> do
    ty' <- fromType path ty
    lhs' <- fromExpr path lhs
    rhs' <- fromExpr path rhs
    pure (EEqual ty' lhs' rhs')
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

fromUniverse :: FilePath -> String -> SExpr -> Either String Int
fromUniverse path context se = case se of
  SAtom t
    | Just n <- parseUniverseAtom t -> Right n
    | otherwise ->
        Left (path ++ ": " ++ context ++ " expects TypeN, found " ++ DT.unpack t)
  _ -> Left (path ++ ": " ++ context ++ " expects TypeN, found " ++ renderHead se)

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
    Right m  -> do
      validateImports path (modImports m)
      Right m

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
  body <- pDataDef <|> pTypeclassDef <|> pInstanceDef <|> pValue
  pure (Definition tr name body)

pDataDef :: Parser Expr
pDataDef = M.try $ do
  keyword "data"
  params <- manyTill pParam (keyword "in")
  level <- pUniverseLevel
  let universe = ETypeUniverse level
  cases <- manyTill pDataCase (keyword "end")
  pure (EData params universe cases)

pDataCase :: Parser DataCase
pDataCase = M.try $ do
  keyword "case"
  name <- pIdentifier
  keyword "of-type"
  ty <- pType
  pure (DataCase name ty)

pTypeclassDef :: Parser Expr
pTypeclassDef = M.try $ do
  keyword "typeclass"
  param <- pIdentifier
  keyword "of-kind"
  kind <- pTypeParam
  keyword "where"
  methods <- manyTill pClassMethod (keyword "end")
  pure (ETypeClass param kind methods)

pClassMethod :: Parser (Text, Expr)
pClassMethod = M.try $ do
  name <- pIdentifier
  keyword "of-type"
  ty <- pTypeParam
  pure (name, ty)

pInstanceDef :: Parser Expr
pInstanceDef = M.try $ do
  keyword "instance"
  className <- pIdentifier
  instTy <- pTypeParam
  keyword "where"
  methods <- manyTill pInstanceMethod (keyword "end")
  pure (EInstance className instTy methods)

pInstanceMethod :: Parser (Text, Expr)
pInstanceMethod = M.try $ do
  name <- pIdentifier
  keyword "as"
  body <- pValue
  pure (name, body)

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
pValue =
  pLet
    <|> pMatch
    <|> pFunction
    <|> pCompute
    <|> pPack
    <|> pUnpack
    <|> pLiftValue
    <|> pUp
    <|> pDown
    <|> pForAll
    <|> pThereExists
    <|> pComputation
    <|> pRewrite
    <|> pReflexive
    <|> pEqual
    <|> pAnnot
    <|> pApp

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

pParam :: Parser Param
pParam = do
  name <- pIdentifier
  ty <- pTypeParam
  pure (Param name ty)

pFunction :: Parser Expr
pFunction = M.try $ do
  keyword "function"
  params <- manyTill pParam (M.lookAhead (keyword "returns" <|> keyword "requires"))
  constraints <- fromMaybe [] <$> M.optional pConstraints
  keyword "returns"
  retTy <- pType
  body <- (FunctionValue <$> (keyword "value" *> pValue))
      <|> (FunctionCompute <$> (keyword "compute" *> pComp))
  keyword "end"
  pure (EFunction params constraints retTy body)

pConstraints :: Parser [Constraint]
pConstraints = M.try $ do
  keyword "requires"
  first <- pConstraint
  rest <- many (keyword "and" *> pConstraint)
  pure (first:rest)

pConstraint :: Parser Constraint
pConstraint = M.try $ do
  className <- pIdentifier
  arg <- pTypeParam
  pure (Constraint className arg)

pCompute :: Parser Expr
pCompute = M.try $ do
  keyword "compute"
  comp <- pComp
  keyword "end"
  pure (ECompute comp)

pPack :: Parser Expr
pPack = M.try $ do
  keyword "pack"
  v <- pIdentifier
  keyword "as"
  dom <- pTypeParam
  keyword "in"
  cod <- pType
  keyword "with"
  witness <- pValueAtom
  body <- pValue
  keyword "end"
  pure (EPack v dom cod witness body)

pUnpack :: Parser Expr
pUnpack = M.try $ do
  keyword "unpack"
  packed <- pValue
  keyword "as"
  x <- pIdentifier
  y <- pIdentifier
  keyword "in"
  body <- pValue
  keyword "end"
  pure (EUnpack packed x y body)

pLiftValue :: Parser Expr
pLiftValue = pLiftExpr

pUp :: Parser Expr
pUp = M.try $ do
  keyword "up"
  ty <- pTypeParam
  keyword "from"
  fromLevel <- pUniverseLevel
  keyword "to"
  toLevel <- pUniverseLevel
  body <- pValue
  pure (EUp ty fromLevel toLevel body)

pDown :: Parser Expr
pDown = M.try $ do
  keyword "down"
  ty <- pTypeParam
  keyword "from"
  fromLevel <- pUniverseLevel
  keyword "to"
  toLevel <- pUniverseLevel
  body <- pValue
  pure (EDown ty fromLevel toLevel body)

pEqual :: Parser Expr
pEqual = M.try $ do
  keyword "equal"
  ty <- pTypeParam
  lhs <- pValueAtom
  rhs <- pValueAtom
  pure (EEqual ty lhs rhs)

pReflexive :: Parser Expr
pReflexive = M.try $ do
  keyword "reflexive"
  ty <- pTypeParam
  term <- pValueAtom
  pure (EReflexive ty term)

pRewrite :: Parser Expr
pRewrite = M.try $ do
  keyword "rewrite"
  family <- pValueAtom
  proof <- pValueAtom
  keyword "as"
  body <- pValue
  pure (ERewrite family proof body)

pAnnot :: Parser Expr
pAnnot = M.try $ do
  keyword "of-type"
  e <- pValueAtom
  ty <- pType
  pure (EAnnot e ty)

pMatch :: Parser Expr
pMatch = M.try $ do
  keyword "match"
  scrut <- pValue
  keyword "of-type"
  scrutTy <- pType
  keyword "as"
  scrutName <- pIdentifier
  keyword "returns"
  retTy <- pType
  cases <- manyTill pMatchCase (keyword "end")
  pure (EMatch scrut scrutTy scrutName retTy cases)

pMatchCase :: Parser MatchCase
pMatchCase = M.try $ do
  keyword "case"
  ctor <- pIdentifier
  binders <- optional (keyword "with" *> someTill pParam (keyword "as"))
  case binders of
    Nothing -> do
      keyword "as"
      body <- pValue
      pure (MatchCase ctor [] body)
    Just params -> do
      body <- pValue
      pure (MatchCase ctor params body)

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
  <|> pListLiteral
  <|> (EVar "recur" <$ keyword "recur")
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

pListLiteral :: Parser Expr
pListLiteral = M.try $ do
  _ <- mSymbol "["
  elems <- sepBy pValue (mSymbol ",")
  _ <- mSymbol "]"
  pure (EListLiteral elems)

-- Type parsing

pType :: Parser Expr
pType =
  pLetType <|> pForAll <|> pThereExists <|> pFunction <|> pComputation <|> pLiftType <|> pEqual <|> pMatch <|> pTypeApp

pTypeParam :: Parser Expr
pTypeParam = parens pType <|> pLetType <|> pTypeSimple

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

pLetType :: Parser Expr
pLetType = M.try $ do
  keyword "let"
  keyword "value"
  v <- pIdentifier
  keyword "be"
  val <- pValue
  keyword "in"
  body <- pType
  keyword "end"
  pure (ELet v val body)

pComputation :: Parser Expr
pComputation = M.try $ do
  keyword "computation"
  ECompType <$> pType

pLiftType :: Parser Expr
pLiftType = pLiftExpr

pLiftExpr :: Parser Expr
pLiftExpr = M.try $ do
  keyword "lift"
  ty <- pTypeParam
  keyword "from"
  fromLevel <- pUniverseLevel
  keyword "to"
  toLevel <- pUniverseLevel
  pure (ELift ty fromLevel toLevel)

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
pUniverse = ETypeUniverse <$> pUniverseLevel

pUniverseLevel :: Parser Int
pUniverseLevel = mLexeme . M.try $ do
  _ <- C.string "Type"
  L.decimal

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
  , "data", "case"
  , "for-all", "there-exists", "computation", "to", "lift", "up", "down", "pack", "unpack"
  , "equal", "reflexive", "rewrite"
  , "typeclass", "of-kind", "instance", "where", "requires", "and"
  , "recur"
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
  EListLiteral elems ->
    "(list" <> renderList elems <> ")"
  ETypeConst tc   -> T.typeConstName tc
  ETypeUniverse n -> "Type" <> DT.pack (show n)
  EForAll v dom cod ->
    "(for-all (" <> v <> " " <> T.typeToSExpr dom <> ") " <> T.typeToSExpr cod <> ")"
  EThereExists v dom cod ->
    "(there-exists (" <> v <> " " <> T.typeToSExpr dom <> ") " <> T.typeToSExpr cod <> ")"
  ECompType t -> "(computation " <> T.typeToSExpr t <> ")"
  EEqual ty lhs rhs ->
    "(equal " <> T.typeToSExpr ty <> " " <> renderExpr lhs <> " " <> renderExpr rhs <> ")"
  EReflexive ty term ->
    "(reflexive " <> T.typeToSExpr ty <> " " <> renderExpr term <> ")"
  ERewrite family proof body ->
    "(rewrite " <> renderExpr family <> " " <> renderExpr proof <> " " <> renderExpr body <> ")"
  EPack v dom cod witness body ->
    "(pack (" <> v <> " " <> T.typeToSExpr dom <> ") " <> T.typeToSExpr cod
      <> " " <> renderExpr witness <> " " <> renderExpr body <> ")"
  EUnpack packed x y body ->
    "(unpack " <> renderExpr packed <> " (" <> x <> " " <> y <> ") "
      <> renderExpr body <> ")"
  ELift ty fromLevel toLevel ->
    "(lift " <> T.typeToSExpr ty <> " " <> renderUniverseAtom fromLevel
      <> " " <> renderUniverseAtom toLevel <> ")"
  EUp ty fromLevel toLevel body ->
    "(up " <> T.typeToSExpr ty <> " " <> renderUniverseAtom fromLevel
      <> " " <> renderUniverseAtom toLevel <> " " <> renderExpr body <> ")"
  EDown ty fromLevel toLevel body ->
    "(down " <> T.typeToSExpr ty <> " " <> renderUniverseAtom fromLevel
      <> " " <> renderUniverseAtom toLevel <> " " <> renderExpr body <> ")"
  EApp f args     -> "(" <> DT.unwords (renderExpr f : map renderExpr args) <> ")"
  EFunction params constraints retTy body ->
    "(function " <> DT.unwords (map renderParam params)
      <> (if null params then "" else " ")
      <> renderConstraintsS constraints
      <> T.typeToSExpr retTy <> " " <> renderFunctionBody body <> ")"
  ELet name val body ->
    "(let (" <> name <> " " <> renderExpr val <> ") " <> renderExpr body <> ")"
  ECompute comp -> "(compute " <> renderComp comp <> ")"
  EMatch scrut scrutTy scrutName retTy cases ->
    "(match (of-type " <> renderExpr scrut <> " " <> T.typeToSExpr scrutTy <> ") "
      <> scrutName <> " " <> T.typeToSExpr retTy <> " "
      <> DT.unwords (map renderMatchCase cases) <> ")"
  EData params universe cases ->
    "(data " <> DT.unwords (map renderParam params ++ [renderExpr universe] ++ map renderDataCase cases) <> ")"
  EAnnot e ty -> "(of-type " <> renderExpr e <> " " <> T.typeToSExpr ty <> ")"
  ETyped e _ -> renderExpr e
  EDict className impls ->
    "(dict " <> className <> " " <>
    DT.intercalate " " [n <> " " <> renderExpr e | (n, e) <- impls] <> ")"
  EDictAccess d method -> "(dict-access " <> renderExpr d <> " " <> method <> ")"
  ETypeClass param kind methods ->
    "(typeclass (" <> param <> " of-kind " <> T.typeToSExpr kind <> ") "
      <> DT.unwords (map renderClassMethod methods) <> ")"
  EInstance className instTy methods ->
    "(instance " <> className <> " " <> T.typeToSExpr instTy <> " "
      <> DT.unwords (map renderInstanceMethod methods) <> ")"
  where
    renderList elems =
      case elems of
        [] -> ""
        _ -> " " <> DT.unwords (map renderExpr elems)

exprToSExprText :: Expr -> DT.Text
exprToSExprText = renderExpr

renderParam :: Param -> DT.Text
renderParam (Param name ty) = "(" <> name <> " " <> T.typeToSExpr ty <> ")"

renderConstraintsS :: [Constraint] -> DT.Text
renderConstraintsS [] = ""
renderConstraintsS cs =
  " (requires " <> DT.unwords (map renderConstraintS cs) <> ") "

renderConstraintS :: Constraint -> DT.Text
renderConstraintS (Constraint className ty) =
  "(" <> className <> " " <> T.typeToSExpr ty <> ")"

renderClassMethod :: (Text, Expr) -> DT.Text
renderClassMethod (name, ty) =
  "(" <> name <> " of-type " <> T.typeToSExpr ty <> ")"

renderInstanceMethod :: (Text, Expr) -> DT.Text
renderInstanceMethod (name, body) =
  "(" <> name <> " " <> renderExpr body <> ")"

renderFunctionBody :: FunctionBody -> DT.Text
renderFunctionBody body = case body of
  FunctionValue e -> "(value " <> renderExpr e <> ")"
  FunctionCompute c -> "(compute " <> renderComp c <> ")"

renderMatchCase :: MatchCase -> DT.Text
renderMatchCase (MatchCase ctor binders body) =
  "(case " <> ctor <> " " <> DT.unwords (map renderParam binders ++ [renderExpr body]) <> ")"

renderDataCase :: DataCase -> DT.Text
renderDataCase (DataCase ctor ty) =
  "(case " <> ctor <> " " <> T.typeToSExpr ty <> ")"

renderComp :: Comp -> DT.Text
renderComp comp = case comp of
  CReturn e      -> "(return " <> renderExpr e <> ")"
  CPerform e     -> "(perform " <> renderExpr e <> ")"
  CBind v c1 c2  -> "(bind (" <> v <> " " <> renderComp c1 <> ") " <> renderComp c2 <> ")"

renderLit :: Literal -> DT.Text
renderLit lit = case lit of
  LNatural n -> DT.pack (show n)
  LString s  -> DT.pack (show (DT.unpack s))
  LBoolean b -> if b then "true" else "false"
  LUnit      -> "tt"

renderUniverseAtom :: Int -> DT.Text
renderUniverseAtom n = "Type" <> DT.pack (show n)

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
  EListLiteral elems ->
    case elems of
      [] -> "[]"
      _ -> "[" <> DT.intercalate ", " (map renderMExpr elems) <> "]"
  ETypeConst tc -> T.typeConstName tc
  ETypeUniverse n -> "Type" <> DT.pack (show n)
  EForAll v dom cod ->
    "for-all " <> v <> " as " <> T.prettyTypeAtom dom <> " to " <> T.prettyType cod
  EThereExists v dom cod ->
    "there-exists " <> v <> " as " <> T.prettyTypeAtom dom <> " in " <> T.prettyType cod
  ECompType t -> "computation " <> T.prettyTypeAtom t
  EEqual ty lhs rhs ->
    "equal " <> T.prettyTypeAtom ty <> " " <> renderMExpr lhs <> " " <> renderMExpr rhs
  EReflexive ty term ->
    "reflexive " <> T.prettyTypeAtom ty <> " " <> renderMExpr term
  ERewrite family proof body ->
    "rewrite " <> renderMExpr family <> " " <> renderMExpr proof <> " as " <> renderMExpr body
  EPack v dom cod witness body ->
    "pack " <> v <> " as " <> T.prettyTypeAtom dom <> " in " <> T.prettyType cod
      <> " with " <> renderMExpr witness <> " " <> renderMExpr body <> " end"
  EUnpack packed x y body ->
    "unpack " <> renderMExpr packed <> " as " <> x <> " " <> y <> " in "
      <> renderMExpr body <> " end"
  ELift ty fromLevel toLevel ->
    "lift " <> T.prettyTypeAtom ty <> " from " <> renderUniverseAtom fromLevel
      <> " to " <> renderUniverseAtom toLevel
  EUp ty fromLevel toLevel body ->
    "up " <> T.prettyTypeAtom ty <> " from " <> renderUniverseAtom fromLevel
      <> " to " <> renderUniverseAtom toLevel <> " " <> renderMExpr body
  EDown ty fromLevel toLevel body ->
    "down " <> T.prettyTypeAtom ty <> " from " <> renderUniverseAtom fromLevel
      <> " to " <> renderUniverseAtom toLevel <> " " <> renderMExpr body
  EApp f args -> "(" <> DT.unwords (renderMExpr f : map renderMExpr args) <> ")"
  EFunction params constraints retTy body ->
    "function " <> renderParams params <> renderConstraintsM constraints
      <> " returns " <> T.prettyType retTy <> " " <> renderFunctionBodyM body <> " end"
  ELet name val body ->
    "let value " <> name <> " be " <> renderMExpr val <> " in " <> renderMExpr body <> " end"
  ECompute comp -> "compute " <> renderMComp comp <> " end"
  EMatch scrut scrutTy scrutName retTy cases ->
    "match " <> renderMExpr scrut <> " of-type " <> T.prettyType scrutTy
      <> " as " <> scrutName <> " returns " <> T.prettyType retTy <> " "
      <> DT.unwords (map renderMatchCaseM cases) <> " end"
  EData params universe cases ->
    "data " <> renderParams params <> (if null params then "" else " ")
      <> "in " <> renderMExpr universe <> " " <> DT.unwords (map renderDataCaseM cases) <> " end"
  EAnnot e ty -> "of-type " <> renderMExpr e <> " " <> T.prettyType ty
  ETyped e _ -> renderMExpr e
  EDict className impls ->
    "(dict " <> className <> " " <>
    DT.intercalate " " [n <> " " <> renderMExpr e | (n, e) <- impls] <> ")"
  EDictAccess d method -> "(dict-access " <> renderMExpr d <> " " <> method <> ")"
  ETypeClass param kind methods ->
    "typeclass " <> param <> " of-kind " <> T.prettyTypeAtom kind
      <> " where " <> DT.unwords (map renderClassMethodM methods) <> " end"
  EInstance className instTy methods ->
    "instance " <> className <> " " <> T.prettyTypeAtom instTy <> " where "
      <> DT.unwords (map renderInstanceMethodM methods) <> " end"

exprToMExprText :: Expr -> DT.Text
exprToMExprText = renderMExpr

renderParams :: [Param] -> DT.Text
renderParams params = DT.unwords (map renderParamM params)

renderParamM :: Param -> DT.Text
renderParamM (Param name ty) = name <> " " <> T.prettyTypeAtom ty

renderFunctionBodyM :: FunctionBody -> DT.Text
renderFunctionBodyM body = case body of
  FunctionValue e -> "value " <> renderMExpr e
  FunctionCompute c -> "compute " <> renderMComp c

renderConstraintsM :: [Constraint] -> DT.Text
renderConstraintsM [] = ""
renderConstraintsM cs =
  " requires " <> DT.intercalate " and " (map renderConstraintM cs)

renderConstraintM :: Constraint -> DT.Text
renderConstraintM (Constraint className ty) =
  className <> " " <> T.prettyTypeAtom ty

renderClassMethodM :: (Text, Expr) -> DT.Text
renderClassMethodM (name, ty) =
  name <> " of-type " <> T.prettyType ty

renderInstanceMethodM :: (Text, Expr) -> DT.Text
renderInstanceMethodM (name, body) =
  name <> " as " <> renderMExpr body

renderMatchCaseM :: MatchCase -> DT.Text
renderMatchCaseM (MatchCase ctor binders body) =
  case binders of
    [] -> "case " <> ctor <> " as " <> renderMExpr body
    _ ->
      "case " <> ctor <> " with " <> renderParams binders <> " as " <> renderMExpr body

renderDataCaseM :: DataCase -> DT.Text
renderDataCaseM (DataCase ctor ty) =
  "case " <> ctor <> " of-type " <> T.prettyType ty

renderMComp :: Comp -> DT.Text
renderMComp comp = case comp of
  CReturn e  -> "return " <> renderMExpr e
  CPerform e -> "perform " <> renderMExpr e
  CBind v c1 c2 ->
    "bind " <> v <> " from " <> renderMComp c1 <> " then " <> renderMComp c2 <> " end"
