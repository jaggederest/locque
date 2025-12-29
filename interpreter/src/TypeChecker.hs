{-# LANGUAGE OverloadedStrings #-}
module TypeChecker
  ( typeCheckModule
  , typeCheckModuleWithImports
  , annotateModule
  , TypeError(..)
  , TypeEnv
  ) where

import Control.Monad (foldM, when)
import Control.Monad.State
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath ((</>), (<.>), takeExtension)
import System.IO.Error (catchIOError, isDoesNotExistError)
import AST
import Type (prettyType)
import Parser (parseModuleFile, parseMExprFile)
import SourceLoc
import ErrorMsg
import Utils (modNameToPath, qualifyName)

data MatchKind
  = CaseEmpty
  | CaseCons
  | CaseFalse
  | CaseTrue
  | CasePair
  deriving (Eq, Ord)

data ClassInfo = ClassInfo
  { className    :: Text
  , classParam   :: Text
  , classMethods :: [(Text, Expr)]
  } deriving (Show, Eq)

data InstanceInfo = InstanceInfo
  { instName    :: Text
  , instClass   :: Text
  , instType    :: Expr
  , instMethods :: Map.Map Text Expr
  } deriving (Show, Eq)

-- | Type errors with context
data TypeError
  = VarNotInScope SrcLoc Text TypeEnv
  | TypeMismatch SrcLoc Expr Expr
  | CannotApply SrcLoc Expr Expr
  | NotAFunction SrcLoc Expr
  | NotAComputation SrcLoc Expr
  | ExpectedType SrcLoc Expr
  | InvalidLift SrcLoc Int Int
  | ExpectedThereExists SrcLoc Expr
  | ExpectedEquality SrcLoc Expr
  | MatchCaseError SrcLoc Text
  | TypeclassError SrcLoc Text
  | RecursionError SrcLoc Text

instance Show TypeError where
  show (VarNotInScope loc var env) =
    let candidates = Map.keys env
        suggestions = case findFuzzyMatches var candidates of
                        [] -> []
                        xs -> [DidYouMean (T.intercalate ", " xs)]
        msg = ErrorMsg loc ("Variable not in scope: " <> var) Nothing suggestions Nothing
    in T.unpack (formatError msg)

  show (TypeMismatch loc expected actual) =
    let mainMsg = "Type mismatch"
        note = Just $ "Expected: " <> prettyType expected <>
                      "\n  Actual:   " <> prettyType actual
        msg = ErrorMsg loc mainMsg Nothing [] note
    in T.unpack (formatError msg)

  show (CannotApply loc fType argType) =
    let mainMsg = "Cannot apply function"
        note = Just $ "Function type: " <> prettyType fType <>
                      "\n  Argument type: " <> prettyType argType
        msg = ErrorMsg loc mainMsg Nothing [] note
    in T.unpack (formatError msg)

  show (NotAFunction loc ty) =
    let msg = ErrorMsg loc ("Expected function, got: " <> prettyType ty) Nothing [] Nothing
    in T.unpack (formatError msg)

  show (NotAComputation loc ty) =
    let msg = ErrorMsg loc ("Expected computation, got: " <> prettyType ty) Nothing [] Nothing
    in T.unpack (formatError msg)

  show (ExpectedType loc ty) =
    let msg = ErrorMsg loc ("Expected a type, got: " <> prettyType ty) Nothing [] Nothing
    in T.unpack (formatError msg)

  show (InvalidLift loc fromLevel toLevel) =
    let msg = ErrorMsg loc
          ("Invalid lift: from Type" <> T.pack (show fromLevel) <>
           " to Type" <> T.pack (show toLevel) <> " (from must be <= to)")
          Nothing [] Nothing
    in T.unpack (formatError msg)

  show (ExpectedThereExists loc ty) =
    let msg = ErrorMsg loc ("Expected there-exists type, got: " <> prettyType ty) Nothing [] Nothing
    in T.unpack (formatError msg)

  show (ExpectedEquality loc ty) =
    let msg = ErrorMsg loc ("Expected equality type, got: " <> prettyType ty) Nothing [] Nothing
    in T.unpack (formatError msg)

  show (MatchCaseError loc msgText) =
    let msg = ErrorMsg loc ("Match error: " <> msgText) Nothing [] Nothing
    in T.unpack (formatError msg)

  show (TypeclassError loc msgText) =
    let msg = ErrorMsg loc ("Typeclass error: " <> msgText) Nothing [] Nothing
    in T.unpack (formatError msg)

  show (RecursionError loc msgText) =
    let msg = ErrorMsg loc ("Recursion error: " <> msgText) Nothing [] Nothing
    in T.unpack (formatError msg)

-- | Eq instance compares by error type (ignoring location for equality)
instance Eq TypeError where
  VarNotInScope _ a _ == VarNotInScope _ b _ = a == b
  TypeMismatch _ a1 a2 == TypeMismatch _ b1 b2 = a1 == b1 && a2 == b2
  CannotApply _ a1 a2 == CannotApply _ b1 b2 = a1 == b1 && a2 == b2
  NotAFunction _ a == NotAFunction _ b = a == b
  NotAComputation _ a == NotAComputation _ b = a == b
  ExpectedType _ a == ExpectedType _ b = a == b
  InvalidLift _ a b == InvalidLift _ c d = a == c && b == d
  ExpectedThereExists _ a == ExpectedThereExists _ b = a == b
  ExpectedEquality _ a == ExpectedEquality _ b = a == b
  MatchCaseError _ a == MatchCaseError _ b = a == b
  TypeclassError _ a == TypeclassError _ b = a == b
  RecursionError _ a == RecursionError _ b = a == b
  _ == _ = False

type TypeEnv = Map.Map Text Expr
type DefEnv = Map.Map Text (Transparency, Expr)

data TCEnv = TCEnv
  { tcTypes  :: TypeEnv
  , tcDefs   :: DefEnv
  , tcLocals :: Set.Set Text
  , tcClasses :: Map.Map Text ClassInfo
  , tcInstances :: Map.Map Text [InstanceInfo]
  }

type TypeCheckM a = StateT Int (Either TypeError) a

runTypeCheck :: TypeCheckM a -> Either TypeError a
runTypeCheck tc = evalStateT tc 0

freshNameAvoid :: Text -> Set.Set Text -> TypeCheckM Text
freshNameAvoid base avoid = do
  n <- get
  put (n + 1)
  let candidate = base <> "_" <> T.pack (show n)
  if candidate `Set.member` avoid
    then freshNameAvoid base avoid
    else pure candidate

--------------------------------------------------------------------------------
-- Helpers

tNat :: Expr
tNat = ETypeConst TCNatural

tString :: Expr
tString = ETypeConst TCString

tBool :: Expr
tBool = ETypeConst TCBoolean

tUnit :: Expr
tUnit = ETypeConst TCUnit

tType :: Int -> Expr
tType = ETypeUniverse

tList :: Expr -> Expr
tList a = EApp (ETypeConst TCList) [a]

tPair :: Expr -> Expr -> Expr
tPair a b = EApp (ETypeConst TCPair) [a, b]

tComp :: Expr -> Expr
tComp = ECompType

tFun :: Expr -> Expr -> Expr
tFun dom cod = EForAll "_" dom cod

tEqual :: Expr -> Expr -> Expr -> Expr
tEqual = EEqual

tFalse :: Expr
tFalse = EEqual tBool (ELit (LBoolean True)) (ELit (LBoolean False))

tNot :: Expr -> Expr
tNot p = EForAll "x" p tFalse

tDecidable :: Expr -> Expr
tDecidable p =
  EThereExists "b" tBool
    (EMatch (EVar "b") tBool "ignored" (tType 0)
      [ MatchFalse (tNot p)
      , MatchTrue p
      ])

tDecider :: Expr -> Expr
tDecider ty =
  EForAll "x" ty
    (EForAll "y" ty
      (tDecidable (tEqual ty (EVar "x") (EVar "y"))))

mkPi :: [Param] -> Expr -> Expr
mkPi params ret = foldr (\(Param name ty) acc -> EForAll name ty acc) ret params

classType :: Text -> Expr
classType param = EForAll param (tType 0) (tType 0)

emptyEnv :: TCEnv
emptyEnv = TCEnv buildPrimitiveEnv Map.empty Set.empty Map.empty Map.empty

extendLocal :: TCEnv -> Text -> Expr -> TCEnv
extendLocal env name ty =
  env { tcTypes = Map.insert name ty (tcTypes env)
      , tcLocals = Set.insert name (tcLocals env)
      }

extendGlobal :: TCEnv -> Text -> Expr -> Transparency -> Expr -> TCEnv
extendGlobal env name ty tr body =
  env { tcTypes = Map.insert name ty (tcTypes env)
      , tcDefs = Map.insert name (tr, body) (tcDefs env)
      }

extendClass :: TCEnv -> Text -> ClassInfo -> Expr -> TCEnv
extendClass env name info classTy =
  env { tcTypes = Map.insert name classTy (tcTypes env)
      , tcClasses = Map.insert name info (tcClasses env)
      }

extendInstance :: TCEnv -> Text -> InstanceInfo -> Expr -> TCEnv
extendInstance env name info instTy =
  env { tcTypes = Map.insert name instTy (tcTypes env)
      , tcInstances = Map.insertWith (++) (instClass info) [info] (tcInstances env)
      }

lookupVar :: SrcLoc -> TCEnv -> Text -> TypeCheckM Expr
lookupVar loc env v =
  case Map.lookup v (tcTypes env) of
    Just ty -> pure ty
    Nothing -> lift (Left (VarNotInScope loc v (tcTypes env)))

lookupClass :: SrcLoc -> TCEnv -> Text -> TypeCheckM ClassInfo
lookupClass loc env name =
  case Map.lookup name (tcClasses env) of
    Just info -> pure info
    Nothing -> lift (Left (TypeclassError loc ("Unknown typeclass: " <> name)))

constraintMethods :: TCEnv -> [Constraint] -> TypeCheckM [(Text, Expr)]
constraintMethods env constraints = foldM addConstraint [] constraints
  where
    addConstraint acc (Constraint clsName ty) = do
      classInfo <- lookupClass noLoc env clsName
      level <- inferUniverse env ty
      when (level /= 0) $
        lift (Left (TypeclassError noLoc "Constraint type must be in Type0"))
      let param = classParam classInfo
      methods' <- mapM (specializeMethod param ty acc) (classMethods classInfo)
      pure (acc ++ methods')

    specializeMethod param ty acc (methodName, methodTy) = do
      when (methodName `elem` map fst acc) $
        lift (Left (TypeclassError noLoc ("Ambiguous method name: " <> methodName)))
      methodTy' <- subst param ty methodTy
      pure (methodName, methodTy')

ensureUniqueMethodNames :: [Text] -> TypeCheckM ()
ensureUniqueMethodNames names =
  case [n | (n, count) <- Map.toList counts, count > (1 :: Int)] of
    (dup:_) -> lift (Left (TypeclassError noLoc ("Duplicate method name: " <> dup)))
    [] -> pure ()
  where
    counts = Map.fromListWith (+) [(n, 1 :: Int) | n <- names]

--------------------------------------------------------------------------------
-- Free variables

freeVars :: Expr -> Set.Set Text
freeVars = freeVarsWithBound Set.empty

freeVarsWithBound :: Set.Set Text -> Expr -> Set.Set Text
freeVarsWithBound bound expr = case expr of
  EVar v -> if v `Set.member` bound then Set.empty else Set.singleton v
  ELit _ -> Set.empty
  ETypeConst _ -> Set.empty
  ETypeUniverse _ -> Set.empty
  EForAll v dom cod ->
    freeVarsWithBound bound dom `Set.union` freeVarsWithBound (Set.insert v bound) cod
  EThereExists v dom cod ->
    freeVarsWithBound bound dom `Set.union` freeVarsWithBound (Set.insert v bound) cod
  ECompType t -> freeVarsWithBound bound t
  EEqual ty lhs rhs ->
    freeVarsWithBound bound ty
      `Set.union` freeVarsWithBound bound lhs
      `Set.union` freeVarsWithBound bound rhs
  EReflexive ty term ->
    freeVarsWithBound bound ty `Set.union` freeVarsWithBound bound term
  ERewrite family proof body ->
    freeVarsWithBound bound family
      `Set.union` freeVarsWithBound bound proof
      `Set.union` freeVarsWithBound bound body
  EPack v dom cod witness body ->
    freeVarsWithBound bound dom
      `Set.union` freeVarsWithBound (Set.insert v bound) cod
      `Set.union` freeVarsWithBound bound witness
      `Set.union` freeVarsWithBound bound body
  EUnpack packed x y body ->
    freeVarsWithBound bound packed
      `Set.union` freeVarsWithBound (Set.insert x (Set.insert y bound)) body
  ELift ty _ _ -> freeVarsWithBound bound ty
  EUp ty _ _ body ->
    freeVarsWithBound bound ty `Set.union` freeVarsWithBound bound body
  EDown ty _ _ body ->
    freeVarsWithBound bound ty `Set.union` freeVarsWithBound bound body
  EApp f args ->
    Set.unions (freeVarsWithBound bound f : map (freeVarsWithBound bound) args)
  EFunction params constraints ret body ->
    let (bound', paramsFree) = foldl step (bound, Set.empty) params
        step (b, acc) (Param name ty) =
          let acc' = acc `Set.union` freeVarsWithBound b ty
          in (Set.insert name b, acc')
        constraintsFree = Set.unions [freeVarsWithBound bound cTy | Constraint _ cTy <- constraints]
        retFree = freeVarsWithBound bound' ret
        bodyFree = freeVarsBody bound' body
    in paramsFree `Set.union` constraintsFree `Set.union` retFree `Set.union` bodyFree
  ELet name val body ->
    freeVarsWithBound bound val `Set.union` freeVarsWithBound (Set.insert name bound) body
  ECompute comp -> freeVarsComp bound comp
  EMatch scrut scrutTy scrutName retTy cases ->
    let scrutFree = freeVarsWithBound bound scrut
        tyFree = freeVarsWithBound bound scrutTy
        retFree = freeVarsWithBound (Set.insert scrutName bound) retTy
        casesFree = Set.unions (map (freeVarsCase (Set.insert scrutName bound)) cases)
    in scrutFree `Set.union` tyFree `Set.union` retFree `Set.union` casesFree
  EAnnot e ty -> freeVarsWithBound bound e `Set.union` freeVarsWithBound bound ty
  ETyped e ty -> freeVarsWithBound bound e `Set.union` freeVarsWithBound bound ty
  EDict _ impls -> Set.unions [freeVarsWithBound bound e | (_, e) <- impls]
  EDictAccess e _ -> freeVarsWithBound bound e
  ETypeClass param methods ->
    Set.unions [freeVarsWithBound (Set.insert param bound) ty | (_, ty) <- methods]
  EInstance _ instTy methods ->
    freeVarsWithBound bound instTy `Set.union`
      Set.unions [freeVarsWithBound bound e | (_, e) <- methods]

freeVarsBody :: Set.Set Text -> FunctionBody -> Set.Set Text
freeVarsBody bound body = case body of
  FunctionValue e -> freeVarsWithBound bound e
  FunctionCompute c -> freeVarsComp bound c

freeVarsComp :: Set.Set Text -> Comp -> Set.Set Text
freeVarsComp bound comp = case comp of
  CReturn e -> freeVarsWithBound bound e
  CPerform e -> freeVarsWithBound bound e
  CBind v c1 c2 ->
    freeVarsComp bound c1 `Set.union` freeVarsComp (Set.insert v bound) c2
  CSeq c1 c2 ->
    freeVarsComp bound c1 `Set.union` freeVarsComp bound c2

freeVarsCase :: Set.Set Text -> MatchCase -> Set.Set Text
freeVarsCase bound caseExpr = case caseExpr of
  MatchEmpty body -> freeVarsWithBound bound body
  MatchFalse body -> freeVarsWithBound bound body
  MatchTrue body -> freeVarsWithBound bound body
  MatchCons h hTy t tTy body ->
    freeVarsWithBound bound hTy
      `Set.union` freeVarsWithBound bound tTy
      `Set.union` freeVarsWithBound (Set.insert h (Set.insert t bound)) body
  MatchPair a aTy b bTy body ->
    freeVarsWithBound bound aTy
      `Set.union` freeVarsWithBound bound bTy
      `Set.union` freeVarsWithBound (Set.insert a (Set.insert b bound)) body

--------------------------------------------------------------------------------
-- Substitution (capture-avoiding)

subst :: Text -> Expr -> Expr -> TypeCheckM Expr
subst name replacement expr = go Set.empty expr
  where
    replacementFree = freeVars replacement

    go bound e = case e of
      EVar v
        | v == name && v `Set.notMember` bound -> pure replacement
        | otherwise -> pure e
      ELit _ -> pure e
      ETypeConst _ -> pure e
      ETypeUniverse _ -> pure e
      EForAll v dom cod -> do
        dom' <- go bound dom
        if v == name
          then pure (EForAll v dom' cod)
          else do
            (v', cod') <- refreshBinder bound v cod
            cod'' <- go (Set.insert v' bound) cod'
            pure (EForAll v' dom' cod'')
      EThereExists v dom cod -> do
        dom' <- go bound dom
        if v == name
          then pure (EThereExists v dom' cod)
          else do
            (v', cod') <- refreshBinder bound v cod
            cod'' <- go (Set.insert v' bound) cod'
            pure (EThereExists v' dom' cod'')
      ECompType t -> ECompType <$> go bound t
      EEqual ty lhs rhs ->
        EEqual <$> go bound ty <*> go bound lhs <*> go bound rhs
      EReflexive ty term ->
        EReflexive <$> go bound ty <*> go bound term
      ERewrite family proof body ->
        ERewrite <$> go bound family <*> go bound proof <*> go bound body
      EPack v dom cod witness body -> do
        dom' <- go bound dom
        if v == name
          then do
            witness' <- go bound witness
            body' <- go bound body
            pure (EPack v dom' cod witness' body')
          else do
            (v', cod') <- refreshBinder bound v cod
            cod'' <- go (Set.insert v' bound) cod'
            witness' <- go bound witness
            body' <- go bound body
            pure (EPack v' dom' cod'' witness' body')
      EUnpack packed x y body -> do
        packed' <- go bound packed
        if x == name || y == name
          then pure (EUnpack packed' x y body)
          else do
            (x', body1) <- refreshBinder bound x body
            let bound' = Set.insert x' bound
            (y', body2) <- refreshBinder bound' y body1
            body' <- go (Set.insert y' bound') body2
            pure (EUnpack packed' x' y' body')
      ELift ty fromLevel toLevel ->
        ELift <$> go bound ty <*> pure fromLevel <*> pure toLevel
      EUp ty fromLevel toLevel body ->
        EUp <$> go bound ty <*> pure fromLevel <*> pure toLevel <*> go bound body
      EDown ty fromLevel toLevel body ->
        EDown <$> go bound ty <*> pure fromLevel <*> pure toLevel <*> go bound body
      EApp f args -> EApp <$> go bound f <*> mapM (go bound) args
      EFunction params constraints ret body -> do
        (params', constraints', ret', body') <- goParams bound params constraints ret body
        pure (EFunction params' constraints' ret' body')
      ELet v val body -> do
        val' <- go bound val
        if v == name
          then pure (ELet v val' body)
          else do
            (v', body') <- refreshBinder bound v body
            body'' <- go (Set.insert v' bound) body'
            pure (ELet v' val' body'')
      ECompute comp -> ECompute <$> goComp bound comp
      EMatch scrut scrutTy scrutName retTy cases -> do
        scrut' <- go bound scrut
        scrutTy' <- go bound scrutTy
        if scrutName == name
          then do
            cases' <- mapM (goCase bound) cases
            pure (EMatch scrut' scrutTy' scrutName retTy cases')
          else do
            (scrutName', retTy') <- refreshBinder bound scrutName retTy
            cases' <- mapM (goCase (Set.insert scrutName' bound)) cases
            pure (EMatch scrut' scrutTy' scrutName' retTy' cases')
      EAnnot e1 ty -> EAnnot <$> go bound e1 <*> go bound ty
      ETyped e1 ty -> ETyped <$> go bound e1 <*> go bound ty
      EDict cls impls -> do
        impls' <- mapM (\(n, e1) -> (n,) <$> go bound e1) impls
        pure (EDict cls impls')
      EDictAccess e1 method -> EDictAccess <$> go bound e1 <*> pure method
      ETypeClass param methods -> do
        if param == name
          then pure (ETypeClass param methods)
          else do
            (param', methods') <- refreshClassBinder bound param methods
            methods'' <- mapM (\(n, ty) -> (n,) <$> go (Set.insert param' bound) ty) methods'
            pure (ETypeClass param' methods'')
      EInstance cls instTy methods -> do
        instTy' <- go bound instTy
        methods' <- mapM (\(n, e1) -> (n,) <$> go bound e1) methods
        pure (EInstance cls instTy' methods')

    refreshBinder bound v body =
      if v `Set.member` replacementFree
        then do
          let avoid = Set.unions [replacementFree, freeVars body, bound]
          v' <- freshNameAvoid v avoid
          body' <- subst v (EVar v') body
          pure (v', body')
        else pure (v, body)

    refreshClassBinder bound param methods =
      if param `Set.member` replacementFree
        then do
          let avoid = Set.unions
                [ replacementFree
                , Set.unions [freeVarsWithBound bound ty | (_, ty) <- methods]
                , bound
                ]
          param' <- freshNameAvoid param avoid
          methods' <- mapM (\(n, ty) -> (n,) <$> subst param (EVar param') ty) methods
          pure (param', methods')
        else pure (param, methods)

    goParams bound params constraints ret body = case params of
      [] -> do
        constraints' <- mapM (goConstraint bound) constraints
        ret' <- go bound ret
        body' <- goBody bound body
        pure ([], constraints', ret', body')
      (Param v ty) : rest -> do
        ty' <- go bound ty
        if v == name
          then pure (Param v ty' : rest, constraints, ret, body)
          else do
            (v', rest', constraints', ret', body') <- if v `Set.member` replacementFree
              then do
                let avoid = Set.unions
                      [ replacementFree
                      , freeVars ret
                      , freeVarsBody Set.empty body
                      , freeVarsParams bound rest
                      , freeVarsConstraints bound constraints
                      , bound
                      ]
                v' <- freshNameAvoid v avoid
                rest' <- mapM (renameParam v v') rest
                constraints' <- mapM (renameConstraint v v') constraints
                ret' <- subst v (EVar v') ret
                body' <- renameBody v v' body
                pure (v', rest', constraints', ret', body')
              else pure (v, rest, constraints, ret, body)
            (restFinal, constraintsFinal, retFinal, bodyFinal) <-
              goParams (Set.insert v' bound) rest' constraints' ret' body'
            pure (Param v' ty' : restFinal, constraintsFinal, retFinal, bodyFinal)

    freeVarsParams bound params =
      Set.unions [freeVarsWithBound bound (paramType p) | p <- params]

    freeVarsConstraints bound constraints =
      Set.unions [freeVarsWithBound bound ty | Constraint _ ty <- constraints]

    renameParam old newName (Param v ty)
      | v == old = Param v ty <$ pure ()
      | otherwise = Param v <$> subst old (EVar newName) ty

    renameConstraint old newName (Constraint cls ty) =
      Constraint cls <$> subst old (EVar newName) ty

    goConstraint bound (Constraint cls ty) = do
      ty' <- go bound ty
      pure (Constraint cls ty')

    renameCompVar old newName comp = do
      renamed <- subst old (EVar newName) (ECompute comp)
      case renamed of
        ECompute comp' -> pure comp'
        _ -> pure comp

    renameBody old newName body = case body of
      FunctionValue e1 -> FunctionValue <$> subst old (EVar newName) e1
      FunctionCompute c1 -> FunctionCompute <$> renameCompVar old newName c1

    goBody bound body = case body of
      FunctionValue e1 -> FunctionValue <$> go bound e1
      FunctionCompute c1 -> FunctionCompute <$> goComp bound c1

    goComp bound comp = case comp of
      CReturn e1 -> CReturn <$> go bound e1
      CPerform e1 -> CPerform <$> go bound e1
      CBind v c1 c2 -> do
        c1' <- goComp bound c1
        if v == name
          then pure (CBind v c1' c2)
          else do
            (v', c2') <- refreshBinderComp bound v c2
            c2'' <- goComp (Set.insert v' bound) c2'
            pure (CBind v' c1' c2'')
      CSeq c1 c2 -> CSeq <$> goComp bound c1 <*> goComp bound c2

    goCase bound caseExpr = case caseExpr of
      MatchEmpty body -> MatchEmpty <$> go bound body
      MatchFalse body -> MatchFalse <$> go bound body
      MatchTrue body -> MatchTrue <$> go bound body
      MatchCons h hTy t tTy body -> do
        hTy' <- go bound hTy
        tTy' <- go bound tTy
        if h == name || t == name
          then pure (MatchCons h hTy' t tTy' body)
          else do
            (h', body1) <- refreshBinder bound h body
            let bound' = Set.insert h' bound
            (t', body2) <- refreshBinder bound' t body1
            body' <- go (Set.insert t' bound') body2
            pure (MatchCons h' hTy' t' tTy' body')
      MatchPair a aTy b bTy body -> do
        aTy' <- go bound aTy
        bTy' <- go bound bTy
        if a == name || b == name
          then pure (MatchPair a aTy' b bTy' body)
          else do
            (a', body1) <- refreshBinder bound a body
            let bound' = Set.insert a' bound
            (b', body2) <- refreshBinder bound' b body1
            body' <- go (Set.insert b' bound') body2
            pure (MatchPair a' aTy' b' bTy' body')

    refreshBinderComp bound v comp =
      if v `Set.member` replacementFree
        then do
          let avoid = Set.unions [replacementFree, freeVarsComp Set.empty comp, bound]
          v' <- freshNameAvoid v avoid
          comp' <- renameCompVar v v' comp
          pure (v', comp')
        else pure (v, comp)

--------------------------------------------------------------------------------
-- Normalization and conversion

normalize :: TCEnv -> Expr -> TypeCheckM Expr
normalize env expr = do
  wh <- whnf env expr
  case wh of
    EApp f args -> do
      f' <- normalize env f
      args' <- mapM (normalize env) args
      pure (mkApp f' args')
    EForAll v dom cod -> do
      dom' <- normalize env dom
      let env' = extendLocal env v dom'
      cod' <- normalize env' cod
      pure (EForAll v dom' cod')
    EThereExists v dom cod -> do
      dom' <- normalize env dom
      let env' = extendLocal env v dom'
      cod' <- normalize env' cod
      pure (EThereExists v dom' cod')
    ECompType t -> ECompType <$> normalize env t
    EEqual ty lhs rhs ->
      EEqual <$> normalize env ty <*> normalize env lhs <*> normalize env rhs
    EReflexive ty term ->
      EReflexive <$> normalize env ty <*> normalize env term
    ERewrite family proof body -> do
      family' <- normalize env family
      proof' <- normalize env proof
      body' <- normalize env body
      pure (ERewrite family' proof' body')
    EPack v dom cod witness body -> do
      dom' <- normalize env dom
      let env' = extendLocal env v dom'
      cod' <- normalize env' cod
      witness' <- normalize env witness
      body' <- normalize env body
      pure (EPack v dom' cod' witness' body')
    EUnpack packed x y body -> do
      packed' <- normalize env packed
      let env' = extendLocal (extendLocal env x tUnit) y tUnit
      body' <- normalize env' body
      pure (EUnpack packed' x y body')
    ELift ty fromLevel toLevel -> do
      ty' <- normalize env ty
      if fromLevel == toLevel
        then pure ty'
        else pure (ELift ty' fromLevel toLevel)
    EUp ty fromLevel toLevel body -> do
      ty' <- normalize env ty
      body' <- normalize env body
      if fromLevel == toLevel
        then pure body'
        else pure (EUp ty' fromLevel toLevel body')
    EDown ty fromLevel toLevel body -> do
      ty' <- normalize env ty
      body' <- normalize env body
      case body' of
        EUp ty2 from2 to2 inner
          | fromLevel == from2
              && toLevel == to2
              && alphaEq Map.empty ty' ty2 -> normalize env inner
        _ ->
          if fromLevel == toLevel
            then pure body'
            else pure (EDown ty' fromLevel toLevel body')
    EFunction params constraints ret body -> do
      (env', params') <- normalizeParams env params
      constraints' <- mapM (\(Constraint cls ty) -> Constraint cls <$> normalize env' ty) constraints
      ret' <- normalize env' ret
      body' <- case body of
        FunctionValue e1 -> FunctionValue <$> normalize env' e1
        FunctionCompute c1 -> pure (FunctionCompute c1)
      pure (EFunction params' constraints' ret' body')
    ELet v val body -> do
      body' <- subst v val body
      normalize env body'
    EAnnot e1 _ -> normalize env e1
    ETyped e1 _ -> normalize env e1
    EMatch scrut scrutTy scrutName retTy cases -> do
      scrut' <- normalize env scrut
      scrutTy' <- normalize env scrutTy
      let env' = extendLocal env scrutName scrutTy'
      retTy' <- normalize env' retTy
      cases' <- mapM (normalizeCase env') cases
      pure (EMatch scrut' scrutTy' scrutName retTy' cases')
    _ -> pure wh

normalizeCase :: TCEnv -> MatchCase -> TypeCheckM MatchCase
normalizeCase env caseExpr = case caseExpr of
  MatchEmpty body -> MatchEmpty <$> normalize env body
  MatchFalse body -> MatchFalse <$> normalize env body
  MatchTrue body -> MatchTrue <$> normalize env body
  MatchCons h hTy t tTy body -> do
    hTy' <- normalize env hTy
    tTy' <- normalize env tTy
    let env' = extendLocal (extendLocal env h hTy') t tTy'
    body' <- normalize env' body
    pure (MatchCons h hTy' t tTy' body')
  MatchPair a aTy b bTy body -> do
    aTy' <- normalize env aTy
    bTy' <- normalize env bTy
    let env' = extendLocal (extendLocal env a aTy') b bTy'
    body' <- normalize env' body
    pure (MatchPair a aTy' b bTy' body')

normalizeParams :: TCEnv -> [Param] -> TypeCheckM (TCEnv, [Param])
normalizeParams env [] = pure (env, [])
normalizeParams env (Param name ty : rest) = do
  ty' <- normalize env ty
  let env' = extendLocal env name ty'
  (envFinal, rest') <- normalizeParams env' rest
  pure (envFinal, Param name ty' : rest')

whnf :: TCEnv -> Expr -> TypeCheckM Expr
whnf env expr = case expr of
  EAnnot e1 _ -> whnf env e1
  ETyped e1 _ -> whnf env e1
  ELet v val body -> do
    body' <- subst v val body
    whnf env body'
  EUnpack packed x y body -> do
    packed' <- whnf env packed
    case packed' of
      EPack _ _ _ witness packedBody -> do
        body' <- subst x witness body
        body'' <- subst y packedBody body'
        whnf env body''
      _ -> pure (EUnpack packed' x y body)
  ERewrite family proof body -> do
    proof' <- whnf env proof
    case proof' of
      EReflexive _ _ -> whnf env body
      _ -> pure (ERewrite family proof' body)
  EMatch scrut scrutTy scrutName retTy cases -> do
    scrut' <- whnf env scrut
    let reduceWith body boundNames substs = do
          body' <- if scrutName `elem` boundNames
            then pure body
            else subst scrutName scrut' body
          body'' <- foldM (\acc (name, val) -> subst name val acc) body' substs
          whnf env body''
        noReduction = pure (EMatch scrut' scrutTy scrutName retTy cases)
    case scrut' of
      ELit (LBoolean False) ->
        case [body | MatchFalse body <- cases] of
          (body:_) -> reduceWith body [] []
          [] -> noReduction
      ELit (LBoolean True) ->
        case [body | MatchTrue body <- cases] of
          (body:_) -> reduceWith body [] []
          [] -> noReduction
      EApp (EVar "nil-prim") [_elemTy] ->
        case [body | MatchEmpty body <- cases] of
          (body:_) -> reduceWith body [] []
          [] -> noReduction
      EApp (EVar "cons-prim") [_elemTy, headExpr, tailExpr] ->
        case [(h, t, body) | MatchCons h _ t _ body <- cases] of
          ((h, t, body):_) -> reduceWith body [h, t] [(h, headExpr), (t, tailExpr)]
          [] -> noReduction
      EApp (EVar "pair-prim") [_aTy, _bTy, leftExpr, rightExpr] ->
        case [(a, b, body) | MatchPair a _ b _ body <- cases] of
          ((a, b, body):_) -> reduceWith body [a, b] [(a, leftExpr), (b, rightExpr)]
          [] -> noReduction
      _ -> noReduction
  ELift ty fromLevel toLevel ->
    if fromLevel == toLevel
      then whnf env ty
      else pure (ELift ty fromLevel toLevel)
  EVar v ->
    if v `Set.member` tcLocals env
      then pure expr
      else case Map.lookup v (tcDefs env) of
        Just (Transparent, body) -> whnf env body
        _ -> pure expr
  EApp f args -> do
    f' <- whnf env f
    case f' of
      EFunction params constraints ret (FunctionValue body) ->
        applyParams params constraints ret body args
      _ -> pure (mkApp f' args)
  _ -> pure expr

applyParams :: [Param] -> [Constraint] -> Expr -> Expr -> [Expr] -> TypeCheckM Expr
applyParams [] _constraints _ret body args = pure (mkApp body args)
applyParams (Param v ty : rest) constraints ret body args = case args of
  [] -> pure (EFunction (Param v ty : rest) constraints ret (FunctionValue body))
  (a:as) -> do
    body' <- subst v a body
    rest' <- mapM (substParam v a) rest
    constraints' <- mapM (substConstraint v a) constraints
    ret' <- subst v a ret
    applyParams rest' constraints' ret' body' as

substParam :: Text -> Expr -> Param -> TypeCheckM Param
substParam name replacement (Param v ty) =
  Param v <$> subst name replacement ty

substConstraint :: Text -> Expr -> Constraint -> TypeCheckM Constraint
substConstraint name replacement (Constraint cls ty) =
  Constraint cls <$> subst name replacement ty

mkApp :: Expr -> [Expr] -> Expr
mkApp f [] = f
mkApp (EApp f existing) more = EApp f (existing ++ more)
mkApp f more                 = EApp f more

conv :: TCEnv -> Expr -> Expr -> TypeCheckM Bool
conv env a b = do
  a' <- normalize env a
  b' <- normalize env b
  pure (alphaEq Map.empty a' b')

alphaEq :: Map.Map Text Text -> Expr -> Expr -> Bool
alphaEq env a b = case (a, b) of
  (EVar v1, EVar v2) ->
    case Map.lookup v1 env of
      Just v2' -> v2 == v2'
      Nothing -> v1 == v2
  (ELit l1, ELit l2) -> l1 == l2
  (ETypeConst c1, ETypeConst c2) -> c1 == c2
  (ETypeUniverse i, ETypeUniverse j) -> i == j
  (EForAll v1 d1 c1, EForAll v2 d2 c2) ->
    alphaEq env d1 d2 && alphaEq (Map.insert v1 v2 env) c1 c2
  (EThereExists v1 d1 c1, EThereExists v2 d2 c2) ->
    alphaEq env d1 d2 && alphaEq (Map.insert v1 v2 env) c1 c2
  (ECompType t1, ECompType t2) -> alphaEq env t1 t2
  (EEqual ty1 l1 r1, EEqual ty2 l2 r2) ->
    alphaEq env ty1 ty2 && alphaEq env l1 l2 && alphaEq env r1 r2
  (EReflexive ty1 t1, EReflexive ty2 t2) ->
    alphaEq env ty1 ty2 && alphaEq env t1 t2
  (ERewrite f1 p1 b1, ERewrite f2 p2 b2) ->
    alphaEq env f1 f2 && alphaEq env p1 p2 && alphaEq env b1 b2
  (EPack v1 d1 c1 w1 b1, EPack v2 d2 c2 w2 b2) ->
    alphaEq env d1 d2
      && alphaEq (Map.insert v1 v2 env) c1 c2
      && alphaEq env w1 w2
      && alphaEq env b1 b2
  (EUnpack p1 x1 y1 b1, EUnpack p2 x2 y2 b2) ->
    alphaEq env p1 p2
      && alphaEq (Map.insert y1 y2 (Map.insert x1 x2 env)) b1 b2
  (ELift ty1 from1 to1, ELift ty2 from2 to2) ->
    from1 == from2 && to1 == to2 && alphaEq env ty1 ty2
  (EUp ty1 from1 to1 body1, EUp ty2 from2 to2 body2) ->
    from1 == from2 && to1 == to2 && alphaEq env ty1 ty2 && alphaEq env body1 body2
  (EDown ty1 from1 to1 body1, EDown ty2 from2 to2 body2) ->
    from1 == from2 && to1 == to2 && alphaEq env ty1 ty2 && alphaEq env body1 body2
  (EApp f1 a1, EApp f2 a2) ->
    length a1 == length a2 && alphaEq env f1 f2 && and (zipWith (alphaEq env) a1 a2)
  (EFunction ps1 cs1 r1 b1, EFunction ps2 cs2 r2 b2) ->
    length ps1 == length ps2
      && length cs1 == length cs2
      && let (env', paramsOk) = foldl step (env, True) (zip ps1 ps2)
             step (e, ok) (Param n1 t1, Param n2 t2) =
               (Map.insert n1 n2 e, ok && alphaEq e t1 t2)
             constraintsOk = and (zipWith (alphaEqConstraint env') cs1 cs2)
         in paramsOk && constraintsOk && alphaEq env' r1 r2 && alphaEqBody env' b1 b2
  (ELet v1 val1 body1, ELet v2 val2 body2) ->
    alphaEq env val1 val2 && alphaEq (Map.insert v1 v2 env) body1 body2
  (EMatch s1 ty1 n1 r1 c1, EMatch s2 ty2 n2 r2 c2) ->
    alphaEq env s1 s2
      && alphaEq env ty1 ty2
      && alphaEq (Map.insert n1 n2 env) r1 r2
      && alphaEqCases (Map.insert n1 n2 env) c1 c2
  (EAnnot e1 t1, EAnnot e2 t2) ->
    alphaEq env e1 e2 && alphaEq env t1 t2
  (ETyped e1 t1, ETyped e2 t2) ->
    alphaEq env e1 e2 && alphaEq env t1 t2
  (EDict n1 i1, EDict n2 i2) ->
    n1 == n2 && and (zipWith (\(a1, e1) (a2, e2) -> a1 == a2 && alphaEq env e1 e2) i1 i2)
  (EDictAccess d1 m1, EDictAccess d2 m2) ->
    m1 == m2 && alphaEq env d1 d2
  (ETypeClass p1 ms1, ETypeClass p2 ms2) ->
    length ms1 == length ms2
      && let env' = Map.insert p1 p2 env
         in and (zipWith (\(n1, t1) (n2, t2) -> n1 == n2 && alphaEq env' t1 t2) ms1 ms2)
  (EInstance c1 t1 ms1, EInstance c2 t2 ms2) ->
    c1 == c2 && alphaEq env t1 t2
      && length ms1 == length ms2
      && and (zipWith (\(n1, e1) (n2, e2) -> n1 == n2 && alphaEq env e1 e2) ms1 ms2)
  _ -> False

alphaEqConstraint :: Map.Map Text Text -> Constraint -> Constraint -> Bool
alphaEqConstraint env (Constraint c1 t1) (Constraint c2 t2) =
  c1 == c2 && alphaEq env t1 t2

alphaEqBody :: Map.Map Text Text -> FunctionBody -> FunctionBody -> Bool
alphaEqBody env b1 b2 = case (b1, b2) of
  (FunctionValue e1, FunctionValue e2) -> alphaEq env e1 e2
  (FunctionCompute c1, FunctionCompute c2) -> alphaEqComp env c1 c2
  _ -> False

alphaEqComp :: Map.Map Text Text -> Comp -> Comp -> Bool
alphaEqComp env c1 c2 = case (c1, c2) of
  (CReturn e1, CReturn e2) -> alphaEq env e1 e2
  (CPerform e1, CPerform e2) -> alphaEq env e1 e2
  (CBind v1 a1 b1, CBind v2 a2 b2) ->
    alphaEqComp env a1 a2 && alphaEqComp (Map.insert v1 v2 env) b1 b2
  (CSeq a1 b1, CSeq a2 b2) -> alphaEqComp env a1 a2 && alphaEqComp env b1 b2
  _ -> False

alphaEqCases :: Map.Map Text Text -> [MatchCase] -> [MatchCase] -> Bool
alphaEqCases env xs ys =
  length xs == length ys && and (zipWith (alphaEqCase env) xs ys)

alphaEqCase :: Map.Map Text Text -> MatchCase -> MatchCase -> Bool
alphaEqCase env c1 c2 = case (c1, c2) of
  (MatchEmpty b1, MatchEmpty b2) -> alphaEq env b1 b2
  (MatchFalse b1, MatchFalse b2) -> alphaEq env b1 b2
  (MatchTrue b1, MatchTrue b2) -> alphaEq env b1 b2
  (MatchCons h1 hTy1 t1 tTy1 b1, MatchCons h2 hTy2 t2 tTy2 b2) ->
    alphaEq env hTy1 hTy2
      && alphaEq env tTy1 tTy2
      && alphaEq (Map.insert t1 t2 (Map.insert h1 h2 env)) b1 b2
  (MatchPair a1 aTy1 b1 bTy1 body1, MatchPair a2 aTy2 b2 bTy2 body2) ->
    alphaEq env aTy1 aTy2
      && alphaEq env bTy1 bTy2
      && alphaEq (Map.insert b1 b2 (Map.insert a1 a2 env)) body1 body2
  _ -> False

ensureConv :: TCEnv -> Expr -> Expr -> TypeCheckM ()
ensureConv env actual expected = do
  ok <- conv env actual expected
  if ok then pure () else lift (Left (TypeMismatch noLoc expected actual))

ensureLiftLevels :: TCEnv -> Expr -> Int -> Int -> TypeCheckM ()
ensureLiftLevels env ty fromLevel toLevel = do
  actual <- inferUniverse env ty
  if actual == fromLevel
    then pure ()
    else lift (Left (TypeMismatch noLoc (ETypeUniverse fromLevel) (ETypeUniverse actual)))
  if fromLevel <= toLevel
    then pure ()
    else lift (Left (InvalidLift noLoc fromLevel toLevel))

--------------------------------------------------------------------------------
-- Structural recursion checks (value-level only)

data StructInfo = StructInfo
  { structName :: Text
  , structIndex :: Int
  } deriving (Show, Eq)

isListType :: TCEnv -> Expr -> TypeCheckM Bool
isListType env ty = do
  tyWhnf <- whnf env ty
  pure $ case tyWhnf of
    EApp (ETypeConst TCList) [_] -> True
    _ -> False

findStructuralParam :: TCEnv -> [Param] -> TypeCheckM (Maybe StructInfo)
findStructuralParam env params =
  let go _ [] = pure Nothing
      go idx (Param name ty : rest) = do
        isList <- isListType env ty
        if isList
          then pure (Just (StructInfo name idx))
          else go (idx + 1) rest
  in go 0 params

checkStructuralRecursion :: TCEnv -> [Param] -> FunctionBody -> TypeCheckM ()
checkStructuralRecursion env params body = case body of
  FunctionValue expr -> do
    when (any ((== "recur") . paramName) params) $
      lift (Left (RecursionError noLoc "recur is reserved and cannot be a parameter name"))
    structInfo <- findStructuralParam env params
    checkExprRec env structInfo Set.empty True expr
  FunctionCompute comp ->
    checkCompRec env Nothing Set.empty False comp

checkExprRec :: TCEnv -> Maybe StructInfo -> Set.Set Text -> Bool -> Expr -> TypeCheckM ()
checkExprRec env structInfo allowed allowRecur expr = case expr of
  EVar v | v == "recur" ->
    if allowRecur
      then lift (Left (RecursionError noLoc "recur must be applied"))
      else lift (Left (RecursionError noLoc "recur is only allowed in value recursion"))
  EApp (EVar "recur") args -> do
    if allowRecur
      then checkRecurCall structInfo allowed args
      else lift (Left (RecursionError noLoc "recur is only allowed in value recursion"))
    mapM_ (checkExprRec env structInfo allowed allowRecur) args
  EApp f args -> do
    checkExprRec env structInfo allowed allowRecur f
    mapM_ (checkExprRec env structInfo allowed allowRecur) args
  ELet name val body -> do
    checkExprRec env structInfo allowed allowRecur val
    let allowedShadow = Set.delete name allowed
        allowed' = case val of
          EVar v | v `Set.member` allowedShadow -> Set.insert name allowedShadow
          _ -> allowedShadow
    checkExprRec env structInfo allowed' allowRecur body
  ECompute comp ->
    checkCompRec env structInfo allowed False comp
  EMatch scrut scrutTy scrutName _retTy cases -> do
    let allowedBase = Set.delete scrutName allowed
    checkExprRec env structInfo allowedBase allowRecur scrut
    canDecrease <- case structInfo of
      Nothing -> pure False
      Just (StructInfo name _) -> do
        listScrut <- isListType env scrutTy
        pure $ listScrut && case scrut of
          EVar v -> v == name || v `Set.member` allowed
          _ -> False
    mapM_ (checkCaseRec env structInfo allowedBase allowRecur canDecrease) cases
  EUnpack packed x y body -> do
    checkExprRec env structInfo allowed allowRecur packed
    let allowed' = Set.delete y (Set.delete x allowed)
    checkExprRec env structInfo allowed' allowRecur body
  EPack _ _ _ witness body -> do
    checkExprRec env structInfo allowed allowRecur witness
    checkExprRec env structInfo allowed allowRecur body
  EFunction _ _ _ nestedBody -> case nestedBody of
    FunctionValue nestedExpr ->
      checkExprRec env structInfo allowed False nestedExpr
    FunctionCompute nestedComp ->
      checkCompRec env structInfo allowed False nestedComp
  EEqual ty lhs rhs -> do
    checkExprRec env structInfo allowed allowRecur ty
    checkExprRec env structInfo allowed allowRecur lhs
    checkExprRec env structInfo allowed allowRecur rhs
  EReflexive ty term -> do
    checkExprRec env structInfo allowed allowRecur ty
    checkExprRec env structInfo allowed allowRecur term
  ERewrite family proof body -> do
    checkExprRec env structInfo allowed allowRecur family
    checkExprRec env structInfo allowed allowRecur proof
    checkExprRec env structInfo allowed allowRecur body
  EAnnot e1 _ -> checkExprRec env structInfo allowed allowRecur e1
  ETyped e1 _ -> checkExprRec env structInfo allowed allowRecur e1
  _ -> pure ()

checkCaseRec :: TCEnv -> Maybe StructInfo -> Set.Set Text -> Bool -> Bool -> MatchCase -> TypeCheckM ()
checkCaseRec env structInfo allowed allowRecur canDecrease caseExpr = case caseExpr of
  MatchEmpty body ->
    checkExprRec env structInfo allowed allowRecur body
  MatchFalse body ->
    checkExprRec env structInfo allowed allowRecur body
  MatchTrue body ->
    checkExprRec env structInfo allowed allowRecur body
  MatchCons h _ t tTy body -> do
    let allowedShadow = Set.delete t (Set.delete h allowed)
    tailOk <- if canDecrease then isListType env tTy else pure False
    let allowed' = if tailOk then Set.insert t allowedShadow else allowedShadow
    checkExprRec env structInfo allowed' allowRecur body
  MatchPair a _ b _ body -> do
    let allowed' = Set.delete b (Set.delete a allowed)
    checkExprRec env structInfo allowed' allowRecur body

checkCompRec :: TCEnv -> Maybe StructInfo -> Set.Set Text -> Bool -> Comp -> TypeCheckM ()
checkCompRec env structInfo allowed allowRecur comp = case comp of
  CReturn e1 ->
    checkExprRec env structInfo allowed allowRecur e1
  CBind name c1 c2 -> do
    checkCompRec env structInfo allowed allowRecur c1
    let allowed' = Set.delete name allowed
    checkCompRec env structInfo allowed' allowRecur c2
  CPerform e1 ->
    checkExprRec env structInfo allowed allowRecur e1
  CSeq c1 c2 -> do
    checkCompRec env structInfo allowed allowRecur c1
    checkCompRec env structInfo allowed allowRecur c2

checkRecurCall :: Maybe StructInfo -> Set.Set Text -> [Expr] -> TypeCheckM ()
checkRecurCall structInfo allowed args = case structInfo of
  Nothing ->
    lift (Left (RecursionError noLoc "recur requires a list parameter to decrease"))
  Just (StructInfo _ idx) ->
    if length args <= idx
      then lift (Left (RecursionError noLoc "recur must be applied to the structural argument"))
      else case args !! idx of
        EVar v | v `Set.member` allowed -> pure ()
        _ -> lift (Left (RecursionError noLoc "recur must use a structurally smaller argument"))

--------------------------------------------------------------------------------
-- Type inference/checking

infer :: TCEnv -> Expr -> TypeCheckM Expr
infer env expr = case expr of
  EVar v -> lookupVar noLoc env v
  ELit lit -> pure $ case lit of
    LNatural _ -> tNat
    LString _ -> tString
    LBoolean _ -> tBool
    LUnit -> tUnit
  ETypeConst tc -> pure (typeOfTypeConst tc)
  ETypeUniverse n -> pure (ETypeUniverse (n + 1))
  EForAll v dom cod -> do
    domLevel <- inferUniverse env dom
    let env' = extendLocal env v dom
    codLevel <- inferUniverse env' cod
    pure (ETypeUniverse (max domLevel codLevel))
  EThereExists v dom cod -> do
    domLevel <- inferUniverse env dom
    let env' = extendLocal env v dom
    codLevel <- inferUniverse env' cod
    pure (ETypeUniverse (max domLevel codLevel))
  ECompType t -> do
    level <- inferUniverse env t
    pure (ETypeUniverse level)
  EEqual ty lhs rhs -> do
    level <- inferUniverse env ty
    check env lhs ty
    check env rhs ty
    pure (ETypeUniverse level)
  EReflexive ty term -> do
    _ <- inferUniverse env ty
    check env term ty
    pure (EEqual ty term term)
  ERewrite family proof body -> do
    familyTy <- infer env family
    familyTyWhnf <- whnf env familyTy
    case familyTyWhnf of
      EForAll v dom cod -> do
        _ <- inferUniverse (extendLocal env v dom) cod
        proofTy <- infer env proof
        proofTyWhnf <- whnf env proofTy
        case proofTyWhnf of
          EEqual eqTy lhs rhs -> do
            ensureConv env eqTy dom
            expected <- reduceFamilyApp family lhs
            check env body expected
            reduceFamilyApp family rhs
          _ -> lift (Left (ExpectedEquality noLoc proofTyWhnf))
      _ -> lift (Left (NotAFunction noLoc familyTyWhnf))
    where
      reduceFamilyApp fam arg = case fam of
        EAnnot e _ -> reduceFamilyApp e arg
        ETyped e _ -> reduceFamilyApp e arg
        EFunction params constraints ret (FunctionValue bodyExpr) ->
          applyParams params constraints ret bodyExpr [arg]
        EVar name ->
          case Map.lookup name (tcDefs env) of
            Just (Transparent, bodyExpr) -> reduceFamilyApp bodyExpr arg
            _ -> pure (mkApp fam [arg])
        _ -> pure (mkApp fam [arg])
  EPack v dom cod witness body -> do
    _ <- inferUniverse env dom
    let env' = extendLocal env v dom
    _ <- inferUniverse env' cod
    check env witness dom
    codApplied <- subst v witness cod
    check env body codApplied
    pure (EThereExists v dom cod)
  EUnpack packed x y body -> do
    packedTy <- infer env packed
    packedTyWhnf <- whnf env packedTy
    case packedTyWhnf of
      EThereExists v dom cod -> do
        cod' <- subst v (EVar x) cod
        let env' = extendLocal (extendLocal env x dom) y cod'
        infer env' body
      _ -> lift (Left (ExpectedThereExists noLoc packedTyWhnf))
  ELift ty fromLevel toLevel -> do
    ensureLiftLevels env ty fromLevel toLevel
    pure (ETypeUniverse toLevel)
  EUp ty fromLevel toLevel body -> do
    ensureLiftLevels env ty fromLevel toLevel
    check env body ty
    pure (ELift ty fromLevel toLevel)
  EDown ty fromLevel toLevel body -> do
    ensureLiftLevels env ty fromLevel toLevel
    check env body (ELift ty fromLevel toLevel)
    pure ty
  EApp f args -> do
    fTy <- infer env f
    foldM (applyArg env) fTy args
  EFunction params constraints ret body -> do
    envParams <- foldM (\e (Param n ty) -> do
                          _ <- inferUniverse e ty
                          pure (extendLocal e n ty)) env params
    _ <- inferUniverse envParams ret
    methods <- constraintMethods envParams constraints
    let envWithMethods = foldl addMethod envParams methods
        addMethod e (methodName, methodTy) =
          if methodName `Set.member` tcLocals e
            then e
            else extendLocal e methodName methodTy
    let funTy = mkPi params ret
        envWithRecur = extendLocal envWithMethods "recur" funTy
    checkStructuralRecursion envWithRecur params body
    case body of
      FunctionValue e1 -> check envWithRecur e1 ret
      FunctionCompute c1 -> do
        inner <- expectCompType envParams ret
        compTy <- inferComp envWithRecur c1
        ensureConv envWithRecur compTy inner
    pure (mkPi params ret)
  ELet name val body -> do
    valTy <- infer env val
    infer (extendLocal env name valTy) body
  ECompute comp -> do
    compTy <- inferComp env comp
    pure (ECompType compTy)
  EMatch scrut scrutTy scrutName retTy cases -> do
    scrutInferred <- infer env scrut
    ensureConv env scrutInferred scrutTy
    _ <- inferUniverse env scrutTy
    let envScrut = extendLocal env scrutName scrutTy
    _ <- inferUniverse envScrut retTy
    checkMatch envScrut scrutName retTy scrutTy cases
    subst scrutName scrut retTy
  EAnnot e1 ty -> do
    _ <- inferUniverse env ty
    check env e1 ty
    pure ty
  ETyped e1 ty -> do
    check env e1 ty
    pure ty
  EDict _ _ -> lift (Left (TypeclassError noLoc "dict nodes not supported"))
  EDictAccess _ _ -> lift (Left (TypeclassError noLoc "dict access nodes not supported"))
  ETypeClass param methods -> do
    ensureUniqueMethodNames (map fst methods)
    let env' = extendLocal env param (tType 0)
    mapM_ (\(_, ty) -> inferUniverse env' ty) methods
    pure (classType param)
  EInstance clsName instTy methods -> do
    classInfo <- lookupClass noLoc env clsName
    ensureUniqueMethodNames (map fst methods)
    let knownNames = Map.keysSet (tcTypes env)
        implicitVars = Set.toList (freeVars instTy `Set.difference` knownNames)
        env' = foldl (\e v -> extendLocal e v (tType 0)) env implicitVars
    instLevel <- inferUniverse env' instTy
    when (instLevel /= 0) $
      lift (Left (TypeclassError noLoc "Instance type must be in Type0"))
    let classMethodNames = map fst (classMethods classInfo)
        instMethodNames = map fst methods
        missing = filter (`notElem` instMethodNames) classMethodNames
        extras = filter (`notElem` classMethodNames) instMethodNames
    when (not (null missing)) $
      lift (Left (TypeclassError noLoc ("Instance missing methods: " <> T.intercalate ", " missing)))
    when (not (null extras)) $
      lift (Left (TypeclassError noLoc ("Instance has extra methods: " <> T.intercalate ", " extras)))
    let methodMap = Map.fromList methods
        param = classParam classInfo
    mapM_ (\(methodName, methodTy) -> case Map.lookup methodName methodMap of
              Nothing -> pure ()
              Just impl -> do
                expectedTy <- subst param instTy methodTy
                check env' impl expectedTy
          ) (classMethods classInfo)
    pure (EApp (EVar (className classInfo)) [instTy])

check :: TCEnv -> Expr -> Expr -> TypeCheckM ()
check env expr expected = do
  inferred <- infer env expr
  ensureConv env inferred expected

inferComp :: TCEnv -> Comp -> TypeCheckM Expr
inferComp env comp = case comp of
  CReturn e1 -> infer env e1
  CBind name c1 c2 -> do
    t1 <- inferComp env c1
    inferComp (extendLocal env name t1) c2
  CPerform e1 -> do
    t <- infer env e1
    inner <- expectCompType env t
    pure inner
  CSeq c1 c2 -> do
    _ <- inferComp env c1
    inferComp env c2

checkMatch :: TCEnv -> Text -> Expr -> Expr -> [MatchCase] -> TypeCheckM ()
checkMatch env scrutName retTy scrutTy cases = do
  scrutTy' <- whnf env scrutTy
  case scrutTy' of
    ETypeConst TCBoolean -> do
      ensureCaseSet [CaseFalse, CaseTrue] cases
      checkBoolMatch env scrutName retTy cases
    EApp (ETypeConst TCList) [elemTy] -> do
      ensureCaseSet [CaseEmpty, CaseCons] cases
      checkListMatch env scrutName retTy elemTy cases
    EApp (ETypeConst TCPair) [a, b] -> do
      ensureCaseSet [CasePair] cases
      checkPairMatch env scrutName retTy a b cases
    _ -> lift (Left (MatchCaseError noLoc "unsupported scrutinee type"))

ensureCaseSet :: [MatchKind] -> [MatchCase] -> TypeCheckM ()
ensureCaseSet allowed cases =
  let kinds = map caseKind cases
      allowedSet = Set.fromList allowed
      extras = filter (`Set.notMember` allowedSet) kinds
      counts = Map.fromListWith (+) [(k, 1 :: Int) | k <- kinds]
      duplicates = Map.keys (Map.filter (>1) counts)
  in case extras of
    (k:_) -> lift (Left (MatchCaseError noLoc ("unexpected " <> caseKindName k)))
    [] -> case duplicates of
      (k:_) -> lift (Left (MatchCaseError noLoc ("duplicate " <> caseKindName k)))
      [] -> pure ()

caseKind :: MatchCase -> MatchKind
caseKind caseExpr = case caseExpr of
  MatchEmpty _ -> CaseEmpty
  MatchCons _ _ _ _ _ -> CaseCons
  MatchFalse _ -> CaseFalse
  MatchTrue _ -> CaseTrue
  MatchPair _ _ _ _ _ -> CasePair

caseKindName :: MatchKind -> Text
caseKindName kind = case kind of
  CaseEmpty -> "empty-case"
  CaseCons -> "cons-case"
  CaseFalse -> "false-case"
  CaseTrue -> "true-case"
  CasePair -> "pair-case"

substEnvTypes :: Text -> Expr -> TCEnv -> TypeCheckM TCEnv
substEnvTypes name replacement env = do
  types' <- Map.traverseWithKey (\_ ty -> subst name replacement ty) (tcTypes env)
  pure env { tcTypes = types' }

checkBoolMatch :: TCEnv -> Text -> Expr -> [MatchCase] -> TypeCheckM ()
checkBoolMatch env scrutName retTy cases = do
  falseBody <- case [body | MatchFalse body <- cases] of
    (body:_) -> pure body
    [] -> lift (Left (MatchCaseError noLoc "missing false-case"))
  trueBody <- case [body | MatchTrue body <- cases] of
    (body:_) -> pure body
    [] -> lift (Left (MatchCaseError noLoc "missing true-case"))
  expectedFalse <- subst scrutName (ELit (LBoolean False)) retTy
  expectedTrue <- subst scrutName (ELit (LBoolean True)) retTy
  envFalse <- substEnvTypes scrutName (ELit (LBoolean False)) env
  envTrue <- substEnvTypes scrutName (ELit (LBoolean True)) env
  check envFalse falseBody expectedFalse
  check envTrue trueBody expectedTrue

checkListMatch :: TCEnv -> Text -> Expr -> Expr -> [MatchCase] -> TypeCheckM ()
checkListMatch env scrutName retTy elemTy cases = do
  emptyBody <- case [body | MatchEmpty body <- cases] of
    (body:_) -> pure body
    [] -> lift (Left (MatchCaseError noLoc "missing empty-case"))
  consCase <- case [(h, hTy, t, tTy, body) | MatchCons h hTy t tTy body <- cases] of
    (c:_) -> pure c
    [] -> lift (Left (MatchCaseError noLoc "missing cons-case"))
  let (h, hTy, t, tTy, consBody) = consCase
      listTy = tList elemTy
  ensureConv env hTy elemTy
  ensureConv env tTy listTy
  expectedEmpty <- subst scrutName (listNilTerm elemTy) retTy
  expectedCons <- subst scrutName (listConsTerm elemTy h t) retTy
  envEmpty <- substEnvTypes scrutName (listNilTerm elemTy) env
  check envEmpty emptyBody expectedEmpty
  envConsBase <- substEnvTypes scrutName (listConsTerm elemTy h t) env
  let env' = extendLocal (extendLocal envConsBase h elemTy) t listTy
  check env' consBody expectedCons

checkPairMatch :: TCEnv -> Text -> Expr -> Expr -> Expr -> [MatchCase] -> TypeCheckM ()
checkPairMatch env scrutName retTy aTy bTy cases = do
  pairCase <- case [(x, xTy, y, yTy, body) | MatchPair x xTy y yTy body <- cases] of
    (c:_) -> pure c
    [] -> lift (Left (MatchCaseError noLoc "missing pair-case"))
  let (x, xTy, y, yTy, body) = pairCase
  ensureConv env xTy aTy
  ensureConv env yTy bTy
  expected <- subst scrutName (pairTerm aTy bTy x y) retTy
  envPair <- substEnvTypes scrutName (pairTerm aTy bTy x y) env
  check (extendLocal (extendLocal envPair x aTy) y bTy) body expected

listNilTerm :: Expr -> Expr
listNilTerm elemTy = EApp (EVar "nil-prim") [elemTy]

listConsTerm :: Expr -> Text -> Text -> Expr
listConsTerm elemTy h t =
  EApp (EVar "cons-prim") [elemTy, EVar h, EVar t]

pairTerm :: Expr -> Expr -> Text -> Text -> Expr
pairTerm aTy bTy x y =
  EApp (EVar "pair-prim") [aTy, bTy, EVar x, EVar y]

applyArg :: TCEnv -> Expr -> Expr -> TypeCheckM Expr
applyArg env funTy argExpr = do
  funTyWhnf <- whnf env funTy
  case funTyWhnf of
    EForAll v dom cod -> do
      argTy <- infer env argExpr
      ensureConv env argTy dom
      subst v argExpr cod
    _ -> lift (Left (NotAFunction noLoc funTyWhnf))

expectCompType :: TCEnv -> Expr -> TypeCheckM Expr
expectCompType env ty = do
  ty' <- whnf env ty
  case ty' of
    ECompType inner -> pure inner
    _ -> lift (Left (NotAComputation noLoc ty'))

inferUniverse :: TCEnv -> Expr -> TypeCheckM Int
inferUniverse env tyExpr = do
  ty <- infer env tyExpr
  tyWhnf <- whnf env ty
  case tyWhnf of
    ETypeUniverse n -> pure n
    _ -> lift (Left (ExpectedType noLoc tyWhnf))

typeOfTypeConst :: TypeConst -> Expr
typeOfTypeConst tc = case tc of
  TCNatural -> tType 0
  TCString -> tType 0
  TCBoolean -> tType 0
  TCUnit -> tType 0
  TCList -> EForAll "A" (tType 0) (tType 0)
  TCPair -> EForAll "A" (tType 0) (EForAll "B" (tType 0) (tType 0))

--------------------------------------------------------------------------------
-- Primitive environment

buildPrimitiveEnv :: TypeEnv
buildPrimitiveEnv = Map.fromList
  [ ("add-nat-prim", tFun tNat (tFun tNat tNat))
  , ("sub-nat-prim", tFun tNat (tFun tNat tNat))
  , ("mul-nat-prim", tFun tNat (tFun tNat tNat))
  , ("div-nat-prim", tFun tNat (tFun tNat tNat))
  , ("mod-nat-prim", tFun tNat (tFun tNat tNat))

  , ("eq-nat-prim", tFun tNat (tFun tNat tBool))
  , ("lt-nat-prim", tFun tNat (tFun tNat tBool))
  , ("le-nat-prim", tFun tNat (tFun tNat tBool))
  , ("gt-nat-prim", tFun tNat (tFun tNat tBool))
  , ("ge-nat-prim", tFun tNat (tFun tNat tBool))

  , ("concat-string-prim", tFun tString (tFun tString tString))
  , ("eq-string-prim", tFun tString (tFun tString tBool))
  , ("decide-eq-nat-prim",
      EForAll "x" tNat
        (EForAll "y" tNat
          (tDecidable (tEqual tNat (EVar "x") (EVar "y")))))
  , ("decide-eq-string-prim",
      EForAll "x" tString
        (EForAll "y" tString
          (tDecidable (tEqual tString (EVar "x") (EVar "y")))))
  , ("decide-eq-bool-prim",
      EForAll "x" tBool
        (EForAll "y" tBool
          (tDecidable (tEqual tBool (EVar "x") (EVar "y")))))
  , ("decide-eq-pair-prim",
      EForAll "A" (tType 0)
        (EForAll "B" (tType 0)
          (EForAll "eqA" (tDecider (EVar "A"))
            (EForAll "eqB" (tDecider (EVar "B"))
              (EForAll "p" (tPair (EVar "A") (EVar "B"))
                (EForAll "q" (tPair (EVar "A") (EVar "B"))
                  (tDecidable
                    (tEqual (tPair (EVar "A") (EVar "B")) (EVar "p") (EVar "q")))))))))
  , ("decide-eq-list-prim",
      EForAll "A" (tType 0)
        (EForAll "eqA" (tDecider (EVar "A"))
          (EForAll "xs" (tList (EVar "A"))
            (EForAll "ys" (tList (EVar "A"))
              (tDecidable
                (tEqual (tList (EVar "A")) (EVar "xs") (EVar "ys")))))))
  , ("string-to-list-prim", tFun tString (tList tString))

  , ("nil-prim", EForAll "A" (tType 0) (tList (EVar "A")))
  , ("cons-prim", EForAll "A" (tType 0) (tFun (EVar "A") (tFun (tList (EVar "A")) (tList (EVar "A")))))
  , ("natural-to-peano-prim", tFun tNat (tList tUnit))
  , ("pair-prim", EForAll "A" (tType 0)
      (EForAll "B" (tType 0)
        (tFun (EVar "A") (tFun (EVar "B") (tPair (EVar "A") (EVar "B"))))))

  , ("print-prim", tFun tString (tComp tUnit))
  , ("assert-hit-prim", tComp tUnit)
  , ("get-line-prim", tComp tString)
  , ("cli-args-prim", tComp (tList tString))
  , ("current-directory-prim", tComp tString)
  , ("read-file-prim", tFun tString (tComp tString))
  , ("write-file-prim", tFun tString (tFun tString (tComp tUnit)))
  , ("shell-prim", tFun tString (tComp tString))
  , ("list-dir-prim", tFun tString (tComp (tList tString)))
  , ("path-exists-prim", tFun tString (tComp tBool))
  , ("is-directory-prim", tFun tString (tComp tBool))
  , ("is-file-prim", tFun tString (tComp tBool))
  , ("make-directory-prim", tFun tString (tComp tUnit))
  , ("remove-file-prim", tFun tString (tComp tUnit))
  , ("remove-directory-prim", tFun tString (tComp tUnit))
  , ("append-file-prim", tFun tString (tFun tString (tComp tUnit)))
  , ("copy-file-prim", tFun tString (tFun tString (tComp tUnit)))
  , ("copy-tree-prim", tFun tString (tFun tString (tComp tUnit)))
  , ("rename-path-prim", tFun tString (tFun tString (tComp tUnit)))
  , ("walk-prim", tFun tString (tComp (tList (tPair tString tBool))))
  , ("stat-prim", tFun tString (tComp (tPair tString (tPair tNat tNat))))

  , ("nat-to-string-prim", tFun tNat tString)

  , ("validate-prim", tFun tString tBool)
  , ("error-prim", EForAll "A" (tType 0) (tFun tString (EVar "A")))
  ]

--------------------------------------------------------------------------------
-- Module checking

typeCheckModule :: Module -> Either TypeError TypeEnv
typeCheckModule m = do
  let envWithOpens = processOpens (modOpens m) emptyEnv
  env <- runTypeCheck (typeCheckModuleWithEnv envWithOpens m)
  pure (tcTypes env)

typeCheckModuleWithImports :: FilePath -> Text -> Module -> IO (Either TypeError TypeEnv)
typeCheckModuleWithImports projectRoot _sourceContents m = do
  importedEnv <- loadTypeImports projectRoot m
  let envWithOpens = processOpens (modOpens m) importedEnv
  pure $ fmap tcTypes (runTypeCheck (typeCheckModuleWithEnv envWithOpens m))

typeCheckModuleWithEnv :: TCEnv -> Module -> TypeCheckM TCEnv
typeCheckModuleWithEnv initialEnv (Module _name _imports _opens defs) =
  foldM typeCheckDef initialEnv defs

typeCheckDef :: TCEnv -> Definition -> TypeCheckM TCEnv
typeCheckDef env (Definition tr name body) = case body of
  ETypeClass param methods -> do
    _ <- infer env body
    let info = ClassInfo
          { className = name
          , classParam = param
          , classMethods = methods
          }
    pure (extendClass env name info (classType param))
  EInstance clsName instTy methods -> do
    ty <- infer env body
    classInfo <- lookupClass noLoc env clsName
    let info = InstanceInfo
          { instName = name
          , instClass = className classInfo
          , instType = instTy
          , instMethods = Map.fromList methods
          }
    pure (extendInstance env name info ty)
  _ -> do
    ty <- infer env body
    pure (extendGlobal env name ty tr body)

annotateModule :: TypeEnv -> Module -> Either TypeError Module
annotateModule _env m = Right m

--------------------------------------------------------------------------------
-- Import loading

loadTypeImports :: FilePath -> Module -> IO TCEnv
loadTypeImports projectRoot (Module _ imports _ _) = do
  envs <- mapM (loadTypeImport projectRoot) imports
  let mergedTypes = Map.unions (tcTypes emptyEnv : map tcTypes envs)
      mergedDefs = Map.unions (tcDefs emptyEnv : map tcDefs envs)
      mergedClasses = Map.unions (tcClasses emptyEnv : map tcClasses envs)
      mergedInstances = Map.unionsWith (++) (tcInstances emptyEnv : map tcInstances envs)
  pure emptyEnv
    { tcTypes = mergedTypes
    , tcDefs = mergedDefs
    , tcClasses = mergedClasses
    , tcInstances = mergedInstances
    }

loadTypeImport :: FilePath -> Import -> IO TCEnv
loadTypeImport projectRoot (Import modName alias) = do
  let modPath = modNameToPath modName
      basePath = if "test/" `isPrefixOf` modPath
                 then projectRoot </> modPath
                 else projectRoot </> "lib" </> modPath
      lqPath = basePath <.> "lq"
      lqsPath = basePath <.> "lqs"

  (path, contents) <- tryLoadFile lqPath `catchIOError` \e ->
    if isDoesNotExistError e
      then tryLoadFile lqsPath
      else ioError e

  parsed <- case takeExtension path of
    ".lq"  -> case parseMExprFile path contents of
      Left err -> error err
      Right m -> pure m
    ".lqs" -> case parseModuleFile path contents of
      Left err -> error err
      Right m -> pure m
    _      -> error $ "Unknown file extension: " ++ path

  let Module _ _ opens defs = parsed
  envImports <- loadTypeImports projectRoot parsed
  let envWithOpens = processOpens opens envImports
  case runTypeCheck (typeCheckModuleWithEnv envWithOpens parsed) of
    Left tcErr -> error $ "Type error in " ++ path ++ ": " ++ show tcErr
    Right envSelf -> do
      let defNames = map defName defs
          localClassNames = [defName d | d@(Definition _ _ body) <- defs, isClass body]
          localInstanceNames = [defName d | d@(Definition _ _ body) <- defs, isInstance body]
          localClassSet = Set.fromList localClassNames
          envValues = foldl (insertQualified alias envSelf) envWithOpens defNames
          envClasses = foldl (insertQualifiedClass alias envSelf) envValues localClassNames
          envFinal = foldl (insertQualifiedInstance alias envSelf localClassSet) envClasses localInstanceNames
      pure envFinal
  where
    tryLoadFile p = do
      c <- TIO.readFile p
      pure (p, c)
    isClass expr = case expr of
      ETypeClass _ _ -> True
      _ -> False
    isInstance expr = case expr of
      EInstance _ _ _ -> True
      _ -> False

insertQualified :: Text -> TCEnv -> TCEnv -> Text -> TCEnv
insertQualified alias envSelf env name =
  case Map.lookup name (tcTypes envSelf) of
    Just scheme ->
      let qualified = qualifyName alias name
          envWithType = env { tcTypes = Map.insert qualified scheme (tcTypes env) }
      in case Map.lookup name (tcDefs envSelf) of
          Just defn -> envWithType { tcDefs = Map.insert qualified defn (tcDefs envWithType) }
          Nothing -> envWithType
    Nothing -> env

insertQualifiedClass :: Text -> TCEnv -> TCEnv -> Text -> TCEnv
insertQualifiedClass alias envSelf env name =
  case Map.lookup name (tcClasses envSelf) of
    Just info ->
      let qualified = qualifyName alias name
          info' = info { className = qualified }
          ty = classType (classParam info)
      in extendClass env qualified info' ty
    Nothing -> env

insertQualifiedInstance :: Text -> TCEnv -> Set.Set Text -> TCEnv -> Text -> TCEnv
insertQualifiedInstance alias envSelf localClasses env name =
  case findInstanceByName envSelf name of
    Nothing -> env
    Just info ->
      let qualifiedName = qualifyName alias name
          className' =
            if instClass info `Set.member` localClasses
              then qualifyName alias (instClass info)
              else instClass info
          info' = info { instName = qualifiedName, instClass = className' }
          ty = EApp (EVar className') [instType info]
      in extendInstance env qualifiedName info' ty

findInstanceByName :: TCEnv -> Text -> Maybe InstanceInfo
findInstanceByName env name =
  let allInstances = concat (Map.elems (tcInstances env))
  in case filter (\info -> instName info == name) allInstances of
      (info:_) -> Just info
      [] -> Nothing

processOpens :: [Open] -> TCEnv -> TCEnv
processOpens opens env = foldl processOneOpen env opens
  where
    processOneOpen e (Open modAlias names) =
      foldl (insertOpen modAlias) e names

    insertOpen modAlias e name =
      let qualifiedName = qualifyName modAlias name
          withValue = case Map.lookup qualifiedName (tcTypes e) of
            Just scheme -> e { tcTypes = Map.insert name scheme (tcTypes e) }
            Nothing -> e
      in case Map.lookup qualifiedName (tcClasses e) of
          Just info -> withValue { tcClasses = Map.insert name info (tcClasses withValue) }
          Nothing -> withValue
