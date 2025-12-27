{-# LANGUAGE OverloadedStrings #-}
module TypeChecker
  ( typeCheckModule
  , typeCheckModuleWithImports
  , annotateModule
  , TypeError(..)
  , TypeEnv
  ) where

import Control.Monad (foldM)
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

-- | Type errors with context
data TypeError
  = VarNotInScope SrcLoc Text TypeEnv
  | TypeMismatch SrcLoc Expr Expr
  | CannotApply SrcLoc Expr Expr
  | NotAFunction SrcLoc Expr
  | NotAComputation SrcLoc Expr
  | ExpectedType SrcLoc Expr
  | MatchCaseError SrcLoc Text

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

  show (MatchCaseError loc msgText) =
    let msg = ErrorMsg loc ("Match error: " <> msgText) Nothing [] Nothing
    in T.unpack (formatError msg)

-- | Eq instance compares by error type (ignoring location for equality)
instance Eq TypeError where
  VarNotInScope _ a _ == VarNotInScope _ b _ = a == b
  TypeMismatch _ a1 a2 == TypeMismatch _ b1 b2 = a1 == b1 && a2 == b2
  CannotApply _ a1 a2 == CannotApply _ b1 b2 = a1 == b1 && a2 == b2
  NotAFunction _ a == NotAFunction _ b = a == b
  NotAComputation _ a == NotAComputation _ b = a == b
  ExpectedType _ a == ExpectedType _ b = a == b
  MatchCaseError _ a == MatchCaseError _ b = a == b
  _ == _ = False

type TypeEnv = Map.Map Text Expr
type DefEnv = Map.Map Text (Transparency, Expr)

data TCEnv = TCEnv
  { tcTypes  :: TypeEnv
  , tcDefs   :: DefEnv
  , tcLocals :: Set.Set Text
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

mkPi :: [Param] -> Expr -> Expr
mkPi params ret = foldr (\(Param name ty) acc -> EForAll name ty acc) ret params

emptyEnv :: TCEnv
emptyEnv = TCEnv buildPrimitiveEnv Map.empty Set.empty

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

lookupVar :: SrcLoc -> TCEnv -> Text -> TypeCheckM Expr
lookupVar loc env v =
  case Map.lookup v (tcTypes env) of
    Just ty -> pure ty
    Nothing -> lift (Left (VarNotInScope loc v (tcTypes env)))

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
  EApp f args ->
    Set.unions (freeVarsWithBound bound f : map (freeVarsWithBound bound) args)
  EFunction params ret body ->
    let (bound', paramsFree) = foldl step (bound, Set.empty) params
        step (b, acc) (Param name ty) =
          let acc' = acc `Set.union` freeVarsWithBound b ty
          in (Set.insert name b, acc')
        retFree = freeVarsWithBound bound' ret
        bodyFree = freeVarsBody bound' body
    in paramsFree `Set.union` retFree `Set.union` bodyFree
  ELet name val body ->
    freeVarsWithBound bound val `Set.union` freeVarsWithBound (Set.insert name bound) body
  ECompute comp -> freeVarsComp bound comp
  EMatch scrut scrutTy cases ->
    let scrutFree = freeVarsWithBound bound scrut
        tyFree = freeVarsWithBound bound scrutTy
        casesFree = Set.unions (map (freeVarsCase bound) cases)
    in scrutFree `Set.union` tyFree `Set.union` casesFree
  EAnnot e ty -> freeVarsWithBound bound e `Set.union` freeVarsWithBound bound ty
  ETyped e ty -> freeVarsWithBound bound e `Set.union` freeVarsWithBound bound ty
  EDict _ impls -> Set.unions [freeVarsWithBound bound e | (_, e) <- impls]
  EDictAccess e _ -> freeVarsWithBound bound e

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
      EApp f args -> EApp <$> go bound f <*> mapM (go bound) args
      EFunction params ret body -> do
        (params', ret', body') <- goParams bound params ret body
        pure (EFunction params' ret' body')
      ELet v val body -> do
        val' <- go bound val
        if v == name
          then pure (ELet v val' body)
          else do
            (v', body') <- refreshBinder bound v body
            body'' <- go (Set.insert v' bound) body'
            pure (ELet v' val' body'')
      ECompute comp -> ECompute <$> goComp bound comp
      EMatch scrut scrutTy cases -> do
        scrut' <- go bound scrut
        scrutTy' <- go bound scrutTy
        cases' <- mapM (goCase bound) cases
        pure (EMatch scrut' scrutTy' cases')
      EAnnot e1 ty -> EAnnot <$> go bound e1 <*> go bound ty
      ETyped e1 ty -> ETyped <$> go bound e1 <*> go bound ty
      EDict cls impls -> do
        impls' <- mapM (\(n, e1) -> (n,) <$> go bound e1) impls
        pure (EDict cls impls')
      EDictAccess e1 method -> EDictAccess <$> go bound e1 <*> pure method

    refreshBinder bound v body =
      if v `Set.member` replacementFree
        then do
          let avoid = Set.unions [replacementFree, freeVars body, bound]
          v' <- freshNameAvoid v avoid
          body' <- subst v (EVar v') body
          pure (v', body')
        else pure (v, body)

    goParams bound params ret body = case params of
      [] -> do
        ret' <- go bound ret
        body' <- goBody bound body
        pure ([], ret', body')
      (Param v ty) : rest -> do
        ty' <- go bound ty
        if v == name
          then pure (Param v ty' : rest, ret, body)
          else do
            (v', rest', ret', body') <- if v `Set.member` replacementFree
              then do
                let avoid = Set.unions [replacementFree, freeVars ret, freeVarsBody Set.empty body, bound]
                v' <- freshNameAvoid v avoid
                rest' <- mapM (renameParam v v') rest
                ret' <- subst v (EVar v') ret
                body' <- renameBody v v' body
                pure (v', rest', ret', body')
              else pure (v, rest, ret, body)
            (restFinal, retFinal, bodyFinal) <- goParams (Set.insert v' bound) rest' ret' body'
            pure (Param v' ty' : restFinal, retFinal, bodyFinal)

    renameParam old newName (Param v ty)
      | v == old = Param v ty <$ pure ()
      | otherwise = Param v <$> subst old (EVar newName) ty

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
      cod' <- normalize env cod
      pure (EForAll v dom' cod')
    EThereExists v dom cod -> do
      dom' <- normalize env dom
      cod' <- normalize env cod
      pure (EThereExists v dom' cod')
    ECompType t -> ECompType <$> normalize env t
    EFunction params ret body -> do
      params' <- mapM (\(Param n ty) -> Param n <$> normalize env ty) params
      ret' <- normalize env ret
      body' <- case body of
        FunctionValue e1 -> FunctionValue <$> normalize env e1
        FunctionCompute c1 -> pure (FunctionCompute c1)
      pure (EFunction params' ret' body')
    ELet v val body -> do
      body' <- subst v val body
      normalize env body'
    EAnnot e1 _ -> normalize env e1
    ETyped e1 _ -> normalize env e1
    EMatch scrut scrutTy cases -> do
      scrut' <- normalize env scrut
      scrutTy' <- normalize env scrutTy
      cases' <- mapM (normalizeCase env) cases
      pure (EMatch scrut' scrutTy' cases')
    _ -> pure wh

normalizeCase :: TCEnv -> MatchCase -> TypeCheckM MatchCase
normalizeCase env caseExpr = case caseExpr of
  MatchEmpty body -> MatchEmpty <$> normalize env body
  MatchFalse body -> MatchFalse <$> normalize env body
  MatchTrue body -> MatchTrue <$> normalize env body
  MatchCons h hTy t tTy body ->
    MatchCons h <$> normalize env hTy <*> pure t <*> normalize env tTy <*> normalize env body
  MatchPair a aTy b bTy body ->
    MatchPair a <$> normalize env aTy <*> pure b <*> normalize env bTy <*> normalize env body

whnf :: TCEnv -> Expr -> TypeCheckM Expr
whnf env expr = case expr of
  EAnnot e1 _ -> whnf env e1
  ETyped e1 _ -> whnf env e1
  ELet v val body -> do
    body' <- subst v val body
    whnf env body'
  EVar v ->
    if v `Set.member` tcLocals env
      then pure expr
      else case Map.lookup v (tcDefs env) of
        Just (Transparent, body) -> whnf env body
        _ -> pure expr
  EApp f args -> do
    f' <- whnf env f
    case f' of
      EFunction params _ (FunctionValue body) ->
        applyParams env params body args
      _ -> pure (mkApp f' args)
  _ -> pure expr

applyParams :: TCEnv -> [Param] -> Expr -> [Expr] -> TypeCheckM Expr
applyParams _env [] body args = pure (mkApp body args)
applyParams env (Param v _ : rest) body args = case args of
  [] -> pure (EFunction (Param v tUnit : rest) tUnit (FunctionValue body))
  (a:as) -> do
    body' <- subst v a body
    applyParams env rest body' as

mkApp :: Expr -> [Expr] -> Expr
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
  (EApp f1 a1, EApp f2 a2) ->
    length a1 == length a2 && alphaEq env f1 f2 && and (zipWith (alphaEq env) a1 a2)
  (EFunction ps1 r1 b1, EFunction ps2 r2 b2) ->
    length ps1 == length ps2
      && let (env', paramsOk) = foldl step (env, True) (zip ps1 ps2)
             step (e, ok) (Param n1 t1, Param n2 t2) =
               (Map.insert n1 n2 e, ok && alphaEq e t1 t2)
         in paramsOk && alphaEq env' r1 r2 && alphaEqBody env' b1 b2
  (ELet v1 val1 body1, ELet v2 val2 body2) ->
    alphaEq env val1 val2 && alphaEq (Map.insert v1 v2 env) body1 body2
  (EMatch s1 ty1 c1, EMatch s2 ty2 c2) ->
    alphaEq env s1 s2 && alphaEq env ty1 ty2 && alphaEqCases env c1 c2
  (EAnnot e1 t1, EAnnot e2 t2) ->
    alphaEq env e1 e2 && alphaEq env t1 t2
  (ETyped e1 t1, ETyped e2 t2) ->
    alphaEq env e1 e2 && alphaEq env t1 t2
  (EDict n1 i1, EDict n2 i2) ->
    n1 == n2 && and (zipWith (\(a1, e1) (a2, e2) -> a1 == a2 && alphaEq env e1 e2) i1 i2)
  (EDictAccess d1 m1, EDictAccess d2 m2) ->
    m1 == m2 && alphaEq env d1 d2
  _ -> False

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
    _ <- inferUniverse env dom
    let env' = extendLocal env v dom
    _ <- inferUniverse env' cod
    let level = max (universeLevel env dom) (universeLevel env' cod)
    pure (ETypeUniverse level)
  EThereExists v dom cod -> do
    _ <- inferUniverse env dom
    let env' = extendLocal env v dom
    _ <- inferUniverse env' cod
    let level = max (universeLevel env dom) (universeLevel env' cod)
    pure (ETypeUniverse level)
  ECompType t -> do
    _ <- inferUniverse env t
    pure (ETypeUniverse (universeLevel env t))
  EApp f args -> do
    fTy <- infer env f
    foldM (applyArg env) fTy args
  EFunction params ret body -> do
    env' <- foldM (\e (Param n ty) -> do
                    _ <- inferUniverse e ty
                    pure (extendLocal e n ty)) env params
    case body of
      FunctionValue e1 -> check env' e1 ret
      FunctionCompute c1 -> do
        inner <- expectCompType env ret
        compTy <- inferComp env' c1
        ensureConv env' compTy inner
    pure (mkPi params ret)
  ELet name val body -> do
    valTy <- infer env val
    infer (extendLocal env name valTy) body
  ECompute comp -> do
    compTy <- inferComp env comp
    pure (ECompType compTy)
  EMatch scrut scrutTy cases -> do
    scrutInferred <- infer env scrut
    ensureConv env scrutInferred scrutTy
    _ <- inferUniverse env scrutTy
    inferMatch env scrutTy cases
  EAnnot e1 ty -> do
    _ <- inferUniverse env ty
    check env e1 ty
    pure ty
  ETyped e1 ty -> do
    check env e1 ty
    pure ty
  EDict _ _ -> lift (Left (MatchCaseError noLoc "dict nodes not supported"))
  EDictAccess _ _ -> lift (Left (MatchCaseError noLoc "dict access nodes not supported"))

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

inferMatch :: TCEnv -> Expr -> [MatchCase] -> TypeCheckM Expr
inferMatch env scrutTy cases = do
  scrutTy' <- whnf env scrutTy
  case scrutTy' of
    ETypeConst TCBoolean -> inferBoolMatch env cases
    EApp (ETypeConst TCList) [elemTy] -> inferListMatch env elemTy cases
    EApp (ETypeConst TCPair) [a, b] -> inferPairMatch env a b cases
    _ -> lift (Left (MatchCaseError noLoc "unsupported scrutinee type"))

inferBoolMatch :: TCEnv -> [MatchCase] -> TypeCheckM Expr
inferBoolMatch env cases = do
  falseBody <- case [body | MatchFalse body <- cases] of
    (body:_) -> pure body
    [] -> lift (Left (MatchCaseError noLoc "missing false-case"))
  trueBody <- case [body | MatchTrue body <- cases] of
    (body:_) -> pure body
    [] -> lift (Left (MatchCaseError noLoc "missing true-case"))
  falseTy <- infer env falseBody
  trueTy <- infer env trueBody
  ensureConv env trueTy falseTy
  pure falseTy

inferListMatch :: TCEnv -> Expr -> [MatchCase] -> TypeCheckM Expr
inferListMatch env elemTy cases = do
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
  emptyTy <- infer env emptyBody
  consTy <- infer (extendLocal (extendLocal env h elemTy) t listTy) consBody
  ensureConv env consTy emptyTy
  pure emptyTy

inferPairMatch :: TCEnv -> Expr -> Expr -> [MatchCase] -> TypeCheckM Expr
inferPairMatch env aTy bTy cases = do
  pairCase <- case [(x, xTy, y, yTy, body) | MatchPair x xTy y yTy body <- cases] of
    (c:_) -> pure c
    [] -> lift (Left (MatchCaseError noLoc "missing pair-case"))
  let (x, xTy, y, yTy, body) = pairCase
  ensureConv env xTy aTy
  ensureConv env yTy bTy
  infer (extendLocal (extendLocal env x aTy) y bTy) body

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

universeLevel :: TCEnv -> Expr -> Int
universeLevel _ tyExpr = case tyExpr of
  ETypeUniverse n -> n
  _ -> 0

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
  , ("add-nat", tFun tNat (tFun tNat tNat))
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
  , ("length-string-prim", tFun tString tNat)
  , ("eq-string-prim", tFun tString (tFun tString tBool))
  , ("split-on-prim", tFun tString (tFun tString (tList tString)))
  , ("join-with-prim", tFun tString (tFun (tList tString) tString))
  , ("trim-prim", tFun tString tString)
  , ("substring-prim", tFun tNat (tFun tNat (tFun tString tString)))
  , ("char-at-prim", tFun tNat (tFun tString tString))
  , ("contains-prim", tFun tString (tFun tString tBool))
  , ("starts-with-prim", tFun tString (tFun tString tBool))
  , ("ends-with-prim", tFun tString (tFun tString tBool))
  , ("index-of-prim", tFun tString (tFun tString tNat))
  , ("reverse-string-prim", tFun tString tString)

  , ("nil-prim", EForAll "A" (tType 0) (tList (EVar "A")))
  , ("cons-prim", EForAll "A" (tType 0) (tFun (EVar "A") (tFun (tList (EVar "A")) (tList (EVar "A")))))
  , ("head-prim", EForAll "A" (tType 0) (tFun (tList (EVar "A")) (EVar "A")))
  , ("tail-prim", EForAll "A" (tType 0) (tFun (tList (EVar "A")) (tList (EVar "A"))))
  , ("length-list-prim", EForAll "A" (tType 0) (tFun (tList (EVar "A")) tNat))
  , ("append-prim", EForAll "A" (tType 0) (tFun (tList (EVar "A")) (tFun (tList (EVar "A")) (tList (EVar "A")))))
  , ("nth-prim", EForAll "A" (tType 0) (tFun tNat (tFun (tList (EVar "A")) (EVar "A"))))
  , ("take-prim", EForAll "A" (tType 0) (tFun tNat (tFun (tList (EVar "A")) (tList (EVar "A")))))
  , ("drop-prim", EForAll "A" (tType 0) (tFun tNat (tFun (tList (EVar "A")) (tList (EVar "A")))))
  , ("last-prim", EForAll "A" (tType 0) (tFun (tList (EVar "A")) (EVar "A")))
  , ("init-prim", EForAll "A" (tType 0) (tFun (tList (EVar "A")) (tList (EVar "A"))))
  , ("drop-until-prim", tFun tString (tFun (tList tString) (tList tString)))

  , ("filter-prim", EForAll "A" (tType 0)
      (tFun (tFun (EVar "A") tBool) (tFun (tList (EVar "A")) (tList (EVar "A")))))
  , ("map-prim", EForAll "A" (tType 0)
      (EForAll "B" (tType 0)
        (tFun (tFun (EVar "A") (EVar "B"))
              (tFun (tList (EVar "A")) (tList (EVar "B"))))))
  , ("fold-prim", EForAll "A" (tType 0)
      (EForAll "B" (tType 0)
        (tFun (tFun (EVar "B") (tFun (EVar "A") (EVar "B")))
              (tFun (EVar "B") (tFun (tList (EVar "A")) (EVar "B"))))))

  , ("not-prim", tFun tBool tBool)
  , ("if-bool-prim", EForAll "A" (tType 0)
      (tFun tBool (tFun (EVar "A") (tFun (EVar "A") (EVar "A")))))
  , ("pair-prim", EForAll "A" (tType 0)
      (EForAll "B" (tType 0)
        (tFun (EVar "A") (tFun (EVar "B") (tPair (EVar "A") (EVar "B"))))))
  , ("fst-prim", EForAll "A" (tType 0)
      (EForAll "B" (tType 0)
        (tFun (tPair (EVar "A") (EVar "B")) (EVar "A"))))
  , ("snd-prim", EForAll "A" (tType 0)
      (EForAll "B" (tType 0)
        (tFun (tPair (EVar "A") (EVar "B")) (EVar "B"))))
  , ("pair-to-list-prim", EForAll "A" (tType 0)
      (tFun (tPair (EVar "A") (EVar "A")) (tList (EVar "A"))))

  , ("match-list-prim", EForAll "A" (tType 0)
      (EForAll "B" (tType 0)
        (tFun (tList (EVar "A"))
              (tFun (tFun tUnit (EVar "B"))
                    (tFun (tFun (EVar "A") (tFun (tList (EVar "A")) (EVar "B")))
                          (EVar "B"))))))
  , ("match-bool-prim", EForAll "B" (tType 0)
      (tFun tBool
            (tFun (tFun tUnit (EVar "B"))
                  (tFun (tFun tUnit (EVar "B"))
                        (EVar "B")))))
  , ("match-pair-prim", EForAll "A" (tType 0)
      (EForAll "B" (tType 0)
        (EForAll "C" (tType 0)
          (tFun (tPair (EVar "A") (EVar "B"))
                (tFun (tFun tUnit (EVar "C"))
                      (tFun (tFun (EVar "A") (tFun (EVar "B") (EVar "C")))
                            (EVar "C")))))))

  , ("print-prim", tFun tString (tComp tUnit))
  , ("read-file-prim", tFun tString (tComp tString))
  , ("write-file-prim", tFun tString (tFun tString (tComp tUnit)))
  , ("shell-prim", tFun tString (tComp tString))

  , ("assert-eq-nat-prim", tFun tNat (tFun tNat (tComp tUnit)))
  , ("assert-eq-string-prim", tFun tString (tFun tString (tComp tUnit)))
  , ("assert-eq-bool-prim", tFun tBool (tFun tBool (tComp tUnit)))

  , ("nat-to-string-prim", tFun tNat tString)

  , ("validate-prim", tFun tString tBool)
  , ("error-prim", EForAll "A" (tType 0) (tFun tString (EVar "A")))
  ]

--------------------------------------------------------------------------------
-- Module checking

typeCheckModule :: Module -> Either TypeError TypeEnv
typeCheckModule m = do
  env <- runTypeCheck (typeCheckModuleWithEnv emptyEnv m)
  pure (tcTypes env)

typeCheckModuleWithImports :: FilePath -> Text -> Module -> IO (Either TypeError TypeEnv)
typeCheckModuleWithImports projectRoot _sourceContents m = do
  importedEnv <- loadTypeImports projectRoot m
  pure $ fmap tcTypes (runTypeCheck (typeCheckModuleWithEnv importedEnv m))

typeCheckModuleWithEnv :: TCEnv -> Module -> TypeCheckM TCEnv
typeCheckModuleWithEnv initialEnv (Module _name _imports _opens defs) =
  foldM typeCheckDef initialEnv defs

typeCheckDef :: TCEnv -> Definition -> TypeCheckM TCEnv
typeCheckDef env (Definition tr name body) = do
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
  pure emptyEnv { tcTypes = mergedTypes, tcDefs = mergedDefs }

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
          envFinal = foldl (insertQualified alias envSelf) envWithOpens defNames
      pure envFinal
  where
    tryLoadFile p = do
      c <- TIO.readFile p
      pure (p, c)

insertQualified :: Text -> TCEnv -> TCEnv -> Text -> TCEnv
insertQualified alias envSelf env name =
  case Map.lookup name (tcTypes envSelf) of
    Just scheme ->
      env { tcTypes = Map.insert (qualifyName alias name) scheme (tcTypes env) }
    Nothing -> env

processOpens :: [Open] -> TCEnv -> TCEnv
processOpens opens env = foldl processOneOpen env opens
  where
    processOneOpen e (Open modAlias names) =
      foldl (insertOpen modAlias) e names

    insertOpen modAlias e name =
      let qualifiedName = qualifyName modAlias name
      in case Map.lookup qualifiedName (tcTypes e) of
          Just scheme -> e { tcTypes = Map.insert name scheme (tcTypes e) }
          Nothing ->
            error $ "open: qualified name not found: " <> T.unpack qualifiedName <>
                    "\nAvailable keys: " <> show (take 20 $ Map.keys (tcTypes e))
