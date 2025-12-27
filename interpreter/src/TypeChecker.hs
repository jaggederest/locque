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
import Type
import Parser (parseModuleFile, parseMExprFile)
import SourceLoc
import ErrorMsg
import Utils (modNameToPath, qualifyName)

-- | Type errors with context
data TypeError
  = VarNotInScope SrcLoc Text TypeEnv    -- Added location + env for suggestions
  | TypeMismatch SrcLoc Type Type        -- Added location
  | CannotApply SrcLoc Type Type         -- Added location
  | NotAFunction SrcLoc Type             -- Added location
  | KindMismatch SrcLoc DefKind          -- Added location
  | OccursCheck SrcLoc Text Type         -- Added location
  | PolymorphicInstantiationError SrcLoc Text  -- Added location
  | UnexpectedAnnotation SrcLoc          -- Added location
  -- Typeclass errors
  | DuplicateMethod SrcLoc Text Text     -- class name, method name
  | UnknownTypeClass SrcLoc Text         -- class name
  | MissingMethod SrcLoc Text Text Text  -- class name, instance type, method name
  | ExtraMethod SrcLoc Text Text Text    -- class name, instance type, method name
  | MethodTypeMismatch SrcLoc Text Text Type Type  -- class, method, expected, actual
  -- Constraint resolution errors
  | NoMatchingInstance SrcLoc Text Type  -- class name, concrete type
  | AmbiguousInstance SrcLoc Text Type   -- class name, type (multiple matches)
  | UnresolvedConstraint SrcLoc Constraint  -- constraint could not be resolved

-- | Pretty-print type errors
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

  show (KindMismatch loc kind) =
    let msg = ErrorMsg loc ("Kind mismatch: expected " <> T.pack (show kind)) Nothing [] Nothing
    in T.unpack (formatError msg)

  show (OccursCheck loc var ty) =
    let mainMsg = "Occurs check failed"
        note = Just $ var <> " occurs in " <> prettyType ty
        msg = ErrorMsg loc mainMsg Nothing [] note
    in T.unpack (formatError msg)

  show (PolymorphicInstantiationError loc txt) =
    let msg = ErrorMsg loc ("Polymorphic instantiation error: " <> txt) Nothing [] Nothing
    in T.unpack (formatError msg)

  show (UnexpectedAnnotation loc) =
    let msg = ErrorMsg loc "Unexpected annotation" Nothing
                [Hint "Lambda requires type annotation for parameter"] Nothing
    in T.unpack (formatError msg)

  show (DuplicateMethod loc className methodName) =
    let msg = ErrorMsg loc ("Duplicate method '" <> methodName <> "' in typeclass '" <> className <> "'") Nothing [] Nothing
    in T.unpack (formatError msg)

  show (UnknownTypeClass loc className) =
    let msg = ErrorMsg loc ("Unknown type class: " <> className) Nothing [] Nothing
    in T.unpack (formatError msg)

  show (MissingMethod loc className instType methodName) =
    let msg = ErrorMsg loc ("Missing method '" <> methodName <> "' in instance " <> className <> " " <> instType)
              Nothing [] Nothing
    in T.unpack (formatError msg)

  show (ExtraMethod loc className instType methodName) =
    let msg = ErrorMsg loc ("Extra method '" <> methodName <> "' in instance " <> className <> " " <> instType <>
              " (not declared in typeclass)") Nothing [] Nothing
    in T.unpack (formatError msg)

  show (MethodTypeMismatch loc className methodName expected actual) =
    let note = Just $ "Expected: " <> prettyType expected <>
                      "\n  Actual:   " <> prettyType actual
        msg = ErrorMsg loc ("Method '" <> methodName <> "' in instance " <> className <>
              " has wrong type") Nothing [] note
    in T.unpack (formatError msg)

  show (NoMatchingInstance loc className ty) =
    let msg = ErrorMsg loc ("No instance of '" <> className <> "' for type " <> prettyType ty)
              Nothing [] Nothing
    in T.unpack (formatError msg)

  show (AmbiguousInstance loc className ty) =
    let msg = ErrorMsg loc ("Ambiguous instance of '" <> className <> "' for type " <> prettyType ty)
              Nothing [Hint "Multiple instances match this type"] Nothing
    in T.unpack (formatError msg)

  show (UnresolvedConstraint loc constraint) =
    let msg = ErrorMsg loc ("Unresolved constraint: " <> prettyConstraint constraint)
              Nothing [Hint "The constraint has unbound type variables"] Nothing
    in T.unpack (formatError msg)

-- Eq instance compares by error type (ignoring location for equality)
instance Eq TypeError where
  VarNotInScope _ a _ == VarNotInScope _ b _ = a == b
  TypeMismatch _ a1 a2 == TypeMismatch _ b1 b2 = a1 == b1 && a2 == b2
  CannotApply _ a1 a2 == CannotApply _ b1 b2 = a1 == b1 && a2 == b2
  NotAFunction _ a == NotAFunction _ b = a == b
  KindMismatch _ a == KindMismatch _ b = a == b
  OccursCheck _ a1 a2 == OccursCheck _ b1 b2 = a1 == b1 && a2 == b2
  PolymorphicInstantiationError _ a == PolymorphicInstantiationError _ b = a == b
  UnexpectedAnnotation _ == UnexpectedAnnotation _ = True
  DuplicateMethod _ a1 a2 == DuplicateMethod _ b1 b2 = a1 == b1 && a2 == b2
  UnknownTypeClass _ a == UnknownTypeClass _ b = a == b
  MissingMethod _ a1 a2 a3 == MissingMethod _ b1 b2 b3 = a1 == b1 && a2 == b2 && a3 == b3
  ExtraMethod _ a1 a2 a3 == ExtraMethod _ b1 b2 b3 = a1 == b1 && a2 == b2 && a3 == b3
  MethodTypeMismatch _ a1 a2 a3 a4 == MethodTypeMismatch _ b1 b2 b3 b4 =
    a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4
  NoMatchingInstance _ a1 a2 == NoMatchingInstance _ b1 b2 = a1 == b1 && a2 == b2
  AmbiguousInstance _ a1 a2 == AmbiguousInstance _ b1 b2 = a1 == b1 && a2 == b2
  UnresolvedConstraint _ a == UnresolvedConstraint _ b = a == b
  _ == _ = False

-- | Fresh variable counter for generating unique type variables
type FreshCounter = Integer

-- | Type family environment: maps family name to its definition
type TypeFamilyEnv = Map.Map Text TypeFamilyBody

-- | Type class environment: maps class name to its methods
type TypeClassEnv = Map.Map Text TypeClassBody

-- | Instance environment: maps class name to list of instances
type InstanceEnv = Map.Map Text [InstanceBody]

-- | Full type checker state
data TCState = TCState
  { tcFreshCounter :: FreshCounter
  , tcFamilyEnv    :: TypeFamilyEnv
  , tcClassEnv     :: TypeClassEnv
  , tcInstEnv      :: InstanceEnv
  , tcConstraints  :: [Constraint]  -- Collected constraints to solve
  , tcCurrentSubst :: Subst         -- Current substitution from unification
  }

-- | Initial type checker state
initialTCState :: TCState
initialTCState = TCState 0 Map.empty Map.empty Map.empty [] Map.empty

-- | Type checking monad with full state
type TypeCheckM a = StateT TCState (Either TypeError) a

-- | Run type checking computation with initial state
runTypeCheck :: TypeCheckM a -> Either TypeError a
runTypeCheck tc = evalStateT tc initialTCState

-- | Lift Either TypeError into TypeCheckM
liftTC :: Either TypeError a -> TypeCheckM a
liftTC = lift

-- | Generate a fresh type variable name
freshVar :: Text -> TypeCheckM Text
freshVar base = do
  counter <- gets tcFreshCounter
  modify (\s -> s { tcFreshCounter = tcFreshCounter s + 1 })
  pure (base <> "$" <> T.pack (show counter))

-- | Generate N fresh variables from a list of base names
freshVars :: [Text] -> TypeCheckM [Text]
freshVars = mapM freshVar

-- | Add a constraint to the collection
addConstraint :: Constraint -> TypeCheckM ()
addConstraint c = modify (\s -> s { tcConstraints = c : tcConstraints s })

-- | Get all collected constraints
getConstraints :: TypeCheckM [Constraint]
getConstraints = gets tcConstraints

-- | Clear collected constraints
clearConstraints :: TypeCheckM ()
clearConstraints = modify (\s -> s { tcConstraints = [] })

-- | Update the current substitution
updateSubst :: Subst -> TypeCheckM ()
updateSubst newSubst = modify (\s -> s { tcCurrentSubst = composeSubst newSubst (tcCurrentSubst s) })

-- | Get the current substitution
getCurrentSubst :: TypeCheckM Subst
getCurrentSubst = gets tcCurrentSubst

-- | Main entry point: type check entire module (without imports)
-- For testing/simple cases. Use typeCheckModuleWithImports for full checking.
typeCheckModule :: Module -> Either TypeError TypeEnv
typeCheckModule m = runTypeCheck (typeCheckModuleWithEnv buildPrimitiveEnv m)

-- | Type check module with imports loaded from filesystem
-- Source contents parameter for future context display (currently unused)
typeCheckModuleWithImports :: FilePath -> Text -> Module -> IO (Either TypeError TypeEnv)
typeCheckModuleWithImports projectRoot _sourceContents m = do
  importedEnv <- loadTypeImports projectRoot m
  pure $ runTypeCheck (typeCheckModuleWithEnv importedEnv m)

-- | Type check a module given an initial type environment (includes primitives and imports)
typeCheckModuleWithEnv :: TypeEnv -> Module -> TypeCheckM TypeEnv
typeCheckModuleWithEnv initialEnv (Module _name _imports _opens defs) = do
  -- Type check all definitions with the initial environment
  typeCheckDefs initialEnv defs

-- | Type check all definitions, threading environment
typeCheckDefs :: TypeEnv -> [Definition] -> TypeCheckM TypeEnv
typeCheckDefs env [] = pure env
typeCheckDefs env (def:defs) = do
  env' <- typeCheckDef env def
  typeCheckDefs env' defs

-- | Type check a single definition and add to environment
typeCheckDef :: TypeEnv -> Definition -> TypeCheckM TypeEnv
typeCheckDef env (Definition _ name kind mType body) = do
  -- Clear constraints at start of each definition
  clearConstraints
  scheme <- case mType of
    Just s  -> do
      -- Check that body matches declared type
      checkDefinition env kind s body
      pure s
    Nothing ->
      -- Infer type from body
      inferDefinition env name kind body
  -- Resolve collected constraints before completing this definition
  -- (Skip for type family, typeclass, and instance definitions - they don't generate constraints)
  case kind of
    ValueDef -> resolveAllConstraints
    ComputationDef -> resolveAllConstraints
    _ -> pure ()
  -- Add to environment
  pure (Map.insert name scheme env)

-- | Infer type scheme for definition (no annotation)
inferDefinition :: TypeEnv -> Text -> DefKind -> DefBody -> TypeCheckM TypeScheme
inferDefinition env _ ValueDef (ValueBody expr) = do
  ty <- inferExpr env expr
  pure (generalize env ty)
inferDefinition env _ ComputationDef (ComputationBody comp) = do
  ty <- inferComp env comp
  pure (generalize env ty)
inferDefinition _ name FamilyDef (FamilyBody tfBody) = do
  -- Register type family in environment
  registerTypeFamily name tfBody
  pure (TypeScheme [] TUnit)
inferDefinition _ name TypeClassDef (ClassBody tcBody) = do
  -- Register type class in environment
  registerTypeClass name tcBody
  pure (TypeScheme [] TUnit)
inferDefinition _ name InstanceDef (InstBody instBody) = do
  -- Register instance in environment
  registerInstance name instBody
  pure (TypeScheme [] TUnit)
inferDefinition _ _ _ _ = liftTC (Left (KindMismatch noLoc ValueDef))

-- | Register a type family in the environment
registerTypeFamily :: Text -> TypeFamilyBody -> TypeCheckM ()
registerTypeFamily name body = do
  modify (\s -> s { tcFamilyEnv = Map.insert name body (tcFamilyEnv s) })

-- | Register a type class in the environment with validation
registerTypeClass :: Text -> TypeClassBody -> TypeCheckM ()
registerTypeClass name body = do
  let methodNames = map fst (tcbMethods body)
  case findDuplicates methodNames of
    Just dupName -> liftTC (Left (DuplicateMethod noLoc name dupName))
    Nothing -> modify (\s -> s { tcClassEnv = Map.insert name body (tcClassEnv s) })

-- | Find first duplicate in a list, if any
findDuplicates :: [Text] -> Maybe Text
findDuplicates [] = Nothing
findDuplicates (x:xs)
  | x `elem` xs = Just x
  | otherwise   = findDuplicates xs

-- | Register an instance in the environment with validation
registerInstance :: Text -> InstanceBody -> TypeCheckM ()
registerInstance _defName body = do
  let className = instClassName body
  classEnv <- gets tcClassEnv
  case Map.lookup className classEnv of
    Nothing -> liftTC (Left (UnknownTypeClass noLoc className))
    Just (TypeClassBody typeParam methods) -> do
      -- Validate method coverage
      let implNames = map fst (instImpls body)
          methodNames = map fst methods
          missing = filter (`notElem` implNames) methodNames
          extra = filter (`notElem` methodNames) implNames
      case (missing, extra) of
        (m:_, _) -> liftTC (Left (MissingMethod noLoc className (prettyType (instType body)) m))
        ([], e:_) -> liftTC (Left (ExtraMethod noLoc className (prettyType (instType body)) e))
        ([], []) -> do
          -- Type check method implementations
          typeCheckInstanceMethods className (instType body) typeParam methods (instImpls body)
          modify (\s -> s { tcInstEnv = Map.insertWith (++) className [body] (tcInstEnv s) })

-- | Type check all method implementations against class signatures
typeCheckInstanceMethods :: Text -> Type -> Text -> [(Text, Type)] -> [(Text, Expr)] -> TypeCheckM ()
typeCheckInstanceMethods _className instanceType typeParam methods impls = do
  let checkEnv = buildPrimitiveEnv
  mapM_ (checkMethod checkEnv) impls
  where
    checkMethod :: TypeEnv -> (Text, Expr) -> TypeCheckM ()
    checkMethod env (methodName, implExpr) = do
      case lookup methodName methods of
        Nothing -> pure ()  -- Already checked for extra methods above
        Just methodSig -> do
          -- Substitute class type variable with instance type
          let instantiatedSig = subst typeParam instanceType methodSig
          -- Type check the implementation
          checkExpr env implExpr instantiatedSig

-- | Check definition against declared type
checkDefinition :: TypeEnv -> DefKind -> TypeScheme -> DefBody -> TypeCheckM ()
checkDefinition env ValueDef (TypeScheme vars ty) (ValueBody expr) = do
  ty' <- instantiate vars ty
  checkExpr env expr ty'
checkDefinition env ComputationDef (TypeScheme vars ty) (ComputationBody comp) = do
  ty' <- instantiate vars ty
  case ty' of
    TComp innerTy -> checkComp env comp innerTy
    _ -> liftTC (Left (TypeMismatch noLoc (TComp (TVar "a")) ty'))
checkDefinition _ FamilyDef _ (FamilyBody _) = pure ()  -- Type families checked separately
checkDefinition _ TypeClassDef _ (ClassBody _) = pure ()  -- Type classes checked separately
checkDefinition _ InstanceDef _ (InstBody _) = pure ()  -- Instances checked separately
checkDefinition _ kind _ _ = liftTC (Left (KindMismatch noLoc kind))

-- | BIDIRECTIONAL CHECKING: Synthesis mode (infer type)
inferExpr :: TypeEnv -> Expr -> TypeCheckM Type
inferExpr env (EVar v) =
  case Map.lookup v env of
    Just (TypeScheme vars ty) -> instantiate vars ty
    Nothing -> do
      -- Variable not in scope - check if it's a typeclass method
      classEnv <- gets tcClassEnv
      case findMethodInClasses v classEnv of
        Just (className, methodType, typeParam) -> do
          -- Generate fresh type variable for the class parameter
          freshTyVar <- freshVar typeParam
          -- Add constraint: ClassName freshTyVar
          addConstraint (Constraint className (TVar freshTyVar))
          -- Substitute type param with fresh var in method type
          pure (subst typeParam (TVar freshTyVar) methodType)
        Nothing -> liftTC (Left (VarNotInScope noLoc v env))

inferExpr _ (ELit lit) = pure $ case lit of
  LNatural _    -> TNatural
  LString _ -> TString
  LBoolean _   -> TBoolean

inferExpr env (ELam param (Just paramType) body) = do
  let env' = Map.insert param (TypeScheme [] paramType) env
  bodyType <- inferExpr env' body
  pure (TFun paramType bodyType)

inferExpr _ (ELam param Nothing _) =
  -- Phase 1: require annotation; Phase 2+: generate fresh variable
  -- This should only be hit if lambda is used in synthesis mode
  liftTC (Left (PolymorphicInstantiationError noLoc ("Unannotated lambda parameter: " <> param)))

inferExpr env (ELamMulti params (Just paramType) body) = do
  -- Multi-param lambda desugars to nested function types
  -- paramType should be the full function type (a -> b -> c)
  inferNestedLambdas env params paramType body
  where
    inferNestedLambdas :: TypeEnv -> [Text] -> Type -> Expr -> TypeCheckM Type
    inferNestedLambdas _ [] ty _ = pure ty
    inferNestedLambdas e [p] ty b = do
      case ty of
        TFun pTy retTy -> do
          let e' = Map.insert p (TypeScheme [] pTy) e
          bodyType <- inferExpr e' b
          unify retTy bodyType
          pure ty
        _ -> liftTC (Left (TypeMismatch noLoc (TFun (TVar "a") (TVar "b")) ty))
    inferNestedLambdas e (p:ps) ty b = do
      case ty of
        TFun pTy restTy -> do
          let e' = Map.insert p (TypeScheme [] pTy) e
          inferNestedLambdas e' ps restTy (ELamMulti ps Nothing b)
          pure ty
        _ -> liftTC (Left (TypeMismatch noLoc (TFun (TVar "a") (TVar "b")) ty))

inferExpr _ (ELamMulti params Nothing _) =
  liftTC (Left (PolymorphicInstantiationError noLoc ("Unannotated multi-param lambda: " <> head params)))

inferExpr env (EApp f args) = do
  -- Special case: if function is unannotated lambda, infer arg type first
  case (f, args) of
    (ELam param Nothing body, [arg]) -> do
      -- Infer argument type
      argType <- inferExpr env arg
      -- Check lambda body with parameter bound to argument type
      let env' = Map.insert param (TypeScheme [] argType) env
      inferExpr env' body
    _ -> do
      -- General case: infer function type, then check arguments
      fType <- inferExpr env f
      foldM checkApp fType args
  where
    checkApp fnType arg = do
      case fnType of
        TFun paramType retType -> do
          checkExpr env arg paramType
          pure retType
        _ -> liftTC (Left (NotAFunction noLoc fnType))

inferExpr env (EAnnot expr ty) = do
  checkExpr env expr ty
  pure ty

-- ETyped: Already has inferred type, just validate the inner expression
inferExpr env (ETyped expr ty) = do
  checkExpr env expr ty
  pure ty

-- EDict: Dictionary nodes shouldn't appear during type checking
-- They're generated by DictPass after type checking
inferExpr _ (EDict className _) =
  liftTC (Left (PolymorphicInstantiationError noLoc ("Unexpected dictionary node: " <> className)))

-- EDictAccess: Same as EDict - shouldn't appear during type checking
inferExpr _ (EDictAccess _ methodName) =
  liftTC (Left (PolymorphicInstantiationError noLoc ("Unexpected dictionary access: " <> methodName)))

-- | BIDIRECTIONAL CHECKING: Checking mode (verify against expected type)
checkExpr :: TypeEnv -> Expr -> Type -> TypeCheckM ()
-- Special case: check lambda without annotation against function type
checkExpr env (ELam param Nothing body) (TFun paramType retType) = do
  let env' = Map.insert param (TypeScheme [] paramType) env
  checkExpr env' body retType

-- NEW CASE: check lambda WITH annotation against function type
checkExpr env (ELam param (Just paramType) body) (TFun expectedParamType expectedReturnType) = do
  -- Verify annotated parameter type matches expected type
  _subst <- unify paramType expectedParamType
  -- Extend environment with parameter at annotated type
  let env' = Map.insert param (TypeScheme [] paramType) env
  -- Check body against expected return type
  checkExpr env' body expectedReturnType

-- Multi-param lambda without annotation
checkExpr env (ELamMulti params Nothing body) expectedType = do
  checkNestedLambdas env params expectedType body
  where
    checkNestedLambdas :: TypeEnv -> [Text] -> Type -> Expr -> TypeCheckM ()
    checkNestedLambdas e [p] (TFun pTy retTy) b = do
      let e' = Map.insert p (TypeScheme [] pTy) e
      checkExpr e' b retTy
    checkNestedLambdas e (p:ps) (TFun pTy restTy) b = do
      let e' = Map.insert p (TypeScheme [] pTy) e
      checkNestedLambdas e' ps restTy b
    checkNestedLambdas _ _ ty _ =
      liftTC (Left (NotAFunction noLoc ty))

-- Multi-param lambda with annotation
checkExpr env (ELamMulti params (Just annotatedType) body) expectedType = do
  _subst <- unify annotatedType expectedType
  checkNestedLambdasAnnotated env params annotatedType body
  where
    checkNestedLambdasAnnotated :: TypeEnv -> [Text] -> Type -> Expr -> TypeCheckM ()
    checkNestedLambdasAnnotated e [p] (TFun pTy retTy) b = do
      let e' = Map.insert p (TypeScheme [] pTy) e
      checkExpr e' b retTy
    checkNestedLambdasAnnotated e (p:ps) (TFun pTy restTy) b = do
      let e' = Map.insert p (TypeScheme [] pTy) e
      checkNestedLambdasAnnotated e' ps restTy b
    checkNestedLambdasAnnotated _ _ ty _ =
      liftTC (Left (NotAFunction noLoc ty))

-- General case: infer and unify
checkExpr env expr expected = do
  actual <- inferExpr env expr
  -- Use unification to allow type variables to be solved
  _subst <- unify expected actual
  pure ()

-- | Type checking for computations (infer mode)
inferComp :: TypeEnv -> Comp -> TypeCheckM Type
inferComp env (CReturn expr) = do
  ty <- inferExpr env expr
  pure (TComp ty)

inferComp env (CBind var comp1 comp2) = do
  ty1 <- inferComp env comp1
  case ty1 of
    TComp innerTy -> do
      let env' = Map.insert var (TypeScheme [] innerTy) env
      inferComp env' comp2
    _ -> liftTC (Left (TypeMismatch noLoc (TComp (TVar "a")) ty1))

inferComp env (CPerform expr) = do
  ty <- inferExpr env expr
  -- The expression should evaluate to a computation type
  case ty of
    TComp _ -> pure ty
    _ -> liftTC (Left (TypeMismatch noLoc (TComp (TVar "a")) ty))

inferComp env (CVar v) =
  case Map.lookup v env of
    Nothing -> liftTC (Left (VarNotInScope noLoc v env))
    Just (TypeScheme vars ty) -> do
      ty' <- instantiate vars ty
      -- If it's a computation type, return as-is; otherwise lift to computation
      case ty' of
        TComp _ -> pure ty'
        _ -> pure (TComp ty')

inferComp env (CSeq comp1 comp2) = do
  _ <- inferComp env comp1
  inferComp env comp2

-- | Type checking for computations (checking mode)
checkComp :: TypeEnv -> Comp -> Type -> TypeCheckM ()
checkComp env comp expected = do
  actual <- inferComp env comp
  -- Use unification to allow type variables to be solved
  _subst <- unify expected actual
  pure ()

-- | Generalization: abstract free type variables
generalize :: TypeEnv -> Type -> TypeScheme
generalize env ty =
  let envVars = foldMap freeVarsScheme (Map.elems env)
      tyVars = freeVars ty Set.\\ envVars
  in TypeScheme (Set.toList tyVars) ty

-- | Instantiation: replace quantified variables with fresh ones
instantiate :: [Text] -> Type -> TypeCheckM Type
instantiate [] ty = pure ty
instantiate vars ty = do
  fresh <- freshVars vars
  pure (substMany (zip vars fresh) ty)

-- | Substitute multiple type variables at once
substMany :: [(Text, Text)] -> Type -> Type
substMany substs ty = foldr (\(old, new) t -> subst old (TVar new) t) ty substs

-- | Substitute a type variable with a type
subst :: Text -> Type -> Type -> Type
subst v replacement ty = case ty of
  TNatural -> TNatural
  TString -> TString
  TBoolean -> TBoolean
  TUnit -> TUnit
  TList t -> TList (subst v replacement t)
  TPair a b -> TPair (subst v replacement a) (subst v replacement b)
  TFun a b -> TFun (subst v replacement a) (subst v replacement b)
  TVar x -> if x == v then replacement else TVar x
  TForAll x t -> if x == v then TForAll x t else TForAll x (subst v replacement t)
  TComp t -> TComp (subst v replacement t)
  TFamilyApp name args -> TFamilyApp name (map (subst v replacement) args)
  TConstrained cs inner ->
    TConstrained (map (substConstraint v replacement) cs) (subst v replacement inner)

-- | Substitute in a constraint
substConstraint :: Text -> Type -> Constraint -> Constraint
substConstraint v replacement (Constraint cls ty) =
  Constraint cls (subst v replacement ty)

-- | Type substitution map
type Subst = Map.Map Text Type

-- | Apply substitution to a type
applySubst :: Subst -> Type -> Type
applySubst s ty = case ty of
  TNatural -> TNatural
  TString -> TString
  TBoolean -> TBoolean
  TUnit -> TUnit
  TList t -> TList (applySubst s t)
  TPair a b -> TPair (applySubst s a) (applySubst s b)
  TFun a b -> TFun (applySubst s a) (applySubst s b)
  TVar x -> case Map.lookup x s of
    Just t -> t
    Nothing -> TVar x
  TForAll x t -> TForAll x (applySubst (Map.delete x s) t)
  TComp t -> TComp (applySubst s t)
  TFamilyApp name args -> TFamilyApp name (map (applySubst s) args)
  TConstrained cs inner ->
    TConstrained (map (applySubstConstraint s) cs) (applySubst s inner)

-- | Apply substitution to a constraint
applySubstConstraint :: Subst -> Constraint -> Constraint
applySubstConstraint s (Constraint cls ty) = Constraint cls (applySubst s ty)

-- | Compose substitutions (apply s2 then s1)
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.union (Map.map (applySubst s1) s2) s1

-- ============================================================================
-- Type Family Reduction
-- ============================================================================

-- | Match a type pattern against a concrete type (one-way matching)
-- Returns Just substitution if match succeeds, Nothing otherwise
matchTypePattern :: Type -> Type -> Maybe Subst
matchTypePattern pattern target = case (pattern, target) of
  -- Variables in pattern match anything
  (TVar v, t) -> Just (Map.singleton v t)

  -- Same constructors must match structurally
  (TNatural, TNatural) -> Just Map.empty
  (TString, TString) -> Just Map.empty
  (TBoolean, TBoolean) -> Just Map.empty
  (TUnit, TUnit) -> Just Map.empty

  (TList pInner, TList tInner) -> matchTypePattern pInner tInner

  (TPair p1 p2, TPair t1 t2) -> do
    s1 <- matchTypePattern p1 t1
    s2 <- matchTypePattern (applySubst s1 p2) (applySubst s1 t2)
    Just (composeSubst s2 s1)

  (TFun p1 p2, TFun t1 t2) -> do
    s1 <- matchTypePattern p1 t1
    s2 <- matchTypePattern (applySubst s1 p2) (applySubst s1 t2)
    Just (composeSubst s2 s1)

  (TComp p, TComp t) -> matchTypePattern p t

  -- Mismatch
  _ -> Nothing

-- | Match multiple patterns against multiple arguments
matchTypePatterns :: [Type] -> [Type] -> Maybe Subst
matchTypePatterns [] [] = Just Map.empty
matchTypePatterns (p:ps) (t:ts) = do
  s1 <- matchTypePattern p t
  s2 <- matchTypePatterns (map (applySubst s1) ps) (map (applySubst s1) ts)
  Just (composeSubst s2 s1)
matchTypePatterns _ _ = Nothing  -- Arity mismatch

-- | Reduce a type family application to its result type
reduceTypeFamily :: Text -> [Type] -> TypeCheckM Type
reduceTypeFamily familyName args = do
  familyEnv <- gets tcFamilyEnv
  case Map.lookup familyName familyEnv of
    Nothing -> liftTC (Left (PolymorphicInstantiationError noLoc
                              ("Unknown type family: " <> familyName)))
    Just (TypeFamilyBody _kind cases) -> tryMatchCases cases
  where
    tryMatchCases [] = liftTC (Left (PolymorphicInstantiationError noLoc
                                      ("No matching case for type family " <> familyName)))
    tryMatchCases (TypeFamilyCase patterns result : rest) =
      case matchTypePatterns patterns args of
        Just subst -> pure (applySubst subst result)
        Nothing -> tryMatchCases rest

-- | Normalize a type by reducing all type family applications
normalizeType :: Type -> TypeCheckM Type
normalizeType ty = case ty of
  TFamilyApp name args -> do
    normalizedArgs <- mapM normalizeType args
    result <- reduceTypeFamily name normalizedArgs
    normalizeType result  -- Result might contain more families
  TList t -> TList <$> normalizeType t
  TPair a b -> TPair <$> normalizeType a <*> normalizeType b
  TFun a b -> TFun <$> normalizeType a <*> normalizeType b
  TComp t -> TComp <$> normalizeType t
  TForAll v t -> TForAll v <$> normalizeType t
  TConstrained cs inner -> do
    normalizedInner <- normalizeType inner
    normalizedCs <- mapM normalizeConstraint cs
    pure (TConstrained normalizedCs normalizedInner)
  t -> pure t
  where
    normalizeConstraint (Constraint cls ty') = do
      normalizedTy <- normalizeType ty'
      pure (Constraint cls normalizedTy)

-- ============================================================================
-- Unification
-- ============================================================================

-- | Unification algorithm (normalizes type families before unifying)
unify :: Type -> Type -> TypeCheckM Subst
unify t1 t2 = do
  -- Normalize type family applications before unifying
  n1 <- normalizeType t1
  n2 <- normalizeType t2
  unifyNormalized n1 n2

-- | Unification on normalized types (no TFamilyApp)
unifyNormalized :: Type -> Type -> TypeCheckM Subst
unifyNormalized t1 t2 = case (t1, t2) of
  (TNatural, TNatural) -> pure Map.empty
  (TString, TString) -> pure Map.empty
  (TBoolean, TBoolean) -> pure Map.empty
  (TUnit, TUnit) -> pure Map.empty

  (TList a, TList b) -> unify a b
  (TPair a1 a2, TPair b1 b2) -> do
    s1 <- unify a1 b1
    s2 <- unify (applySubst s1 a2) (applySubst s1 b2)
    pure (composeSubst s2 s1)

  (TFun a1 a2, TFun b1 b2) -> do
    s1 <- unify a1 b1
    s2 <- unify (applySubst s1 a2) (applySubst s1 b2)
    pure (composeSubst s2 s1)

  (TComp a, TComp b) -> unify a b

  (TVar x, t) -> bindVar x t
  (t, TVar x) -> bindVar x t

  -- TFamilyApp should be normalized away, but handle for completeness
  (TFamilyApp name1 _, TFamilyApp name2 _) | name1 == name2 ->
    liftTC (Left (PolymorphicInstantiationError noLoc
                   ("Cannot unify unreduced type family: " <> name1)))

  -- TConstrained unification: constraints must match, inner types unify
  (TConstrained cs1 inner1, TConstrained cs2 inner2) ->
    if constraintsEqual cs1 cs2
      then unify inner1 inner2
      else liftTC (Left (TypeMismatch noLoc t1 t2))

  -- Collect constraints during unification instead of stripping them
  (TConstrained cs inner, t) -> do
    mapM_ addConstraint cs  -- Collect constraints
    unify inner t
  (t, TConstrained cs inner) -> do
    mapM_ addConstraint cs  -- Collect constraints
    unify t inner

  _ -> liftTC (Left (TypeMismatch noLoc t1 t2))

-- | Check if two constraint lists are equal (order-independent)
constraintsEqual :: [Constraint] -> [Constraint] -> Bool
constraintsEqual cs1 cs2 =
  length cs1 == length cs2 && all (`elem` cs2) cs1

-- ============================================================================
-- Instance Resolution
-- ============================================================================

-- | Find instance that matches a constraint
findMatchingInstance :: Constraint -> TypeCheckM (Maybe InstanceBody)
findMatchingInstance (Constraint className ty) = do
  instEnv <- gets tcInstEnv
  case Map.lookup className instEnv of
    Nothing -> pure Nothing
    Just instances -> findMatch ty instances

-- | Try to match concrete type against instance types
findMatch :: Type -> [InstanceBody] -> TypeCheckM (Maybe InstanceBody)
findMatch _ty [] = pure Nothing
findMatch ty (inst:rest) = do
  -- Try to match ty with instType inst using type pattern matching
  case matchTypePattern (instType inst) ty of
    Just _ -> pure (Just inst)  -- Found a matching instance
    Nothing -> findMatch ty rest

-- | Check if a constraint has concrete (ground) type
isConcreteConstraint :: Constraint -> Bool
isConcreteConstraint (Constraint _ ty) = Set.null (freeVars ty)

-- | Resolve a single constraint by finding matching instance
resolveConstraint :: Constraint -> TypeCheckM ()
resolveConstraint (Constraint className ty) = do
  -- Apply current substitutions to get concrete type
  currentSubst <- getCurrentSubst
  let concreteType = applySubst currentSubst ty

  -- Try to find matching instance
  result <- findMatchingInstance (Constraint className concreteType)
  case result of
    Nothing -> liftTC (Left (NoMatchingInstance noLoc className concreteType))
    Just _inst -> pure ()  -- Constraint satisfied

-- | Resolve all collected constraints
resolveAllConstraints :: TypeCheckM ()
resolveAllConstraints = do
  constraints <- getConstraints
  -- Apply current substitutions to all constraints first
  currentSubst <- getCurrentSubst
  let groundedConstraints = map (applySubstConstraint currentSubst) constraints
  -- Filter to constraints that are now concrete (no free vars in type)
  let (concrete, deferred) = partition isConcreteConstraint groundedConstraints
  -- Resolve concrete constraints
  mapM_ resolveConstraint concrete
  -- For now, error on deferred constraints (future: keep as obligations)
  case deferred of
    [] -> pure ()
    (c:_) -> liftTC (Left (UnresolvedConstraint noLoc c))
  where
    partition p xs = (filter p xs, filter (not . p) xs)

-- | Find a method in any registered type class
-- Returns (className, methodType, typeParam) if found
findMethodInClasses :: Text -> TypeClassEnv -> Maybe (Text, Type, Text)
findMethodInClasses methodName classEnv =
  foldr checkClass Nothing (Map.toList classEnv)
  where
    checkClass (className, TypeClassBody typeParam methods) acc =
      case lookup methodName methods of
        Just methodType -> Just (className, methodType, typeParam)
        Nothing -> acc

-- | Bind a type variable to a type (with occurs check)
bindVar :: Text -> Type -> TypeCheckM Subst
bindVar x t
  | TVar x == t = pure Map.empty
  | x `Set.member` freeVars t = liftTC (Left (OccursCheck noLoc x t))
  | otherwise = do
      let s = Map.singleton x t
      -- Update global substitution state
      updateSubst s
      pure s

--------------------------------------------------------------------------------
-- Import loading for type checking

-- | Load type environments from all imports
loadTypeImports :: FilePath -> Module -> IO TypeEnv
loadTypeImports projectRoot (Module _ imports _ _) = do
  envs <- mapM (loadTypeImport projectRoot) imports
  pure $ Map.unions (buildPrimitiveEnv : envs)

-- | Load a single import's type environment
loadTypeImport :: FilePath -> Import -> IO TypeEnv
loadTypeImport projectRoot (Import modName alias) = do
  let modPath = modNameToPath modName
      -- If module path starts with "test/", use projectRoot directly
      -- Otherwise, use projectRoot </> "lib"
      basePath = if "test/" `isPrefixOf` modPath
                 then projectRoot </> modPath
                 else projectRoot </> "lib" </> modPath
      lqPath = basePath <.> "lq"
      lqsPath = basePath <.> "lqs"

  -- Try .lq first, fall back to .lqs if not found
  (path, contents) <- tryLoadFile lqPath `catchIOError` \e ->
    if isDoesNotExistError e
      then tryLoadFile lqsPath
      else ioError e

  -- Parse the module
  parsed <- case takeExtension path of
    ".lq"  -> case parseMExprFile path contents of
      Left err -> error err
      Right m -> pure m
    ".lqs" -> case parseModuleFile path contents of
      Left err -> error err
      Right m -> pure m
    _      -> error $ "Unknown file extension: " ++ path

  -- Recursively load imports and type check
  let Module name _ opens defs = parsed
  envImports <- loadTypeImports projectRoot parsed
  -- Process opens BEFORE type-checking so unqualified names are available
  let envWithOpens = processTypeOpens opens envImports
  case runTypeCheck (typeCheckModuleWithEnv envWithOpens parsed) of
    Left tcErr -> error $ "Type error in " ++ path ++ ": " ++ show tcErr
    Right envSelf -> do
      -- Insert all type-checked definitions with qualified names
      -- For each definition that was type-checked, add qualified versions
      let defNames = map defName defs
          envFinal = foldl (insertQualified alias name envSelf) envWithOpens defNames
      pure envFinal
  where
    tryLoadFile p = do
      c <- TIO.readFile p
      pure (p, c)

insertQualified :: Text -> Text -> TypeEnv -> TypeEnv -> Text -> TypeEnv
insertQualified alias _modName envSelf env defName =
  case Map.lookup defName envSelf of
    Just scheme -> Map.insert (qualifyName alias defName) scheme env
    Nothing -> env

processTypeOpens :: [Open] -> TypeEnv -> TypeEnv
processTypeOpens opens env = foldl processOneOpen env opens
  where
    processOneOpen :: TypeEnv -> Open -> TypeEnv
    processOneOpen e (Open modAlias nameList) =
      foldl (openOneName modAlias) e nameList

    openOneName :: Text -> TypeEnv -> Text -> TypeEnv
    openOneName modAlias e name =
      let qualifiedName = qualifyName modAlias name
      in case Map.lookup qualifiedName e of
           Just scheme -> Map.insert name scheme e
           Nothing ->
             error $ "open: qualified name not found: " <> T.unpack qualifiedName <>
                     "\nAvailable keys: " <> show (take 20 $ Map.keys e)

--------------------------------------------------------------------------------
-- Primitive type environment

-- | Build primitive environment with all primitive type signatures
buildPrimitiveEnv :: TypeEnv
buildPrimitiveEnv = Map.fromList
  -- Arithmetic (Nat -> Nat -> Nat)
  [ ("add-nat-prim", mono (TFun TNatural (TFun TNatural TNatural)))
  , ("sub-nat-prim", mono (TFun TNatural (TFun TNatural TNatural)))
  , ("mul-nat-prim", mono (TFun TNatural (TFun TNatural TNatural)))
  , ("div-nat-prim", mono (TFun TNatural (TFun TNatural TNatural)))
  , ("mod-nat-prim", mono (TFun TNatural (TFun TNatural TNatural)))

  -- Comparison (Nat -> Nat -> Bool)
  , ("eq-nat-prim", mono (TFun TNatural (TFun TNatural TBoolean)))
  , ("lt-nat-prim", mono (TFun TNatural (TFun TNatural TBoolean)))
  , ("le-nat-prim", mono (TFun TNatural (TFun TNatural TBoolean)))
  , ("gt-nat-prim", mono (TFun TNatural (TFun TNatural TBoolean)))
  , ("ge-nat-prim", mono (TFun TNatural (TFun TNatural TBoolean)))

  -- String operations
  , ("concat-string-prim", mono (TFun TString (TFun TString TString)))
  , ("length-string-prim", mono (TFun TString TNatural))
  , ("eq-string-prim", mono (TFun TString (TFun TString TBoolean)))
  , ("split-on-prim", mono (TFun TString (TFun TString (TList TString))))
  , ("join-with-prim", mono (TFun TString (TFun (TList TString) TString)))
  , ("trim-prim", mono (TFun TString TString))
  , ("substring-prim", mono (TFun TNatural (TFun TNatural (TFun TString TString))))
  , ("char-at-prim", mono (TFun TNatural (TFun TString TString)))
  , ("contains-prim", mono (TFun TString (TFun TString TBoolean)))
  , ("starts-with-prim", mono (TFun TString (TFun TString TBoolean)))
  , ("ends-with-prim", mono (TFun TString (TFun TString TBoolean)))
  , ("index-of-prim", mono (TFun TString (TFun TString TNatural)))
  , ("reverse-string-prim", mono (TFun TString TString))

  -- Polymorphic list operations: ∀a. ...
  , ("nil-prim", poly ["a"] (TList (TVar "a")))
  , ("cons-prim", poly ["a"] (TFun (TVar "a") (TFun (TList (TVar "a")) (TList (TVar "a")))))
  , ("head-prim", poly ["a"] (TFun (TList (TVar "a")) (TVar "a")))
  , ("tail-prim", poly ["a"] (TFun (TList (TVar "a")) (TList (TVar "a"))))
  , ("length-list-prim", poly ["a"] (TFun (TList (TVar "a")) TNatural))
  , ("append-prim", poly ["a"] (TFun (TList (TVar "a")) (TFun (TList (TVar "a")) (TList (TVar "a")))))
  , ("nth-prim", poly ["a"] (TFun TNatural (TFun (TList (TVar "a")) (TVar "a"))))
  , ("take-prim", poly ["a"] (TFun TNatural (TFun (TList (TVar "a")) (TList (TVar "a")))))
  , ("drop-prim", poly ["a"] (TFun TNatural (TFun (TList (TVar "a")) (TList (TVar "a")))))
  , ("last-prim", poly ["a"] (TFun (TList (TVar "a")) (TVar "a")))
  , ("init-prim", poly ["a"] (TFun (TList (TVar "a")) (TList (TVar "a"))))
  , ("drop-until-prim", mono (TFun TString (TFun (TList TString) (TList TString))))

  -- Higher-order functions: ∀a b. ...
  , ("map-prim", poly ["a", "b"]
      (TFun (TFun (TVar "a") (TVar "b"))
            (TFun (TList (TVar "a")) (TList (TVar "b")))))
  , ("filter-prim", poly ["a"]
      (TFun (TFun (TVar "a") TBoolean)
            (TFun (TList (TVar "a")) (TList (TVar "a")))))
  , ("fold-prim", poly ["a", "b"]
      (TFun (TFun (TVar "b") (TFun (TVar "a") (TVar "b")))
            (TFun (TVar "b") (TFun (TList (TVar "a")) (TVar "b")))))

  -- Boolean
  , ("not-prim", mono (TFun TBoolean TBoolean))
  , ("if-bool-prim", poly ["a"]
      (TFun TBoolean (TFun (TVar "a") (TFun (TVar "a") (TVar "a")))))

  -- Pairs
  , ("pair-prim", poly ["a", "b"]
      (TFun (TVar "a") (TFun (TVar "b") (TPair (TVar "a") (TVar "b")))))
  , ("fst-prim", poly ["a", "b"] (TFun (TPair (TVar "a") (TVar "b")) (TVar "a")))
  , ("snd-prim", poly ["a", "b"] (TFun (TPair (TVar "a") (TVar "b")) (TVar "b")))
  , ("pair-to-list-prim", poly ["a"]
      (TFun (TPair (TVar "a") (TVar "a")) (TList (TVar "a"))))

  -- Pattern matching: Type-specific match primitives
  -- List matching: ∀a b. List a -> (() -> b) -> (a -> List a -> b) -> b
  , ("match-list-prim", poly ["a", "b"]
      (TFun (TList (TVar "a"))
            (TFun (TFun TUnit (TVar "b"))
                  (TFun (TFun (TVar "a") (TFun (TList (TVar "a")) (TVar "b")))
                        (TVar "b")))))

  -- Bool matching: ∀b. Bool -> (() -> b) -> (() -> b) -> b
  , ("match-bool-prim", poly ["b"]
      (TFun TBoolean
            (TFun (TFun TUnit (TVar "b"))
                  (TFun (TFun TUnit (TVar "b"))
                        (TVar "b")))))

  -- Pair matching: ∀a b c. Pair a b -> (() -> c) -> (a -> b -> c) -> c
  , ("match-pair-prim", poly ["a", "b", "c"]
      (TFun (TPair (TVar "a") (TVar "b"))
            (TFun (TFun TUnit (TVar "c"))
                  (TFun (TFun (TVar "a") (TFun (TVar "b") (TVar "c")))
                        (TVar "c")))))

  -- Old unified match (kept for backward compatibility)
  , ("match-prim", poly ["a", "b"]
      (TFun (TVar "a")  -- scrutinee
            (TFun (TFun TUnit (TVar "b"))  -- empty/false case
                  (TFun (TFun (TVar "a") (TFun (TVar "a") (TVar "b")))  -- non-empty case (simplified)
                        (TVar "b")))))

  -- IO operations (return Comp types)
  , ("print-prim", mono (TFun TString (TComp TUnit)))
  , ("read-file-prim", mono (TFun TString (TComp TString)))
  , ("write-file-prim", mono (TFun TString (TFun TString (TComp TUnit))))
  , ("shell-prim", mono (TFun TString (TComp TString)))

  -- Assertions
  , ("assert-eq-nat-prim", mono (TFun TNatural (TFun TNatural (TComp TUnit))))
  , ("assert-eq-string-prim", mono (TFun TString (TFun TString (TComp TUnit))))
  , ("assert-eq-bool-prim", mono (TFun TBoolean (TFun TBoolean (TComp TUnit))))

  -- Conversion/Display
  , ("nat-to-string-prim", mono (TFun TNatural TString))

  -- Misc
  , ("tt-prim", mono TUnit)
  , ("tt", mono TUnit)
  , ("validate-prim", mono (TFun TString TBoolean))
  , ("error-prim", poly ["a"] (TFun TString (TVar "a")))
  ]
  where
    mono t = TypeScheme [] t
    poly vars t = TypeScheme vars t

--------------------------------------------------------------------------------
-- Type Annotation Pass
--
-- After type checking succeeds, this pass wraps expressions with ETyped
-- so that DictPass can use type information for typeclass method resolution.
--------------------------------------------------------------------------------

-- | Annotate a module with inferred types
-- This runs after type checking succeeds and wraps expressions with ETyped
annotateModule :: TypeEnv -> Module -> Either TypeError Module
annotateModule env m = runTypeCheck $ do
  -- First register typeclasses and instances so method names are in scope
  mapM_ registerDefsForAnnotation (modDefs m)
  -- Then annotate all definitions
  defs' <- mapM (annotateDef env) (modDefs m)
  pure m { modDefs = defs' }

-- | Register typeclasses and instances for the annotation pass
registerDefsForAnnotation :: Definition -> TypeCheckM ()
registerDefsForAnnotation def = case defBody def of
  ClassBody body -> registerTypeClass (defName def) body
  InstBody body -> registerInstance (defName def) body
  _ -> pure ()

-- | Annotate a single definition
annotateDef :: TypeEnv -> Definition -> TypeCheckM Definition
annotateDef env def = case defBody def of
  ValueBody expr -> do
    expr' <- annotateExpr env expr
    pure def { defBody = ValueBody expr' }
  ComputationBody comp -> do
    comp' <- annotateComp env comp
    pure def { defBody = ComputationBody comp' }
  _ -> pure def  -- TypeFamily, TypeClass, Instance unchanged

-- | Annotate an expression with its inferred type
-- This is conservative: only wraps with ETyped when type inference is straightforward.
-- Unannotated lambdas in synthesis mode are handled by recursing without wrapping.
annotateExpr :: TypeEnv -> Expr -> TypeCheckM Expr
annotateExpr env expr = case expr of
  -- Literals: infer type and wrap
  ELit lit -> do
    let ty = case lit of
               LNatural _ -> TNatural
               LString _  -> TString
               LBoolean _ -> TBoolean
    pure (ETyped expr ty)

  -- Variables: look up type and wrap
  EVar v -> case Map.lookup v env of
    Just (TypeScheme vars ty) -> do
      ty' <- instantiate vars ty
      pure (ETyped expr ty')
    Nothing -> do
      -- Check if it's a typeclass method
      classEnv <- gets tcClassEnv
      case findMethodInClasses v classEnv of
        Just (className, methodType, typeParam) -> do
          freshTyVar <- freshVar typeParam
          addConstraint (Constraint className (TVar freshTyVar))
          let ty = subst typeParam (TVar freshTyVar) methodType
          pure (ETyped expr ty)
        Nothing -> pure expr  -- Unknown var, don't wrap

  -- Applications: annotate and try to infer result type
  EApp f args -> do
    f' <- annotateExpr env f
    args' <- mapM (annotateExpr env) args
    let app' = EApp f' args'
    -- Try to get function type from annotated function
    case getFunctionType f' of
      Just fTy -> do
        let resultTy = getResultType fTy (length args)
        pure (ETyped app' resultTy)
      Nothing -> pure app'  -- Can't determine type, don't wrap

  -- Annotated lambdas: infer and wrap
  ELam p (Just pTy) body -> do
    let env' = Map.insert p (TypeScheme [] pTy) env
    body' <- annotateExpr env' body
    let retTy = getExprType body'
        lamTy = TFun pTy retTy
    pure (ETyped (ELam p (Just pTy) body') lamTy)

  -- Unannotated lambdas: just recurse, can't infer in synthesis mode
  ELam p Nothing body -> do
    body' <- annotateExpr env body
    pure (ELam p Nothing body')

  -- Multi-param lambdas: similar treatment
  ELamMulti ps (Just ty) body -> do
    let paramTypes = extractParamTypes ty ps
        env' = foldr (\(p, pTy) e -> Map.insert p (TypeScheme [] pTy) e) env paramTypes
    body' <- annotateExpr env' body
    pure (ETyped (ELamMulti ps (Just ty) body') ty)

  ELamMulti ps Nothing body -> do
    body' <- annotateExpr env body
    pure (ELamMulti ps Nothing body')

  -- Annotation: use provided type
  EAnnot e t -> do
    e' <- annotateExpr env e
    pure (ETyped (EAnnot e' t) t)

  -- Already typed: just recurse
  ETyped e t -> do
    e' <- annotateExpr env e
    pure (ETyped e' t)

  -- Dictionaries: recurse
  EDict className impls -> do
    impls' <- mapM (\(n, e) -> do { e' <- annotateExpr env e; pure (n, e') }) impls
    pure (EDict className impls')

  -- Dict access: recurse
  EDictAccess d method -> do
    d' <- annotateExpr env d
    pure (EDictAccess d' method)

-- | Get type from ETyped wrapper if present
getExprType :: Expr -> Type
getExprType (ETyped _ ty) = ty
getExprType _ = TVar "unknown"

-- | Get function type from wrapped expression
getFunctionType :: Expr -> Maybe Type
getFunctionType (ETyped _ ty@(TFun _ _)) = Just ty
getFunctionType (ETyped _ ty) = Just ty  -- Might be a type var that will unify
getFunctionType _ = Nothing

-- | Get result type after applying n arguments to a function type
getResultType :: Type -> Int -> Type
getResultType ty 0 = ty
getResultType (TFun _ ret) n = getResultType ret (n - 1)
getResultType ty _ = ty  -- Not enough function arrows

-- | Extract parameter types from a function type
extractParamTypes :: Type -> [Text] -> [(Text, Type)]
extractParamTypes ty params = go ty params
  where
    go _ [] = []
    go (TFun pTy rest) (p:ps) = (p, pTy) : go rest ps
    go _ (p:ps) = (p, TVar "unknown") : go (TVar "unknown") ps

-- | Annotate a computation with inferred types on its expressions
annotateComp :: TypeEnv -> Comp -> TypeCheckM Comp
annotateComp env comp = case comp of
  CReturn e -> do
    e' <- annotateExpr env e
    pure (CReturn e')
  CBind v c1 c2 -> do
    -- First annotate c1, then c2
    -- Note: we don't know c1's result type precisely here, but we can still annotate
    c1' <- annotateComp env c1
    -- For bind, we need to infer the type of c1 to extend env for c2
    -- This is a simplification - just annotate without extending env
    c2' <- annotateComp env c2
    pure (CBind v c1' c2')
  CPerform e -> do
    e' <- annotateExpr env e
    pure (CPerform e')
  CSeq c1 c2 -> do
    c1' <- annotateComp env c1
    c2' <- annotateComp env c2
    pure (CSeq c1' c2')
  CVar _ -> pure comp
