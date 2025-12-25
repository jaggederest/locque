{-# LANGUAGE OverloadedStrings #-}
module TypeChecker
  ( typeCheckModule
  , typeCheckModuleWithImports
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
  _ == _ = False

-- | Fresh variable counter for generating unique type variables
type FreshCounter = Integer

-- | Type checking monad with fresh variable state
type TypeCheckM a = StateT FreshCounter (Either TypeError) a

-- | Run type checking computation with initial counter
runTypeCheck :: TypeCheckM a -> Either TypeError a
runTypeCheck tc = evalStateT tc 0

-- | Lift Either TypeError into TypeCheckM
liftTC :: Either TypeError a -> TypeCheckM a
liftTC = lift

-- | Generate a fresh type variable name
freshVar :: Text -> TypeCheckM Text
freshVar base = do
  counter <- get
  modify (+ 1)
  pure (base <> "$" <> T.pack (show counter))

-- | Generate N fresh variables from a list of base names
freshVars :: [Text] -> TypeCheckM [Text]
freshVars = mapM freshVar

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
  scheme <- case mType of
    Just s  -> do
      -- Check that body matches declared type
      checkDefinition env kind s body
      pure s
    Nothing ->
      -- Infer type from body
      inferDefinition env kind body
  -- Add to environment
  pure (Map.insert name scheme env)

-- | Infer type scheme for definition (no annotation)
inferDefinition :: TypeEnv -> DefKind -> Either Expr Comp -> TypeCheckM TypeScheme
inferDefinition env ValueDef (Left expr) = do
  ty <- inferExpr env expr
  pure (generalize env ty)
inferDefinition env ComputationDef (Right comp) = do
  ty <- inferComp env comp
  pure (generalize env ty)
inferDefinition _ _ _ = liftTC (Left (KindMismatch noLoc ValueDef))

-- | Check definition against declared type
checkDefinition :: TypeEnv -> DefKind -> TypeScheme -> Either Expr Comp -> TypeCheckM ()
checkDefinition env ValueDef (TypeScheme vars ty) (Left expr) = do
  ty' <- instantiate vars ty
  checkExpr env expr ty'
checkDefinition env ComputationDef (TypeScheme vars ty) (Right comp) = do
  ty' <- instantiate vars ty
  case ty' of
    TComp innerTy -> checkComp env comp innerTy
    _ -> liftTC (Left (TypeMismatch noLoc (TComp (TVar "a")) ty'))
checkDefinition _ kind _ _ = liftTC (Left (KindMismatch noLoc kind))

-- | BIDIRECTIONAL CHECKING: Synthesis mode (infer type)
inferExpr :: TypeEnv -> Expr -> TypeCheckM Type
inferExpr env (EVar v) =
  case Map.lookup v env of
    Nothing -> liftTC (Left (VarNotInScope noLoc v env))
    Just (TypeScheme vars ty) -> instantiate vars ty

inferExpr _ (ELit lit) = pure $ case lit of
  LNat _    -> TNat
  LString _ -> TString
  LBool _   -> TBool

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
  TNat -> TNat
  TString -> TString
  TBool -> TBool
  TUnit -> TUnit
  TList t -> TList (subst v replacement t)
  TPair a b -> TPair (subst v replacement a) (subst v replacement b)
  TFun a b -> TFun (subst v replacement a) (subst v replacement b)
  TVar x -> if x == v then replacement else TVar x
  TForAll x t -> if x == v then TForAll x t else TForAll x (subst v replacement t)
  TComp t -> TComp (subst v replacement t)

-- | Type substitution map
type Subst = Map.Map Text Type

-- | Apply substitution to a type
applySubst :: Subst -> Type -> Type
applySubst s ty = case ty of
  TNat -> TNat
  TString -> TString
  TBool -> TBool
  TUnit -> TUnit
  TList t -> TList (applySubst s t)
  TPair a b -> TPair (applySubst s a) (applySubst s b)
  TFun a b -> TFun (applySubst s a) (applySubst s b)
  TVar x -> case Map.lookup x s of
    Just t -> t
    Nothing -> TVar x
  TForAll x t -> TForAll x (applySubst (Map.delete x s) t)
  TComp t -> TComp (applySubst s t)

-- | Compose substitutions (apply s2 then s1)
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.union (Map.map (applySubst s1) s2) s1

-- | Unification algorithm
unify :: Type -> Type -> TypeCheckM Subst
unify t1 t2 = case (t1, t2) of
  (TNat, TNat) -> pure Map.empty
  (TString, TString) -> pure Map.empty
  (TBool, TBool) -> pure Map.empty
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

  _ -> liftTC (Left (TypeMismatch noLoc t1 t2))

-- | Bind a type variable to a type (with occurs check)
bindVar :: Text -> Type -> TypeCheckM Subst
bindVar x t
  | TVar x == t = pure Map.empty
  | x `Set.member` freeVars t = liftTC (Left (OccursCheck noLoc x t))
  | otherwise = pure (Map.singleton x t)

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

-- | Convert module name to file path (Some::Module::Name -> some/module/name)
modNameToPath :: Text -> FilePath
modNameToPath modName =
  let withSlashes = T.replace "::" "/" modName
      lowercased = T.toLower withSlashes
  in T.unpack lowercased

-- | Insert a type-checked definition with qualified names
-- Looks up the type from envSelf and inserts qualified versions
insertQualified :: Text -> Text -> TypeEnv -> TypeEnv -> Text -> TypeEnv
insertQualified alias modName envSelf env defName =
  case Map.lookup defName envSelf of
    Just scheme ->
      -- Only insert qualified name with alias
      let names = [qualName alias defName]
      in foldl (\acc n -> Map.insert n scheme acc) env names
    Nothing -> env  -- Definition not in type-checked environment
  where
    qualName :: Text -> Text -> Text
    qualName prefix n = prefix <> "." <> n

-- Process open statements to bring unqualified type names into scope
processTypeOpens :: [Open] -> TypeEnv -> TypeEnv
processTypeOpens opens env = foldl processOneOpen env opens
  where
    processOneOpen :: TypeEnv -> Open -> TypeEnv
    processOneOpen e (Open modAlias nameList) =
      foldl (openOneName modAlias) e nameList

    openOneName :: Text -> TypeEnv -> Text -> TypeEnv
    openOneName modAlias e name =
      let qualifiedName = modAlias <> "." <> name
      in case Map.lookup qualifiedName e of
           Just scheme -> Map.insert name scheme e
           Nothing ->
             -- DEBUG: Print when name not found
             error $ "open: qualified name not found: " <> T.unpack qualifiedName <>
                     "\nAvailable keys: " <> show (take 20 $ Map.keys e)

--------------------------------------------------------------------------------
-- Primitive type environment

-- | Build primitive environment with all primitive type signatures
buildPrimitiveEnv :: TypeEnv
buildPrimitiveEnv = Map.fromList
  -- Arithmetic (Nat -> Nat -> Nat)
  [ ("add-nat-prim", mono (TFun TNat (TFun TNat TNat)))
  , ("sub-nat-prim", mono (TFun TNat (TFun TNat TNat)))
  , ("mul-nat-prim", mono (TFun TNat (TFun TNat TNat)))
  , ("div-nat-prim", mono (TFun TNat (TFun TNat TNat)))
  , ("mod-nat-prim", mono (TFun TNat (TFun TNat TNat)))

  -- Comparison (Nat -> Nat -> Bool)
  , ("eq-nat-prim", mono (TFun TNat (TFun TNat TBool)))
  , ("lt-nat-prim", mono (TFun TNat (TFun TNat TBool)))
  , ("le-nat-prim", mono (TFun TNat (TFun TNat TBool)))
  , ("gt-nat-prim", mono (TFun TNat (TFun TNat TBool)))
  , ("ge-nat-prim", mono (TFun TNat (TFun TNat TBool)))

  -- String operations
  , ("concat-string-prim", mono (TFun TString (TFun TString TString)))
  , ("length-string-prim", mono (TFun TString TNat))
  , ("eq-string-prim", mono (TFun TString (TFun TString TBool)))
  , ("split-on-prim", mono (TFun TString (TFun TString (TList TString))))
  , ("join-with-prim", mono (TFun TString (TFun (TList TString) TString)))
  , ("trim-prim", mono (TFun TString TString))
  , ("substring-prim", mono (TFun TNat (TFun TNat (TFun TString TString))))
  , ("char-at-prim", mono (TFun TNat (TFun TString TString)))
  , ("contains-prim", mono (TFun TString (TFun TString TBool)))
  , ("starts-with-prim", mono (TFun TString (TFun TString TBool)))
  , ("ends-with-prim", mono (TFun TString (TFun TString TBool)))
  , ("index-of-prim", mono (TFun TString (TFun TString TNat)))
  , ("reverse-string-prim", mono (TFun TString TString))

  -- Polymorphic list operations: ∀a. ...
  , ("nil-prim", poly ["a"] (TList (TVar "a")))
  , ("cons-prim", poly ["a"] (TFun (TVar "a") (TFun (TList (TVar "a")) (TList (TVar "a")))))
  , ("head-prim", poly ["a"] (TFun (TList (TVar "a")) (TVar "a")))
  , ("tail-prim", poly ["a"] (TFun (TList (TVar "a")) (TList (TVar "a"))))
  , ("length-list-prim", poly ["a"] (TFun (TList (TVar "a")) TNat))
  , ("append-prim", poly ["a"] (TFun (TList (TVar "a")) (TFun (TList (TVar "a")) (TList (TVar "a")))))
  , ("nth-prim", poly ["a"] (TFun TNat (TFun (TList (TVar "a")) (TVar "a"))))
  , ("take-prim", poly ["a"] (TFun TNat (TFun (TList (TVar "a")) (TList (TVar "a")))))
  , ("drop-prim", poly ["a"] (TFun TNat (TFun (TList (TVar "a")) (TList (TVar "a")))))
  , ("last-prim", poly ["a"] (TFun (TList (TVar "a")) (TVar "a")))
  , ("init-prim", poly ["a"] (TFun (TList (TVar "a")) (TList (TVar "a"))))
  , ("drop-until-prim", mono (TFun TString (TFun (TList TString) (TList TString))))

  -- Higher-order functions: ∀a b. ...
  , ("map-prim", poly ["a", "b"]
      (TFun (TFun (TVar "a") (TVar "b"))
            (TFun (TList (TVar "a")) (TList (TVar "b")))))
  , ("filter-prim", poly ["a"]
      (TFun (TFun (TVar "a") TBool)
            (TFun (TList (TVar "a")) (TList (TVar "a")))))
  , ("fold-prim", poly ["a", "b"]
      (TFun (TFun (TVar "b") (TFun (TVar "a") (TVar "b")))
            (TFun (TVar "b") (TFun (TList (TVar "a")) (TVar "b")))))

  -- Boolean
  , ("not-prim", mono (TFun TBool TBool))
  , ("if-bool-prim", poly ["a"]
      (TFun TBool (TFun (TVar "a") (TFun (TVar "a") (TVar "a")))))

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
      (TFun TBool
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
  , ("assert-eq-nat-prim", mono (TFun TNat (TFun TNat (TComp TUnit))))
  , ("assert-eq-string-prim", mono (TFun TString (TFun TString (TComp TUnit))))

  -- Misc
  , ("tt-prim", mono TUnit)
  , ("tt", mono TUnit)
  , ("validate-prim", mono (TFun TString TBool))
  , ("error-prim", poly ["a"] (TFun TString (TVar "a")))
  ]
  where
    mono t = TypeScheme [] t
    poly vars t = TypeScheme vars t
