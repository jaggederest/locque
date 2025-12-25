{-# LANGUAGE OverloadedStrings #-}
module TypeChecker
  ( typeCheckModule
  , typeCheckModuleWithImports
  , TypeError(..)
  , TypeEnv
  ) where

import Control.Monad (foldM)
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

-- | Type errors with context
data TypeError
  = VarNotInScope Text
  | TypeMismatch Type Type
  | CannotApply Type Type
  | NotAFunction Type
  | KindMismatch DefKind
  | OccursCheck Text Type
  | PolymorphicInstantiationError Text
  | UnexpectedAnnotation

-- | Pretty-print type errors
instance Show TypeError where
  show (VarNotInScope v) = "Variable not in scope: " ++ T.unpack v
  show (TypeMismatch expected actual) =
    "Type mismatch:\n  Expected: " ++ T.unpack (prettyType expected) ++
    "\n  Actual:   " ++ T.unpack (prettyType actual)
  show (CannotApply fType argType) =
    "Cannot apply function of type " ++ T.unpack (prettyType fType) ++
    " to argument of type " ++ T.unpack (prettyType argType)
  show (NotAFunction ty) =
    "Expected a function type, but got: " ++ T.unpack (prettyType ty)
  show (KindMismatch kind) =
    "Kind mismatch: expected " ++ show kind
  show (OccursCheck var ty) =
    "Occurs check failed: " ++ T.unpack var ++ " occurs in " ++ T.unpack (prettyType ty)
  show (PolymorphicInstantiationError msg) =
    "Polymorphic instantiation error: " ++ T.unpack msg
  show UnexpectedAnnotation =
    "Unexpected annotation: lambda requires type annotation for parameter in Phase 1"

-- Eq instance still uses structural equality
instance Eq TypeError where
  VarNotInScope a == VarNotInScope b = a == b
  TypeMismatch a1 a2 == TypeMismatch b1 b2 = a1 == b1 && a2 == b2
  CannotApply a1 a2 == CannotApply b1 b2 = a1 == b1 && a2 == b2
  NotAFunction a == NotAFunction b = a == b
  KindMismatch a == KindMismatch b = a == b
  OccursCheck a1 a2 == OccursCheck b1 b2 = a1 == b1 && a2 == b2
  PolymorphicInstantiationError a == PolymorphicInstantiationError b = a == b
  UnexpectedAnnotation == UnexpectedAnnotation = True
  _ == _ = False

-- | Main entry point: type check entire module (without imports)
-- For testing/simple cases. Use typeCheckModuleWithImports for full checking.
typeCheckModule :: Module -> Either TypeError TypeEnv
typeCheckModule = typeCheckModuleWithEnv buildPrimitiveEnv

-- | Type check module with imports loaded from filesystem
typeCheckModuleWithImports :: FilePath -> Module -> IO (Either TypeError TypeEnv)
typeCheckModuleWithImports projectRoot m = do
  importedEnv <- loadTypeImports projectRoot m
  pure $ typeCheckModuleWithEnv importedEnv m

-- | Type check a module given an initial type environment (includes primitives and imports)
typeCheckModuleWithEnv :: TypeEnv -> Module -> Either TypeError TypeEnv
typeCheckModuleWithEnv initialEnv (Module _name _imports defs) = do
  -- Type check all definitions with the initial environment
  typeCheckDefs initialEnv defs

-- | Type check all definitions, threading environment
typeCheckDefs :: TypeEnv -> [Definition] -> Either TypeError TypeEnv
typeCheckDefs env [] = Right env
typeCheckDefs env (def:defs) = do
  env' <- typeCheckDef env def
  typeCheckDefs env' defs

-- | Type check a single definition and add to environment
typeCheckDef :: TypeEnv -> Definition -> Either TypeError TypeEnv
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
inferDefinition :: TypeEnv -> DefKind -> Either Expr Comp -> Either TypeError TypeScheme
inferDefinition env ValueDef (Left expr) = do
  ty <- inferExpr env expr
  pure (generalize env ty)
inferDefinition env ComputationDef (Right comp) = do
  ty <- inferComp env comp
  pure (generalize env ty)
inferDefinition _ _ _ = Left (KindMismatch ValueDef)

-- | Check definition against declared type
checkDefinition :: TypeEnv -> DefKind -> TypeScheme -> Either Expr Comp -> Either TypeError ()
checkDefinition env ValueDef (TypeScheme vars ty) (Left expr) = do
  ty' <- instantiate vars ty
  checkExpr env expr ty'
checkDefinition env ComputationDef (TypeScheme vars ty) (Right comp) = do
  ty' <- instantiate vars ty
  case ty' of
    TComp innerTy -> checkComp env comp innerTy
    _ -> Left (TypeMismatch (TComp (TVar "a")) ty')
checkDefinition _ kind _ _ = Left (KindMismatch kind)

-- | BIDIRECTIONAL CHECKING: Synthesis mode (infer type)
inferExpr :: TypeEnv -> Expr -> Either TypeError Type
inferExpr env (EVar v) =
  case Map.lookup v env of
    Nothing -> Left (VarNotInScope v)
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
  Left (PolymorphicInstantiationError ("Unannotated lambda parameter: " <> param))

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
        _ -> Left (NotAFunction fnType)

inferExpr env (EAnnot expr ty) = do
  checkExpr env expr ty
  pure ty

-- | BIDIRECTIONAL CHECKING: Checking mode (verify against expected type)
checkExpr :: TypeEnv -> Expr -> Type -> Either TypeError ()
-- Special case: check lambda without annotation against function type
checkExpr env (ELam param Nothing body) (TFun paramType retType) = do
  let env' = Map.insert param (TypeScheme [] paramType) env
  checkExpr env' body retType

-- General case: infer and unify
checkExpr env expr expected = do
  actual <- inferExpr env expr
  -- Use unification to allow type variables to be solved
  _subst <- unify expected actual
  pure ()

-- | Type checking for computations (infer mode)
inferComp :: TypeEnv -> Comp -> Either TypeError Type
inferComp env (CReturn expr) = do
  ty <- inferExpr env expr
  pure (TComp ty)

inferComp env (CBind var comp1 comp2) = do
  ty1 <- inferComp env comp1
  case ty1 of
    TComp innerTy -> do
      let env' = Map.insert var (TypeScheme [] innerTy) env
      inferComp env' comp2
    _ -> Left (TypeMismatch (TComp (TVar "a")) ty1)

inferComp env (CPerform expr) = do
  ty <- inferExpr env expr
  -- The expression should evaluate to a computation type
  case ty of
    TComp _ -> pure ty
    _ -> Left (TypeMismatch (TComp (TVar "a")) ty)

inferComp env (CVar v) =
  case Map.lookup v env of
    Nothing -> Left (VarNotInScope v)
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
checkComp :: TypeEnv -> Comp -> Type -> Either TypeError ()
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

-- | Instantiation: replace quantified variables
-- Phase 1: simple version (no fresh variables yet)
-- For now, just return the type as-is for monomorphic checking
-- | Simple fresh variable counter (using Data.IORef would be better, but this works)
-- For now, we use a simple text-based substitution
instantiate :: [Text] -> Type -> Either TypeError Type
instantiate [] ty = pure ty
instantiate vars ty = pure (substMany (zip vars freshVars) ty)
  where
    -- Generate fresh variables like a', b', c', etc.
    freshVars = map (\v -> v <> "'") vars

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
unify :: Type -> Type -> Either TypeError Subst
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

  _ -> Left (TypeMismatch t1 t2)

-- | Bind a type variable to a type (with occurs check)
bindVar :: Text -> Type -> Either TypeError Subst
bindVar x t
  | TVar x == t = pure Map.empty
  | x `Set.member` freeVars t = Left (OccursCheck x t)
  | otherwise = pure (Map.singleton x t)

--------------------------------------------------------------------------------
-- Import loading for type checking

-- | Load type environments from all imports
loadTypeImports :: FilePath -> Module -> IO TypeEnv
loadTypeImports projectRoot (Module _ imports _) = do
  envs <- mapM (loadTypeImport projectRoot) imports
  pure $ Map.unions (buildPrimitiveEnv : envs)

-- | Load a single import's type environment
loadTypeImport :: FilePath -> Import -> IO TypeEnv
loadTypeImport projectRoot (Import modName alias) = do
  let basePath = projectRoot </> "lib" </> modNameToPath modName
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
  let Module name _ defs = parsed
  envImports <- loadTypeImports projectRoot parsed
  case typeCheckModuleWithEnv envImports parsed of
    Left tcErr -> error $ "Type error in " ++ path ++ ": " ++ show tcErr
    Right envSelf -> do
      -- Insert all type-checked definitions with qualified names
      -- For each definition that was type-checked, add qualified versions
      let defNames = map defName defs
          envWithQualified = foldl (insertQualified alias name envSelf) envImports defNames
      pure envWithQualified
  where
    tryLoadFile p = do
      c <- TIO.readFile p
      pure (p, c)

-- | Convert module name to file path (replace dots with slashes)
modNameToPath :: Text -> FilePath
modNameToPath = T.unpack . T.replace "." "/"

-- | Insert a type-checked definition with qualified names
-- Looks up the type from envSelf and inserts qualified versions
insertQualified :: Text -> Text -> TypeEnv -> TypeEnv -> Text -> TypeEnv
insertQualified alias modName envSelf env defName =
  case Map.lookup defName envSelf of
    Just scheme ->
      let names = [qualName alias defName, qualName modName defName, defName]
      in foldl (\acc n -> Map.insert n scheme acc) env names
    Nothing -> env  -- Definition not in type-checked environment
  where
    qualName :: Text -> Text -> Text
    qualName prefix n = prefix <> "." <> n

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

  -- Pattern matching (complex polymorphic type)
  , ("match-prim", poly ["a", "b"]
      (TFun (TVar "a")  -- scrutinee
            (TFun (TFun TUnit (TVar "b"))  -- empty/false case
                  (TFun (TFun (TVar "a") (TFun (TVar "a") (TVar "b")))  -- non-empty case (simplified)
                        (TVar "b")))))

  -- IO operations (return Comp types)
  , ("print-prim", mono (TFun TString (TComp TUnit)))
  , ("read-file-prim", mono (TFun TString (TComp TString)))
  , ("write-file-prim", mono (TFun TString (TFun TString (TComp TUnit))))

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
