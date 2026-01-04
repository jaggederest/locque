{-# LANGUAGE OverloadedStrings #-}
module TypeChecker
  ( typeCheckModule
  , typeCheckModuleWithImports
  , typeCheckAndNormalizeWithEnv
  , typeCheckAndNormalizeWithImports
  , normalizeModuleWithImports
  , normalizeTypeEnvWithImports
  , annotateModule
  , moduleDigest
  , moduleDigestWithImports
  , moduleDigestWithImportsScope
  , TypeError(..)
  , TypeEnv
  , freeVars
  ) where

import Control.Monad (foldM, when)
import Control.Monad.State
import Data.Bits (xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (ord)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Word (Word64)
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), (<.>), isAbsolute, makeRelative, takeDirectory, takeExtension)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)
import AST
import Type (prettyType)
import Parser (parseModuleFile, parseMExprFile, exprToMExprText)
import SourceLoc
import Recursor (injectRecursors, recursorDefinition)
import ErrorMsg
import ImportResolver (ImportScope(..), ResolvedModule(..), resolveModulePath)
import Utils (qualifyName)

data DataInfo = DataInfo
  { dataName :: Text
  , dataParams :: [Param]
  , dataUniverse :: Int
  , dataCtors :: [CtorInfo]
  } deriving (Show, Read, Eq)

data CtorInfo = CtorInfo
  { ctorName :: Text
  , ctorData :: Text
  , ctorParams :: [Param]
  , ctorResult :: Expr
  , ctorType :: Expr
  } deriving (Show, Read, Eq)

data ClassInfo = ClassInfo
  { className    :: Text
  , classParam   :: Text
  , classParamKind :: Expr
  , classMethods :: [(Text, Expr)]
  } deriving (Show, Read, Eq)

data InstanceInfo = InstanceInfo
  { instName    :: Text
  , instClass   :: Text
  , instType    :: Expr
  , instMethods :: Map.Map Text Expr
  } deriving (Show, Read, Eq)

-- | Type errors with context
data TypeError
  = VarNotInScope SrcLoc Text TypeEnv
  | TypeMismatch SrcLoc Expr Expr (Maybe Text)
  | CannotApply SrcLoc Expr Expr
  | NotAFunction SrcLoc Expr
  | NotAComputation SrcLoc Expr
  | ExpectedType SrcLoc Expr
  | InvalidLift SrcLoc Int Int
  | EmptyListLiteral SrcLoc
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

  show (TypeMismatch loc expected actual context) =
    let mainMsg = "Type mismatch"
        locLine = case loc of
          NoLoc -> ""
          _ -> "Location: " <> prettyLoc loc <> "\n"
        contextLine = case context of
          Nothing -> ""
          Just ctx -> "While checking: " <> ctx <> "\n"
        note = Just $ locLine <> contextLine <>
                      "Expected: " <> prettyType expected <>
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

  show (EmptyListLiteral loc) =
    let msg = ErrorMsg loc "Empty list literal requires type annotation" Nothing [] Nothing
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
  TypeMismatch _ a1 a2 _ == TypeMismatch _ b1 b2 _ = a1 == b1 && a2 == b2
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
  , tcData :: Map.Map Text DataInfo
  , tcCtors :: Map.Map Text CtorInfo
  } deriving (Show, Read, Eq)

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

isKindExpr :: Expr -> Bool
isKindExpr expr = case expr of
  ETypeUniverse _ -> True
  EForAll _ dom cod -> isUniverseExpr dom && isKindExpr cod
  EThereExists _ dom cod -> isUniverseExpr dom && isKindExpr cod
  ELift ty _ _ -> isKindExpr ty
  EApp f args ->
    case f of
      EVar name | isKindAliasName name -> all isUniverseExpr args
      _ -> False
  EVar name -> isBaseTypeAlias name
  EAnnot e _ -> isKindExpr e
  ETyped e _ -> isKindExpr e
  _ -> False

isUniverseExpr :: Expr -> Bool
isUniverseExpr expr = case expr of
  ETypeUniverse _ -> True
  EVar name -> isBaseTypeAlias name
  EAnnot e _ -> isUniverseExpr e
  ETyped e _ -> isUniverseExpr e
  _ -> False

isKindAliasName :: Text -> Bool
isKindAliasName name =
  T.isSuffixOf "TypeFunction" name || T.isSuffixOf "BinaryTypeFunction" name

isBaseTypeAlias :: Text -> Bool
isBaseTypeAlias name = T.isSuffixOf "BaseType" name

stripInstanceTypeParams :: Set.Set Text -> Expr -> Expr
stripInstanceTypeParams allowed ty = case ty of
  EForAll v dom cod
    | v `Set.member` allowed && isKindExpr dom ->
        stripInstanceTypeParams (Set.delete v allowed) cod
  _ -> ty

tListener :: Expr
tListener = ETypeConst TCListener

tSocket :: Expr
tSocket = ETypeConst TCSocket

tList :: Expr -> Expr
tList a = EApp (ETypeConst TCList) [a]

tPair :: Expr -> Expr -> Expr
tPair a b = EApp (ETypeConst TCPair) [a, b]

tOption :: Expr -> Expr
tOption a = EApp (EVar "Option") [a]

tDictionary :: Expr -> Expr -> Expr
tDictionary k v = EApp (ETypeConst TCDictionary) [k, v]

tComp :: Expr -> Expr
tComp t = ECompType effectAnyExpr t

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
      [ MatchCase "Boolean::false" [] (tNot p)
      , MatchCase "Boolean::true" [] p
      ])

tDecider :: Expr -> Expr
tDecider ty =
  EForAll "x" ty
    (EForAll "y" ty
      (tDecidable (tEqual ty (EVar "x") (EVar "y"))))

mkPi :: [Param] -> Expr -> Expr
mkPi params ret = foldr (\(Param name ty) acc -> EForAll name ty acc) ret params

closeCtorType :: [Param] -> Expr -> Expr
closeCtorType params ctorType =
  foldr (\(Param name ty) acc -> EForAll name ty acc) ctorType params

splitForAll :: Expr -> ([Param], Expr)
splitForAll expr = go Set.empty expr
  where
    go used e = case e of
      EForAll v dom cod ->
        let avoid = Set.unions [used, freeVars dom, freeVars cod]
            v' = if v `Set.member` used
              then freshNameAvoidPure v avoid
              else v
            cod' = if v' == v then cod else renameBound v v' cod
            used' = Set.insert v' used
            (params, result) = go used' cod'
        in (Param v' dom : params, result)
      _ -> ([], e)

freshNameAvoidPure :: Text -> Set.Set Text -> Text
freshNameAvoidPure base avoid =
  if base `Set.member` avoid
    then go (1 :: Int)
    else base
  where
    go n =
      let candidate = base <> "_" <> T.pack (show n)
      in if candidate `Set.member` avoid
          then go (n + 1)
          else candidate

renameBound :: Text -> Text -> Expr -> Expr
renameBound oldName newName = go Set.empty
  where
    go bound expr = case expr of
      EVar v
        | v == oldName && v `Set.notMember` bound -> EVar newName
        | otherwise -> expr
      ELit _ -> expr
      EListLiteral elems -> EListLiteral (map (go bound) elems)
      ETypeConst _ -> expr
      ETypeUniverse _ -> expr
      EForAll v dom cod ->
        let dom' = go bound dom
        in if v == oldName
          then EForAll v dom' cod
          else EForAll v dom' (go (Set.insert v bound) cod)
      EThereExists v dom cod ->
        let dom' = go bound dom
        in if v == oldName
          then EThereExists v dom' cod
          else EThereExists v dom' (go (Set.insert v bound) cod)
      ECompType eff t -> ECompType (go bound eff) (go bound t)
      EEqual ty lhs rhs ->
        EEqual (go bound ty) (go bound lhs) (go bound rhs)
      EReflexive ty term ->
        EReflexive (go bound ty) (go bound term)
      ERewrite family proof body ->
        ERewrite (go bound family) (go bound proof) (go bound body)
      EPack v dom cod witness body ->
        let dom' = go bound dom
        in if v == oldName
          then EPack v dom' cod (go bound witness) (go bound body)
          else EPack v dom' (go (Set.insert v bound) cod) (go bound witness) (go bound body)
      EUnpack packed x y body ->
        let packed' = go bound packed
        in if x == oldName || y == oldName
          then EUnpack packed' x y body
          else EUnpack packed' x y (go (Set.insert y (Set.insert x bound)) body)
      ELift ty fromLevel toLevel ->
        ELift (go bound ty) fromLevel toLevel
      EUp ty fromLevel toLevel body ->
        EUp (go bound ty) fromLevel toLevel (go bound body)
      EDown ty fromLevel toLevel body ->
        EDown (go bound ty) fromLevel toLevel (go bound body)
      EApp f args -> EApp (go bound f) (map (go bound) args)
      EFunction params constraints ret body ->
        let (params', constraints', ret', body') =
              renameParams bound params constraints ret body
        in EFunction params' constraints' ret' body'
      ELet v val body ->
        let val' = go bound val
        in if v == oldName
          then ELet v val' body
          else ELet v val' (go (Set.insert v bound) body)
      ECompute comp -> ECompute (goComp bound comp)
      EMatch scrut scrutTy scrutName retTy cases ->
        let scrut' = go bound scrut
            scrutTy' = go bound scrutTy
        in if scrutName == oldName
          then EMatch scrut' scrutTy' scrutName retTy (map (goCase bound) cases)
          else EMatch scrut' scrutTy' scrutName (go (Set.insert scrutName bound) retTy)
            (map (goCase (Set.insert scrutName bound)) cases)
      EData params universe cases ->
        let universe' = go bound universe
            (params', cases') = renameDataParams bound params cases
        in EData params' universe' cases'
      EAnnot e ty -> EAnnot (go bound e) (go bound ty)
      ETyped e ty -> ETyped (go bound e) (go bound ty)
      EDict cls impls ->
        EDict cls [ (n, go bound e) | (n, e) <- impls ]
      EDictAccess e method -> EDictAccess (go bound e) method
      ETypeClass param kind methods ->
        let kind' = go bound kind
        in if param == oldName
          then ETypeClass param kind' methods
          else
            let bound' = Set.insert param bound
                methods' = [ (n, go bound' ty) | (n, ty) <- methods ]
            in ETypeClass param kind' methods'
      EInstance cls instTy methods ->
        EInstance cls (go bound instTy) [ (n, go bound e) | (n, e) <- methods ]

    renameParams bound params constraints ret body =
      let (bound', params') = renameParamSeq bound params
          constraints' =
            [ Constraint cls (go bound' ty)
            | Constraint cls ty <- constraints
            ]
          ret' = go bound' ret
          body' = renameBody bound' body
      in (params', constraints', ret', body')

    renameParamSeq bound params =
      foldl step (bound, []) params
      where
        step (b, acc) (Param name ty) =
          let ty' = go b ty
              b' = Set.insert name b
          in (b', acc ++ [Param name ty'])

    renameBody bound body = case body of
      FunctionValue e -> FunctionValue (go bound e)
      FunctionCompute c -> FunctionCompute (goComp bound c)

    goCase bound (MatchCase ctor binders body) =
      let (bound', binders') = renameParamSeq bound binders
      in MatchCase ctor binders' (go bound' body)

    renameDataParams bound params cases =
      let (bound', params') = renameParamSeq bound params
          cases' = [ DataCase ctor (go bound' ty) | DataCase ctor ty <- cases ]
      in (params', cases')

    goComp bound comp = case comp of
      CReturn e -> CReturn (go bound e)
      CPerform e -> CPerform (go bound e)
      CBind v c1 c2 ->
        let c1' = goComp bound c1
            bound' = Set.insert v bound
        in if v == oldName
          then CBind v c1' c2
          else CBind v c1' (goComp bound' c2)

collectApp :: Expr -> (Expr, [Expr])
collectApp expr = case expr of
  EApp f args ->
    let (headExpr, more) = collectApp f
    in (headExpr, more ++ args)
  _ -> (expr, [])

mkCtorInfo :: Text -> Text -> Expr -> CtorInfo
mkCtorInfo dataTy ctorName ctorTy =
  let (params, result) = splitForAll ctorTy
  in CtorInfo
      { ctorName = ctorName
      , ctorData = dataTy
      , ctorParams = params
      , ctorResult = result
      , ctorType = ctorTy
  }

dataParamsForCtor :: DataInfo -> CtorInfo -> [Param]
dataParamsForCtor info ctor =
  let free = freeVars (ctorType ctor)
  in filter (\(Param name _) -> name `Set.member` free) (dataParams info)

builtinDataInfos :: [DataInfo]
builtinDataInfos =
  [ DataInfo
      { dataName = "Boolean"
      , dataParams = []
      , dataUniverse = 0
      , dataCtors =
          [ mkCtorInfo "Boolean" "Boolean::false" tBool
          , mkCtorInfo "Boolean" "Boolean::true" tBool
          ]
      }
  , DataInfo
      { dataName = "Unit"
      , dataParams = []
      , dataUniverse = 0
      , dataCtors =
          [ mkCtorInfo "Unit" "Unit::tt" tUnit
          ]
      }
  , DataInfo
      { dataName = "List"
      , dataParams = [Param "A" (tType 0)]
      , dataUniverse = 0
      , dataCtors =
          [ mkCtorInfo "List" "List::empty" (tList (EVar "A"))
          , mkCtorInfo "List" "List::cons"
              (EForAll "h" (EVar "A") (EForAll "t" (tList (EVar "A")) (tList (EVar "A"))))
          ]
      }
  , DataInfo
      { dataName = "Pair"
      , dataParams = [Param "A" (tType 0), Param "B" (tType 0)]
      , dataUniverse = 0
      , dataCtors =
          [ mkCtorInfo "Pair" "Pair::pair"
              (EForAll "a" (EVar "A") (EForAll "b" (EVar "B") (tPair (EVar "A") (EVar "B"))))
          ]
      }
  ]

classType :: Text -> Expr -> Expr
classType param kind = EForAll param kind (tType 0)

emptyEnv :: TCEnv
emptyEnv =
  let dataInfos = builtinDataInfos
      dataMap = Map.fromList [(dataName info, info) | info <- dataInfos]
      ctorMap = Map.fromList [(ctorName ctor, ctor) | info <- dataInfos, ctor <- dataCtors info]
  in TCEnv buildPrimitiveEnv Map.empty Set.empty Map.empty Map.empty dataMap ctorMap

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

extendType :: TCEnv -> Text -> Expr -> TCEnv
extendType env name ty =
  env { tcTypes = Map.insert name ty (tcTypes env) }

extendCtor :: TCEnv -> Text -> Expr -> CtorInfo -> TCEnv
extendCtor env name ty info =
  env { tcTypes = Map.insert name ty (tcTypes env)
      , tcCtors = Map.insert name info (tcCtors env)
      }

extendData :: TCEnv -> Text -> Expr -> DataInfo -> TCEnv
extendData env name ty info =
  let envWithType = env
        { tcTypes = Map.insert name ty (tcTypes env)
        , tcData = Map.insert name info (tcData env)
        }
      envWithCtors = foldl
        (\e ctor ->
            let fullTy = closeCtorType (dataParamsForCtor info ctor) (ctorType ctor)
            in extendCtor e (ctorName ctor) fullTy ctor)
        envWithType
        (dataCtors info)
  in envWithCtors

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
      tyKind <- infer env ty
      ensureConv env tyKind (classParamKind classInfo)
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
  EListLiteral elems ->
    Set.unions (map (freeVarsWithBound bound) elems)
  ETypeConst _ -> Set.empty
  ETypeUniverse _ -> Set.empty
  EForAll v dom cod ->
    freeVarsWithBound bound dom `Set.union` freeVarsWithBound (Set.insert v bound) cod
  EThereExists v dom cod ->
    freeVarsWithBound bound dom `Set.union` freeVarsWithBound (Set.insert v bound) cod
  ECompType eff t ->
    freeVarsWithBound bound eff `Set.union` freeVarsWithBound bound t
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
  EData params universe cases ->
    let (bound', paramsFree) = freeVarsParamsSeq bound params
        universeFree = freeVarsWithBound bound universe
        casesFree = Set.unions [freeVarsWithBound bound' (dataCaseType c) | c <- cases]
    in paramsFree `Set.union` universeFree `Set.union` casesFree
  EAnnot e ty -> freeVarsWithBound bound e `Set.union` freeVarsWithBound bound ty
  ETyped e ty -> freeVarsWithBound bound e `Set.union` freeVarsWithBound bound ty
  EDict _ impls -> Set.unions [freeVarsWithBound bound e | (_, e) <- impls]
  EDictAccess e _ -> freeVarsWithBound bound e
  ETypeClass param kind methods ->
    let kindFree = freeVarsWithBound bound kind
        methodsFree = Set.unions [freeVarsWithBound (Set.insert param bound) ty | (_, ty) <- methods]
    in kindFree `Set.union` methodsFree
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

freeVarsParamsSeq :: Set.Set Text -> [Param] -> (Set.Set Text, Set.Set Text)
freeVarsParamsSeq bound params =
  foldl step (bound, Set.empty) params
  where
    step (b, acc) (Param name ty) =
      let acc' = acc `Set.union` freeVarsWithBound b ty
      in (Set.insert name b, acc')

freeVarsCase :: Set.Set Text -> MatchCase -> Set.Set Text
freeVarsCase bound (MatchCase _ binders body) =
  let (bound', paramsFree) = freeVarsParamsSeq bound binders
  in paramsFree `Set.union` freeVarsWithBound bound' body

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
      EListLiteral elems -> EListLiteral <$> mapM (go bound) elems
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
      ECompType eff t -> ECompType <$> go bound eff <*> go bound t
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
      EData params universe cases -> do
        universe' <- go bound universe
        (params', cases') <- goDataParams bound params cases
        pure (EData params' universe' cases')
      EAnnot e1 ty -> EAnnot <$> go bound e1 <*> go bound ty
      ETyped e1 ty -> ETyped <$> go bound e1 <*> go bound ty
      EDict cls impls -> do
        impls' <- mapM (\(n, e1) -> (n,) <$> go bound e1) impls
        pure (EDict cls impls')
      EDictAccess e1 method -> EDictAccess <$> go bound e1 <*> pure method
      ETypeClass param kind methods -> do
        kind' <- go bound kind
        if param == name
          then pure (ETypeClass param kind' methods)
          else do
            (param', methods') <- refreshClassBinder bound param methods
            methods'' <- mapM (\(n, ty) -> (n,) <$> go (Set.insert param' bound) ty) methods'
            pure (ETypeClass param' kind' methods'')
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
                      , freeVarsParamsLocal bound rest
                      , freeVarsConstraintsLocal bound constraints
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

    freeVarsParamsLocal bound params =
      Set.unions [freeVarsWithBound bound (paramType p) | p <- params]

    freeVarsConstraintsLocal bound constraints =
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

    goCase bound (MatchCase ctor binders body) = do
      (binders', body') <- goCaseBinders bound binders body
      pure (MatchCase ctor binders' body')

    goCaseBinders bound binders body = case binders of
      [] -> do
        body' <- go bound body
        pure ([], body')
      (Param v ty) : rest -> do
        ty' <- go bound ty
        if v == name
          then do
            (rest', body') <- goCaseBinders (Set.insert v bound) rest body
            pure (Param v ty' : rest', body')
          else do
            (v', rest', body') <- refreshCaseBinder bound v rest body
            (restFinal, bodyFinal) <- goCaseBinders (Set.insert v' bound) rest' body'
            pure (Param v' ty' : restFinal, bodyFinal)

    refreshCaseBinder bound v rest body =
      if v `Set.member` replacementFree
        then do
          let avoid = Set.unions
                [ replacementFree
                , Set.unions [freeVars (paramType p) | p <- rest]
                , freeVars body
                , bound
                ]
          v' <- freshNameAvoid v avoid
          rest' <- mapM (renameParam v v') rest
          body' <- subst v (EVar v') body
          pure (v', rest', body')
        else pure (v, rest, body)

    goDataParams bound params cases = case params of
      [] -> do
        cases' <- mapM (goDataCase bound) cases
        pure ([], cases')
      (Param v ty) : rest -> do
        ty' <- go bound ty
        if v == name
          then do
            (rest', cases') <- goDataParams (Set.insert v bound) rest cases
            pure (Param v ty' : rest', cases')
          else do
            (v', rest', cases') <- refreshDataBinder bound v rest cases
            (restFinal, casesFinal) <- goDataParams (Set.insert v' bound) rest' cases'
            pure (Param v' ty' : restFinal, casesFinal)

    goDataCase bound (DataCase ctor ty) =
      DataCase ctor <$> go bound ty

    refreshDataBinder bound v rest cases =
      if v `Set.member` replacementFree
        then do
          let avoid = Set.unions
                [ replacementFree
                , Set.unions [freeVars (paramType p) | p <- rest]
                , Set.unions [freeVars (dataCaseType c) | c <- cases]
                , bound
                ]
          v' <- freshNameAvoid v avoid
          rest' <- mapM (renameParam v v') rest
          cases' <- mapM (renameDataCase v v') cases
          pure (v', rest', cases')
        else pure (v, rest, cases)

    renameDataCase old newName (DataCase ctor ty) =
      DataCase ctor <$> subst old (EVar newName) ty

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
    EListLiteral elems ->
      EListLiteral <$> mapM (normalize env) elems
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
    ECompType eff t -> ECompType <$> normalize env eff <*> normalize env t
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
              && alphaEq env Map.empty ty' ty2 -> normalize env inner
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
    EData params universe cases -> do
      universe' <- normalize env universe
      (env', params') <- normalizeParams env params
      cases' <- mapM (normalizeDataCase env') cases
      pure (EData params' universe' cases')
    _ -> pure wh

normalizeCase :: TCEnv -> MatchCase -> TypeCheckM MatchCase
normalizeCase env (MatchCase ctor binders body) = do
  (env', binders') <- normalizeParams env binders
  body' <- normalize env' body
  pure (MatchCase ctor binders' body')

normalizeDataCase :: TCEnv -> DataCase -> TypeCheckM DataCase
normalizeDataCase env (DataCase ctor ty) =
  DataCase ctor <$> normalize env ty

normalizeParams :: TCEnv -> [Param] -> TypeCheckM (TCEnv, [Param])
normalizeParams env [] = pure (env, [])
normalizeParams env (Param name ty : rest) = do
  ty' <- normalize env ty
  let env' = extendLocal env name ty'
  (envFinal, rest') <- normalizeParams env' rest
  pure (envFinal, Param name ty' : rest')

normalizeModule :: TCEnv -> Module -> TypeCheckM Module
normalizeModule env (Module name imports opens defs) = do
  defs' <- mapM (normalizeDef env) defs
  pure (Module name imports opens defs')

normalizeDef :: TCEnv -> Definition -> TypeCheckM Definition
normalizeDef env (Definition tr name body) = do
  body' <- normalize env body
  pure (Definition tr name body')

normalizeTypeEnv :: TCEnv -> TypeCheckM TypeEnv
normalizeTypeEnv env =
  Map.traverseWithKey (\_ ty -> normalize env ty) (tcTypes env)

whnf :: TCEnv -> Expr -> TypeCheckM Expr
whnf env expr = case expr of
  EAnnot e1 _ -> whnf env e1
  ETyped e1 _ -> whnf env e1
  EListLiteral elems -> pure (EListLiteral elems)
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
        reduceCtor ctorName argExprs =
          case [(binders, body) | MatchCase name binders body <- cases, name == ctorName] of
            ((binders, body):_) ->
              if length binders == length argExprs
                then
                  let names = map paramName binders
                      substs = zip names argExprs
                  in reduceWith body names substs
                else noReduction
            [] -> noReduction
        reduceCtorFromScrut ctorName args =
          case Map.lookup ctorName (tcCtors env) of
            Nothing -> noReduction
            Just ctorInfo ->
              case Map.lookup (ctorData ctorInfo) (tcData env) of
                Nothing -> noReduction
                Just dataInfo ->
                  let dataParamCount = length (dataParamsForCtor dataInfo ctorInfo)
                      total = dataParamCount + length (ctorParams ctorInfo)
                  in if length args == total
                    then reduceCtor ctorName (drop dataParamCount args)
                    else noReduction
        noReduction = pure (EMatch scrut' scrutTy scrutName retTy cases)
    case scrut' of
      ELit (LBoolean False) ->
        reduceCtor "Boolean::false" []
      ELit (LBoolean True) ->
        reduceCtor "Boolean::true" []
      EVar "Boolean::false" ->
        reduceCtor "Boolean::false" []
      EVar "Boolean::true" ->
        reduceCtor "Boolean::true" []
      ELit LUnit ->
        reduceCtor "Unit::tt" []
      EVar "Unit::tt" ->
        reduceCtor "Unit::tt" []
      EApp (EVar "List::empty") [_elemTy] ->
        reduceCtor "List::empty" []
      EApp (EVar "List::cons") [_elemTy, headExpr, tailExpr] ->
        reduceCtor "List::cons" [headExpr, tailExpr]
      EListLiteral [] ->
        reduceCtor "List::empty" []
      EListLiteral (headExpr:tailExprs) ->
        reduceCtor "List::cons" [headExpr, EListLiteral tailExprs]
      EApp (EVar "Pair::pair") [_aTy, _bTy, leftExpr, rightExpr] ->
        reduceCtor "Pair::pair" [leftExpr, rightExpr]
      EVar name ->
        reduceCtorFromScrut name []
      EApp (EVar name) args ->
        reduceCtorFromScrut name args
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
        applyParams params constraints ret body args >>= whnf env
      _ -> pure (mkApp f' args)
  _ -> pure expr

applyParams :: [Param] -> [Constraint] -> Expr -> Expr -> [Expr] -> TypeCheckM Expr
applyParams [] _constraints _ret body args = pure (mkApp body args)
applyParams (Param v ty : rest) constraints ret body args = case args of
  [] -> pure (EFunction (Param v ty : rest) constraints ret (FunctionValue body))
  (a:as) -> do
    (restSafe, constraintsSafe, retSafe, bodySafe) <-
      refreshParamsForCapture a rest constraints ret body
    body' <- subst v a bodySafe
    rest' <- mapM (substParam v a) restSafe
    constraints' <- mapM (substConstraint v a) constraintsSafe
    ret' <- subst v a retSafe
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

refreshParamsForCapture
  :: Expr
  -> [Param]
  -> [Constraint]
  -> Expr
  -> Expr
  -> TypeCheckM ([Param], [Constraint], Expr, Expr)
refreshParamsForCapture replacement params constraints ret body = do
  let replacementFree = freeVars replacement
      paramNames = map paramName params
      namesToRefresh = filter (`Set.member` replacementFree) paramNames
      avoid =
        Set.unions
          [ replacementFree
          , Set.fromList paramNames
          , freeVars ret
          , freeVars body
          , freeVarsParams params
          , freeVarsConstraints constraints
          ]
  (params', constraints', ret', body', _) <-
    foldM refreshOne (params, constraints, ret, body, avoid) namesToRefresh
  pure (params', constraints', ret', body')
  where
    refreshOne (paramsAcc, constraintsAcc, retAcc, bodyAcc, avoidAcc) name = do
      fresh <- freshNameAvoid name avoidAcc
      (params', constraints', ret', body') <-
        renameParamBinder name fresh paramsAcc constraintsAcc retAcc bodyAcc
      let avoid' = Set.insert fresh avoidAcc
      pure (params', constraints', ret', body', avoid')

freeVarsParams :: [Param] -> Set.Set Text
freeVarsParams params =
  Set.unions [freeVars (paramType p) | p <- params]

freeVarsConstraints :: [Constraint] -> Set.Set Text
freeVarsConstraints constraints =
  Set.unions [freeVars ty | Constraint _ ty <- constraints]

renameParamBinder
  :: Text
  -> Text
  -> [Param]
  -> [Constraint]
  -> Expr
  -> Expr
  -> TypeCheckM ([Param], [Constraint], Expr, Expr)
renameParamBinder oldName newName params constraints ret body = do
  params' <- mapM (renameParamName oldName newName) params
  constraints' <- mapM (renameConstraintName oldName newName) constraints
  ret' <- subst oldName (EVar newName) ret
  body' <- subst oldName (EVar newName) body
  pure (params', constraints', ret', body')

renameParamName :: Text -> Text -> Param -> TypeCheckM Param
renameParamName oldName newName (Param v ty) = do
  ty' <- subst oldName (EVar newName) ty
  if v == oldName
    then pure (Param newName ty')
    else pure (Param v ty')

renameConstraintName :: Text -> Text -> Constraint -> TypeCheckM Constraint
renameConstraintName oldName newName (Constraint cls ty) =
  Constraint cls <$> subst oldName (EVar newName) ty

conv :: TCEnv -> Expr -> Expr -> TypeCheckM Bool
conv env a b = convWith env Map.empty a b

convWith :: TCEnv -> Map.Map Text Text -> Expr -> Expr -> TypeCheckM Bool
convWith env ren a b = do
  a' <- whnf env a
  b' <- whnf env b
  simplifyUpDown env ren a' b'
  where
    simplifyUpDown env' ren' a1 b1 = do
      ma <- reduceUpDown env' ren' a1
      case ma of
        Just a2 -> convWith env' ren' a2 b1
        Nothing -> do
          mb <- reduceUpDown env' ren' b1
          case mb of
            Just b2 -> convWith env' ren' a1 b2
            Nothing -> convWhnf env' ren' a1 b1

reduceUpDown :: TCEnv -> Map.Map Text Text -> Expr -> TypeCheckM (Maybe Expr)
reduceUpDown env ren expr = case expr of
  EUp _ fromLevel toLevel body
    | fromLevel == toLevel -> pure (Just body)
  EDown _ fromLevel toLevel body
    | fromLevel == toLevel -> pure (Just body)
  EDown ty fromLevel toLevel body -> case body of
    EUp ty2 from2 to2 inner
      | fromLevel == from2 && toLevel == to2 -> do
          ok <- convWith env ren ty ty2
          pure (if ok then Just inner else Nothing)
    _ -> pure Nothing
  _ -> pure Nothing

isEffectAny :: Expr -> Bool
isEffectAny expr = case expr of
  EVar name -> name == effectAnyName
  _ -> False

convWhnf :: TCEnv -> Map.Map Text Text -> Expr -> Expr -> TypeCheckM Bool
convWhnf env ren a b =
  case (a, b) of
      (EVar v1, EVar v2) ->
        case Map.lookup v1 ren of
          Just v2' -> pure (v2 == v2')
          Nothing -> pure (canonicalGlobal env v1 == canonicalGlobal env v2)
      (ELit l1, ELit l2) -> pure (l1 == l2)
      (EListLiteral xs, EListLiteral ys) -> convList env ren xs ys
      (ETypeConst c1, ETypeConst c2) -> pure (c1 == c2)
      (ETypeUniverse n1, ETypeUniverse n2) -> pure (n1 == n2)
      (EForAll v1 dom1 cod1, EForAll v2 dom2 cod2) -> do
        okDom <- convWith env ren dom1 dom2
        if not okDom
          then pure False
          else do
            let env' = extendLocalPair env v1 dom1 v2 dom2
                ren' = Map.insert v1 v2 ren
            convWith env' ren' cod1 cod2
      (EThereExists v1 dom1 cod1, EThereExists v2 dom2 cod2) -> do
        okDom <- convWith env ren dom1 dom2
        if not okDom
          then pure False
          else do
            let env' = extendLocalPair env v1 dom1 v2 dom2
                ren' = Map.insert v1 v2 ren
            convWith env' ren' cod1 cod2
      (ECompType eff1 t1, ECompType eff2 t2) -> do
        okTy <- convWith env ren t1 t2
        if not okTy
          then pure False
          else if isEffectAny eff1 || isEffectAny eff2
            then pure True
            else convWith env ren eff1 eff2
      (EEqual ty1 l1 r1, EEqual ty2 l2 r2) -> do
        okTy <- convWith env ren ty1 ty2
        if not okTy
          then pure False
          else do
            okL <- convWith env ren l1 l2
            if not okL then pure False else convWith env ren r1 r2
      (EReflexive ty1 t1, EReflexive ty2 t2) -> do
        okTy <- convWith env ren ty1 ty2
        if not okTy then pure False else convWith env ren t1 t2
      (ERewrite f1 p1 b1, ERewrite f2 p2 b2) -> do
        okF <- convWith env ren f1 f2
        if not okF
          then pure False
          else do
            okP <- convWith env ren p1 p2
            if not okP then pure False else convWith env ren b1 b2
      (EPack v1 dom1 cod1 w1 b1, EPack v2 dom2 cod2 w2 b2) -> do
        okDom <- convWith env ren dom1 dom2
        if not okDom
          then pure False
          else do
            let env' = extendLocalPair env v1 dom1 v2 dom2
                ren' = Map.insert v1 v2 ren
            okCod <- convWith env' ren' cod1 cod2
            if not okCod
              then pure False
              else do
                okW <- convWith env ren w1 w2
                if not okW then pure False else convWith env ren b1 b2
      (EUnpack p1 x1 y1 b1, EUnpack p2 x2 y2 b2) -> do
        okP <- convWith env ren p1 p2
        if not okP
          then pure False
          else do
            let env' = extendLocalPair (extendLocalPair env x1 tUnit x2 tUnit) y1 tUnit y2 tUnit
                ren' = Map.insert y1 y2 (Map.insert x1 x2 ren)
            convWith env' ren' b1 b2
      (ELift ty1 from1 to1, ELift ty2 from2 to2) ->
        if from1 == from2 && to1 == to2
          then convWith env ren ty1 ty2
          else pure False
      (EUp ty1 from1 to1 body1, EUp ty2 from2 to2 body2) ->
        if from1 == from2 && to1 == to2
          then do
            okTy <- convWith env ren ty1 ty2
            if not okTy then pure False else convWith env ren body1 body2
          else pure False
      (EDown ty1 from1 to1 body1, EDown ty2 from2 to2 body2) ->
        if from1 == from2 && to1 == to2
          then do
            okTy <- convWith env ren ty1 ty2
            if not okTy then pure False else convWith env ren body1 body2
          else pure False
      (EApp f1 args1, EApp f2 args2)
        | length args1 == length args2 -> do
            okF <- convWith env ren f1 f2
            if not okF then pure False else convList env ren args1 args2
      (EFunction params1 constraints1 ret1 body1, EFunction params2 constraints2 ret2 body2)
        | length params1 == length params2
          , length constraints1 == length constraints2 -> do
            paramsRes <- convParams env ren params1 params2
            case paramsRes of
              Nothing -> pure False
              Just (env', ren') -> do
                okConstraints <- convConstraints env' ren' constraints1 constraints2
                if not okConstraints
                  then pure False
                  else do
                    okRet <- convWith env' ren' ret1 ret2
                    if not okRet then pure False else convBody env' ren' body1 body2
      (ECompute c1, ECompute c2) -> convComp env ren c1 c2
      (EMatch s1 ty1 n1 r1 c1, EMatch s2 ty2 n2 r2 c2)
        | length c1 == length c2 -> do
            okS <- convWith env ren s1 s2
            if not okS
              then pure False
              else do
                okTy <- convWith env ren ty1 ty2
                if not okTy
                  then pure False
                  else do
                    let env' = extendLocalPair env n1 ty1 n2 ty2
                        ren' = Map.insert n1 n2 ren
                    okRet <- convWith env' ren' r1 r2
                    if not okRet
                      then pure False
                      else convCases env' ren' c1 c2
      (EData params1 uni1 cases1, EData params2 uni2 cases2)
        | length params1 == length params2
          , length cases1 == length cases2 -> do
            okUni <- convWith env ren uni1 uni2
            if not okUni
              then pure False
              else do
                paramsRes <- convParams env ren params1 params2
                case paramsRes of
                  Nothing -> pure False
                  Just (env', ren') -> convDataCases env' ren' cases1 cases2
      (EDict cls1 impls1, EDict cls2 impls2)
        | cls1 == cls2 && length impls1 == length impls2 ->
            convDictImpls env ren impls1 impls2
      (EDictAccess d1 m1, EDictAccess d2 m2)
        | m1 == m2 -> convWith env ren d1 d2
      (ETypeClass p1 k1 ms1, ETypeClass p2 k2 ms2)
        | length ms1 == length ms2 -> do
            okK <- convWith env ren k1 k2
            if not okK
              then pure False
              else do
                let env' = extendLocalPair env p1 k1 p2 k2
                    ren' = Map.insert p1 p2 ren
                convMethodTypes env' ren' ms1 ms2
      (EInstance c1 t1 ms1, EInstance c2 t2 ms2)
        | c1 == c2 && length ms1 == length ms2 -> do
            okT <- convWith env ren t1 t2
            if not okT then pure False else convDictImpls env ren ms1 ms2
      (EAnnot e1 _t1, _) -> convWith env ren e1 b
      (_, EAnnot e2 _t2) -> convWith env ren a e2
      (ETyped e1 _t1, _) -> convWith env ren e1 b
      (_, ETyped e2 _t2) -> convWith env ren a e2
      _ -> pure False

extendLocalPair :: TCEnv -> Text -> Expr -> Text -> Expr -> TCEnv
extendLocalPair env v1 ty1 v2 ty2 =
  let env' = extendLocal env v1 ty1
  in if v2 == v1 then env' else extendLocal env' v2 ty2

convList :: TCEnv -> Map.Map Text Text -> [Expr] -> [Expr] -> TypeCheckM Bool
convList _ _ [] [] = pure True
convList env ren (x:xs) (y:ys) = do
  ok <- convWith env ren x y
  if ok then convList env ren xs ys else pure False
convList _ _ _ _ = pure False

convParams :: TCEnv -> Map.Map Text Text -> [Param] -> [Param] -> TypeCheckM (Maybe (TCEnv, Map.Map Text Text))
convParams env ren [] [] = pure (Just (env, ren))
convParams env ren (Param v1 ty1 : rest1) (Param v2 ty2 : rest2) = do
  ok <- convWith env ren ty1 ty2
  if not ok
    then pure Nothing
    else do
      let env' = extendLocalPair env v1 ty1 v2 ty2
          ren' = Map.insert v1 v2 ren
      convParams env' ren' rest1 rest2
convParams _ _ _ _ = pure Nothing

convConstraints :: TCEnv -> Map.Map Text Text -> [Constraint] -> [Constraint] -> TypeCheckM Bool
convConstraints _ _ [] [] = pure True
convConstraints env ren (Constraint c1 t1 : rest1) (Constraint c2 t2 : rest2) =
  if c1 /= c2
    then pure False
    else do
      ok <- convWith env ren t1 t2
      if ok then convConstraints env ren rest1 rest2 else pure False
convConstraints _ _ _ _ = pure False

convBody :: TCEnv -> Map.Map Text Text -> FunctionBody -> FunctionBody -> TypeCheckM Bool
convBody env ren b1 b2 = case (b1, b2) of
  (FunctionValue e1, FunctionValue e2) -> convWith env ren e1 e2
  (FunctionCompute c1, FunctionCompute c2) -> convComp env ren c1 c2
  _ -> pure False

convComp :: TCEnv -> Map.Map Text Text -> Comp -> Comp -> TypeCheckM Bool
convComp env ren c1 c2 = case (c1, c2) of
  (CReturn e1, CReturn e2) -> convWith env ren e1 e2
  (CPerform e1, CPerform e2) -> convWith env ren e1 e2
  (CBind v1 c1' c1'', CBind v2 c2' c2'') -> do
    ok <- convComp env ren c1' c2'
    if not ok
      then pure False
      else do
        let env' = extendLocalPair env v1 tUnit v2 tUnit
            ren' = Map.insert v1 v2 ren
        convComp env' ren' c1'' c2''
  _ -> pure False

convCases :: TCEnv -> Map.Map Text Text -> [MatchCase] -> [MatchCase] -> TypeCheckM Bool
convCases _ _ [] [] = pure True
convCases env ren (MatchCase ctor1 binders1 body1 : rest1) (MatchCase ctor2 binders2 body2 : rest2) =
  if ctor1 /= ctor2 || length binders1 /= length binders2
    then pure False
    else do
      paramsRes <- convParams env ren binders1 binders2
      case paramsRes of
        Nothing -> pure False
        Just (env', ren') -> do
          okBody <- convWith env' ren' body1 body2
          if okBody then convCases env ren rest1 rest2 else pure False
convCases _ _ _ _ = pure False

convDataCases :: TCEnv -> Map.Map Text Text -> [DataCase] -> [DataCase] -> TypeCheckM Bool
convDataCases _ _ [] [] = pure True
convDataCases env ren (DataCase n1 t1 : rest1) (DataCase n2 t2 : rest2) =
  if n1 /= n2
    then pure False
    else do
      ok <- convWith env ren t1 t2
      if ok then convDataCases env ren rest1 rest2 else pure False
convDataCases _ _ _ _ = pure False

convDictImpls :: TCEnv -> Map.Map Text Text -> [(Text, Expr)] -> [(Text, Expr)] -> TypeCheckM Bool
convDictImpls _ _ [] [] = pure True
convDictImpls env ren ((n1, e1) : rest1) ((n2, e2) : rest2) =
  if n1 /= n2
    then pure False
    else do
      ok <- convWith env ren e1 e2
      if ok then convDictImpls env ren rest1 rest2 else pure False
convDictImpls _ _ _ _ = pure False

convMethodTypes :: TCEnv -> Map.Map Text Text -> [(Text, Expr)] -> [(Text, Expr)] -> TypeCheckM Bool
convMethodTypes = convDictImpls

type UnifySubst = Map.Map Text Expr

applyUnifySubst :: UnifySubst -> Expr -> TypeCheckM Expr
applyUnifySubst sub expr =
  foldM (\acc (name, val) -> subst name val acc) expr (Map.toList sub)

applyUnifySubstMap :: Text -> Expr -> UnifySubst -> TypeCheckM UnifySubst
applyUnifySubstMap name replacement sub =
  Map.traverseWithKey (\_ ty -> subst name replacement ty) sub

unifyIndices :: TCEnv -> Set.Set Text -> Expr -> Expr -> TypeCheckM (Maybe UnifySubst)
unifyIndices env solvables left right = go Map.empty left right
  where
    go sub a b = do
      a' <- applyUnifySubst sub a >>= whnf env
      b' <- applyUnifySubst sub b >>= whnf env
      case (a', b') of
        (EVar v, _) | v `Set.member` solvables -> bindVar sub v b'
        (_, EVar v) | v `Set.member` solvables -> bindVar sub v a'
        _ -> unifyRigid sub a' b'

    unifyRigid sub a b = case (a, b) of
      (EVar v1, EVar v2) | v1 == v2 -> pure (Just sub)
      (ELit l1, ELit l2) | l1 == l2 -> pure (Just sub)
      (EListLiteral xs, EListLiteral ys)
        | length xs == length ys -> unifyArgs sub xs ys
      (ETypeConst c1, ETypeConst c2) | c1 == c2 -> pure (Just sub)
      (ETypeUniverse n1, ETypeUniverse n2) | n1 == n2 -> pure (Just sub)
      (EApp f1 args1, EApp f2 args2)
        | length args1 == length args2 -> do
            res <- go sub f1 f2
            case res of
              Nothing -> pure Nothing
              Just sub' -> unifyArgs sub' args1 args2
      _ -> do
        ok <- conv env a b
        pure (if ok then Just sub else Nothing)

    unifyArgs sub [] [] = pure (Just sub)
    unifyArgs sub (x:xs) (y:ys) = do
      res <- go sub x y
      case res of
        Nothing -> pure Nothing
        Just sub' -> unifyArgs sub' xs ys
    unifyArgs _ _ _ = pure Nothing

    bindVar sub v expr = do
      expr' <- applyUnifySubst sub expr >>= whnf env
      case Map.lookup v sub of
        Just existing -> go sub existing expr'
        Nothing ->
          if expr' == EVar v
            then pure (Just sub)
            else if v `Set.member` freeVars expr'
              then pure Nothing
              else do
                sub' <- applyUnifySubstMap v expr' sub
                pure (Just (Map.insert v expr' sub'))

alphaEq :: TCEnv -> Map.Map Text Text -> Expr -> Expr -> Bool
alphaEq tcEnv env a b = case (a, b) of
  (EVar v1, EVar v2) ->
    case Map.lookup v1 env of
      Just v2' -> v2 == v2'
      Nothing -> canonicalGlobal tcEnv v1 == canonicalGlobal tcEnv v2
  (ELit l1, ELit l2) -> l1 == l2
  (EListLiteral xs, EListLiteral ys) ->
    length xs == length ys && and (zipWith (alphaEq tcEnv env) xs ys)
  (ETypeConst c1, ETypeConst c2) -> c1 == c2
  (ETypeUniverse i, ETypeUniverse j) -> i == j
  (EForAll v1 d1 c1, EForAll v2 d2 c2) ->
    alphaEq tcEnv env d1 d2 && alphaEq tcEnv (Map.insert v1 v2 env) c1 c2
  (EThereExists v1 d1 c1, EThereExists v2 d2 c2) ->
    alphaEq tcEnv env d1 d2 && alphaEq tcEnv (Map.insert v1 v2 env) c1 c2
  (ECompType eff1 t1, ECompType eff2 t2) ->
    alphaEq tcEnv env eff1 eff2 && alphaEq tcEnv env t1 t2
  (EEqual ty1 l1 r1, EEqual ty2 l2 r2) ->
    alphaEq tcEnv env ty1 ty2 && alphaEq tcEnv env l1 l2 && alphaEq tcEnv env r1 r2
  (EReflexive ty1 t1, EReflexive ty2 t2) ->
    alphaEq tcEnv env ty1 ty2 && alphaEq tcEnv env t1 t2
  (ERewrite f1 p1 b1, ERewrite f2 p2 b2) ->
    alphaEq tcEnv env f1 f2 && alphaEq tcEnv env p1 p2 && alphaEq tcEnv env b1 b2
  (EPack v1 d1 c1 w1 b1, EPack v2 d2 c2 w2 b2) ->
    alphaEq tcEnv env d1 d2
      && alphaEq tcEnv (Map.insert v1 v2 env) c1 c2
      && alphaEq tcEnv env w1 w2
      && alphaEq tcEnv env b1 b2
  (EUnpack p1 x1 y1 b1, EUnpack p2 x2 y2 b2) ->
    alphaEq tcEnv env p1 p2
      && alphaEq tcEnv (Map.insert y1 y2 (Map.insert x1 x2 env)) b1 b2
  (ELift ty1 from1 to1, ELift ty2 from2 to2) ->
    from1 == from2 && to1 == to2 && alphaEq tcEnv env ty1 ty2
  (EUp ty1 from1 to1 body1, EUp ty2 from2 to2 body2) ->
    from1 == from2 && to1 == to2 && alphaEq tcEnv env ty1 ty2 && alphaEq tcEnv env body1 body2
  (EDown ty1 from1 to1 body1, EDown ty2 from2 to2 body2) ->
    from1 == from2 && to1 == to2 && alphaEq tcEnv env ty1 ty2 && alphaEq tcEnv env body1 body2
  (EApp f1 a1, EApp f2 a2) ->
    length a1 == length a2 && alphaEq tcEnv env f1 f2 && and (zipWith (alphaEq tcEnv env) a1 a2)
  (EFunction ps1 cs1 r1 b1, EFunction ps2 cs2 r2 b2) ->
    length ps1 == length ps2
      && length cs1 == length cs2
      && let (env', paramsOk) = foldl step (env, True) (zip ps1 ps2)
             step (e, ok) (Param n1 t1, Param n2 t2) =
               (Map.insert n1 n2 e, ok && alphaEq tcEnv e t1 t2)
             constraintsOk = and (zipWith (alphaEqConstraint tcEnv env') cs1 cs2)
         in paramsOk && constraintsOk && alphaEq tcEnv env' r1 r2 && alphaEqBody tcEnv env' b1 b2
  (ELet v1 val1 body1, ELet v2 val2 body2) ->
    alphaEq tcEnv env val1 val2 && alphaEq tcEnv (Map.insert v1 v2 env) body1 body2
  (EMatch s1 ty1 n1 r1 c1, EMatch s2 ty2 n2 r2 c2) ->
    alphaEq tcEnv env s1 s2
      && alphaEq tcEnv env ty1 ty2
      && alphaEq tcEnv (Map.insert n1 n2 env) r1 r2
      && alphaEqCases tcEnv (Map.insert n1 n2 env) c1 c2
  (EData ps1 u1 cs1, EData ps2 u2 cs2) ->
    length ps1 == length ps2
      && alphaEq tcEnv env u1 u2
      && let (env', ok) =
               foldl
                 (\(e, acc) (Param n1 t1, Param n2 t2) ->
                     (Map.insert n1 n2 e, acc && alphaEq tcEnv e t1 t2))
                 (env, True)
                 (zip ps1 ps2)
         in ok && alphaEqDataCases tcEnv env' cs1 cs2
  (EAnnot e1 t1, EAnnot e2 t2) ->
    alphaEq tcEnv env e1 e2 && alphaEq tcEnv env t1 t2
  (ETyped e1 t1, ETyped e2 t2) ->
    alphaEq tcEnv env e1 e2 && alphaEq tcEnv env t1 t2
  (EDict n1 i1, EDict n2 i2) ->
    n1 == n2 && and (zipWith (\(a1, e1) (a2, e2) -> a1 == a2 && alphaEq tcEnv env e1 e2) i1 i2)
  (EDictAccess d1 m1, EDictAccess d2 m2) ->
    m1 == m2 && alphaEq tcEnv env d1 d2
  (ETypeClass p1 k1 ms1, ETypeClass p2 k2 ms2) ->
    length ms1 == length ms2
      && alphaEq tcEnv env k1 k2
      && let env' = Map.insert p1 p2 env
         in and (zipWith (\(n1, t1) (n2, t2) -> n1 == n2 && alphaEq tcEnv env' t1 t2) ms1 ms2)
  (EInstance c1 t1 ms1, EInstance c2 t2 ms2) ->
    c1 == c2 && alphaEq tcEnv env t1 t2
      && length ms1 == length ms2
      && and (zipWith (\(n1, e1) (n2, e2) -> n1 == n2 && alphaEq tcEnv env e1 e2) ms1 ms2)
  _ -> False

alphaEqConstraint :: TCEnv -> Map.Map Text Text -> Constraint -> Constraint -> Bool
alphaEqConstraint tcEnv env (Constraint c1 t1) (Constraint c2 t2) =
  c1 == c2 && alphaEq tcEnv env t1 t2

alphaEqBody :: TCEnv -> Map.Map Text Text -> FunctionBody -> FunctionBody -> Bool
alphaEqBody tcEnv env b1 b2 = case (b1, b2) of
  (FunctionValue e1, FunctionValue e2) -> alphaEq tcEnv env e1 e2
  (FunctionCompute c1, FunctionCompute c2) -> alphaEqComp tcEnv env c1 c2
  _ -> False

alphaEqComp :: TCEnv -> Map.Map Text Text -> Comp -> Comp -> Bool
alphaEqComp tcEnv env c1 c2 = case (c1, c2) of
  (CReturn e1, CReturn e2) -> alphaEq tcEnv env e1 e2
  (CPerform e1, CPerform e2) -> alphaEq tcEnv env e1 e2
  (CBind v1 a1 b1, CBind v2 a2 b2) ->
    alphaEqComp tcEnv env a1 a2 && alphaEqComp tcEnv (Map.insert v1 v2 env) b1 b2
  _ -> False

alphaEqCases :: TCEnv -> Map.Map Text Text -> [MatchCase] -> [MatchCase] -> Bool
alphaEqCases tcEnv env xs ys =
  length xs == length ys && and (zipWith (alphaEqCase tcEnv env) xs ys)

alphaEqCase :: TCEnv -> Map.Map Text Text -> MatchCase -> MatchCase -> Bool
alphaEqCase tcEnv env (MatchCase c1 bs1 b1) (MatchCase c2 bs2 b2) =
  c1 == c2
    && length bs1 == length bs2
    && let (env', ok) =
             foldl
               (\(e, acc) (Param n1 t1, Param n2 t2) ->
                   (Map.insert n1 n2 e, acc && alphaEq tcEnv e t1 t2))
               (env, True)
               (zip bs1 bs2)
        in ok && alphaEq tcEnv env' b1 b2

canonicalGlobal :: TCEnv -> Text -> Text
canonicalGlobal env name =
  case Map.lookup name (tcData env) of
    Just info -> dataName info
    Nothing ->
      case Map.lookup name (tcCtors env) of
        Just info -> ctorName info
        Nothing ->
          case Map.lookup name (tcClasses env) of
            Just info -> className info
            Nothing -> name

alphaEqDataCases :: TCEnv -> Map.Map Text Text -> [DataCase] -> [DataCase] -> Bool
alphaEqDataCases tcEnv env xs ys =
  length xs == length ys && and (zipWith (alphaEqDataCase tcEnv env) xs ys)

alphaEqDataCase :: TCEnv -> Map.Map Text Text -> DataCase -> DataCase -> Bool
alphaEqDataCase tcEnv env (DataCase n1 t1) (DataCase n2 t2) =
  n1 == n2 && alphaEq tcEnv env t1 t2

ensureConv :: TCEnv -> Expr -> Expr -> TypeCheckM ()
ensureConv env actual expected =
  ensureConvWith env actual expected Nothing

ensureConvWith :: TCEnv -> Expr -> Expr -> Maybe Text -> TypeCheckM ()
ensureConvWith env actual expected context = do
  ok <- conv env actual expected
  if ok then pure () else lift (Left (TypeMismatch noLoc expected actual context))

ensureLiftLevels :: TCEnv -> Expr -> Int -> Int -> TypeCheckM ()
ensureLiftLevels env ty fromLevel toLevel = do
  actual <- inferUniverse env ty
  if actual == fromLevel
    then pure ()
    else lift (Left (TypeMismatch noLoc (ETypeUniverse fromLevel) (ETypeUniverse actual) Nothing))
  if fromLevel <= toLevel
    then pure ()
    else lift (Left (InvalidLift noLoc fromLevel toLevel))

--------------------------------------------------------------------------------
-- Structural recursion checks (value-level only)

data StructParam = StructParam
  { structName :: Text
  , structIndex :: Int
  , structData :: Text
  } deriving (Show, Eq)

dataTypeOf :: TCEnv -> Expr -> TypeCheckM (Maybe Text)
dataTypeOf env ty = do
  tyWhnf <- whnf env ty
  let (headExpr, _) = collectApp tyWhnf
  pure $ case headExpr of
    ETypeConst TCBoolean -> Just "Boolean"
    ETypeConst TCUnit -> Just "Unit"
    ETypeConst TCList -> Just "List"
    ETypeConst TCPair -> Just "Pair"
    EVar name ->
      if Map.member name (tcData env)
        then Just name
        else Nothing
    _ -> Nothing

findStructuralParams :: TCEnv -> [Param] -> TypeCheckM [StructParam]
findStructuralParams env params =
  let go _ [] = pure []
      go idx (Param name ty : rest) = do
        rest' <- go (idx + 1) rest
        mData <- dataTypeOf env ty
        case mData of
          Just dataName -> pure (StructParam name idx dataName : rest')
          Nothing -> pure rest'
  in go 0 params

checkStructuralRecursion :: TCEnv -> [Param] -> FunctionBody -> TypeCheckM ()
checkStructuralRecursion env params body = case body of
  FunctionValue expr -> do
    when (any ((== "recur") . paramName) params) $
      lift (Left (RecursionError noLoc "recur is reserved and cannot be a parameter name"))
    structParams <- findStructuralParams env params
    checkExprRec env structParams Set.empty True expr
  FunctionCompute comp ->
    checkCompRec env [] Set.empty False comp

checkExprRec :: TCEnv -> [StructParam] -> Set.Set Text -> Bool -> Expr -> TypeCheckM ()
checkExprRec env structParams allowed allowRecur expr = case expr of
  EVar v | v == "recur" ->
    if allowRecur
      then lift (Left (RecursionError noLoc "recur must be applied"))
      else lift (Left (RecursionError noLoc "recur is only allowed in value recursion"))
  EApp (EVar "recur") args -> do
    if allowRecur
      then checkRecurCall structParams allowed args
      else lift (Left (RecursionError noLoc "recur is only allowed in value recursion"))
    mapM_ (checkExprRec env structParams allowed allowRecur) args
  EListLiteral elems ->
    mapM_ (checkExprRec env structParams allowed allowRecur) elems
  EApp f args -> do
    checkExprRec env structParams allowed allowRecur f
    mapM_ (checkExprRec env structParams allowed allowRecur) args
  ELet name val body -> do
    checkExprRec env structParams allowed allowRecur val
    let allowedShadow = Set.delete name allowed
        allowed' = case val of
          EVar v | v `Set.member` allowedShadow -> Set.insert name allowedShadow
          _ -> allowedShadow
    checkExprRec env structParams allowed' allowRecur body
  ECompute comp ->
    checkCompRec env structParams allowed False comp
  EMatch scrut scrutTy scrutName _retTy cases -> do
    let allowedBase = Set.delete scrutName allowed
    checkExprRec env structParams allowedBase allowRecur scrut
    mData <- dataTypeOf env scrutTy
    let canDecrease = case (mData, scrut) of
          (Just dataName, EVar v) ->
            v `Set.member` allowed ||
            any (\p -> structName p == v && structData p == dataName) structParams
          _ -> False
        caseData = if canDecrease then mData else Nothing
    mapM_ (checkCaseRec env structParams allowedBase allowRecur caseData) cases
  EUnpack packed x y body -> do
    checkExprRec env structParams allowed allowRecur packed
    let allowed' = Set.delete y (Set.delete x allowed)
    checkExprRec env structParams allowed' allowRecur body
  EPack _ _ _ witness body -> do
    checkExprRec env structParams allowed allowRecur witness
    checkExprRec env structParams allowed allowRecur body
  EFunction _ _ _ nestedBody -> case nestedBody of
    FunctionValue nestedExpr ->
      checkExprRec env structParams allowed False nestedExpr
    FunctionCompute nestedComp ->
      checkCompRec env structParams allowed False nestedComp
  EEqual ty lhs rhs -> do
    checkExprRec env structParams allowed allowRecur ty
    checkExprRec env structParams allowed allowRecur lhs
    checkExprRec env structParams allowed allowRecur rhs
  EReflexive ty term -> do
    checkExprRec env structParams allowed allowRecur ty
    checkExprRec env structParams allowed allowRecur term
  ERewrite family proof body -> do
    checkExprRec env structParams allowed allowRecur family
    checkExprRec env structParams allowed allowRecur proof
    checkExprRec env structParams allowed allowRecur body
  EAnnot e1 _ -> checkExprRec env structParams allowed allowRecur e1
  ETyped e1 _ -> checkExprRec env structParams allowed allowRecur e1
  _ -> pure ()

checkCaseRec :: TCEnv -> [StructParam] -> Set.Set Text -> Bool -> Maybe Text -> MatchCase -> TypeCheckM ()
checkCaseRec env structParams allowed allowRecur caseData (MatchCase ctor binders body) = do
  let binderNames = map paramName binders
      allowedShadow = foldr Set.delete allowed binderNames
  case caseData of
    Nothing ->
      checkExprRec env structParams allowedShadow allowRecur body
    Just dataName ->
      case Map.lookup ctor (tcCtors env) of
        Just info | ctorData info == dataName -> do
          allowed' <- foldM
            (\acc (Param name ty) -> do
                mData <- dataTypeOf env ty
                pure (if mData == Just dataName then Set.insert name acc else acc))
            allowedShadow
            binders
          checkExprRec env structParams allowed' allowRecur body
        _ ->
          checkExprRec env structParams allowedShadow allowRecur body

checkCompRec :: TCEnv -> [StructParam] -> Set.Set Text -> Bool -> Comp -> TypeCheckM ()
checkCompRec env structParams allowed allowRecur comp = case comp of
  CReturn e1 ->
    checkExprRec env structParams allowed allowRecur e1
  CBind name c1 c2 -> do
    checkCompRec env structParams allowed allowRecur c1
    let allowed' = Set.delete name allowed
    checkCompRec env structParams allowed' allowRecur c2
  CPerform e1 ->
    checkExprRec env structParams allowed allowRecur e1

checkRecurCall :: [StructParam] -> Set.Set Text -> [Expr] -> TypeCheckM ()
checkRecurCall structParams allowed args = do
  let indices = map structIndex structParams
      validIndices = filter (< length args) indices
      ok = any (\idx -> case args !! idx of
                          EVar v | v `Set.member` allowed -> True
                          _ -> False) validIndices
  if null structParams
    then lift (Left (RecursionError noLoc "recur requires a data parameter to decrease"))
    else if null validIndices
      then lift (Left (RecursionError noLoc "recur must be applied to the structural argument"))
      else if ok
        then pure ()
        else lift (Left (RecursionError noLoc "recur must use a structurally smaller argument"))

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
  EListLiteral elems -> case elems of
    [] -> lift (Left (EmptyListLiteral noLoc))
    (first:rest) -> do
      elemTy <- infer env first
      mapM_ (\e -> check env e elemTy) rest
      pure (tList elemTy)
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
  ECompType _eff t -> do
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
    pure (ECompType effectAnyExpr compTy)
  EData params universe cases -> do
    level <- case universe of
      ETypeUniverse n -> pure n
      _ -> lift (Left (ExpectedType noLoc universe))
    envParams <- foldM (\e (Param n ty) -> do
                          _ <- inferUniverse e ty
                          pure (extendLocal e n ty)) env params
    mapM_ (\(DataCase _ ty) -> inferUniverse envParams ty) cases
    pure (mkPi params (ETypeUniverse level))
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
  ETypeClass param kind methods -> do
    ensureUniqueMethodNames (map fst methods)
    _ <- inferUniverse env kind
    let env' = extendLocal env param kind
    mapM_ (\(_, ty) -> inferUniverse env' ty) methods
    pure (classType param kind)
  EInstance clsName instTy methods -> do
    classInfo <- lookupClass noLoc env clsName
    ensureUniqueMethodNames (map fst methods)
    let knownNames = Map.keysSet (tcTypes env)
        implicitVars = Set.toList (freeVars instTy `Set.difference` knownNames)
        implicitSet = Set.fromList implicitVars
        env' = foldl (\e v -> extendLocal e v (tType 0)) env implicitVars
    instKind <- infer env' instTy
    ensureConv env' instKind (classParamKind classInfo)
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
                implTy <- infer env' impl
                let implTy' = stripInstanceTypeParams implicitSet implTy
                ensureConv env' implTy' expectedTy
          ) (classMethods classInfo)
    pure (EApp (EVar (className classInfo)) [instTy])

check :: TCEnv -> Expr -> Expr -> TypeCheckM ()
check env expr expected = case expr of
  EListLiteral elems -> do
    expectedWhnf <- whnf env expected
    case expectedWhnf of
      EApp (ETypeConst TCList) [elemTy] -> mapM_ (\e -> check env e elemTy) elems
      _ -> do
        inferred <- infer env expr
        ensureConvWith env inferred expected (Just (exprToMExprText expr))
  _ -> do
    inferred <- infer env expr
    ensureConvWith env inferred expected (Just (exprToMExprText expr))

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

checkMatch :: TCEnv -> Text -> Expr -> Expr -> [MatchCase] -> TypeCheckM ()
checkMatch env scrutName retTy scrutTy cases = do
  (dataInfo, dataArgs) <- lookupDataInfo env scrutTy
  resolved <- mapM (resolveCase env dataInfo) cases
  ensureCaseSet dataInfo resolved
  mapM_ (checkCase env scrutName retTy scrutTy dataInfo dataArgs) resolved

resolveCase :: TCEnv -> DataInfo -> MatchCase -> TypeCheckM (CtorInfo, [Param], Expr)
resolveCase env dataInfo (MatchCase ctor binders body) = do
  ctorInfo <- lookupCtor env ctor
  when (ctorData ctorInfo /= dataName dataInfo) $
    lift (Left (MatchCaseError noLoc ("unexpected case " <> ctor)))
  pure (ctorInfo, binders, body)

lookupCtor :: TCEnv -> Text -> TypeCheckM CtorInfo
lookupCtor env name =
  case Map.lookup name (tcCtors env) of
    Just info -> pure info
    Nothing -> lift (Left (MatchCaseError noLoc ("unknown constructor " <> name)))

lookupDataInfo :: TCEnv -> Expr -> TypeCheckM (DataInfo, [Expr])
lookupDataInfo env scrutTy = do
  scrutTy' <- whnf env scrutTy
  case scrutTy' of
    ETypeConst TCBoolean -> lookupBuiltin "Boolean" []
    ETypeConst TCUnit -> lookupBuiltin "Unit" []
    EApp (ETypeConst TCList) [elemTy] -> lookupBuiltin "List" [elemTy]
    EApp (ETypeConst TCPair) [a, b] -> lookupBuiltin "Pair" [a, b]
    _ ->
      case collectApp scrutTy' of
        (EVar name, args) ->
          case Map.lookup name (tcData env) of
            Just info -> do
              let expected = length (dataParams info)
              when (length args /= expected) $
                lift (Left (MatchCaseError noLoc ("scrutinee type expects " <> T.pack (show expected) <> " arguments")))
              pure (info, args)
            Nothing -> lift (Left (MatchCaseError noLoc "unsupported scrutinee type"))
        _ -> lift (Left (MatchCaseError noLoc "unsupported scrutinee type"))
  where
    lookupBuiltin name args =
      case Map.lookup name (tcData env) of
        Just info -> pure (info, args)
        Nothing -> lift (Left (MatchCaseError noLoc "unsupported scrutinee type"))

ensureCaseSet :: DataInfo -> [(CtorInfo, [Param], Expr)] -> TypeCheckM ()
ensureCaseSet dataInfo cases = do
  let expected = map ctorName (dataCtors dataInfo)
      actual = map (\(ctorInfo, _, _) -> ctorName ctorInfo) cases
      counts = Map.fromListWith (+) [(n, 1 :: Int) | n <- actual]
      duplicates = Map.keys (Map.filter (>1) counts)
      missing = filter (`notElem` actual) expected
  case duplicates of
    (name:_) -> lift (Left (MatchCaseError noLoc ("duplicate case " <> name)))
    [] -> case missing of
      (name:_) -> lift (Left (MatchCaseError noLoc ("missing case " <> name)))
      [] -> pure ()

checkCase :: TCEnv -> Text -> Expr -> Expr -> DataInfo -> [Expr] -> (CtorInfo, [Param], Expr) -> TypeCheckM ()
checkCase env scrutName retTy scrutTy dataInfo dataArgs (ctorInfo, binders, body) = do
  let ctorParamsList = ctorParams ctorInfo
  when (length ctorParamsList /= length binders) $
    lift (Left (MatchCaseError noLoc ("case " <> ctorName ctorInfo <> " expects " <> T.pack (show (length ctorParamsList)) <> " binders")))
  let dataSubsts = zip (map paramName (dataParams dataInfo)) dataArgs
  (env', substs) <- foldM checkBinder (env, dataSubsts) (zip ctorParamsList binders)
  ctorResult' <- substMany substs (ctorResult ctorInfo)
  let scrutVars = freeVars scrutTy `Set.intersection` tcLocals env
      binderNames = Set.fromList (map paramName binders)
      solvables = Set.delete scrutName (scrutVars `Set.difference` binderNames)
  unifyRes <- unifyIndices env' solvables ctorResult' scrutTy
  case unifyRes of
    Nothing ->
      pure ()
    Just indexSubsts -> do
      let dataArgMap = Map.fromList (zip (map paramName (dataParams dataInfo)) dataArgs)
          ctorDataArgs =
            [ arg
            | Param name _ <- dataParamsForCtor dataInfo ctorInfo
            , Just arg <- [Map.lookup name dataArgMap]
            ]
          ctorTerm = EApp (EVar (ctorName ctorInfo)) (ctorDataArgs ++ map (EVar . paramName) binders)
          indexSubstsList = Map.toList indexSubsts
      ctorTerm' <- substMany indexSubstsList ctorTerm
      expected <- subst scrutName ctorTerm' retTy
      expected' <- substMany indexSubstsList expected
      scrutTy' <- substMany indexSubstsList scrutTy
      let envWithScrut = extendLocal env' scrutName scrutTy'
      envCase <- substEnvTypes scrutName ctorTerm' envWithScrut
      envCase' <- substEnvTypesMany indexSubstsList envCase
      check envCase' body expected'
  where
    checkBinder (envAcc, substs) (Param ctorName' ctorTy, Param caseName caseTy) = do
      let substsNoShadow = filter ((/= ctorName') . fst) substs
      expectedTy <- substMany substsNoShadow ctorTy
      let context = "case binder " <> caseName <> " in " <> ctorName ctorInfo
      ensureConvWith envAcc caseTy expectedTy (Just context)
      let env' = extendLocal envAcc caseName expectedTy
          substs' = (ctorName', EVar caseName) : substsNoShadow
      pure (env', substs')

substEnvTypes :: Text -> Expr -> TCEnv -> TypeCheckM TCEnv
substEnvTypes name replacement env = do
  types' <- Map.traverseWithKey (\_ ty -> subst name replacement ty) (tcTypes env)
  pure env { tcTypes = types' }

substMany :: [(Text, Expr)] -> Expr -> TypeCheckM Expr
substMany subs expr =
  foldM (\acc (n, val) -> subst n val acc) expr subs

substEnvTypesMany :: [(Text, Expr)] -> TCEnv -> TypeCheckM TCEnv
substEnvTypesMany subs env =
  foldM (\acc (n, val) -> substEnvTypes n val acc) env subs

applyArg :: TCEnv -> Expr -> Expr -> TypeCheckM Expr
applyArg env funTy argExpr = do
  funTyWhnf <- whnf env funTy
  case funTyWhnf of
    EForAll v dom cod -> do
      argTy <- infer env argExpr
      ensureConvWith env argTy dom (Just (exprToMExprText argExpr))
      subst v argExpr cod
    _ -> lift (Left (NotAFunction noLoc funTyWhnf))

expectCompType :: TCEnv -> Expr -> TypeCheckM Expr
expectCompType env ty = do
  ty' <- whnf env ty
  case ty' of
    ECompType _ inner -> pure inner
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
  TCDictionary ->
    EForAll "K" (tType 0)
      (EForAll "V" (tType 0) (tType 0))
  TCListener -> tType 0
  TCSocket -> tType 0

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
  , ("char-code-prim", tFun tString tNat)
  , ("char-from-code-prim", tFun tNat tString)
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

  , ("natural-to-peano-prim", tFun tNat (tList tUnit))
  , ("List::empty", EForAll "A" (tType 0) (tList (EVar "A")))
  , ("List::cons", EForAll "A" (tType 0) (tFun (EVar "A") (tFun (tList (EVar "A")) (tList (EVar "A")))))
  , ("Pair::pair", EForAll "A" (tType 0)
      (EForAll "B" (tType 0)
        (tFun (EVar "A") (tFun (EVar "B") (tPair (EVar "A") (EVar "B"))))))
  , ("dictionary-empty-prim",
      EForAll "K" (tType 0)
        (EForAll "V" (tType 0)
          (tDictionary (EVar "K") (EVar "V"))))
  , ("dictionary-insert-prim",
      EForAll "K" (tType 0)
        (EForAll "V" (tType 0)
          (tFun (tFun (EVar "K") tNat)
            (tFun (tFun (EVar "K") (tFun (EVar "K") tBool))
              (tFun (EVar "K")
                (tFun (EVar "V")
                  (tFun (tDictionary (EVar "K") (EVar "V"))
                        (tDictionary (EVar "K") (EVar "V")))))))))
  , ("dictionary-lookup-prim",
      EForAll "K" (tType 0)
        (EForAll "V" (tType 0)
          (tFun (tFun (EVar "K") tNat)
            (tFun (tFun (EVar "K") (tFun (EVar "K") tBool))
              (tFun (EVar "K")
                (tFun (tDictionary (EVar "K") (EVar "V"))
                      (tOption (EVar "V"))))))))
  , ("dictionary-remove-prim",
      EForAll "K" (tType 0)
        (EForAll "V" (tType 0)
          (tFun (tFun (EVar "K") tNat)
            (tFun (tFun (EVar "K") (tFun (EVar "K") tBool))
              (tFun (EVar "K")
                (tFun (tDictionary (EVar "K") (EVar "V"))
                      (tDictionary (EVar "K") (EVar "V"))))))))
  , ("dictionary-size-prim",
      EForAll "K" (tType 0)
        (EForAll "V" (tType 0)
          (tFun (tDictionary (EVar "K") (EVar "V")) tNat)))
  , ("dictionary-to-list-prim",
      EForAll "K" (tType 0)
        (EForAll "V" (tType 0)
          (tFun (tDictionary (EVar "K") (EVar "V"))
                (tList (tPair (EVar "K") (EVar "V"))))))
  , ("Boolean::false", tBool)
  , ("Boolean::true", tBool)
  , ("Unit::tt", tUnit)

  , ("tcp-listen-prim", tFun tNat (tComp tListener))
  , ("tcp-accept-prim", tFun tListener (tComp tSocket))
  , ("tcp-recv-prim", tFun tSocket (tComp tString))
  , ("tcp-send-prim", tFun tSocket (tFun tString (tComp tUnit)))
  , ("tcp-close-prim", tFun tSocket (tComp tUnit))
  , ("tcp-close-listener-prim", tFun tListener (tComp tUnit))
  , ("tcp-select-listener-prim", tFun tListener (tFun tNat (tComp tBool)))
  , ("tcp-select-socket-prim", tFun tSocket (tFun tNat (tComp tBool)))
  , ("sleep-prim", tFun tNat (tComp tUnit))
  , ("timeout-prim",
      EForAll "A" (tType 0)
        (tFun tNat (tFun (tComp (EVar "A")) (tComp (tOption (EVar "A"))))))
  , ("panic-prim", tFun tString (tComp tUnit))

  , ("print-prim", tFun tString (tComp tUnit))
  , ("capture-output-prim",
      EForAll "A" (tType 0)
        (tFun (tComp (EVar "A"))
          (tComp (tPair (tList tString) (EVar "A")))))
  , ("forever-prim", tFun (tComp tUnit) (tComp tUnit))
  , ("on-signal-prim", tFun tString (tFun (tComp tUnit) (tComp tUnit)))
  , ("assert-hit-prim", tComp tUnit)
  , ("get-line-prim", tComp tString)
  , ("cli-args-prim", tComp (tList tString))
  , ("current-directory-prim", tComp tString)
  , ("time-now-prim", tComp tNat)
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
  , ("walk-filter-prim", tFun tString (tFun tString (tFun (tList tString) (tComp (tList tString)))))
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

normalizeModuleWithImports :: FilePath -> Text -> Module -> IO (Either TypeError Module)
normalizeModuleWithImports projectRoot _sourceContents m = do
  importedEnv <- loadTypeImports projectRoot m
  let envWithOpens = processOpens (modOpens m) importedEnv
  pure $ runTypeCheck $ do
    env <- typeCheckModuleWithEnv envWithOpens m
    normalized <- normalizeModule env m
    case injectRecursors "<module>" normalized of
      Left msg -> lift (Left (MatchCaseError noLoc (T.pack msg)))
      Right m' -> pure m'

normalizeTypeEnvWithImports :: FilePath -> Text -> Module -> IO (Either TypeError TypeEnv)
normalizeTypeEnvWithImports projectRoot _sourceContents m = do
  importedEnv <- loadTypeImports projectRoot m
  let envWithOpens = processOpens (modOpens m) importedEnv
  pure $ runTypeCheck $ do
    env <- typeCheckModuleWithEnv envWithOpens m
    normalizeTypeEnv env

typeCheckAndNormalizeWithEnv :: TCEnv -> Module -> Either TypeError (TypeEnv, Module)
typeCheckAndNormalizeWithEnv importedEnv m =
  runTypeCheck $ do
    let envWithOpens = processOpens (modOpens m) importedEnv
    env <- typeCheckModuleWithEnv envWithOpens m
    normalized <- normalizeModule env m
    normalizedWithRecursors <- case injectRecursors "<module>" normalized of
      Left msg -> lift (Left (MatchCaseError noLoc (T.pack msg)))
      Right m' -> pure m'
    pure (tcTypes env, normalizedWithRecursors)

typeCheckAndNormalizeWithImports :: FilePath -> Text -> Module -> IO (Either TypeError (TypeEnv, Module))
typeCheckAndNormalizeWithImports projectRoot _sourceContents m = do
  importedEnv <- loadTypeImports projectRoot m
  let envWithOpens = processOpens (modOpens m) importedEnv
  pure $ runTypeCheck $ do
    env <- typeCheckModuleWithEnv envWithOpens m
    normalized <- normalizeModule env m
    normalizedWithRecursors <- case injectRecursors "<module>" normalized of
      Left msg -> lift (Left (MatchCaseError noLoc (T.pack msg)))
      Right m' -> pure m'
    pure (tcTypes env, normalizedWithRecursors)

moduleDigest :: FilePath -> Text -> Module -> IO String
moduleDigest projectRoot sourceContents m = do
  fst <$> moduleDigestWithImports projectRoot sourceContents m

moduleDigestWithImports :: FilePath -> Text -> Module -> IO (String, TCEnv)
moduleDigestWithImports projectRoot sourceContents m = do
  moduleDigestWithImportsScope projectRoot ProjectScope sourceContents m

moduleDigestWithImportsScope :: FilePath -> ImportScope -> Text -> Module -> IO (String, TCEnv)
moduleDigestWithImportsScope projectRoot scope sourceContents m = do
  let sourceHash = hashText sourceContents
  (importedEnv, depHashes) <- loadTypeImportsWithDigest projectRoot scope Set.empty (modImports m)
  let depMap = Map.fromList depHashes
      depDigest = computeDepDigest depMap
  pure (computeDigest sourceHash depDigest, importedEnv)

typeCheckModuleWithEnv :: TCEnv -> Module -> TypeCheckM TCEnv
typeCheckModuleWithEnv initialEnv (Module _name _imports _opens defs) =
  foldM typeCheckDef initialEnv defs

typeCheckDef :: TCEnv -> Definition -> TypeCheckM TCEnv
typeCheckDef env (Definition tr name body) = case body of
  EData params universe cases -> do
    (info, ty) <- checkDataDef env name params universe cases
    let envWithData = extendData env name ty info
        recName = name <> "::recursor"
    when (Map.member recName (tcTypes envWithData)) $
      lift (Left (MatchCaseError noLoc ("recursor name already defined: " <> recName)))
    let recCases = [DataCase (ctorName ctor) (ctorType ctor) | ctor <- dataCtors info]
        recDef = recursorDefinition name params (dataUniverse info) recCases
    recTy <- infer envWithData (defBody recDef)
    pure (extendGlobal envWithData recName recTy Transparent (defBody recDef))
  ETypeClass param kind methods -> do
    _ <- infer env body
    let info = ClassInfo
          { className = name
          , classParam = param
          , classParamKind = kind
          , classMethods = methods
          }
    pure (extendClass env name info (classType param kind))
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

checkDataDef :: TCEnv -> Text -> [Param] -> Expr -> [DataCase] -> TypeCheckM (DataInfo, Expr)
checkDataDef env name params universe cases = do
  level <- case universe of
    ETypeUniverse n -> pure n
    _ -> lift (Left (ExpectedType noLoc universe))
  let ctorNames = map dataCaseName cases
      counts = Map.fromListWith (+) [(n, 1 :: Int) | n <- ctorNames]
      duplicates = Map.keys (Map.filter (>1) counts)
  case duplicates of
    (dup:_) -> lift (Left (MatchCaseError noLoc ("duplicate constructor " <> dup)))
    [] -> pure ()
  envParams <- foldM (\e (Param n ty) -> do
                        _ <- inferUniverse e ty
                        pure (extendLocal e n ty)) env params
  let dataTy = mkPi params (ETypeUniverse level)
      envWithType = extendType envParams name dataTy
  ctorInfos <- mapM (checkCtor envWithType) cases
  let info = DataInfo
        { dataName = name
        , dataParams = params
        , dataUniverse = level
        , dataCtors = ctorInfos
        }
  pure (info, dataTy)
  where
    checkCtor env' (DataCase ctorName ctorTy) = do
      let prefix = name <> "::"
      when (not (prefix `T.isPrefixOf` ctorName)) $
        lift (Left (MatchCaseError noLoc ("constructor must be type-qualified: " <> ctorName)))
      _ <- inferUniverse env' ctorTy
      ctorTy' <- normalize env' ctorTy
      let (_ctorParams, ctorResult) = splitForAll ctorTy'
          (headExpr, args) = collectApp ctorResult
      case headExpr of
        EVar headName | headName == name -> do
          let expectedArgs = length params
          when (length args /= expectedArgs) $
            lift (Left (MatchCaseError noLoc ("constructor result arity mismatch: " <> ctorName)))
          pure (mkCtorInfo name ctorName ctorTy')
        _ ->
          lift (Left (MatchCaseError noLoc ("constructor result must be " <> name)))

annotateModule :: TypeEnv -> Module -> Either TypeError Module
annotateModule _env m = Right m

--------------------------------------------------------------------------------
-- Import loading

data TypeImportCache = TypeImportCache
  { cacheVersion :: Int
  , cacheSourceHash :: String
  , cacheDepMap :: Map.Map Text String
  , cacheDepDigest :: String
  , cacheEnvSelf :: TCEnv
  , cacheParsed :: Module
  } deriving (Show, Read)

type ImportCache = Map.Map FilePath TypeImportCache

{-# NOINLINE importCacheRef #-}
importCacheRef :: IORef ImportCache
importCacheRef = unsafePerformIO (newIORef Map.empty)

cacheVersionCurrent :: Int
cacheVersionCurrent = 3

cacheRoot :: FilePath -> FilePath
cacheRoot projectRoot = projectRoot </> ".locque-cache" </> "typecheck"

cachePathFor :: FilePath -> FilePath -> FilePath
cachePathFor projectRoot sourcePath =
  let rel = makeRelative projectRoot sourcePath
      safeRel =
        if isAbsolute rel || ".." `isPrefixOf` rel
          then hashString sourcePath
          else rel
  in cacheRoot projectRoot </> safeRel <.> "cache"

hashText :: Text -> String
hashText = hashString . T.unpack

hashString :: String -> String
hashString str =
  let fnvOffset :: Word64
      fnvOffset = 14695981039346656037
      fnvPrime :: Word64
      fnvPrime = 1099511628211
      step acc c = (acc `xor` fromIntegral (ord c)) * fnvPrime
      hashValue = foldl step fnvOffset str
  in showHex hashValue ""

computeDepDigest :: Map.Map Text String -> String
computeDepDigest deps =
  let depsStr = concatMap (\(name, dep) -> T.unpack name ++ ":" ++ dep ++ ";") (Map.toList deps)
  in hashString depsStr

computeDigest :: String -> String -> String
computeDigest sourceHash depDigest =
  hashString (sourceHash ++ "|" ++ depDigest)

readCacheEntry :: FilePath -> FilePath -> String -> IO (Maybe TypeImportCache)
readCacheEntry projectRoot sourcePath sourceHash = do
  cacheMap <- readIORef importCacheRef
  case Map.lookup sourcePath cacheMap of
    Just entry
      | cacheVersion entry == cacheVersionCurrent
      , cacheSourceHash entry == sourceHash -> pure (Just entry)
    _ -> do
      let cachePath = cachePathFor projectRoot sourcePath
      exists <- doesFileExist cachePath
      if not exists
        then pure Nothing
        else do
          raw <- BS.readFile cachePath
          case readMaybe (BS8.unpack raw) of
            Just entry
              | cacheVersion entry == cacheVersionCurrent
              , cacheSourceHash entry == sourceHash -> do
                  modifyIORef' importCacheRef (Map.insert sourcePath entry)
                  pure (Just entry)
            _ -> pure Nothing

writeCacheEntry :: FilePath -> FilePath -> TypeImportCache -> IO ()
writeCacheEntry projectRoot sourcePath entry = do
  let cachePath = cachePathFor projectRoot sourcePath
  createDirectoryIfMissing True (takeDirectory cachePath)
  writeFile cachePath (show entry)
  modifyIORef' importCacheRef (Map.insert sourcePath entry)

loadTypeImports :: FilePath -> Module -> IO TCEnv
loadTypeImports projectRoot (Module _ imports _ _) =
  fst <$> loadTypeImportsWithDigest projectRoot ProjectScope Set.empty imports

loadTypeImportWithDigest :: FilePath -> ImportScope -> Set.Set Text -> Import -> IO (TCEnv, (Text, String))
loadTypeImportWithDigest projectRoot scope visiting (Import modName alias) = do
  when (modName `Set.member` visiting) $
    error $ "Import cycle detected: " ++ T.unpack modName
  ResolvedModule path nextScope <-
    resolveModulePath projectRoot "lib" scope modName
  contents <- TIO.readFile path
  let sourceHash = hashText contents
  cached <- readCacheEntry projectRoot path sourceHash

  parsed <- case cached of
    Just entry -> pure (cacheParsed entry)
    Nothing -> case takeExtension path of
      ".lq"  -> case parseMExprFile path contents of
        Left err -> error err
        Right m -> pure m
      ".lqs" -> case parseModuleFile path contents of
        Left err -> error err
        Right m -> pure m
      _      -> error $ "Unknown file extension: " ++ path

  let Module _ _ opens defs = parsed
      visiting' = Set.insert modName visiting
  (envImports, depHashes) <- loadTypeImportsWithDigest projectRoot nextScope visiting' (modImports parsed)
  let envWithOpens = processOpens opens envImports
      depMap = Map.fromList depHashes
      depDigest = computeDepDigest depMap
  envSelf <- case cached of
    Just entry
      | cacheDepMap entry == depMap -> pure (cacheEnvSelf entry)
    _ ->
      case runTypeCheck (typeCheckModuleWithEnv envWithOpens parsed) of
        Left tcErr -> error $ "Type error in " ++ path ++ ": " ++ show tcErr
        Right env -> do
          let entry = TypeImportCache
                { cacheVersion = cacheVersionCurrent
                , cacheSourceHash = sourceHash
                , cacheDepMap = depMap
                , cacheDepDigest = depDigest
                , cacheEnvSelf = env
                , cacheParsed = parsed
                }
          writeCacheEntry projectRoot path entry
          pure env
  let defNames = map defName defs
      localDataNames = [defName d | d@(Definition _ _ body) <- defs, isData body]
      localClassNames = [defName d | d@(Definition _ _ body) <- defs, isClass body]
      localInstanceNames = [defName d | d@(Definition _ _ body) <- defs, isInstance body]
      localClassSet = Set.fromList localClassNames
      localNames = Set.fromList defNames
      envValues = foldl (insertQualified alias envSelf localNames) envWithOpens defNames
      envData = foldl (insertQualifiedData alias envSelf) envValues localDataNames
      envClasses = foldl (insertQualifiedClass alias envSelf) envData localClassNames
      envFinal = foldl (insertQualifiedInstance alias envSelf localClassSet) envClasses localInstanceNames
      digest = computeDigest sourceHash depDigest
  pure (envFinal, (modName, digest))
  where
    isClass expr = case expr of
      ETypeClass _ _ _ -> True
      _ -> False
    isInstance expr = case expr of
      EInstance _ _ _ -> True
      _ -> False
    isData expr = case expr of
      EData _ _ _ -> True
      _ -> False

loadTypeImportsWithDigest :: FilePath -> ImportScope -> Set.Set Text -> [Import] -> IO (TCEnv, [(Text, String)])
loadTypeImportsWithDigest projectRoot scope visiting imports = do
  envsWithDigests <- mapM (loadTypeImportWithDigest projectRoot scope visiting) imports
  let envs = map fst envsWithDigests
      digests = map snd envsWithDigests
  let mergedTypes = Map.unions (tcTypes emptyEnv : map tcTypes envs)
      mergedDefs = Map.unions (tcDefs emptyEnv : map tcDefs envs)
      mergedClasses = Map.unions (tcClasses emptyEnv : map tcClasses envs)
      mergedInstances = Map.unionsWith (++) (tcInstances emptyEnv : map tcInstances envs)
      mergedData = Map.unions (tcData emptyEnv : map tcData envs)
      mergedCtors = Map.unions (tcCtors emptyEnv : map tcCtors envs)
  pure (emptyEnv
    { tcTypes = mergedTypes
    , tcDefs = mergedDefs
    , tcClasses = mergedClasses
    , tcInstances = mergedInstances
    , tcData = mergedData
    , tcCtors = mergedCtors
    }, digests)

insertQualified :: Text -> TCEnv -> Set.Set Text -> TCEnv -> Text -> TCEnv
insertQualified alias envSelf localNames env name =
  case Map.lookup name (tcTypes envSelf) of
    Just scheme ->
      let qualified = qualifyName alias name
          scheme' = qualifyDefBody alias localNames scheme
          envWithType = env { tcTypes = Map.insert qualified scheme' (tcTypes env) }
      in case Map.lookup name (tcDefs envSelf) of
          Just (tr, body) ->
            let body' = qualifyDefBody alias localNames body
            in envWithType { tcDefs = Map.insert qualified (tr, body') (tcDefs envWithType) }
          Nothing -> envWithType
    Nothing -> env

insertQualifiedData :: Text -> TCEnv -> TCEnv -> Text -> TCEnv
insertQualifiedData alias envSelf env name =
  case Map.lookup name (tcData envSelf) of
    Just info ->
      let info' = qualifyDataInfo alias info
          ty = mkPi (dataParams info') (ETypeUniverse (dataUniverse info'))
      in extendData env (dataName info') ty info'
    Nothing -> env

qualifyDataInfo :: Text -> DataInfo -> DataInfo
qualifyDataInfo alias info =
  let qualifiedName = qualifyName alias (dataName info)
      renameExpr expr =
        case runTypeCheck (subst (dataName info) (EVar qualifiedName) expr) of
          Left err -> error ("Failed to qualify data type: " ++ show err)
          Right e -> e
      params' = [Param n (renameExpr (paramType p)) | p@(Param n _) <- dataParams info]
      ctors' = map (qualifyCtor qualifiedName renameExpr) (dataCtors info)
  in info
      { dataName = qualifiedName
      , dataParams = params'
      , dataCtors = ctors'
      }
  where
    qualifyCtor qualifiedName renameExpr ctor =
      let ctorTy' = renameExpr (ctorType ctor)
          ctorName' = qualifyName alias (ctorName ctor)
      in mkCtorInfo qualifiedName ctorName' ctorTy'

qualifyDefBody :: Text -> Set.Set Text -> Expr -> Expr
qualifyDefBody alias localNames body =
  foldl qualifyOne body (Set.toList localNames)
  where
    qualifyOne expr name =
      case runTypeCheck (subst name (EVar (qualifyName alias name)) expr) of
        Left err -> error ("Failed to qualify definition body: " ++ show err)
        Right e -> e

insertQualifiedClass :: Text -> TCEnv -> TCEnv -> Text -> TCEnv
insertQualifiedClass alias envSelf env name =
  case Map.lookup name (tcClasses envSelf) of
    Just info ->
      let qualified = qualifyName alias name
          info' = info { className = qualified }
          ty = classType (classParam info) (classParamKind info)
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
          withDefs = case Map.lookup qualifiedName (tcDefs withValue) of
            Just def -> withValue { tcDefs = Map.insert name def (tcDefs withValue) }
            Nothing -> withValue
          withData = case Map.lookup qualifiedName (tcData withDefs) of
            Just info -> withDefs { tcData = Map.insert name info (tcData withDefs) }
            Nothing -> withDefs
          withCtors = case Map.lookup qualifiedName (tcCtors withData) of
            Just info -> withData { tcCtors = Map.insert name info (tcCtors withData) }
            Nothing -> withData
      in case Map.lookup qualifiedName (tcClasses withCtors) of
          Just info -> withCtors { tcClasses = Map.insert name info (tcClasses withCtors) }
          Nothing -> withCtors
