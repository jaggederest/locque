{-# LANGUAGE OverloadedStrings #-}
module DictPass
  ( transformModuleWithEnvs
  ) where

import AST
import Parser (parseMExprFile, parseModuleFile)
import Utils (modNameToPath, qualifyName)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import System.FilePath ((</>), (<.>), takeExtension)
import qualified Data.Text.IO as TIO
import System.IO.Error (catchIOError, isDoesNotExistError)

data ClassInfo = ClassInfo
  { classParam   :: Text
  , classParamKind :: Expr
  , classMethods :: [(Text, Expr)]
  }

data FnSig = FnSig
  { fnSigName        :: Text
  , fnSigParams      :: [Param]
  , fnSigConstraints :: [Constraint]
  }

data InstanceInfo = InstanceInfo
  { instDefName     :: Text
  , instClassName   :: Text
  , instType        :: Expr
  , instMethods     :: Map.Map Text Expr
  , instMethodNames :: Map.Map Text Text
  , instTransparency :: Transparency
  }

data DictEnvs = DictEnvs
  { envClasses   :: Map.Map Text ClassInfo
  , envInstances :: Map.Map Text [InstanceInfo]
  , envFns       :: Map.Map Text FnSig
  }

data LocalInfo = LocalInfo
  { localClasses   :: Map.Map Text ClassInfo
  , localFns       :: Map.Map Text FnSig
  , localInstances :: Map.Map Text InstanceInfo
  }

data TransformCtx = TransformCtx
  { ctxClasses         :: Map.Map Text ClassInfo
  , ctxInstances       :: Map.Map Text [InstanceInfo]
  , ctxFns             :: Map.Map Text FnSig
  , ctxOpenClassAliases :: Map.Map Text Text
  , ctxMethodMap       :: Map.Map Text Text
  , ctxBound           :: Set.Set Text
  }

emptyEnvs :: DictEnvs
emptyEnvs = DictEnvs Map.empty Map.empty Map.empty

mergeEnvs :: DictEnvs -> DictEnvs -> DictEnvs
mergeEnvs left right =
  DictEnvs
    { envClasses = Map.union (envClasses left) (envClasses right)
    , envInstances =
        Map.map dedupeInstances
          (Map.unionWith (++) (envInstances left) (envInstances right))
    , envFns = Map.union (envFns left) (envFns right)
    }

dedupeInstances :: [InstanceInfo] -> [InstanceInfo]
dedupeInstances insts =
  let step (seen, acc) inst =
        let name = instDefName inst
        in if Set.member name seen
            then (seen, acc)
            else (Set.insert name seen, acc ++ [inst])
  in snd (foldl step (Set.empty, []) insts)

--------------------------------------------------------------------------------
-- Entry point

transformModuleWithEnvs :: FilePath -> Module -> IO Module
transformModuleWithEnvs projectRoot m@(Module name imports opens defs) = do
  importEnvs <- loadDictImports projectRoot m
  let localClassesMap = collectLocalClasses defs
      localClassNames = Set.fromList (Map.keys localClassesMap)
      openAliases = buildOpenClassAliases localClassNames importEnvs opens
      classEnvForResolution = Map.union localClassesMap (envClasses importEnvs)
      localFnsMap = collectLocalFns classEnvForResolution openAliases defs
      localInstancesMap = collectLocalInstances classEnvForResolution openAliases defs
      localInfo = LocalInfo localClassesMap localFnsMap localInstancesMap
      importOpened = applyFnOpens importEnvs opens
      envForTransform = mergeEnvs (localInfoToEnvs localInfo) importOpened
      ctx = TransformCtx
        { ctxClasses = envClasses envForTransform
        , ctxInstances = envInstances envForTransform
        , ctxFns = envFns envForTransform
        , ctxOpenClassAliases = openAliases
        , ctxMethodMap = Map.empty
        , ctxBound = Set.empty
        }
      transformedDefs = concatMap (transformDefinition ctx localInfo) defs
  pure (Module name imports opens transformedDefs)

--------------------------------------------------------------------------------
-- Import envs

loadDictImports :: FilePath -> Module -> IO DictEnvs
loadDictImports projectRoot (Module _ imports _ _) = do
  envs <- mapM (loadDictImport projectRoot) imports
  pure (foldl mergeEnvs emptyEnvs envs)

loadDictImport :: FilePath -> Import -> IO DictEnvs
loadDictImport projectRoot (Import modName alias) = do
  let modPath = modNameToPath modName
      basePath = if "test/" `isPrefixOf` modPath
                 then projectRoot </> modPath
                 else projectRoot </> "lib" </> modPath
      lqPath = basePath <.> "lq"
      lqsPath = basePath <.> "lqs"

  (path, contents) <- tryLoadFile lqPath `catchIOError` \e ->
    if isDoesNotExistError e then tryLoadFile lqsPath else ioError e

  parsed <- case takeExtension path of
    ".lq"  -> case parseMExprFile path contents of
      Left err -> error err
      Right m -> pure m
    ".lqs" -> case parseModuleFile path contents of
      Left err -> error err
      Right m -> pure m
    _      -> error $ "Unknown file extension: " ++ path

  importEnvs <- loadDictImports projectRoot parsed
  let localClassesMap = collectLocalClasses (modDefs parsed)
      localClassNames = Set.fromList (Map.keys localClassesMap)
      openAliases = buildOpenClassAliases localClassNames importEnvs (modOpens parsed)
      classEnvForResolution = Map.union localClassesMap (envClasses importEnvs)
      localFnsMap = collectLocalFns classEnvForResolution openAliases (modDefs parsed)
      localInstancesMap = collectLocalInstances classEnvForResolution openAliases (modDefs parsed)
      localInfo = LocalInfo localClassesMap localFnsMap localInstancesMap

  pure (mergeEnvs (qualifyLocalInfo alias localInfo) importEnvs)
  where
    tryLoadFile p = do
      c <- TIO.readFile p
      pure (p, c)

--------------------------------------------------------------------------------
-- Local env collection

collectLocalClasses :: [Definition] -> Map.Map Text ClassInfo
collectLocalClasses defs =
  Map.fromList
    [ (defName, ClassInfo param kind methods)
    | Definition _ defName (ETypeClass param kind methods) <- defs
    ]

collectLocalFns :: Map.Map Text ClassInfo -> Map.Map Text Text -> [Definition] -> Map.Map Text FnSig
collectLocalFns classEnv openAliases defs =
  Map.fromList (mapMaybe toSig defs)
  where
    toSig (Definition _ name body) =
      case unwrapFunction body of
        Just (params, constraints) ->
          let resolved = map (resolveConstraint classEnv openAliases) constraints
          in if null resolved
              then Nothing
              else Just (name, FnSig name params resolved)
        Nothing -> Nothing

collectLocalInstances :: Map.Map Text ClassInfo -> Map.Map Text Text -> [Definition] -> Map.Map Text InstanceInfo
collectLocalInstances classEnv openAliases defs =
  fst (foldl collect (Map.empty, usedNames) defs)
  where
    usedNames = Set.fromList (map defName defs)

    collect (acc, used) (Definition tr name body) = case body of
      EInstance className instTy methods ->
        let className' = resolveClassName classEnv openAliases className
            methodMap = Map.fromList methods
            (methodNames, used') = buildInstanceMethodNames name (Map.keys methodMap) used
            instInfo = InstanceInfo
              { instDefName = name
              , instClassName = className'
              , instType = instTy
              , instMethods = methodMap
              , instMethodNames = methodNames
              , instTransparency = tr
              }
        in (Map.insert name instInfo acc, used')
      _ -> (acc, used)

buildOpenClassAliases :: Set.Set Text -> DictEnvs -> [Open] -> Map.Map Text Text
buildOpenClassAliases localClassNames importEnvs opens =
  foldl addOpen Map.empty opens
  where
    addOpen acc (Open modAlias names) =
      foldl (addAlias modAlias) acc names

    addAlias modAlias acc name
      | Set.member name localClassNames = acc
      | otherwise =
          let qualified = qualifyName modAlias name
          in if Map.member qualified (envClasses importEnvs)
              then Map.insert name qualified acc
              else acc

applyFnOpens :: DictEnvs -> [Open] -> DictEnvs
applyFnOpens importEnvs opens =
  importEnvs { envFns = fnEnv' }
  where
    qualifiedFns = envFns importEnvs
    fnEnv' = foldl addOpen qualifiedFns opens

    addOpen env (Open modAlias names) =
      foldl (addName modAlias) env names

    addName modAlias env name =
      let qualified = qualifyName modAlias name
      in case Map.lookup qualified qualifiedFns of
          Just sig -> Map.insert name sig env
          Nothing -> env

localInfoToEnvs :: LocalInfo -> DictEnvs
localInfoToEnvs info =
  DictEnvs
    { envClasses = localClasses info
    , envFns = localFns info
    , envInstances = Map.fromListWith (++)
        [ (instClassName inst, [inst])
        | inst <- Map.elems (localInstances info)
        ]
    }

qualifyLocalInfo :: Text -> LocalInfo -> DictEnvs
qualifyLocalInfo alias info =
  let localClassNames = Set.fromList (Map.keys (localClasses info))
      classes' = Map.mapKeys (qualifyName alias) (localClasses info)
      fns' =
        Map.mapKeys (qualifyName alias)
          (Map.map (qualifyFnSig alias localClassNames) (localFns info))
      insts' = Map.map (qualifyInstance alias localClassNames) (localInstances info)
      instEnv = Map.fromListWith (++)
        [ (instClassName inst, [inst])
        | inst <- Map.elems insts'
        ]
  in DictEnvs classes' instEnv fns'

qualifyFnSig :: Text -> Set.Set Text -> FnSig -> FnSig
qualifyFnSig alias localClassNames sig =
  let qualifyClass name =
        if Set.member name localClassNames
          then qualifyName alias name
          else name
      constraints' =
        [ Constraint (qualifyClass cls) ty
        | Constraint cls ty <- fnSigConstraints sig
        ]
  in sig
      { fnSigName = qualifyName alias (fnSigName sig)
      , fnSigConstraints = constraints'
      }

qualifyInstance :: Text -> Set.Set Text -> InstanceInfo -> InstanceInfo
qualifyInstance alias localClassNames inst =
  let className' =
        if Set.member (instClassName inst) localClassNames
          then qualifyName alias (instClassName inst)
          else instClassName inst
      methodNames' = Map.map (qualifyName alias) (instMethodNames inst)
  in inst
      { instClassName = className'
      , instMethodNames = methodNames'
      }

--------------------------------------------------------------------------------
-- Transform

transformDefinition :: TransformCtx -> LocalInfo -> Definition -> [Definition]
transformDefinition ctx localInfo def@(Definition tr name body) = case body of
  ETypeClass _ _ _ -> []
  EInstance _ _ _ -> transformInstanceDefinition ctx localInfo name tr
  _ -> [def { defBody = transformExpr ctx body }]

transformInstanceDefinition :: TransformCtx -> LocalInfo -> Text -> Transparency -> [Definition]
transformInstanceDefinition ctx localInfo instName tr =
  case Map.lookup instName (localInstances localInfo) of
    Nothing -> error $ "Unknown instance definition: " ++ T.unpack instName
    Just instInfo -> emitInstanceMethods ctx instInfo tr

emitInstanceMethods :: TransformCtx -> InstanceInfo -> Transparency -> [Definition]
emitInstanceMethods ctx instInfo tr =
  case Map.lookup (instClassName instInfo) (ctxClasses ctx) of
    Nothing -> error $ "Unknown typeclass in instance: " ++ T.unpack (instClassName instInfo)
    Just classInfo ->
      let classMethodNames = map fst (classMethods classInfo)
          instMethodNames = Map.keys (instMethods instInfo)
          missing = filter (`notElem` instMethodNames) classMethodNames
          extras = filter (`notElem` classMethodNames) instMethodNames
      in if not (null missing)
          then error $ "Instance " ++ T.unpack (instDefName instInfo)
            ++ " missing methods: " ++ show missing
          else if not (null extras)
            then error $ "Instance " ++ T.unpack (instDefName instInfo)
              ++ " has extra methods: " ++ show extras
            else
              map (emitMethod classInfo) (classMethods classInfo)
  where
    emitMethod classInfo (methodName, methodTy) =
      case (Map.lookup methodName (instMethods instInfo), Map.lookup methodName (instMethodNames instInfo)) of
        (Just bodyExpr, Just defName) ->
          let methodType = specializeMethodType classInfo (instType instInfo) methodTy
              body' = transformExpr ctx bodyExpr
          in Definition tr defName (EAnnot body' methodType)
        _ -> error $ "Instance method lookup failed for " ++ T.unpack methodName

specializeMethodType :: ClassInfo -> Expr -> Expr -> Expr
specializeMethodType classInfo instTy methodTy =
  let sub = Map.singleton (classParam classInfo) instTy
  in substType sub methodTy

--------------------------------------------------------------------------------
-- Expression transform

transformExpr :: TransformCtx -> Expr -> Expr
transformExpr ctx expr = case expr of
  EVar name ->
    case Map.lookup name (ctxMethodMap ctx) of
      Just replacement
        | not (Set.member name (ctxBound ctx)) -> EVar replacement
      _ -> expr
  ELit _ -> expr
  ETypeConst _ -> expr
  ETypeUniverse _ -> expr
  EForAll v dom cod ->
    EForAll v (transformType dom) (transformType cod)
  EThereExists v dom cod ->
    EThereExists v (transformType dom) (transformType cod)
  ECompType t -> ECompType (transformType t)
  EEqual ty lhs rhs ->
    EEqual (transformType ty) (transformExpr ctx lhs) (transformExpr ctx rhs)
  EReflexive ty term ->
    EReflexive (transformType ty) (transformExpr ctx term)
  ERewrite family proof body ->
    ERewrite (transformExpr ctx family) (transformExpr ctx proof) (transformExpr ctx body)
  EPack v dom cod witness body ->
    EPack v (transformType dom) (transformType cod)
      (transformExpr ctx witness) (transformExpr ctx body)
  EUnpack packed x y body ->
    let ctx' = ctx { ctxBound = Set.insert x (Set.insert y (ctxBound ctx)) }
    in EUnpack (transformExpr ctx packed) x y (transformExpr ctx' body)
  ELift ty fromLevel toLevel ->
    ELift (transformType ty) fromLevel toLevel
  EUp ty fromLevel toLevel body ->
    EUp (transformType ty) fromLevel toLevel (transformExpr ctx body)
  EDown ty fromLevel toLevel body ->
    EDown (transformType ty) fromLevel toLevel (transformExpr ctx body)
  EApp f args ->
    let fnSig = lookupFnSig ctx f
        f' = transformExpr ctx f
        args' = map (transformExpr ctx) args
        args'' = case fnSig of
          Nothing -> args'
          Just sig -> insertDictArgs ctx sig args'
    in EApp f' args''
  EFunction params constraints ret body ->
    let constraints' = map (resolveConstraint (ctxClasses ctx) (ctxOpenClassAliases ctx)) constraints
        (methodParams, methodMapLocal) =
          if null constraints'
            then ([], Map.empty)
            else buildMethodParams (ctxClasses ctx) constraints' (Set.fromList (map paramName params))
        (typeParams, valueParams) = splitTypeParams params
        params' = typeParams ++ methodParams ++ valueParams
        methodMap = Map.union methodMapLocal (ctxMethodMap ctx)
        bound' = ctxBound ctx `Set.union` Set.fromList (map paramName params')
        ctx' = ctx { ctxMethodMap = methodMap, ctxBound = bound' }
        body' = transformFunctionBody ctx' body
    in EFunction params' [] ret body'
  ELet name val body ->
    let localSig = extractFnSig ctx name val
        val' = transformExpr ctx val
        ctx' = ctx
          { ctxBound = Set.insert name (ctxBound ctx)
          , ctxFns = maybe (ctxFns ctx) (\sig -> Map.insert name sig (ctxFns ctx)) localSig
          }
        body' = transformExpr ctx' body
    in ELet name val' body'
  ECompute comp ->
    ECompute (transformComp ctx comp)
  EMatch scrut scrutTy scrutName retTy cases ->
    let ctx' = ctx { ctxBound = Set.insert scrutName (ctxBound ctx) }
    in EMatch (transformExpr ctx scrut) (transformType scrutTy) scrutName (transformType retTy)
      (map (transformMatchCase ctx') cases)
  EData params universe cases ->
    let params' = [Param name (transformType ty) | Param name ty <- params]
        cases' = map transformDataCase cases
    in EData params' (transformType universe) cases'
  EAnnot e ty -> EAnnot (transformExpr ctx e) (transformType ty)
  ETyped e ty -> ETyped (transformExpr ctx e) (transformType ty)
  EDict className impls ->
    EDict className [ (n, transformExpr ctx e) | (n, e) <- impls ]
  EDictAccess d method -> EDictAccess (transformExpr ctx d) method
  ETypeClass _ _ _ -> error "typeclass nodes should be removed before transformExpr"
  EInstance _ _ _ -> error "instance nodes should be removed before transformExpr"

transformFunctionBody :: TransformCtx -> FunctionBody -> FunctionBody
transformFunctionBody ctx body = case body of
  FunctionValue e -> FunctionValue (transformExpr ctx e)
  FunctionCompute c -> FunctionCompute (transformComp ctx c)

transformComp :: TransformCtx -> Comp -> Comp
transformComp ctx comp = case comp of
  CReturn e -> CReturn (transformExpr ctx e)
  CPerform e -> CPerform (transformExpr ctx e)
  CBind name c1 c2 ->
    let c1' = transformComp ctx c1
        ctx' = ctx { ctxBound = Set.insert name (ctxBound ctx) }
        c2' = transformComp ctx' c2
    in CBind name c1' c2'
  CSeq c1 c2 -> CSeq (transformComp ctx c1) (transformComp ctx c2)

transformMatchCase :: TransformCtx -> MatchCase -> MatchCase
transformMatchCase ctx (MatchCase ctor binders body) =
  let binders' = [Param name (transformType ty) | Param name ty <- binders]
      bound' = ctxBound ctx `Set.union` Set.fromList (map paramName binders)
      ctx' = ctx { ctxBound = bound' }
  in MatchCase ctor binders' (transformExpr ctx' body)

transformDataCase :: DataCase -> DataCase
transformDataCase (DataCase ctor ty) =
  DataCase ctor (transformType ty)

transformType :: Expr -> Expr
transformType = id

--------------------------------------------------------------------------------
-- Constraint resolution and dictionary passing

resolveConstraint :: Map.Map Text ClassInfo -> Map.Map Text Text -> Constraint -> Constraint
resolveConstraint classEnv openAliases (Constraint className ty) =
  Constraint (resolveClassName classEnv openAliases className) ty

resolveClassName :: Map.Map Text ClassInfo -> Map.Map Text Text -> Text -> Text
resolveClassName classEnv openAliases name
  | isQualified name =
      if Map.member name classEnv
        then name
        else error $ "Unknown qualified typeclass: " ++ T.unpack name
  | Map.member name classEnv = name
  | Just qualified <- Map.lookup name openAliases = qualified
  | otherwise = error $ "Unknown typeclass: " ++ T.unpack name

isQualified :: Text -> Bool
isQualified = T.isInfixOf "::"

lookupFnSig :: TransformCtx -> Expr -> Maybe FnSig
lookupFnSig ctx expr = case expr of
  EVar name -> Map.lookup name (ctxFns ctx)
  EAnnot e _ -> lookupFnSig ctx e
  ETyped e _ -> lookupFnSig ctx e
  EFunction params constraints _ _ ->
    let constraints' = map (resolveConstraint (ctxClasses ctx) (ctxOpenClassAliases ctx)) constraints
    in if null constraints'
        then Nothing
        else Just (FnSig "<anon>" params constraints')
  _ -> Nothing

extractFnSig :: TransformCtx -> Text -> Expr -> Maybe FnSig
extractFnSig ctx name expr =
  case unwrapFunction expr of
    Just (params, constraints) ->
      let constraints' = map (resolveConstraint (ctxClasses ctx) (ctxOpenClassAliases ctx)) constraints
      in if null constraints'
          then Nothing
          else Just (FnSig name params constraints')
    Nothing -> Nothing

unwrapFunction :: Expr -> Maybe ([Param], [Constraint])
unwrapFunction expr = case expr of
  EFunction params constraints _ _ -> Just (params, constraints)
  EAnnot e _ -> unwrapFunction e
  ETyped e _ -> unwrapFunction e
  _ -> Nothing

insertDictArgs :: TransformCtx -> FnSig -> [Expr] -> [Expr]
insertDictArgs ctx sig args =
  let (typeParams, _) = splitTypeParams (fnSigParams sig)
      typeArgCount = length typeParams
  in if length args < typeArgCount
      then error $ "Missing type arguments for constrained function: " ++ T.unpack (fnSigName sig)
      else
        let (typeArgs, rest) = splitAt typeArgCount args
            sub = Map.fromList (zip (map paramName typeParams) typeArgs)
            methodArgs = concatMap (constraintArgs ctx sub) (fnSigConstraints sig)
        in typeArgs ++ methodArgs ++ rest

constraintArgs :: TransformCtx -> Map.Map Text Expr -> Constraint -> [Expr]
constraintArgs ctx sub (Constraint className ty) =
  case Map.lookup className (ctxClasses ctx) of
    Nothing -> error $ "Unknown typeclass in constraint: " ++ T.unpack className
    Just classInfo ->
      let ty' = substType sub ty
          methodNames = map fst (classMethods classInfo)
      in case ty' of
          EVar _ ->
            let methodArgs = mapMaybe (`Map.lookup` ctxMethodMap ctx) methodNames
            in if length methodArgs == length methodNames
                then map EVar methodArgs
                else
                  let inst = resolveInstance ctx className ty'
                  in map (methodArg inst) (classMethods classInfo)
          _ ->
            let inst = resolveInstance ctx className ty'
            in map (methodArg inst) (classMethods classInfo)
  where
    methodArg inst (methodName, _) =
      case Map.lookup methodName (instMethodNames inst) of
        Just implName -> EVar implName
        Nothing -> error $ "Instance missing method " ++ T.unpack methodName

resolveInstance :: TransformCtx -> Text -> Expr -> InstanceInfo
resolveInstance ctx className ty =
  case Map.lookup className (ctxInstances ctx) of
    Nothing -> error $ "No instances found for class " ++ T.unpack className
    Just insts ->
      case filter (\inst -> instType inst == ty) insts of
        [inst] -> inst
        [] -> error $ "No matching instance for " ++ T.unpack className
          ++ " at type " ++ T.unpack (renderType ty)
        _ -> error $ "Ambiguous instances for " ++ T.unpack className

renderType :: Expr -> Text
renderType expr = case expr of
  ETypeConst tc -> typeConstName tc
  ETypeUniverse n -> "Type" <> T.pack (show n)
  EVar v -> v
  EForAll v dom cod ->
    "for-all " <> v <> " as " <> renderType dom <> " to " <> renderType cod
  EThereExists v dom cod ->
    "there-exists " <> v <> " as " <> renderType dom <> " in " <> renderType cod
  ECompType t -> "computation " <> renderType t
  ELift ty fromLevel toLevel ->
    "lift " <> renderType ty <> " from Type" <> T.pack (show fromLevel)
      <> " to Type" <> T.pack (show toLevel)
  EApp f args ->
    renderType f <> " " <> T.intercalate " " (map renderType args)
  _ -> "<type>"

typeConstName :: TypeConst -> Text
typeConstName tc = case tc of
  TCNatural -> "Natural"
  TCString -> "String"
  TCBoolean -> "Boolean"
  TCUnit -> "Unit"
  TCList -> "List"
  TCPair -> "Pair"

--------------------------------------------------------------------------------
-- Method param construction

buildMethodParams :: Map.Map Text ClassInfo -> [Constraint] -> Set.Set Text -> ([Param], Map.Map Text Text)
buildMethodParams classEnv constraints used0 =
  let (params, methodMap, _) = foldl addConstraint ([], Map.empty, used0) constraints
  in (params, methodMap)
  where
    addConstraint (params, methodMap, used) (Constraint className ty) =
      case Map.lookup className classEnv of
        Nothing -> error $ "Unknown typeclass in requires: " ++ T.unpack className
        Just classInfo ->
          foldl (addMethod className classInfo ty) (params, methodMap, used) (classMethods classInfo)

    addMethod className classInfo ty (params, methodMap, used) (methodName, methodTy) =
      if Map.member methodName methodMap
        then error $ "Ambiguous method name: " ++ T.unpack methodName
        else
          let baseName = mkMethodParamName className methodName
              (fresh, used') = freshName used baseName
              methodType = specializeMethodType classInfo ty methodTy
              param = Param fresh methodType
          in (params ++ [param], Map.insert methodName fresh methodMap, used')

mkMethodParamName :: Text -> Text -> Text
mkMethodParamName className methodName =
  "__req__" <> sanitizeName className <> "__" <> methodName

buildInstanceMethodNames :: Text -> [Text] -> Set.Set Text -> (Map.Map Text Text, Set.Set Text)
buildInstanceMethodNames defName methodNames used0 =
  foldl addMethod (Map.empty, used0) methodNames
  where
    addMethod (acc, used) methodName =
      let baseName = mkInstanceMethodName defName methodName
          (fresh, used') = freshName used baseName
      in (Map.insert methodName fresh acc, used')

mkInstanceMethodName :: Text -> Text -> Text
mkInstanceMethodName defName methodName =
  "__inst__" <> sanitizeName defName <> "__" <> methodName

sanitizeName :: Text -> Text
sanitizeName = T.replace "::" "_"

freshName :: Set.Set Text -> Text -> (Text, Set.Set Text)
freshName used base
  | Set.notMember base used = (base, Set.insert base used)
  | otherwise = findFresh 1
  where
    findFresh :: Int -> (Text, Set.Set Text)
    findFresh n =
      let candidate = base <> "_" <> T.pack (show n)
      in if Set.member candidate used
          then findFresh (n + 1)
          else (candidate, Set.insert candidate used)

--------------------------------------------------------------------------------
-- Utilities

splitTypeParams :: [Param] -> ([Param], [Param])
splitTypeParams params =
  let (typeParams, rest) = span (isTypeParam . paramType) params
  in (typeParams, rest)

isTypeParam :: Expr -> Bool
isTypeParam expr = case expr of
  ETypeUniverse _ -> True
  _ -> False

substType :: Map.Map Text Expr -> Expr -> Expr
substType sub expr = case expr of
  EVar v -> Map.findWithDefault expr v sub
  ETypeConst _ -> expr
  ETypeUniverse _ -> expr
  EForAll v dom cod ->
    let sub' = Map.delete v sub
    in EForAll v (substType sub' dom) (substType sub' cod)
  EThereExists v dom cod ->
    let sub' = Map.delete v sub
    in EThereExists v (substType sub' dom) (substType sub' cod)
  ECompType t -> ECompType (substType sub t)
  ELift ty fromLevel toLevel -> ELift (substType sub ty) fromLevel toLevel
  EApp f args -> EApp (substType sub f) (map (substType sub) args)
  _ -> expr
