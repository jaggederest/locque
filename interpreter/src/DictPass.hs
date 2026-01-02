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
  , instTypeVars    :: [Text]
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
      localInstancesMap = collectLocalInstances classEnvForResolution openAliases defs
      localFnsMap =
        Map.union
          (collectLocalFns classEnvForResolution openAliases defs)
          (collectInstanceFns classEnvForResolution openAliases localInstancesMap)
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
      localInstancesMap = collectLocalInstances classEnvForResolution openAliases (modDefs parsed)
      localFnsMap =
        Map.union
          (collectLocalFns classEnvForResolution openAliases (modDefs parsed))
          (collectInstanceFns classEnvForResolution openAliases localInstancesMap)
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
              , instTypeVars = collectTypeVarOrder instTy
              , instMethods = methodMap
              , instMethodNames = methodNames
              , instTransparency = tr
              }
        in (Map.insert name instInfo acc, used')
      _ -> (acc, used)

collectInstanceFns :: Map.Map Text ClassInfo -> Map.Map Text Text -> Map.Map Text InstanceInfo -> Map.Map Text FnSig
collectInstanceFns classEnv openAliases instances =
  Map.fromList (mapMaybe toSig methodEntries)
  where
    methodEntries =
      [ (defName, methodExpr)
      | inst <- Map.elems instances
      , (methodName, methodExpr) <- Map.toList (instMethods inst)
      , Just defName <- [Map.lookup methodName (instMethodNames inst)]
      ]

    toSig (name, body) =
      case unwrapFunction body of
        Just (params, constraints) ->
          let resolved = map (resolveConstraint classEnv openAliases) constraints
          in if null resolved
              then Nothing
              else Just (name, FnSig name params resolved)
        Nothing -> Nothing

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
  EListLiteral elems -> EListLiteral (map (transformExpr ctx) elems)
  ETypeConst _ -> expr
  ETypeUniverse _ -> expr
  EForAll v dom cod ->
    EForAll v (transformType dom) (transformType cod)
  EThereExists v dom cod ->
    EThereExists v (transformType dom) (transformType cod)
  ECompType eff t -> ECompType (transformType eff) (transformType t)
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
        recurSig = if null constraints'
          then Nothing
          else Just (FnSig "recur" params constraints')
        ctx' = ctx
          { ctxMethodMap = methodMap
          , ctxBound = bound'
          , ctxFns = maybe (ctxFns ctx) (\sig -> Map.insert "recur" sig (ctxFns ctx)) recurSig
          }
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
                  let (inst, instSub) = resolveInstance ctx className ty'
                  in map (methodArg inst instSub) (classMethods classInfo)
          _ ->
            let (inst, instSub) = resolveInstance ctx className ty'
            in map (methodArg inst instSub) (classMethods classInfo)
  where
    methodArg inst instSub (methodName, _) =
      case Map.lookup methodName (instMethodNames inst) of
        Just implName ->
          let typeArgs = mapMaybe (`Map.lookup` instSub) (instTypeVars inst)
              baseExpr = EVar implName
              argsWithDicts = case lookupMethodSig inst implName methodName of
                Nothing -> typeArgs
                Just sig -> insertDictArgs ctx sig typeArgs
          in if null argsWithDicts
              then baseExpr
              else EApp baseExpr argsWithDicts
        Nothing -> error $ "Instance missing method " ++ T.unpack methodName
    lookupMethodSig inst implName methodName =
      case Map.lookup implName (ctxFns ctx) of
        Just sig -> Just sig
        Nothing ->
          case Map.lookup methodName (instMethods inst) of
            Just expr ->
              case unwrapFunction expr of
                Just (params, constraints) ->
                  let resolved = map (resolveConstraint (ctxClasses ctx) (ctxOpenClassAliases ctx)) constraints
                  in if null resolved
                      then Nothing
                      else Just (FnSig implName params resolved)
                Nothing -> Nothing
            Nothing -> Nothing

resolveInstance :: TransformCtx -> Text -> Expr -> (InstanceInfo, Map.Map Text Expr)
resolveInstance ctx className ty =
  case Map.lookup className (ctxInstances ctx) of
    Nothing -> error $ "No instances found for class " ++ T.unpack className
    Just insts ->
      case [ (inst, sub) | inst <- insts, Just sub <- [matchInstanceType (instType inst) ty] ] of
        [(inst, sub)] -> (inst, sub)
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
  ECompType eff t ->
    if isEffectAny eff
      then "computation " <> renderType t
      else "computation " <> renderType eff <> " " <> renderType t
  ELift ty fromLevel toLevel ->
    "lift " <> renderType ty <> " from Type" <> T.pack (show fromLevel)
      <> " to Type" <> T.pack (show toLevel)
  EApp f args ->
    renderType f <> " " <> T.intercalate " " (map renderType args)
  _ -> "<type>"

stripType :: Expr -> Expr
stripType expr = case expr of
  EAnnot e _ -> stripType e
  ETyped e _ -> stripType e
  _ -> expr

matchInstanceType :: Expr -> Expr -> Maybe (Map.Map Text Expr)
matchInstanceType patternTy targetTy =
  let typeVars = collectTypeVars patternTy
  in matchTypes typeVars Map.empty (stripType patternTy) (stripType targetTy)

matchTypes :: Set.Set Text -> Map.Map Text Expr -> Expr -> Expr -> Maybe (Map.Map Text Expr)
matchTypes typeVars sub patternTy targetTy = case stripType patternTy of
  EVar v ->
    if Set.member v typeVars
      then case Map.lookup v sub of
        Nothing -> Just (Map.insert v (stripType targetTy) sub)
        Just bound ->
          if stripType bound == stripType targetTy
            then Just sub
            else Nothing
      else case stripType targetTy of
        EVar v' | v == v' -> Just sub
        _ -> Nothing
  ETypeConst _ -> if stripType patternTy == stripType targetTy then Just sub else Nothing
  ETypeUniverse _ -> if stripType patternTy == stripType targetTy then Just sub else Nothing
  ECompType eff t -> case stripType targetTy of
    ECompType eff' t' ->
      let sub' =
            if isEffectAny eff || isEffectAny eff'
              then Just sub
              else matchTypes typeVars sub eff eff'
      in case sub' of
          Nothing -> Nothing
          Just sub'' -> matchTypes typeVars sub'' t t'
    _ -> Nothing
  EForAll v dom cod -> case stripType targetTy of
    EForAll v' dom' cod' ->
      case matchTypes typeVars sub dom dom' of
        Nothing -> Nothing
        Just sub' ->
          matchTypes typeVars sub' (renameBound v v' cod) cod'
    _ -> Nothing
  EThereExists v dom cod -> case stripType targetTy of
    EThereExists v' dom' cod' ->
      case matchTypes typeVars sub dom dom' of
        Nothing -> Nothing
        Just sub' ->
          matchTypes typeVars sub' (renameBound v v' cod) cod'
    _ -> Nothing
  EApp f args -> case stripType targetTy of
    EApp f' args' ->
      if length args == length args'
        then do
          sub' <- matchTypes typeVars sub f f'
          foldl (\acc (p, t) -> acc >>= \s -> matchTypes typeVars s p t) (Just sub') (zip args args')
        else Nothing
    _ -> Nothing
  _ -> if stripType patternTy == stripType targetTy then Just sub else Nothing

collectTypeVars :: Expr -> Set.Set Text
collectTypeVars expr = go Set.empty expr
  where
    go acc e = case stripType e of
      EVar v ->
        if isTypeVarName v then Set.insert v acc else acc
      ETypeConst _ -> acc
      ETypeUniverse _ -> acc
      ECompType eff t -> go (go acc eff) t
      EForAll v dom cod ->
        let acc' = go acc dom
        in go (Set.delete v acc') cod
      EThereExists v dom cod ->
        let acc' = go acc dom
        in go (Set.delete v acc') cod
      EApp f args -> foldl go (go acc f) args
      ELift ty _ _ -> go acc ty
      EUp ty _ _ body -> go (go acc ty) body
      EDown ty _ _ body -> go (go acc ty) body
      EAnnot e' ty -> go (go acc e') ty
      ETyped e' ty -> go (go acc e') ty
      _ -> acc

isTypeVarName :: Text -> Bool
isTypeVarName name = T.length name == 1

renameBound :: Text -> Text -> Expr -> Expr
renameBound from to expr =
  if from == to then expr else rename expr
  where
    rename e = case e of
      EVar v | v == from -> EVar to
      ELit _ -> e
      EListLiteral elems -> EListLiteral (map rename elems)
      ETypeConst _ -> e
      ETypeUniverse _ -> e
      EForAll v dom cod ->
        let dom' = rename dom
            cod' = if v == from then cod else rename cod
        in EForAll v dom' cod'
      EThereExists v dom cod ->
        let dom' = rename dom
            cod' = if v == from then cod else rename cod
        in EThereExists v dom' cod'
      ECompType eff t -> ECompType (rename eff) (rename t)
      EEqual ty lhs rhs -> EEqual (rename ty) (rename lhs) (rename rhs)
      EReflexive ty term -> EReflexive (rename ty) (rename term)
      ERewrite family proof body -> ERewrite (rename family) (rename proof) (rename body)
      EPack v dom cod witness body ->
        let dom' = rename dom
            cod' = if v == from then cod else rename cod
        in EPack v dom' cod' (rename witness) (rename body)
      EUnpack packed x y body ->
        let packed' = rename packed
            body' = if x == from || y == from then body else rename body
        in EUnpack packed' x y body'
      ELift ty fromLevel toLevel -> ELift (rename ty) fromLevel toLevel
      EUp ty fromLevel toLevel body -> EUp (rename ty) fromLevel toLevel (rename body)
      EDown ty fromLevel toLevel body -> EDown (rename ty) fromLevel toLevel (rename body)
      EApp fn args -> EApp (rename fn) (map rename args)
      EFunction params constraints ret body ->
        let boundNames = map paramName params
            params' = [Param name (rename ty) | Param name ty <- params]
            constraints' = [Constraint cls (rename ty) | Constraint cls ty <- constraints]
            ret' = rename ret
            body' =
              if from `elem` boundNames then body else renameBody body
        in EFunction params' constraints' ret' body'
      ELet name val body ->
        let val' = rename val
            body' = if name == from then body else rename body
        in ELet name val' body'
      ECompute comp -> ECompute (renameComp comp)
      EMatch scrut scrutTy scrutName retTy cases ->
        let scrut' = rename scrut
            scrutTy' = rename scrutTy
            retTy' = if scrutName == from then retTy else rename retTy
            cases' = map renameCase cases
        in EMatch scrut' scrutTy' scrutName retTy' cases'
      EData params universe cases ->
        let boundNames = map paramName params
            params' = [Param name (rename ty) | Param name ty <- params]
            universe' = rename universe
            cases' = if from `elem` boundNames then cases else map renameDataCase cases
        in EData params' universe' cases'
      EAnnot e ty -> EAnnot (rename e) (rename ty)
      ETyped e ty -> ETyped (rename e) (rename ty)
      EDict className impls -> EDict className [ (n, rename e) | (n, e) <- impls ]
      EDictAccess e method -> EDictAccess (rename e) method
      ETypeClass param kind methods ->
        let kind' = rename kind
            methods' =
              if param == from
                then methods
                else [ (n, rename ty) | (n, ty) <- methods ]
        in ETypeClass param kind' methods'
      EInstance cls instTy methods ->
        EInstance cls (rename instTy) [ (n, rename e) | (n, e) <- methods ]
    renameBody b = case b of
      FunctionValue e -> FunctionValue (rename e)
      FunctionCompute c -> FunctionCompute (renameComp c)
    renameComp c = case c of
      CReturn e -> CReturn (rename e)
      CPerform e -> CPerform (rename e)
      CBind name c1 c2 ->
        let c1' = renameComp c1
            c2' = if name == from then c2 else renameComp c2
        in CBind name c1' c2'
    renameCase (MatchCase ctor binders body) =
      let boundNames = map paramName binders
          binders' = [Param name (rename ty) | Param name ty <- binders]
          body' = if from `elem` boundNames then body else rename body
      in MatchCase ctor binders' body'
    renameDataCase (DataCase name ty) = DataCase name (rename ty)

collectTypeVarOrder :: Expr -> [Text]
collectTypeVarOrder expr = fst (go [] Set.empty expr)
  where
    go acc seen e = case stripType e of
      EVar v ->
        if isTypeVarName v && Set.notMember v seen
          then (acc ++ [v], Set.insert v seen)
          else (acc, seen)
      ETypeConst _ -> (acc, seen)
      ETypeUniverse _ -> (acc, seen)
      ECompType eff t ->
        let (acc', seen') = go acc seen eff
        in go acc' seen' t
      EForAll v dom cod ->
        let (acc', seen') = go acc seen dom
            (acc'', seen'') = go acc' (Set.delete v seen') cod
        in (acc'', seen'')
      EThereExists v dom cod ->
        let (acc', seen') = go acc seen dom
            (acc'', seen'') = go acc' (Set.delete v seen') cod
        in (acc'', seen'')
      EApp f args ->
        let (acc', seen') = go acc seen f
        in foldl (\(a, s) arg -> go a s arg) (acc', seen') args
      ELift ty _ _ -> go acc seen ty
      EUp ty _ _ body -> let (acc', seen') = go acc seen ty in go acc' seen' body
      EDown ty _ _ body -> let (acc', seen') = go acc seen ty in go acc' seen' body
      EAnnot e' ty -> let (acc', seen') = go acc seen e' in go acc' seen' ty
      ETyped e' ty -> let (acc', seen') = go acc seen e' in go acc' seen' ty
      _ -> (acc, seen)

typeConstName :: TypeConst -> Text
typeConstName tc = case tc of
  TCNatural -> "Natural"
  TCString -> "String"
  TCBoolean -> "Boolean"
  TCUnit -> "Unit"
  TCList -> "List"
  TCPair -> "Pair"
  TCDictionary -> "Dictionary"
  TCListener -> "Listener"
  TCSocket -> "Socket"

isEffectAny :: Expr -> Bool
isEffectAny expr = case expr of
  EVar name -> name == effectAnyName
  _ -> False

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
  ECompType eff t -> ECompType (substType sub eff) (substType sub t)
  ELift ty fromLevel toLevel -> ELift (substType sub ty) fromLevel toLevel
  EApp f args -> EApp (substType sub f) (map (substType sub) args)
  _ -> expr
