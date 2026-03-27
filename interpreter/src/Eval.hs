{-# LANGUAGE OverloadedStrings #-}
module Eval
  ( CtorArityMap
  , ctorArityMap
  , runModuleMain
  ) where

import           AST
import           Control.Monad (when)
import           GHC.Conc (getNumCapabilities, setNumCapabilities)
import           Data.IORef

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.FilePath (takeExtension)
import           GHC.Stack (HasCallStack, callStack, prettyCallStack)
import           Parser (parseModuleFile, parseMExprFile)
import           ImportResolver (ImportScope(..), ResolvedModule(..), resolveModulePath)
import           Utils (qualifyName)
import           DictPass (transformModuleWithEnvsScope)
import           Recursor (recursorDefs, insertRecursors)
import qualified Type
import           ErrorMsg (findFuzzyMatches)
import qualified TypeChecker as TC
import qualified RunCache as RC
import           CtorArity (CtorArityMap)
import           Locque.Runtime

-- Build environment from module definitions atop an existing env (imports/prims)
-- Uses lazy evaluation to tie-the-knot: all definitions capture the final environment
bindModule :: CtorArityMap -> Module -> Env -> Env
bindModule ctorArity (Module _modName _ _ defs) base =
  let env = foldl addDef base defs
      addDef e (Definition _ name body) = case stripTypeExpr body of
        EData params _ cases ->
          let e' = Map.insert name (BVal (VTypeData name)) e
              ctorBindings = map (ctorBinding params) cases
          in foldl (\acc (ctorName, ctorVal) -> Map.insert ctorName (BVal ctorVal) acc) e' ctorBindings
        _ ->
          Map.insert name (BValueExpr env body) e
  in env
  where
    ctorBinding params (DataCase ctorName ctorTy) =
      let fallback = (countFreeDataParams params ctorTy, countForAll ctorTy)
          (dataParamCount, termParamCount) = Map.findWithDefault fallback ctorName ctorArity
          total = dataParamCount + termParamCount
          ctorVal = if total == 0
            then VData ctorName []
            else VConstructor ctorName dataParamCount termParamCount []
      in (ctorName, ctorVal)

countFreeDataParams :: [Param] -> Expr -> Int
countFreeDataParams params ctorTy =
  let free = TC.freeVars ctorTy
  in length [() | Param name _ <- params, name `Set.member` free]

runModuleMain :: FilePath -> CtorArityMap -> Module -> IO Int
runModuleMain projectRoot ctorArity m@(Module _modName _ opens _) = do
  caps <- getNumCapabilities
  when (caps < 2) (setNumCapabilities 2)
  -- Initialize the evalFunctionBody callback for apply (breaks circular dependency)
  writeIORef evalFunctionBodyRef evalFunctionBody
  -- Reset assertion counter
  writeIORef assertionCounter 0

  envImports <- loadImports projectRoot m
  let envWithOpens = processOpens opens envImports
      env = bindModule ctorArity m envWithOpens
  case Map.lookup (T.pack "main") env of
    Just b -> do
      val <- resolveBinding env b
      case val of
        VComp action -> do _ <- action; readIORef assertionCounter
        VPrim f -> do _ <- f []; readIORef assertionCounter
        _ -> error "main must evaluate to a computation value"
    _ -> error "No main computation found"

evalExpr :: Env -> Expr -> IO Value
evalExpr env expr = case expr of
  EVar t -> case Map.lookup t env of
    Just b  -> resolveBinding env b
    Nothing ->
      let candidates = Map.keys env
          matches = findFuzzyMatches t candidates
          suggestion = case matches of
                        [] -> T.empty
                        (x:_) -> " (did you mean '" <> x <> "'?)"
      in error $ T.unpack ("Unknown variable: " <> t <> suggestion)
  ELit lit -> pure $ case lit of
    LNatural n    -> VNatural n
    LString s -> VString s
    LBoolean b   -> VBoolean b
    LUnit     -> VUnit
  EListLiteral elems -> VList <$> mapM (evalExpr env) elems
  ETypeConst tc -> pure (VTypeConst tc)
  EData {} -> pure (VTypeData "<data>")
  ETypeUniverse n -> pure (VTypeUniverse n)
  EForAll v dom cod -> pure (VTypeForAll v dom cod)
  EThereExists v dom cod -> pure (VTypeThereExists v dom cod)
  ECompType eff t -> pure (VTypeComp eff t)
  EEqual ty lhs rhs ->
    VTypeEqual <$> evalExpr env ty <*> evalExpr env lhs <*> evalExpr env rhs
  EReflexive _ _ -> pure VUnit
  ERewrite _ _ body -> evalExpr env body
  ELift ty fromLevel toLevel -> pure (VTypeLift ty fromLevel toLevel)
  EUp _ _ _ body -> evalExpr env body
  EDown _ _ _ body -> evalExpr env body
  EPack _ _ _ witness body -> do
    witness' <- evalExpr env witness
    body' <- evalExpr env body
    pure (VPair witness' body')
  EUnpack packed x y body -> do
    packedVal <- evalExpr env packed
    case packedVal of
      VPair a b ->
        let env' = Map.insert y (BVal b) (Map.insert x (BVal a) env)
        in evalExpr env' body
      _ -> error "unpack expects a packed value"
  EFunction params _constraints _retTy body ->
    let recClosure = VClosure envWithRecur (map paramName params) body
        envWithRecur = Map.insert (T.pack "recur") (BVal recClosure) env
    in pure recClosure
  ELet name val body -> do
    val' <- evalExpr env val
    let env' = Map.insert name (BVal val') env
    evalExpr env' body
  ECompute comp -> pure $ VComp (runComp env comp)
  EMatch scrut _scrutTy scrutName _retTy cases -> evalMatch env scrut scrutName cases
  EAnnot annExpr _ty -> evalExpr env annExpr  -- Type annotation ignored in evaluation
  ETyped typedExpr _ty -> evalExpr env typedExpr  -- Inferred type ignored in evaluation
  EApp f args -> do
    vf <- evalExpr env f
    vs <- mapM (evalExpr env) args
    apply vf vs
  EDict className impls -> do
    -- Evaluate each method implementation
    implVals <- mapM (\(name, implExpr) -> do
      val <- evalExpr env implExpr
      pure (name, val)) impls
    pure (VDict className (Map.fromList implVals))
  EDictAccess dictExpr methodName -> do
    dictVal <- evalExpr env dictExpr
    case dictVal of
      VDict _ methods ->
        case Map.lookup methodName methods of
          Just val -> pure val
          Nothing -> error $ "Method not found in dictionary: " ++ T.unpack methodName
      _ -> error "Expected dictionary value"
  ETypeClass {} -> error "Typeclass nodes are not evaluable"
  EInstance {} -> error "Instance nodes are not evaluable"

resolveBinding :: Env -> Binding -> IO Value
resolveBinding _outerEnv b = case b of
  BVal v               -> pure v
  BValueExpr env e     -> evalExpr env e  -- Use captured environment

evalFunctionBody :: Env -> FunctionBody -> IO Value
evalFunctionBody env body = case body of
  FunctionValue expr -> evalExpr env expr
  FunctionCompute comp -> pure (VComp (runComp env comp))

runtimeError :: HasCallStack => String -> a
runtimeError msg =
  error (msg ++ "\nCall stack:\n" ++ prettyCallStack callStack)

describeValue :: Value -> String
describeValue val = case val of
  VClosure _ params _ -> "<closure params=" ++ show params ++ ">"
  VPrim _ -> "<prim>"
  _ -> show val

evalMatch :: HasCallStack => Env -> Expr -> Text -> [MatchCase] -> IO Value
evalMatch env scrut scrutName cases = do
  scrutVal <- evalExpr env scrut
  let envScrut = Map.insert scrutName (BVal scrutVal) env
  case scrutVal of
    VList [] -> matchCtor "List::empty" [] envScrut
    VList (h:t) -> matchCtor "List::cons" [h, VList t] envScrut
    VBoolean False -> matchCtor "Boolean::false" [] envScrut
    VBoolean True -> matchCtor "Boolean::true" [] envScrut
    VPair a b -> matchCtor "Pair::pair" [a, b] envScrut
    VUnit -> matchCtor "Unit::tt" [] envScrut
    VData ctorName args -> matchCtor ctorName args envScrut
    VConstructor ctorName dataParamCount termParamCount args ->
      let total = dataParamCount + termParamCount
      in if length args == total
        then matchCtor ctorName (drop dataParamCount args) envScrut
        else runtimeError ("match: scrutinee not fully applied: " ++ T.unpack ctorName)
    _ ->
      runtimeError
        ("match: unsupported scrutinee value: " ++ describeValue scrutVal
          ++ "\n  scrutinee expr: " ++ show scrut
          ++ "\n  scrutinee binding: " ++ T.unpack scrutName
          ++ "\n  cases: " ++ show (map matchCaseCtor cases))
  where
    matchCtor ctorName args envScrut = do
      case [ (binders, body) | MatchCase caseCtor binders body <- cases, caseCtor == ctorName ] of
        ((binders, body):_) -> do
          when (length binders /= length args) $
            runtimeError ("match: case " ++ T.unpack ctorName ++ " expects " ++ show (length binders) ++ " binders")
          let env' = foldl (\e (name, val) -> Map.insert name (BVal val) e)
                          envScrut
                          (zip (map paramName binders) args)
          evalExpr env' body
        [] -> runtimeError ("match: missing case " ++ T.unpack ctorName)

runComp :: Env -> Comp -> IO Value
runComp env comp = case comp of
  CReturn e    -> evalExpr env e
  CBind v c1 c2 -> do
    val <- runComp env c1
    let env' = Map.insert v (BVal val) env
    runComp env' c2
  CPerform e   -> do
    val <- evalExpr env e
    case val of
      VComp action -> action
      _ -> error "perform expects a computation value"

-- Import loading

loadImports :: FilePath -> Module -> IO Env
loadImports projectRoot (Module _ imports _ _) =
  loadImportsWith projectRoot ProjectScope Set.empty imports

loadImport :: FilePath -> ImportScope -> Set.Set Text -> Import -> IO Env
loadImport projectRoot scope visiting (Import modName alias) = do
  when (modName `Set.member` visiting) $
    error $ "Import cycle detected: " ++ T.unpack modName
  ResolvedModule path nextScope <-
    resolveModulePath projectRoot "lib" scope modName
  contents <- TIO.readFile path
  parsed <- case takeExtension path of
    ".lq"  -> case parseMExprFile path contents of
      Left err -> error err
      Right m -> pure m
    ".lqs" -> case parseModuleFile path contents of
      Left err -> error err
      Right m -> pure m
    _      -> error $ "Unknown file extension: " ++ path

  (digest, importedEnv) <- TC.moduleDigestWithImportsScope projectRoot nextScope contents parsed
  cached <- RC.readRunCache projectRoot path digest
  (annotated, ctorArity) <- case cached of
    Just entry ->
      pure (RC.cacheAnnotated entry, RC.cacheCtorArity entry)
    Nothing -> do
      let tcResult = TC.typeCheckAndNormalizeWithEnv importedEnv parsed
      (annotated, normalized) <- case tcResult of
        Left tcErr -> error $ "Type error in import " ++ T.unpack modName ++ ": " ++ show tcErr
        Right (env, normalized) -> do
          let recDefs = recursorDefs normalized
          prepared <- case insertRecursors parsed recDefs of
            Left msg -> error $ "Transform error in import " ++ T.unpack modName ++ ": " ++ msg
            Right m -> pure m
          case TC.annotateModule env prepared of
            Left annErr -> error $ "Annotation error in import " ++ T.unpack modName ++ ": " ++ show annErr
            Right annotatedPrepared -> do
              transformed <- transformModuleWithEnvsScope projectRoot nextScope annotatedPrepared
              pure (transformed, normalized)
      let arity = ctorArityMap normalized
          cacheEntry = RC.RunCache
            { RC.cacheVersion = RC.cacheVersionCurrent
            , RC.cacheDigest = digest
            , RC.cacheAnnotated = annotated
            , RC.cacheCtorArity = arity
            }
      RC.writeRunCache projectRoot path cacheEntry
      pure (annotated, arity)

  let Module _modName _ opens defs = annotated
      visiting' = Set.insert modName visiting
  envImports <- loadImportsWith projectRoot nextScope visiting' (modImports annotated)
  -- Process opens to bring in unqualified names from open statements
  let envWithOpens = processOpens opens envImports

  -- Bind the module to get all definitions
  let envSelf = bindModule ctorArity annotated envWithOpens
      -- Add qualified names for each definition using the alias
      defNames = map defName defs
      ctorNames = concatMap defCtorNames defs
      envFinal = foldl (insertQualified alias envSelf) envWithOpens (defNames ++ ctorNames)

  pure envFinal
  where
    -- Insert qualified name for a definition (if it exists in envSelf)
    insertQualified :: Text -> Env -> Env -> Text -> Env
    insertQualified aliasPrefix envSelf env name =
      case Map.lookup name envSelf of
        Just binding ->
          let qualifiedName = qualifyName aliasPrefix name
          in Map.insert qualifiedName binding env
        Nothing -> env  -- Definition not in environment

    defCtorNames :: Definition -> [Text]
    defCtorNames def = case stripTypeExpr (defBody def) of
      EData _ _ cases -> map dataCaseName cases
      _ -> []

loadImportsWith :: FilePath -> ImportScope -> Set.Set Text -> [Import] -> IO Env
loadImportsWith projectRoot scope visiting imports = do
  envs <- mapM (loadImport projectRoot scope visiting) imports
  pure $ Map.unions (primEnv : envs)

ctorArityMap :: Module -> CtorArityMap
ctorArityMap (Module _ _ _ defs) =
  Map.fromList (concatMap defCtorArity defs)
  where
    defCtorArity (Definition _ _ body) = case stripTypeExpr body of
      EData params _ cases ->
        [ (ctorName, (countFreeDataParams params ctorTy, countForAll ctorTy))
        | DataCase ctorName ctorTy <- cases
        ]
      _ -> []

stripTypeExpr :: Expr -> Expr
stripTypeExpr expr = case expr of
  EAnnot inner _ -> stripTypeExpr inner
  ETyped inner _ -> stripTypeExpr inner
  _ -> expr

-- Process open statements to bring unqualified names into scope
processOpens :: [Open] -> Env -> Env
processOpens opens env = foldl processOneOpen env opens
  where
    processOneOpen :: Env -> Open -> Env
    processOneOpen e (Open modAlias nameList) =
      foldl (openOneName modAlias) e nameList

    openOneName :: Text -> Env -> Text -> Env
    openOneName modAlias e name =
      let qualifiedName = qualifyName modAlias name
      in case Map.lookup qualifiedName e of
           Just binding -> Map.insert name binding e
           Nothing -> e
