{-# LANGUAGE OverloadedStrings #-}
module Recursor
  ( injectRecursors
  , recursorDefinition
  , recursorDefs
  , insertRecursors
  ) where

import AST
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

injectRecursors :: FilePath -> Module -> Either String Module
injectRecursors path (Module name imports opens defs) = do
  let defNames = Set.fromList (map defName defs)
  defs' <- injectDefs path defNames defs
  pure (Module name imports opens defs')

recursorDefs :: Module -> [Definition]
recursorDefs (Module _ _ _ defs) =
  filter (isRecursorName . defName) defs

insertRecursors :: Module -> [Definition] -> Either String Module
insertRecursors (Module name imports opens defs) recDefs = do
  let recursorNames = map defName recDefs
      existingNames = Set.fromList (map defName defs)
      collisions = filter (`Set.member` existingNames) recursorNames
  case collisions of
    (dup:_) -> Left ("recursor name already defined: " ++ T.unpack dup)
    [] -> do
      recMap <- buildRecursorMap recDefs
      let (defs', remaining) = foldl insertOne ([], recMap) defs
      if Map.null remaining
        then pure (Module name imports opens defs')
        else Left ("recursor definitions with no matching data: " ++ show (Map.keys remaining))
  where
    insertOne (acc, remaining) def@(Definition _ defName' body) =
      case body of
        EData {} ->
          case Map.lookup defName' remaining of
            Just recDef -> (acc ++ [def, recDef], Map.delete defName' remaining)
            Nothing -> (acc ++ [def], remaining)
        _ -> (acc ++ [def], remaining)

    buildRecursorMap recDefs' =
      foldl step (Right Map.empty) recDefs'
      where
        step (Left err) _ = Left err
        step (Right acc) def@(Definition _ recName _) =
          case T.stripSuffix recursorSuffix recName of
            Nothing ->
              Left ("recursor name missing suffix: " ++ T.unpack recName)
            Just dataName ->
              if Map.member dataName acc
                then Left ("duplicate recursor for " ++ T.unpack dataName)
                else Right (Map.insert dataName def acc)

recursorSuffix :: T.Text
recursorSuffix = "::recursor"

isRecursorName :: T.Text -> Bool
isRecursorName name = recursorSuffix `T.isSuffixOf` name

injectDefs :: FilePath -> Set.Set T.Text -> [Definition] -> Either String [Definition]
injectDefs _ _ [] = Right []
injectDefs path defNames (defn@(Definition _ name body) : rest) = case body of
  EData params universe cases -> do
    recDef <- buildRecursorDef path defNames name params universe cases
    rest' <- injectDefs path defNames rest
    pure (defn : recDef : rest')
  _ -> do
    rest' <- injectDefs path defNames rest
    pure (defn : rest')

buildRecursorDef
  :: FilePath
  -> Set.Set T.Text
  -> T.Text
  -> [Param]
  -> Expr
  -> [DataCase]
  -> Either String Definition
buildRecursorDef path defNames dataName params universe cases = do
  level <- case universe of
    ETypeUniverse n -> Right n
    _ -> Left (path ++ ": recursor expects TypeN universe for " ++ T.unpack dataName)
  let recName = dataName <> "::recursor"
  if Set.member recName defNames
    then Left (path ++ ": recursor name already defined: " ++ T.unpack recName)
    else Right (recursorDefinition dataName params level cases)

recursorDefinition :: T.Text -> [Param] -> Int -> [DataCase] -> Definition
recursorDefinition dataName params level cases =
  let dataParamNames = map paramName params
      dataParamExprs = map EVar dataParamNames
      dataTy = mkApp (EVar dataName) dataParamExprs
      used0 = Set.fromList dataParamNames
      (motiveName, used1) = freshName used0 "motive"
      (scrutName, used2) = freshName used1 "scrutinee"
      (caseInfos, _usedFinal) = allocateCaseNames used2 cases
      caseFnNames = map caseFnName caseInfos
      motiveTy = mkPi params (EForAll "x" dataTy (ETypeUniverse level))
      motiveParam = Param motiveName motiveTy
      caseParams = map (caseFnParam dataName params motiveName caseFnNames dataParamExprs) caseInfos
      scrutParam = Param scrutName dataTy
      recParams = params ++ [motiveParam] ++ caseParams ++ [scrutParam]
      retTy = mkApp (EVar motiveName) (dataParamExprs ++ [EVar scrutName])
      matchCases = map (caseMatch dataName params motiveName caseFnNames dataParamExprs) caseInfos
      body = EMatch (EVar scrutName) dataTy scrutName retTy matchCases
      recName = dataName <> "::recursor"
  in Definition Transparent recName (EFunction recParams [] retTy (FunctionValue body))

data CaseInfo = CaseInfo
  { caseCtorName :: T.Text
  , caseCtorType :: Expr
  , caseCtorParams :: [Param]
  , caseFnName :: T.Text
  }

allocateCaseNames :: Set.Set T.Text -> [DataCase] -> ([CaseInfo], Set.Set T.Text)
allocateCaseNames used0 cases =
  foldl step ([], used0) cases
  where
    step (acc, used) (DataCase ctorName ctorTy) =
      let (ctorParams, _) = splitForAll ctorTy
          binderNames = Set.fromList (map paramName ctorParams)
          base = "case-" <> ctorShort ctorName
          (caseName, used') = freshName (Set.union used binderNames) base
          info = CaseInfo
            { caseCtorName = ctorName
            , caseCtorType = ctorTy
            , caseCtorParams = ctorParams
            , caseFnName = caseName
            }
      in (acc ++ [info], Set.insert caseName used')

caseFnParam
  :: T.Text
  -> [Param]
  -> T.Text
  -> [T.Text]
  -> [Expr]
  -> CaseInfo
  -> Param
caseFnParam dataName dataParams motiveName caseFnNames _dataParamExprs info =
  let ctorParams = caseCtorParams info
      ctorDataParams = dataParamsForCtor dataParams (caseCtorType info)
      ctorResultArgs = ctorResultArgsFor dataName (caseCtorType info)
      ctorTerm = mkCtorTerm (caseCtorName info) ctorDataParams ctorParams
      (extendedParams, _recCalls) = extendCtorParams dataName motiveName caseFnNames ctorParams
      caseTy = mkPi extendedParams (mkApp (EVar motiveName) (ctorResultArgs ++ [ctorTerm]))
  in Param (caseFnName info) caseTy

caseMatch
  :: T.Text
  -> [Param]
  -> T.Text
  -> [T.Text]
  -> [Expr]
  -> CaseInfo
  -> MatchCase
caseMatch dataName _dataParams motiveName caseFnNames _dataParamExprs info =
  let ctorParams = caseCtorParams info
      (extendedParams, recCalls) = extendCtorParams dataName motiveName caseFnNames ctorParams
      argExprs = paramsToArgs extendedParams recCalls
      body = mkApp (EVar (caseFnName info)) argExprs
  in MatchCase (caseCtorName info) ctorParams body

extendCtorParams
  :: T.Text
  -> T.Text
  -> [T.Text]
  -> [Param]
  -> ([Param], [(T.Text, Expr)])
extendCtorParams dataName motiveName caseFnNames params =
  let used0 = Set.fromList (map paramName params)
      step (acc, recCalls, used) param =
        if isRecursiveParam dataName (paramType param)
          then
            let base = "rec-" <> paramName param
                (recName, used') = freshName used base
                recDataArgs = dataArgsFromType dataName (paramType param)
                recTy = mkApp (EVar motiveName) (recDataArgs ++ [EVar (paramName param)])
                recParam = Param recName recTy
                recurPrefix = recDataArgs ++ [EVar motiveName] ++ map EVar caseFnNames
                recCall = mkApp (EVar "recur") (recurPrefix ++ [EVar (paramName param)])
            in (acc ++ [param, recParam], recCalls ++ [(recName, recCall)], used')
          else (acc ++ [param], recCalls, Set.insert (paramName param) used)
      (extended, recs, _) = foldl step ([], [], used0) params
  in (extended, recs)

paramsToArgs :: [Param] -> [(T.Text, Expr)] -> [Expr]
paramsToArgs params recCalls =
  let recMap = recCalls
  in map (paramArg recMap) params
  where
    paramArg recMap param =
      case lookup (paramName param) recMap of
        Just val -> val
        Nothing -> EVar (paramName param)

mkCtorTerm :: T.Text -> [Param] -> [Param] -> Expr
mkCtorTerm ctorName dataParams ctorParams =
  let dataArgs = map (EVar . paramName) dataParams
      ctorArgs = map (EVar . paramName) ctorParams
  in mkApp (EVar ctorName) (dataArgs ++ ctorArgs)

ctorResultArgsFor :: T.Text -> Expr -> [Expr]
ctorResultArgsFor dataName ctorTy =
  let (_params, result) = splitForAll ctorTy
      (headExpr, args) = collectApp result
  in case headExpr of
      EVar name | name == dataName -> args
      _ -> []

dataArgsFromType :: T.Text -> Expr -> [Expr]
dataArgsFromType dataName ty =
  case collectApp (stripExpr ty) of
    (EVar name, args) | name == dataName -> args
    _ -> []

dataParamsForCtor :: [Param] -> Expr -> [Param]
dataParamsForCtor params ctorTy =
  let free = freeVars ctorTy
  in filter (\(Param name _) -> name `Set.member` free) params

isRecursiveParam :: T.Text -> Expr -> Bool
isRecursiveParam dataName ty =
  case collectApp (stripExpr ty) of
    (EVar name, _) -> name == dataName
    _ -> False

ctorShort :: T.Text -> T.Text
ctorShort name =
  case T.splitOn "::" name of
    [] -> name
    parts -> last parts

freshName :: Set.Set T.Text -> T.Text -> (T.Text, Set.Set T.Text)
freshName used base =
  if Set.member base used
    then go (1 :: Int)
    else (base, Set.insert base used)
  where
    go n =
      let candidate = base <> "-" <> T.pack (show n)
      in if Set.member candidate used
          then go (n + 1)
          else (candidate, Set.insert candidate used)

mkPi :: [Param] -> Expr -> Expr
mkPi params ret =
  foldr (\(Param name ty) acc -> EForAll name ty acc) ret params

splitForAll :: Expr -> ([Param], Expr)
splitForAll expr = go Set.empty expr
  where
    go used e = case e of
      EForAll v dom cod ->
        let avoid = Set.unions [used, freeVars dom, freeVars cod]
            v' = if v `Set.member` used
              then fst (freshName avoid v)
              else v
            cod' = if v' == v then cod else renameBound v v' cod
            used' = Set.insert v' used
            (params, result) = go used' cod'
        in (Param v' dom : params, result)
      _ -> ([], e)

collectApp :: Expr -> (Expr, [Expr])
collectApp expr = case expr of
  EApp f args ->
    let (headExpr, more) = collectApp f
    in (headExpr, more ++ args)
  _ -> (expr, [])

mkApp :: Expr -> [Expr] -> Expr
mkApp f [] = f
mkApp (EApp f existing) more = EApp f (existing ++ more)
mkApp f more = EApp f more

stripExpr :: Expr -> Expr
stripExpr expr = case expr of
  EAnnot e _ -> stripExpr e
  ETyped e _ -> stripExpr e
  _ -> expr

freeVars :: Expr -> Set.Set T.Text
freeVars = freeVarsWithBound Set.empty

freeVarsWithBound :: Set.Set T.Text -> Expr -> Set.Set T.Text
freeVarsWithBound bound expr = case expr of
  EVar v -> if v `Set.member` bound then Set.empty else Set.singleton v
  ELit _ -> Set.empty
  EListLiteral elems -> Set.unions (map (freeVarsWithBound bound) elems)
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
    let (bound', paramsFree) = freeVarsParamsSeq bound params
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

freeVarsBody :: Set.Set T.Text -> FunctionBody -> Set.Set T.Text
freeVarsBody bound body = case body of
  FunctionValue e -> freeVarsWithBound bound e
  FunctionCompute c -> freeVarsComp bound c

freeVarsComp :: Set.Set T.Text -> Comp -> Set.Set T.Text
freeVarsComp bound comp = case comp of
  CReturn e -> freeVarsWithBound bound e
  CPerform e -> freeVarsWithBound bound e
  CBind v c1 c2 ->
    freeVarsComp bound c1 `Set.union` freeVarsComp (Set.insert v bound) c2

freeVarsParamsSeq :: Set.Set T.Text -> [Param] -> (Set.Set T.Text, Set.Set T.Text)
freeVarsParamsSeq bound params =
  foldl step (bound, Set.empty) params
  where
    step (b, acc) (Param name ty) =
      let acc' = acc `Set.union` freeVarsWithBound b ty
      in (Set.insert name b, acc')

freeVarsCase :: Set.Set T.Text -> MatchCase -> Set.Set T.Text
freeVarsCase bound (MatchCase _ binders body) =
  let (bound', paramsFree) = freeVarsParamsSeq bound binders
  in paramsFree `Set.union` freeVarsWithBound bound' body

renameBound :: T.Text -> T.Text -> Expr -> Expr
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
            (params', cases', _bound') = renameDataParams bound params cases
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
      in (params', cases', bound')

    goComp bound comp = case comp of
      CReturn e -> CReturn (go bound e)
      CPerform e -> CPerform (go bound e)
      CBind v c1 c2 ->
        let c1' = goComp bound c1
            bound' = Set.insert v bound
        in if v == oldName
          then CBind v c1' c2
          else CBind v c1' (goComp bound' c2)
