{-# LANGUAGE OverloadedStrings #-}
module Recursor
  ( injectRecursors
  , recursorDefinition
  , recursorDefs
  , insertRecursors
  ) where

import AST
import ASTUtils (stripExpr, collectApp, mkApp, splitForAll, freshName, freeVars, freeVarsWithBound, freeVarsBody, freeVarsComp, freeVarsParamsSeq, freeVarsCase, renameBound)
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

mkPi :: [Param] -> Expr -> Expr
mkPi params ret =
  foldr (\(Param name ty) acc -> EForAll name ty acc) ret params
