{-# LANGUAGE OverloadedStrings #-}
module ASTUtils
  ( stripExpr
  , isEffectAny
  , typeConstName
  , collectApp
  , mkApp
  , splitForAll
  , freshName
  , freeVars
  , freeVarsWithBound
  , freeVarsBody
  , freeVarsComp
  , freeVarsParamsSeq
  , freeVarsCase
  , renameBound
  ) where

import AST

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

-- | Strip annotation/typed wrappers from an expression
stripExpr :: Expr -> Expr
stripExpr expr = case expr of
  EAnnot e _ -> stripExpr e
  ETyped e _ -> stripExpr e
  _ -> expr

-- | Check whether an expression is the Effects::any sentinel
isEffectAny :: Expr -> Bool
isEffectAny expr = case expr of
  EVar name -> name == effectAnyName
  _ -> False

-- | Render a TypeConst as human-readable text
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

-- | Flatten nested EApp into a head expression and argument list
collectApp :: Expr -> (Expr, [Expr])
collectApp expr = case expr of
  EApp f args ->
    let (headExpr, more) = collectApp f
    in (headExpr, more ++ args)
  _ -> (expr, [])

-- | Build an application, merging with existing EApp if present
mkApp :: Expr -> [Expr] -> Expr
mkApp f [] = f
mkApp (EApp f existing) more = EApp f (existing ++ more)
mkApp f more                 = EApp f more

-- | Decompose a chain of EForAll into a parameter list and body,
--   alpha-renaming to avoid name collisions.
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

-- | Generate a fresh name not in the given set, appending -1, -2, etc.
freshName :: Set.Set Text -> Text -> (Text, Set.Set Text)
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

--------------------------------------------------------------------------------
-- Free variables

freeVars :: Expr -> Set.Set Text
freeVars = freeVarsWithBound Set.empty

freeVarsWithBound :: Set.Set Text -> Expr -> Set.Set Text
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
-- Alpha-renaming

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
