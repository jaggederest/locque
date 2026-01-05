module StripTyped
  ( stripTypedModule
  , stripRecursorsModule
  ) where

import AST
import qualified Data.Text as T

stripTypedModule :: Module -> Module
stripTypedModule (Module name imports opens defs) =
  Module name imports opens (map stripTypedDef defs)

stripRecursorsModule :: Module -> Module
stripRecursorsModule (Module name imports opens defs) =
  Module name imports opens (filter (not . isRecursorDef) defs)

isRecursorDef :: Definition -> Bool
isRecursorDef (Definition _ defName _) =
  T.isSuffixOf (T.pack "::recursor") defName

stripTypedDef :: Definition -> Definition
stripTypedDef (Definition tr name body) =
  Definition tr name (stripTypedExpr body)

stripTypedParam :: Param -> Param
stripTypedParam (Param name ty) = Param name (stripTypedExpr ty)

stripTypedConstraint :: Constraint -> Constraint
stripTypedConstraint (Constraint cls ty) = Constraint cls (stripTypedExpr ty)

stripTypedBody :: FunctionBody -> FunctionBody
stripTypedBody body = case body of
  FunctionValue e -> FunctionValue (stripTypedExpr e)
  FunctionCompute c -> FunctionCompute (stripTypedComp c)

stripTypedCase :: MatchCase -> MatchCase
stripTypedCase (MatchCase ctor binders body) =
  MatchCase ctor (map stripTypedParam binders) (stripTypedExpr body)

stripTypedDataCase :: DataCase -> DataCase
stripTypedDataCase (DataCase ctor ty) = DataCase ctor (stripTypedExpr ty)

stripTypedComp :: Comp -> Comp
stripTypedComp comp = case comp of
  CReturn e -> CReturn (stripTypedExpr e)
  CPerform e -> CPerform (stripTypedExpr e)
  CBind name c1 c2 -> CBind name (stripTypedComp c1) (stripTypedComp c2)

stripTypedExpr :: Expr -> Expr
stripTypedExpr expr = case expr of
  EVar _ -> expr
  ELit _ -> expr
  EListLiteral elems -> EListLiteral (map stripTypedExpr elems)
  ETypeConst _ -> expr
  ETypeUniverse _ -> expr
  EForAll v dom cod -> EForAll v (stripTypedExpr dom) (stripTypedExpr cod)
  EThereExists v dom cod -> EThereExists v (stripTypedExpr dom) (stripTypedExpr cod)
  ECompType eff t -> ECompType (stripTypedExpr eff) (stripTypedExpr t)
  EEqual ty lhs rhs ->
    EEqual (stripTypedExpr ty) (stripTypedExpr lhs) (stripTypedExpr rhs)
  EReflexive ty term -> EReflexive (stripTypedExpr ty) (stripTypedExpr term)
  ERewrite family proof body ->
    ERewrite (stripTypedExpr family) (stripTypedExpr proof) (stripTypedExpr body)
  EPack v dom cod witness body ->
    EPack v (stripTypedExpr dom) (stripTypedExpr cod)
      (stripTypedExpr witness) (stripTypedExpr body)
  EUnpack packed x y body ->
    EUnpack (stripTypedExpr packed) x y (stripTypedExpr body)
  ELift ty fromLevel toLevel -> ELift (stripTypedExpr ty) fromLevel toLevel
  EUp ty fromLevel toLevel body ->
    EUp (stripTypedExpr ty) fromLevel toLevel (stripTypedExpr body)
  EDown ty fromLevel toLevel body ->
    EDown (stripTypedExpr ty) fromLevel toLevel (stripTypedExpr body)
  EApp f args -> EApp (stripTypedExpr f) (map stripTypedExpr args)
  EFunction params constraints ret body ->
    EFunction
      (map stripTypedParam params)
      (map stripTypedConstraint constraints)
      (stripTypedExpr ret)
      (stripTypedBody body)
  ELet name val body ->
    ELet name (stripTypedExpr val) (stripTypedExpr body)
  ECompute comp -> ECompute (stripTypedComp comp)
  EMatch scrut scrutTy scrutName retTy cases ->
    EMatch
      (stripTypedExpr scrut)
      (stripTypedExpr scrutTy)
      scrutName
      (stripTypedExpr retTy)
      (map stripTypedCase cases)
  EData params universe cases ->
    EData
      (map stripTypedParam params)
      (stripTypedExpr universe)
      (map stripTypedDataCase cases)
  EAnnot e ty -> EAnnot (stripTypedExpr e) (stripTypedExpr ty)
  ETyped e _ -> stripTypedExpr e
  EDict className impls ->
    EDict className [ (n, stripTypedExpr e) | (n, e) <- impls ]
  EDictAccess d method -> EDictAccess (stripTypedExpr d) method
  ETypeClass param kind methods ->
    ETypeClass param (stripTypedExpr kind) [ (n, stripTypedExpr ty) | (n, ty) <- methods ]
  EInstance className instTy methods ->
    EInstance className (stripTypedExpr instTy) [ (n, stripTypedExpr e) | (n, e) <- methods ]
