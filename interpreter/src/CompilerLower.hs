{-# LANGUAGE OverloadedStrings #-}
module CompilerLower
  ( lowerModule
  ) where

import qualified AST as AST
import Locque.Compiler.Core

lowerModule :: AST.Module -> CoreModule
lowerModule (AST.Module modName _imports _opens defs) =
  CoreModule (Name modName) (map lowerDefinition defs)

lowerDefinition :: AST.Definition -> CoreDecl
lowerDefinition defn =
  CoreDef
    (Name (AST.defName defn))
    TyUnit
    (lowerExpr (AST.defBody defn))

lowerExpr :: AST.Expr -> CoreValue
lowerExpr expr =
  case expr of
    AST.EVar name -> VVar (Name name)
    AST.ELit lit -> VLit (lowerLiteral lit)
    AST.EListLiteral elems -> lowerListLiteral elems
    AST.ECompute comp -> VCompute (lowerComp comp)
    AST.EAnnot inner _ty -> lowerExpr inner
    AST.ETyped inner _ty -> lowerExpr inner
    AST.EApp fn args -> VCompute (lowerApp fn args)
    AST.EFunction params _constraints _retTy body ->
      lowerFunction params body
    AST.ELet name value body ->
      VCompute (CLet (Name name) (lowerExpr value) (CReturn (lowerExpr body)))
    AST.EMatch scrut _scrutTy _scrutName _retTy cases ->
      VCompute (CMatch (lowerExpr scrut) (map lowerMatchCase cases))
    AST.EData _params _retTy _cases ->
      error "lowerExpr: data declarations are not supported yet"
    _ -> error "lowerExpr: unsupported expression form"

lowerFunction :: [AST.Param] -> AST.FunctionBody -> CoreValue
lowerFunction params body =
  case params of
    [] -> VCompute (lowerFunctionBody body)
    (AST.Param name _ty : rest) ->
      VLam (Name name) TyUnit (CReturn (lowerFunction rest body))

lowerFunctionBody :: AST.FunctionBody -> CoreComp
lowerFunctionBody body =
  case body of
    AST.FunctionValue expr -> CReturn (lowerExpr expr)
    AST.FunctionCompute comp -> lowerComp comp

lowerComp :: AST.Comp -> CoreComp
lowerComp comp =
  case comp of
    AST.CReturn expr -> CReturn (lowerExpr expr)
    AST.CBind name left right ->
      CBind (Name name) (lowerComp left) (lowerComp right)
    AST.CPerform expr -> CPerform (lowerExpr expr)

lowerApp :: AST.Expr -> [AST.Expr] -> CoreComp
lowerApp fn args =
  case args of
    [] -> CReturn (lowerExpr fn)
    [arg] -> CApp (lowerExpr fn) (lowerExpr arg)
    _ -> error "lowerApp: multi-argument applications not supported yet"

lowerMatchCase :: AST.MatchCase -> CoreCase
lowerMatchCase (AST.MatchCase ctor params body) =
  CoreCase
    (Name ctor)
    (map (Name . AST.paramName) params)
    (CReturn (lowerExpr body))

lowerListLiteral :: [AST.Expr] -> CoreValue
lowerListLiteral elems =
  foldr
    (\elemValue acc ->
        VConstructor
          (Name "List::cons")
          [lowerExpr elemValue, acc])
    (VConstructor (Name "List::empty") [])
    elems

lowerLiteral :: AST.Literal -> CoreLiteral
lowerLiteral lit =
  case lit of
    AST.LNatural n -> LitNatural n
    AST.LString s -> LitString s
    AST.LBoolean b -> LitBoolean b
    AST.LUnit -> LitUnit
