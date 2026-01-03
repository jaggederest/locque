{-# LANGUAGE OverloadedStrings #-}
module CompilerLower
  ( lowerModule
  ) where

import qualified Data.Set as Set
import qualified Data.Text as T

import qualified AST as AST
import Locque.Compiler.Core

lowerModule :: AST.Module -> CoreModule
lowerModule (AST.Module modName _imports _opens defs) =
  CoreModule (Name modName) (map lowerDefinition defs)

lowerDefinition :: AST.Definition -> CoreDecl
lowerDefinition defn =
  case AST.defBody defn of
    AST.EData params _universe cases ->
      CoreData (lowerDataDecl (AST.defName defn) params cases)
    body ->
      CoreDef
        (Name (AST.defName defn))
        TyUnit
        (lowerExpr body)

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
    (firstArg : restArgs) ->
      case restArgs of
        [] -> CApp (lowerExpr fn) (lowerExpr firstArg)
        _ ->
          let first = CApp (lowerExpr fn) (lowerExpr firstArg)
           in foldl applyArg first (zip [1 :: Int ..] restArgs)
  where
    applyArg comp (idx, arg) =
      let name = Name (T.pack ("__app" ++ show idx))
       in CBind name comp (CApp (VVar name) (lowerExpr arg))

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

lowerDataDecl :: T.Text -> [AST.Param] -> [AST.DataCase] -> CoreDataDecl
lowerDataDecl typeName params cases =
  let paramNames = Set.fromList (map AST.paramName params)
   in CoreDataDecl
        (Name typeName)
        (map (Name . AST.paramName) params)
        (map (lowerDataCase paramNames) cases)

lowerDataCase :: Set.Set T.Text -> AST.DataCase -> CoreCtor
lowerDataCase paramNames (AST.DataCase ctorName ctorType) =
  CoreCtor
    (Name ctorName)
    (collectCtorFields paramNames ctorType)

collectCtorFields :: Set.Set T.Text -> AST.Expr -> [CoreType]
collectCtorFields paramNames expr =
  case expr of
    AST.EForAll _ dom cod ->
      lowerType paramNames dom : collectCtorFields paramNames cod
    _ -> []

lowerType :: Set.Set T.Text -> AST.Expr -> CoreType
lowerType paramNames expr =
  case expr of
    AST.ETypeConst tc -> lowerTypeConst tc
    AST.ETypeUniverse _ -> TyUnit
    AST.EForAll _ dom cod -> TyFun (lowerType paramNames dom) (lowerType paramNames cod)
    AST.EThereExists _ dom cod -> TyCon (Name "Exists") [lowerType paramNames dom, lowerType paramNames cod]
    AST.ECompType _eff ty -> TyComp (lowerType paramNames ty)
    AST.ELift ty _ _ -> lowerType paramNames ty
    AST.EUp _ _ _ body -> lowerType paramNames body
    AST.EDown _ _ _ body -> lowerType paramNames body
    AST.EVar name ->
      if name `Set.member` paramNames
        then TyVar (Name name)
        else TyCon (Name name) []
    AST.EApp f args ->
      let headTy = lowerType paramNames f
          argTys = map (lowerType paramNames) args
       in case headTy of
            TyCon name existing -> TyCon name (existing ++ argTys)
            TyVar name -> TyCon name argTys
            _ -> headTy
    _ -> TyUnit

lowerTypeConst :: AST.TypeConst -> CoreType
lowerTypeConst tc =
  case tc of
    AST.TCNatural -> TyNatural
    AST.TCString -> TyString
    AST.TCBoolean -> TyBoolean
    AST.TCUnit -> TyUnit
    AST.TCList -> TyCon (Name "List") []
    AST.TCPair -> TyCon (Name "Pair") []
    AST.TCDictionary -> TyCon (Name "Dictionary") []
    AST.TCListener -> TyCon (Name "Listener") []
    AST.TCSocket -> TyCon (Name "Socket") []
