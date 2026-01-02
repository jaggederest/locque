{-# LANGUAGE OverloadedStrings #-}

module Locque.Compiler.CorePretty
  ( renderCoreModule
  , renderCoreDecl
  , renderCoreValue
  , renderCoreComp
  , renderCoreType
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Locque.Compiler.Core

renderCoreModule :: CoreModule -> Text
renderCoreModule (CoreModule name decls) =
  sexp "module" (renderName name : map renderCoreDecl decls)

renderCoreDecl :: CoreDecl -> Text
renderCoreDecl decl =
  case decl of
    CoreDef name ty value ->
      sexp "def" [renderName name, renderCoreType ty, renderCoreValue value]
    CoreDefComp name ty comp ->
      sexp "def-comp" [renderName name, renderCoreType ty, renderCoreComp comp]
    CoreData dataDecl ->
      renderCoreDataDecl dataDecl

renderCoreValue :: CoreValue -> Text
renderCoreValue value =
  case value of
    VVar name -> sexp "vvar" [renderName name]
    VLit lit -> renderLiteral lit
    VLam name ty body ->
      sexp "vlambda" [renderName name, renderCoreType ty, renderCoreComp body]
    VConstructor name args ->
      sexp "vctor" (renderName name : map renderCoreValue args)
    VCompute comp ->
      sexp "vcompute" [renderCoreComp comp]

renderCoreComp :: CoreComp -> Text
renderCoreComp comp =
  case comp of
    CReturn value ->
      sexp "creturn" [renderCoreValue value]
    CBind name left right ->
      sexp "cbind" [renderName name, renderCoreComp left, renderCoreComp right]
    CPerform value ->
      sexp "cperform" [renderCoreValue value]
    CApp fn arg ->
      sexp "capp" [renderCoreValue fn, renderCoreValue arg]
    CLet name value body ->
      sexp "clet" [renderName name, renderCoreValue value, renderCoreComp body]
    CMatch value cases ->
      sexp "cmatch" (renderCoreValue value : map renderCoreCase cases)

renderCoreType :: CoreType -> Text
renderCoreType ty =
  case ty of
    TyVar name -> sexp "tvar" [renderName name]
    TyCon name args -> sexp "tcon" (renderName name : map renderCoreType args)
    TyFun arg res -> sexp "tfun" [renderCoreType arg, renderCoreType res]
    TyComp inner -> sexp "tcomp" [renderCoreType inner]
    TyUnit -> sexp "tunit" []
    TyBoolean -> sexp "tboolean" []
    TyNatural -> sexp "tnatural" []
    TyString -> sexp "tstring" []
    TyCharacter -> sexp "tcharacter" []

renderCoreDataDecl :: CoreDataDecl -> Text
renderCoreDataDecl (CoreDataDecl name params ctors) =
  sexp
    "data"
    (renderName name : renderParamList params : map renderCoreCtor ctors)

renderParamList :: [Name] -> Text
renderParamList params =
  sexp "params" (map renderName params)

renderCoreCtor :: CoreCtor -> Text
renderCoreCtor (CoreCtor name fields) =
  sexp "ctor" (renderName name : map renderCoreType fields)

renderCoreCase :: CoreCase -> Text
renderCoreCase (CoreCase ctor binders body) =
  sexp "ccase" [renderName ctor, renderBinders binders, renderCoreComp body]

renderLiteral :: CoreLiteral -> Text
renderLiteral lit =
  case lit of
    LitNatural nat -> sexp "vlit-natural" [T.pack (show nat)]
    LitString text -> sexp "vlit-string" [renderString text]
    LitBoolean bool -> sexp "vlit-bool" [renderBool bool]
    LitUnit -> sexp "vlit-unit" []

renderBool :: Bool -> Text
renderBool value = if value then "true" else "false"

renderString :: Text -> Text
renderString text =
  T.pack "\"" <> escapeString text <> T.pack "\""

escapeString :: Text -> Text
escapeString = T.concatMap escapeChar

escapeChar :: Char -> Text
escapeChar char =
  case char of
    '\\' -> "\\\\"
    '"' -> "\\\""
    '\n' -> "\\n"
    '\t' -> "\\t"
    '\r' -> "\\r"
    _ -> T.singleton char

renderName :: Name -> Text
renderName (Name name) = name

renderBinders :: [Name] -> Text
renderBinders names =
  "(" <> T.intercalate " " (map renderName names) <> ")"

sexp :: Text -> [Text] -> Text
sexp headAtom atoms =
  "(" <> T.intercalate " " (headAtom : atoms) <> ")"
