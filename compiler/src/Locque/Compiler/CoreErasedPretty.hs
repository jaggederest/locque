{-# LANGUAGE OverloadedStrings #-}

module Locque.Compiler.CoreErasedPretty
  ( renderErasedModule
  , renderErasedDecl
  , renderErasedValue
  , renderErasedComp
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Locque.Compiler.Core (CoreLiteral(..), Name(..))
import Locque.Compiler.CoreErased

renderErasedModule :: ErasedModule -> Text
renderErasedModule (ErasedModule name decls) =
  sexp "module" (renderName name : map renderErasedDecl decls)

renderErasedDecl :: ErasedDecl -> Text
renderErasedDecl decl =
  case decl of
    EDef name value ->
      sexp "def" [renderName name, renderErasedValue value]
    EDefComp name comp ->
      sexp "def-comp" [renderName name, renderErasedComp comp]
    EData dataDecl ->
      renderErasedDataDecl dataDecl

renderErasedValue :: ErasedValue -> Text
renderErasedValue value =
  case value of
    EVar name -> sexp "evar" [renderName name]
    ELit lit -> renderLiteral lit
    EErased -> sexp "eerased" []
    ELam name body ->
      sexp "elambda" [renderName name, renderErasedValue body]
    EAppValue fn arg ->
      sexp "eapp" [renderErasedValue fn, renderErasedValue arg]
    EConstructor name args ->
      sexp "ector" (renderName name : map renderErasedValue args)
    ECompute comp ->
      sexp "ecompute" [renderErasedComp comp]
    ELetValue name val body ->
      sexp "elet" [renderName name, renderErasedValue val, renderErasedValue body]
    EMatchValue scrut cases ->
      sexp "ematch" (renderErasedValue scrut : map renderErasedValueCase cases)

renderErasedComp :: ErasedComp -> Text
renderErasedComp comp =
  case comp of
    EReturn value ->
      sexp "ereturn" [renderErasedValue value]
    EBind name left right ->
      sexp "ebind" [renderName name, renderErasedComp left, renderErasedComp right]
    EPerform value ->
      sexp "eperform" [renderErasedValue value]
    EApp fn arg ->
      sexp "ecapp" [renderErasedValue fn, renderErasedValue arg]
    ELet name value body ->
      sexp "eletc" [renderName name, renderErasedValue value, renderErasedComp body]
    EMatch value cases ->
      sexp "ematchc" (renderErasedValue value : map renderErasedCase cases)

renderErasedDataDecl :: ErasedDataDecl -> Text
renderErasedDataDecl (ErasedDataDecl name ctors) =
  sexp "data" (renderName name : map renderErasedCtor ctors)

renderErasedCtor :: ErasedCtor -> Text
renderErasedCtor (ErasedCtor name arity) =
  sexp "ctor" [renderName name, T.pack (show arity)]

renderErasedCase :: ErasedCase -> Text
renderErasedCase (ErasedCase ctor binders body) =
  sexp "ecase" [renderName ctor, renderBinders binders, renderErasedComp body]

renderErasedValueCase :: ErasedValueCase -> Text
renderErasedValueCase (ErasedValueCase ctor binders body) =
  sexp "evcase" [renderName ctor, renderBinders binders, renderErasedValue body]

renderLiteral :: CoreLiteral -> Text
renderLiteral lit =
  case lit of
    LitNatural nat -> sexp "elit-natural" [T.pack (show nat)]
    LitString text -> sexp "elit-string" [renderString text]
    LitBoolean bool -> sexp "elit-bool" [renderBool bool]
    LitUnit -> sexp "elit-unit" []

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
