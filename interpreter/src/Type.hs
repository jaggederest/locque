{-# LANGUAGE OverloadedStrings #-}
module Type
  ( TypeEnv
  , typeConstName
  , prettyType
  , prettyTypeAtom
  , typeToSExpr
  ) where

import AST
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

type TypeEnv = Map.Map Text Expr

typeConstName :: TypeConst -> Text
typeConstName tc = case tc of
  TCNatural -> "Natural"
  TCString -> "String"
  TCBoolean -> "Boolean"
  TCUnit -> "Unit"
  TCList -> "List"
  TCPair -> "Pair"

prettyType :: Expr -> Text
prettyType expr = case expr of
  ETypeConst tc -> typeConstName tc
  ETypeUniverse n -> "Type" <> T.pack (show n)
  ELit lit -> renderLiteral lit
  EListLiteral elems ->
    "[" <> T.intercalate ", " (map prettyTypeAtom elems) <> "]"
  EVar v -> v
  EForAll v dom cod ->
    "for-all " <> v <> " as " <> prettyTypeAtom dom <> " to " <> prettyType cod
  EThereExists v dom cod ->
    "there-exists " <> v <> " as " <> prettyTypeAtom dom <> " in " <> prettyType cod
  ECompType t -> "computation " <> prettyTypeAtom t
  EEqual ty lhs rhs ->
    "equal " <> prettyTypeAtom ty <> " " <> prettyTypeAtom lhs <> " " <> prettyTypeAtom rhs
  ELift ty fromLevel toLevel ->
    "lift " <> prettyTypeAtom ty <> " from " <> prettyUniverse fromLevel
      <> " to " <> prettyUniverse toLevel
  EApp f args ->
    prettyTypeAtom f <> " " <> T.intercalate " " (map prettyTypeAtom args)
  _ -> "<invalid-type>"

prettyTypeAtom :: Expr -> Text
prettyTypeAtom t = case t of
  ETypeConst _ -> prettyType t
  ETypeUniverse _ -> prettyType t
  ELit _ -> prettyType t
  EVar _ -> prettyType t
  _ -> "(" <> prettyType t <> ")"

typeToSExpr :: Expr -> Text
typeToSExpr expr = case expr of
  ETypeConst tc -> typeConstName tc
  ETypeUniverse n -> "Type" <> T.pack (show n)
  ELit lit -> renderLiteral lit
  EListLiteral elems ->
    "(list" <> renderList elems <> ")"
  EVar v -> v
  EForAll v dom cod ->
    "(for-all (" <> v <> " " <> typeToSExpr dom <> ") " <> typeToSExpr cod <> ")"
  EThereExists v dom cod ->
    "(there-exists (" <> v <> " " <> typeToSExpr dom <> ") " <> typeToSExpr cod <> ")"
  ECompType t -> "(computation " <> typeToSExpr t <> ")"
  EEqual ty lhs rhs ->
    "(equal " <> typeToSExpr ty <> " " <> typeToSExpr lhs <> " " <> typeToSExpr rhs <> ")"
  ELift ty fromLevel toLevel ->
    "(lift " <> typeToSExpr ty <> " " <> prettyUniverse fromLevel
      <> " " <> prettyUniverse toLevel <> ")"
  EApp f args ->
    "(" <> T.intercalate " " (typeToSExpr f : map typeToSExpr args) <> ")"
  _ -> "<invalid-type>"
  where
    renderList elems =
      case elems of
        [] -> ""
        _ -> " " <> T.intercalate " " (map typeToSExpr elems)

prettyUniverse :: Int -> Text
prettyUniverse n = "Type" <> T.pack (show n)

renderLiteral :: Literal -> Text
renderLiteral lit = case lit of
  LNatural n -> T.pack (show n)
  LString s -> T.pack (show (T.unpack s))
  LBoolean b -> if b then "true" else "false"
  LUnit -> "tt"
