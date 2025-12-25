{-# LANGUAGE OverloadedStrings #-}
module Type where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Type representation for locque
data Type
  = TNat                    -- ^ Natural numbers
  | TString                 -- ^ Strings
  | TBool                   -- ^ Booleans
  | TUnit                   -- ^ Unit type (for tt)
  | TList Type              -- ^ Homogeneous lists
  | TPair Type Type         -- ^ Pairs
  | TFun Type Type          -- ^ Function types (A -> B)
  | TVar Text               -- ^ Type variables (a, b, c)
  | TForAll Text Type       -- ^ Universal quantification: ∀a. T
  | TComp Type              -- ^ Computation types (wraps value type)
  deriving (Show, Eq)

-- | Type schemes for polymorphic bindings (∀a b. Type)
data TypeScheme = TypeScheme [Text] Type
  deriving (Show, Eq)

-- | Type environment (maps names to type schemes)
type TypeEnv = Map.Map Text TypeScheme

-- | Get free type variables in a type
freeVars :: Type -> Set.Set Text
freeVars TNat = Set.empty
freeVars TString = Set.empty
freeVars TBool = Set.empty
freeVars TUnit = Set.empty
freeVars (TList t) = freeVars t
freeVars (TPair a b) = freeVars a `Set.union` freeVars b
freeVars (TFun a b) = freeVars a `Set.union` freeVars b
freeVars (TVar v) = Set.singleton v
freeVars (TForAll v t) = Set.delete v (freeVars t)
freeVars (TComp t) = freeVars t

-- | Get free type variables in a type scheme
freeVarsScheme :: TypeScheme -> Set.Set Text
freeVarsScheme (TypeScheme vars ty) = freeVars ty Set.\\ Set.fromList vars

-- | Pretty-print a type in human-readable format
-- Examples: "Nat", "List String", "Nat -> Bool", "∀a. List a -> Nat", "Comp String"
prettyType :: Type -> Text
prettyType TNat = "Nat"
prettyType TString = "String"
prettyType TBool = "Bool"
prettyType TUnit = "Unit"
prettyType (TList t) = "List " <> prettyTypeAtom t
prettyType (TPair a b) = "Pair " <> prettyTypeAtom a <> " " <> prettyTypeAtom b
prettyType (TFun a b) = prettyTypeAtom a <> " -> " <> prettyType b
prettyType (TVar v) = v
prettyType (TForAll v t) = "∀" <> v <> ". " <> prettyType t
prettyType (TComp t) = "Comp " <> prettyTypeAtom t

-- | Pretty-print a type in atomic position (add parens if needed)
prettyTypeAtom :: Type -> Text
prettyTypeAtom t@TNat = prettyType t
prettyTypeAtom t@TString = prettyType t
prettyTypeAtom t@TBool = prettyType t
prettyTypeAtom t@TUnit = prettyType t
prettyTypeAtom t@(TVar _) = prettyType t
prettyTypeAtom t = "(" <> prettyType t <> ")"

-- | Pretty-print a type scheme
prettyTypeScheme :: TypeScheme -> Text
prettyTypeScheme (TypeScheme [] ty) = prettyType ty
prettyTypeScheme (TypeScheme vars ty) = "∀" <> T.intercalate " " vars <> ". " <> prettyType ty

-- | Render a type as S-expression format
-- Examples: "Nat", "(List String)", "(-> Nat Bool)", "(for-all (a) (-> (List a) Nat))"
typeToSExpr :: Type -> Text
typeToSExpr TNat = "Nat"
typeToSExpr TString = "String"
typeToSExpr TBool = "Bool"
typeToSExpr TUnit = "Unit"
typeToSExpr (TList t) = "(List " <> typeToSExpr t <> ")"
typeToSExpr (TPair a b) = "(Pair " <> typeToSExpr a <> " " <> typeToSExpr b <> ")"
typeToSExpr (TFun a b) = "(-> " <> typeToSExpr a <> " " <> typeToSExpr b <> ")"
typeToSExpr (TVar v) = v
typeToSExpr (TForAll v t) = "(for-all (" <> v <> ") " <> typeToSExpr t <> ")"
typeToSExpr (TComp t) = "(Comp " <> typeToSExpr t <> ")"

-- | Render a type scheme as S-expression
typeSchemeToSExpr :: TypeScheme -> Text
typeSchemeToSExpr (TypeScheme [] ty) = typeToSExpr ty
typeSchemeToSExpr (TypeScheme vars ty) =
  "(for-all (" <> T.intercalate " " vars <> ") " <> typeToSExpr ty <> ")"
