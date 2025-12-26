module AST where

import Data.Text (Text)
import qualified Type as T

-- Literals in the language

data Literal
  = LNat Integer
  | LString Text
  | LBool Bool
  deriving (Show, Eq)

-- Expressions (value world)

data Expr
  = EVar Text
  | ELit Literal
  | EApp Expr [Expr]
  | ELam Text (Maybe T.Type) Expr      -- Single parameter lambda
  | ELamMulti [Text] (Maybe T.Type) Expr  -- Multi-parameter lambda (sugar for curried)
  | EAnnot Expr T.Type                 -- Explicit type annotation (expr : Type)
  | EDict Text [(Text, Expr)]          -- Dictionary: className, [(methodName, impl)]
  | EDictAccess Expr Text              -- Extract method from dictionary: dict, methodName
  deriving (Show, Eq)

-- Computations (effectful world)

data Comp
  = CReturn Expr
  | CBind Text Comp Comp
  | CPerform Expr
  | CVar Text
  | CSeq Comp Comp -- sequencing where the result of the first is ignored
  deriving (Show, Eq)

-- Definition kinds and transparency

data DefKind
  = ValueDef
  | ComputationDef
  | FamilyDef         -- Type family definition
  | TypeClassDef      -- Type class definition
  | InstanceDef       -- Type class instance
  deriving (Show, Eq)

data Transparency = Transparent | Opaque deriving (Show, Eq)

data Import = Import
  { impModule :: Text
  , impAlias  :: Text
  } deriving (Show, Eq)

data Open = Open
  { openModule :: Text  -- Module alias to open from
  , openNames  :: [Text]  -- Specific names to bring into scope
  } deriving (Show, Eq)

-- Type family case: pattern types -> result type
data TypeFamilyCase = TypeFamilyCase
  { tfcPatterns :: [T.Type]  -- Pattern types to match (e.g., [List a, b])
  , tfcResult   :: T.Type    -- Result type (e.g., () -> b)
  } deriving (Show, Eq)

-- Type family body
data TypeFamilyBody = TypeFamilyBody
  { tfbKind  :: T.Type       -- Kind signature (e.g., Type -> Type -> Type)
  , tfbCases :: [TypeFamilyCase]
  } deriving (Show, Eq)

-- Type class body
data TypeClassBody = TypeClassBody
  { tcbParam   :: Text              -- Type parameter name (e.g., "a")
  , tcbMethods :: [(Text, T.Type)]  -- Method name -> type signature
  } deriving (Show, Eq)

-- Instance body
data InstanceBody = InstanceBody
  { instClassName :: Text      -- Type class being instantiated
  , instType      :: T.Type    -- Type being instantiated (e.g., List a)
  , instImpls     :: [(Text, Expr)]  -- Method name -> implementation
  } deriving (Show, Eq)

-- Definition body (sum type for different definition kinds)
data DefBody
  = ValueBody Expr
  | ComputationBody Comp
  | FamilyBody TypeFamilyBody
  | ClassBody TypeClassBody
  | InstBody InstanceBody
  deriving (Show, Eq)

-- Module definition

data Definition = Definition
  { defTransparency :: Transparency
  , defName         :: Text
  , defKind         :: DefKind
  , defType         :: Maybe T.TypeScheme  -- Optional type annotation
  , defBody         :: DefBody
  }
  deriving (Show, Eq)


data Module = Module
  { modName    :: Text
  , modImports :: [Import]
  , modOpens   :: [Open]  -- Open statements (explicit unqualified names)
  , modDefs    :: [Definition]
  }
  deriving (Show, Eq)
