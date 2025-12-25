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
  | ELam Text (Maybe T.Type) Expr  -- Optional type annotation for parameter
  | EAnnot Expr T.Type             -- Explicit type annotation (expr : Type)
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

data DefKind = ValueDef | ComputationDef deriving (Show, Eq)

data Transparency = Transparent | Opaque deriving (Show, Eq)

data Import = Import
  { impModule :: Text
  , impAlias  :: Text
  } deriving (Show, Eq)

-- Module definition

data Definition = Definition
  { defTransparency :: Transparency
  , defName         :: Text
  , defKind         :: DefKind
  , defType         :: Maybe T.TypeScheme  -- Optional type annotation
  , defBody         :: Either Expr Comp    -- Left for value, Right for computation
  }
  deriving (Show, Eq)


data Module = Module
  { modName    :: Text
  , modImports :: [Import]
  , modDefs    :: [Definition]
  }
  deriving (Show, Eq)
