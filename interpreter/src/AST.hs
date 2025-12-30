module AST where

import Data.Text (Text)

-- Literals in the language

data Literal
  = LNatural Integer
  | LString Text
  | LBoolean Bool
  | LUnit
  deriving (Show, Eq)

data TypeConst
  = TCNatural
  | TCString
  | TCBoolean
  | TCUnit
  | TCList
  | TCPair
  deriving (Show, Eq)

data Param = Param
  { paramName :: Text
  , paramType :: Expr
  } deriving (Show, Eq)

data Constraint = Constraint
  { constraintClass :: Text
  , constraintType  :: Expr
  } deriving (Show, Eq)

data FunctionBody
  = FunctionValue Expr
  | FunctionCompute Comp
  deriving (Show, Eq)

data DataCase = DataCase
  { dataCaseName :: Text
  , dataCaseType :: Expr
  } deriving (Show, Eq)

data MatchCase = MatchCase
  { matchCaseCtor :: Text
  , matchCaseBinders :: [Param]
  , matchCaseBody :: Expr
  } deriving (Show, Eq)

-- Expressions (value world)

data Expr
  = EVar Text
  | ELit Literal
  | EListLiteral [Expr]
  | ETypeConst TypeConst
  | ETypeUniverse Int
  | EForAll Text Expr Expr
  | EThereExists Text Expr Expr
  | ECompType Expr
  | EEqual Expr Expr Expr
  | EReflexive Expr Expr
  | ERewrite Expr Expr Expr
  | EPack Text Expr Expr Expr Expr
  | EUnpack Expr Text Text Expr
  | ELift Expr Int Int
  | EUp Expr Int Int Expr
  | EDown Expr Int Int Expr
  | EApp Expr [Expr]
  | EFunction [Param] [Constraint] Expr FunctionBody
  | ELet Text Expr Expr
  | ECompute Comp
  | EMatch Expr Expr Text Expr [MatchCase]
  | EData [Param] Expr [DataCase]
  | EAnnot Expr Expr                 -- Explicit type annotation (of-type)
  | ETyped Expr Expr                 -- Inferred type wrapper (added by type checker)
  | EDict Text [(Text, Expr)]          -- Dictionary: className, [(methodName, impl)]
  | EDictAccess Expr Text              -- Extract method from dictionary: dict, methodName
  | ETypeClass Text Expr [(Text, Expr)] -- Param name, kind, [(methodName, methodType)]
  | EInstance Text Expr [(Text, Expr)] -- Class name, instance type, [(methodName, impl)]
  deriving (Show, Eq)

-- Computations (effectful world)

data Comp
  = CReturn Expr
  | CBind Text Comp Comp
  | CPerform Expr
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

-- Module definition

data Definition = Definition
  { defTransparency :: Transparency
  , defName         :: Text
  , defBody         :: Expr
  }
  deriving (Show, Eq)


data Module = Module
  { modName    :: Text
  , modImports :: [Import]
  , modOpens   :: [Open]  -- Open statements (explicit unqualified names)
  , modDefs    :: [Definition]
  }
  deriving (Show, Eq)
