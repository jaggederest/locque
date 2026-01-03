module Locque.Compiler.CoreErased where

import Locque.Compiler.Core (CoreLiteral, Name)

data ErasedModule = ErasedModule
  { erasedModuleName :: Name
  , erasedModuleDecls :: [ErasedDecl]
  } deriving (Show, Read, Eq)

data ErasedDecl
  = EDef Name ErasedValue
  | EDefComp Name ErasedComp
  | EData ErasedDataDecl
  deriving (Show, Read, Eq)

data ErasedDataDecl = ErasedDataDecl
  { erasedDataName :: Name
  , erasedDataCtors :: [ErasedCtor]
  } deriving (Show, Read, Eq)

data ErasedCtor = ErasedCtor
  { erasedCtorName :: Name
  , erasedCtorArity :: Int
  } deriving (Show, Read, Eq)

data ErasedValue
  = EVar Name
  | ELit CoreLiteral
  | EErased
  | ELam Name ErasedValue
  | EAppValue ErasedValue ErasedValue
  | EConstructor Name [ErasedValue]
  | ECompute ErasedComp
  | ELetValue Name ErasedValue ErasedValue
  | EMatchValue ErasedValue [ErasedValueCase]
  deriving (Show, Read, Eq)

data ErasedComp
  = EReturn ErasedValue
  | EBind Name ErasedComp ErasedComp
  | EPerform ErasedValue
  | EApp ErasedValue ErasedValue
  | ELet Name ErasedValue ErasedComp
  | EMatch ErasedValue [ErasedCase]
  deriving (Show, Read, Eq)

data ErasedCase = ErasedCase
  { erasedCaseCtor :: Name
  , erasedCaseBinders :: [Name]
  , erasedCaseBody :: ErasedComp
  } deriving (Show, Read, Eq)

data ErasedValueCase = ErasedValueCase
  { erasedValueCaseCtor :: Name
  , erasedValueCaseBinders :: [Name]
  , erasedValueCaseBody :: ErasedValue
  } deriving (Show, Read, Eq)
