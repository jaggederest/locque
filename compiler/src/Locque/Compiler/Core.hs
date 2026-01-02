module Locque.Compiler.Core where

import Data.Text (Text)

newtype Name = Name { unName :: Text }
  deriving (Show, Read, Eq, Ord)

data CoreLiteral
  = LitNatural Integer
  | LitString Text
  | LitBoolean Bool
  | LitUnit
  deriving (Show, Read, Eq)

data CoreType
  = TyVar Name
  | TyCon Name [CoreType]
  | TyFun CoreType CoreType
  | TyComp CoreType
  | TyUnit
  | TyBoolean
  | TyNatural
  | TyString
  | TyCharacter
  deriving (Show, Read, Eq)

data CoreValue
  = VVar Name
  | VLit CoreLiteral
  | VLam Name CoreType CoreComp
  | VConstructor Name [CoreValue]
  | VCompute CoreComp
  deriving (Show, Read, Eq)

data CoreComp
  = CReturn CoreValue
  | CBind Name CoreComp CoreComp
  | CPerform CoreValue
  | CApp CoreValue CoreValue
  | CLet Name CoreValue CoreComp
  | CMatch CoreValue [CoreCase]
  deriving (Show, Read, Eq)

data CoreCase = CoreCase
  { caseCtor :: Name
  , caseBinders :: [Name]
  , caseBody :: CoreComp
  } deriving (Show, Read, Eq)

data CoreCtor = CoreCtor
  { ctorName :: Name
  , ctorFields :: [CoreType]
  } deriving (Show, Read, Eq)

data CoreDataDecl = CoreDataDecl
  { dataName :: Name
  , dataParams :: [Name]
  , dataCtors :: [CoreCtor]
  } deriving (Show, Read, Eq)

data CoreDecl
  = CoreDef Name CoreType CoreValue
  | CoreDefComp Name CoreType CoreComp
  | CoreData CoreDataDecl
  deriving (Show, Read, Eq)

data CoreModule = CoreModule
  { coreModuleName :: Name
  , coreModuleDecls :: [CoreDecl]
  } deriving (Show, Read, Eq)
