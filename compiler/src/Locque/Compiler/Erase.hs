module Locque.Compiler.Erase
  ( eraseModule
  , eraseDecl
  , eraseDataDecl
  , eraseCtor
  , eraseValue
  , eraseComp
  ) where

import Locque.Compiler.Core
import Locque.Compiler.CoreErased

eraseModule :: CoreModule -> ErasedModule
eraseModule (CoreModule name decls) =
  ErasedModule name (map eraseDecl decls)

eraseDecl :: CoreDecl -> ErasedDecl
eraseDecl decl =
  case decl of
    CoreDef name _ value -> EDef name (eraseValue value)
    CoreDefComp name _ comp -> EDefComp name (eraseComp comp)
    CoreData dataDecl -> EData (eraseDataDecl dataDecl)

eraseDataDecl :: CoreDataDecl -> ErasedDataDecl
eraseDataDecl (CoreDataDecl name _ ctors) =
  ErasedDataDecl name (map eraseCtor ctors)

eraseCtor :: CoreCtor -> ErasedCtor
eraseCtor (CoreCtor name fields) =
  ErasedCtor name (length fields)

eraseValue :: CoreValue -> ErasedValue
eraseValue value =
  case value of
    VVar name -> EVar name
    VLit lit -> ELit lit
    VLam name _ body -> ELam name (eraseComp body)
    VConstructor name args -> EConstructor name (map eraseValue args)
    VCompute comp -> ECompute (eraseComp comp)

eraseComp :: CoreComp -> ErasedComp
eraseComp comp =
  case comp of
    CReturn value -> EReturn (eraseValue value)
    CBind name left right ->
      EBind name (eraseComp left) (eraseComp right)
    CPerform value -> EPerform (eraseValue value)
    CApp fn arg -> EApp (eraseValue fn) (eraseValue arg)
    CLet name value body ->
      ELet name (eraseValue value) (eraseComp body)
    CMatch value cases ->
      EMatch (eraseValue value) (map eraseCase cases)

eraseCase :: CoreCase -> ErasedCase
eraseCase (CoreCase ctor binders body) =
  ErasedCase ctor binders (eraseComp body)
