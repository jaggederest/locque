{-# LANGUAGE OverloadedStrings #-}

module Locque.Compiler.Convert
  ( lowerModule
  , lowerValue
  , lowerComp
  , lowerType
  ) where

import Locque.Compiler.Core

lowerModule :: elabModule -> CoreModule
lowerModule _ = CoreModule (Name "todo") []

lowerValue :: elabValue -> CoreValue
lowerValue _ = VVar (Name "todo")

lowerComp :: elabComp -> CoreComp
lowerComp _ = CReturn (VVar (Name "todo"))

lowerType :: elabType -> CoreType
lowerType _ = TyUnit
