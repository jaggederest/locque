{-# LANGUAGE OverloadedStrings #-}
module DictPass
  ( transformModule
  , transformModuleWithEnvs
  , collectClassEnv
  , collectInstEnv
  ) where

import qualified Data.Map.Strict as Map
import AST

-- | Dictionary pass is currently a no-op until typeclass syntax is reintroduced.
transformModule :: Map.Map a b -> Map.Map c d -> Module -> Module
transformModule _ _ m = m

-- | Collect typeclass definitions from a module (disabled).
collectClassEnv :: Module -> Map.Map a b
collectClassEnv _ = Map.empty

-- | Collect instance definitions from a module (disabled).
collectInstEnv :: Module -> Map.Map a b
collectInstEnv _ = Map.empty

-- | Transform a module (self-contained version that collects envs from the module)
transformModuleWithEnvs :: Module -> Module
transformModuleWithEnvs m =
  transformModule Map.empty Map.empty m
