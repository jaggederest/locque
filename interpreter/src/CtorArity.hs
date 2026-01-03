module CtorArity
  ( CtorArityMap
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)

type CtorArityMap = Map.Map Text (Int, Int)
