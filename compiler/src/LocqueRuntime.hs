module LocqueRuntime
  ( Comp(..)
  , compReturn
  , compBind
  , perform
  , Unit
  , Natural
  , String
  , Character
  , List
  , Pair
  , Option
  , Either
  , Result
  , addNatPrim
  , subNatPrim
  , eqNatPrim
  , eqStringPrim
  , concatStringPrim
  , stringLengthPrim
  , errorPrim
  , printPrim
  , getLinePrim
  , panicPrim
  ) where

import Prelude hiding (Either, String)
import qualified Prelude as P
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

newtype Comp a = Comp { runComp :: IO a }

type Unit = ()
type Natural = Integer
type String = Text
type Character = Text
type List a = [a]
type Pair a b = (a, b)
type Option a = P.Maybe a
type Either a b = P.Either a b
type Result a e = P.Either e a

compReturn :: a -> Comp a
compReturn value = Comp (P.pure value)

compBind :: Comp a -> (a -> Comp b) -> Comp b
compBind (Comp action) next =
  Comp (action P.>>= (runComp . next))

perform :: Comp a -> Comp a
perform = P.id

addNatPrim :: Natural -> Natural -> Natural
addNatPrim = (P.+)

subNatPrim :: Natural -> Natural -> Natural
subNatPrim left right = P.max 0 (left P.- right)

eqNatPrim :: Natural -> Natural -> P.Bool
eqNatPrim = (P.==)

eqStringPrim :: String -> String -> P.Bool
eqStringPrim = (P.==)

concatStringPrim :: String -> String -> String
concatStringPrim = T.append

stringLengthPrim :: String -> Natural
stringLengthPrim = P.fromIntegral . T.length

errorPrim :: String -> a
errorPrim message = P.error (T.unpack message)

printPrim :: String -> Comp Unit
printPrim message = Comp (TIO.putStrLn message)

getLinePrim :: Comp String
getLinePrim = Comp TIO.getLine

panicPrim :: String -> Comp a
panicPrim message =
  Comp (P.ioError (P.userError (T.unpack message)))
