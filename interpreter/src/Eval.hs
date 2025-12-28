{-# LANGUAGE OverloadedStrings #-}
module Eval
  ( runModuleMain
  ) where

import           AST
import           Control.Exception (catch, IOException)
import           Control.Monad (foldM, filterM)
import           Data.IORef
import           Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           System.FilePath ((</>), (<.>), takeExtension)
import qualified Data.Text.IO as TIO
import           System.IO.Unsafe (unsafePerformIO)
import           System.Process (readCreateProcessWithExitCode, shell)
import           Parser (parseModuleFile, parseMExprFile)
import           Validator (checkParens, validateModule)
import           ErrorMsg (findFuzzyMatches)
import           Utils (modNameToPath, qualifyName)
import           DictPass (transformModuleWithEnvs)
import qualified Type as Type
import qualified TypeChecker as TC

-- Global assertion counter (reset at the start of each test run)
{-# NOINLINE assertionCounter #-}
assertionCounter :: IORef Int
assertionCounter = unsafePerformIO (newIORef 0)

-- Runtime values

data Value
  = VNatural Integer
  | VString Text
  | VList [Value]
  | VUnit
  | VBoolean Bool
  | VPair Value Value
  | VTypeConst TypeConst
  | VTypeUniverse Int
  | VTypeForAll Text Expr Expr
  | VTypeThereExists Text Expr Expr
  | VTypeComp Expr
  | VTypeLift Expr Int Int
  | VTypeApp Value [Value]
  | VClosure Env [Text] FunctionBody
  | VPrim ([Value] -> IO Value)
  | VComp (IO Value)
  | VDict Text (Map.Map Text Value)  -- className, method implementations

instance Show Value where
  show (VNatural n)    = show n
  show (VString s) = show s
  show (VList xs)  = "[" ++ inner xs ++ "]"
    where inner = concat . map ((++ ",") . show)
  show VUnit       = "tt"
  show (VBoolean b)   = if b then "true" else "false"
  show (VPair a b) = "(" ++ show a ++ ", " ++ show b ++ ")"
  show (VTypeConst tc) = T.unpack (Type.typeConstName tc)
  show (VTypeUniverse n) = "Type" ++ show n
  show (VTypeForAll _ _ _) = "<for-all>"
  show (VTypeThereExists _ _ _) = "<there-exists>"
  show (VTypeComp _) = "<computation-type>"
  show (VTypeLift _ _ _) = "<lift-type>"
  show (VTypeApp _ _) = "<type-app>"
  show (VClosure _ _ _) = "<closure>"
  show (VPrim _)   = "<prim>"
  show (VComp _)   = "<computation>"
  show (VDict className _) = "<dict:" ++ T.unpack className ++ ">"

data Binding
  = BVal Value
  | BValueExpr Env Expr  -- Capture environment for lazy evaluation (module-local scope)

type Env = Map.Map Text Binding

primEnv :: Env
primEnv = Map.fromList
  [ (T.pack "add-nat-prim", BVal (VPrim primAdd))
  , (T.pack "add-nat", BVal (VPrim primAdd))
  , (T.pack "sub-nat-prim", BVal (VPrim primSub))
  , (T.pack "eq-nat-prim", BVal (VPrim primEqNat))
  , (T.pack "eq-string-prim", BVal (VPrim primEqString))
  , (T.pack "concat-string-prim", BVal (VPrim primConcatString))
  , (T.pack "length-string-prim", BVal (VPrim primLengthString))
  , (T.pack "split-on-prim", BVal (VPrim primSplitOn))
  , (T.pack "join-with-prim", BVal (VPrim primJoinWith))
  , (T.pack "trim-prim", BVal (VPrim primTrim))
  , (T.pack "filter-prim", BVal (VPrim primFilter))
  , (T.pack "fold-prim", BVal (VPrim primFold))
  , (T.pack "print-prim", BVal (VPrim primPrint))
  , (T.pack "read-file-prim", BVal (VPrim primReadFile))
  , (T.pack "write-file-prim", BVal (VPrim primWriteFile))
  , (T.pack "shell-prim", BVal (VPrim primShell))
  , (T.pack "print", BVal (VPrim primPrint))
  , (T.pack "assert-eq-nat-prim", BVal (VPrim primAssertEqNat))
  , (T.pack "assert-eq-string-prim", BVal (VPrim primAssertEqString))
  , (T.pack "tt-prim", BVal VUnit)
  , (T.pack "tt", BVal VUnit)
  , (T.pack "nil-prim", BVal (VPrim primNil))
  , (T.pack "cons-prim", BVal (VPrim primCons))
  , (T.pack "head-prim", BVal (VPrim primHead))
  , (T.pack "tail-prim", BVal (VPrim primTail))
  , (T.pack "length-list-prim", BVal (VPrim primLengthList))
  , (T.pack "append-prim", BVal (VPrim primAppend))
  , (T.pack "map-prim", BVal (VPrim primMap)) -- TODO: drop when map is written in locque
  , (T.pack "not-prim", BVal (VPrim primNot))
  , (T.pack "if-bool-prim", BVal (VPrim primIfBool))
  , (T.pack "match-list-prim", BVal (VPrim primMatchList))
  , (T.pack "match-bool-prim", BVal (VPrim primMatchBool))
  , (T.pack "match-pair-prim", BVal (VPrim primMatchPair))
  , (T.pack "drop-until-prim", BVal (VPrim primDropUntil))
  , (T.pack "pair-prim", BVal (VPrim primPair))
  , (T.pack "fst-prim", BVal (VPrim primFst))
  , (T.pack "snd-prim", BVal (VPrim primSnd))
  , (T.pack "pair-to-list-prim", BVal (VPrim primPairToList))
  , (T.pack "validate-prim", BVal (VPrim primValidate))
  -- Comparison operators
  , (T.pack "lt-nat-prim", BVal (VPrim (primCompareNat (<))))
  , (T.pack "le-nat-prim", BVal (VPrim (primCompareNat (<=))))
  , (T.pack "gt-nat-prim", BVal (VPrim (primCompareNat (>))))
  , (T.pack "ge-nat-prim", BVal (VPrim (primCompareNat (>=))))
  -- Arithmetic operators
  , (T.pack "mul-nat-prim", BVal (VPrim primMulNat))
  , (T.pack "div-nat-prim", BVal (VPrim primDivNat))
  , (T.pack "mod-nat-prim", BVal (VPrim primModNat))
  -- List operations
  , (T.pack "nth-prim", BVal (VPrim primNth))
  , (T.pack "take-prim", BVal (VPrim primTake))
  , (T.pack "drop-prim", BVal (VPrim primDrop))
  -- String operations
  , (T.pack "substring-prim", BVal (VPrim primSubstring))
  , (T.pack "char-at-prim", BVal (VPrim primCharAt))
  , (T.pack "contains-prim", BVal (VPrim primContains))
  , (T.pack "starts-with-prim", BVal (VPrim primStartsWith))
  , (T.pack "ends-with-prim", BVal (VPrim primEndsWith))
  , (T.pack "index-of-prim", BVal (VPrim primIndexOf))
  , (T.pack "reverse-string-prim", BVal (VPrim primReverseString))
  -- List operations (additional)
  , (T.pack "last-prim", BVal (VPrim primLast))
  , (T.pack "init-prim", BVal (VPrim primInit))
  -- Error handling
  , (T.pack "error-prim", BVal (VPrim primError))
  -- Display/conversion
  , (T.pack "nat-to-string-prim", BVal (VPrim primNatToString))
  -- Boolean assertions
  , (T.pack "assert-eq-bool-prim", BVal (VPrim primAssertEqBool))
  ]

primAdd :: [Value] -> IO Value
primAdd vals = do
  ints <- mapM expectNat vals
  pure $ VNatural (sum ints)

primSub :: [Value] -> IO Value
primSub [a,b] = do
  a' <- expectNat a
  b' <- expectNat b
  pure $ VNatural (max 0 (a' - b'))
primSub _ = error "sub-nat-prim expects 2 args"

primEqNat :: [Value] -> IO Value
primEqNat [a,b] = do
  a' <- expectNat a
  b' <- expectNat b
  pure $ VBoolean (a' == b')
primEqNat _ = error "eq-nat-prim expects 2 args"

primEqString :: [Value] -> IO Value
primEqString [a,b] = do
  a' <- expectString a
  b' <- expectString b
  pure $ VBoolean (a' == b')
primEqString _ = error "eq-string-prim expects 2 args"

primConcatString :: [Value] -> IO Value
primConcatString vals = do
  ss <- mapM expectString vals
  pure $ VString (mconcat ss)

primSplitOn :: [Value] -> IO Value
primSplitOn [VString delim, VString s] =
  pure $ VList (map VString (T.splitOn delim s))
primSplitOn _ = error "split-on-prim expects (delimiter, string)"

primJoinWith :: [Value] -> IO Value
primJoinWith [VString sep, VList strs] = do
  parts <- mapM expectString strs
  pure $ VString (T.intercalate sep parts)
primJoinWith _ = error "join-with-prim expects (separator, list-of-strings)"

primTrim :: [Value] -> IO Value
primTrim [VString s] = pure $ VString (T.strip s)
primTrim _ = error "trim-prim expects 1 string"

primFilter :: [Value] -> IO Value
primFilter [ty, fn, VList xs] = do
  expectTypeArg ty
  kept <- filterM (\v -> do
                      res <- apply fn [v]
                      pure (isTruthy res)) xs
  pure $ VList kept
primFilter _ = error "filter-prim expects (Type, predicate, list)"

primFold :: [Value] -> IO Value
primFold [tyA, tyB, fn, z, VList xs] = do
  expectTypeArg tyA
  expectTypeArg tyB
  foldM step z xs
  where
    step acc v = apply fn [acc, v]
primFold _ = error "fold-prim expects (Type, Type, fn, init, list)"

primNil :: [Value] -> IO Value
primNil [ty] = do
  expectTypeArg ty
  pure (VList [])
primNil _ = error "nil-prim expects (Type)"

primMatchList :: [Value] -> IO Value
primMatchList [tyA, tyB, VList [], c1, _c2] = do
  expectTypeArg tyA
  expectTypeArg tyB
  apply c1 [VUnit]
primMatchList [tyA, tyB, VList (h:t), _c1, c2] = do
  expectTypeArg tyA
  expectTypeArg tyB
  apply c2 [h, VList t]
primMatchList [_, _, v, _, _] = error $ "match-list-prim expects List, got " ++ show v
primMatchList _ = error "match-list-prim expects (Type, Type, list, empty-handler, cons-handler)"

primMatchBool :: [Value] -> IO Value
primMatchBool [tyB, VBoolean False, c1, _c2] = do
  expectTypeArg tyB
  apply c1 [VUnit]
primMatchBool [tyB, VBoolean True, _c1, c2] = do
  expectTypeArg tyB
  apply c2 [VUnit]
primMatchBool [_, v, _, _] = error $ "match-bool-prim expects Bool, got " ++ show v
primMatchBool _ = error "match-bool-prim expects (Type, Bool, false-handler, true-handler)"

primMatchPair :: [Value] -> IO Value
primMatchPair [tyA, tyB, tyC, VPair a b, _c1, c2] = do
  expectTypeArg tyA
  expectTypeArg tyB
  expectTypeArg tyC
  apply c2 [a, b]
primMatchPair [_, _, _, v, _, _] = error $ "match-pair-prim expects Pair, got " ++ show v
primMatchPair _ = error "match-pair-prim expects (Type, Type, Type, pair, unreachable-handler, pair-handler)"

primPair :: [Value] -> IO Value
primPair [tyA, tyB, a, b] = do
  expectTypeArg tyA
  expectTypeArg tyB
  pure $ VPair a b
primPair _ = error "pair-prim expects (Type, Type, a, b)"

primFst :: [Value] -> IO Value
primFst [tyA, tyB, VPair a _] = do
  expectTypeArg tyA
  expectTypeArg tyB
  pure a
primFst _ = error "fst-prim expects (Type, Type, pair)"

primSnd :: [Value] -> IO Value
primSnd [tyA, tyB, VPair _ b] = do
  expectTypeArg tyA
  expectTypeArg tyB
  pure b
primSnd _ = error "snd-prim expects (Type, Type, pair)"

primPairToList :: [Value] -> IO Value
primPairToList [tyA, VPair a b] = do
  expectTypeArg tyA
  pure $ VList [a, b]
primPairToList _ = error "pair-to-list-prim expects (Type, pair)"

primValidate :: [Value] -> IO Value
primValidate [VString s] = do
  -- Add trailing newline if missing (parser requires it)
  let s' = if T.isSuffixOf (T.pack "\n") s then s else s <> T.pack "\n"
  case checkParens "<inline>" s' of
    Left _ -> pure (VBoolean False)
    Right _ -> case parseModuleFile "<inline>" s' of
      Left _ -> pure (VBoolean False)
      Right m -> case validateModule m of
        Left _ -> pure (VBoolean False)
        Right _ -> pure (VBoolean True)
primValidate _ = error "validate-prim expects 1 string"

primLengthString :: [Value] -> IO Value
primLengthString [a] = do
  s <- expectString a
  pure $ VNatural (fromIntegral (T.length s))
primLengthString _ = error "length-string-prim expects 1 arg"

primPrint :: [Value] -> IO Value
primPrint [v] = do
  let action = case v of
        VString s -> TIO.putStrLn s
        other     -> TIO.putStrLn (T.pack (show other))
  pure (VComp (action >> pure VUnit))
primPrint _ = error "print-prim expects 1 arg"

primReadFile :: [Value] -> IO Value
primReadFile [VString path] =
  pure (VComp (VString <$> TIO.readFile (T.unpack path)))
primReadFile _ = error "read-file-prim expects 1 string arg"

primWriteFile :: [Value] -> IO Value
primWriteFile [VString path, VString contents] = do
  pure (VComp (TIO.writeFile (T.unpack path) contents >> pure VUnit))
primWriteFile _ = error "write-file-prim expects (path, contents)"

primShell :: [Value] -> IO Value
primShell [VString cmd] = do
  pure (VComp (do
    (_exitCode, stdout, stderr) <- readCreateProcessWithExitCode (shell (T.unpack cmd)) ""
    pure $ VString (T.pack (stdout ++ stderr))))
primShell _ = error "shell-prim expects 1 string arg (command)"

primAssertEqNat :: [Value] -> IO Value
primAssertEqNat [a,b] = do
  a' <- expectNat a
  b' <- expectNat b
  if a' == b'
    then pure (VComp (modifyIORef' assertionCounter (+1) >> pure VUnit))
    else pure (VComp (error $ "assert-eq-nat failed: " ++ show a' ++ " /= " ++ show b'))
primAssertEqNat _ = error "assert-eq-nat expects 2 args"

primAssertEqString :: [Value] -> IO Value
primAssertEqString [a,b] = do
  a' <- expectString a
  b' <- expectString b
  if a' == b'
    then pure (VComp (modifyIORef' assertionCounter (+1) >> pure VUnit))
    else pure (VComp (error $ "assert-eq-string failed: " ++ show a' ++ " /= " ++ show b'))
primAssertEqString _ = error "assert-eq-string expects 2 args"

primCons :: [Value] -> IO Value
primCons [ty, h, VList t] = do
  expectTypeArg ty
  pure $ VList (h:t)
primCons _ = error "cons-prim expects (Type, head, list)"

primHead :: [Value] -> IO Value
primHead [ty, VList (h:_)] = do
  expectTypeArg ty
  pure h
primHead [ty, VList []] = do
  expectTypeArg ty
  pure (VList [])
primHead _ = error "head-prim expects (Type, list)"

primTail :: [Value] -> IO Value
primTail [ty, VList (_:t)] = do
  expectTypeArg ty
  pure (VList t)
primTail [ty, VList []] = do
  expectTypeArg ty
  pure (VList [])
primTail _ = error "tail-prim expects (Type, list)"

primLengthList :: [Value] -> IO Value
primLengthList [ty, VList xs] = do
  expectTypeArg ty
  pure $ VNatural (fromIntegral (length xs))
primLengthList _ = error "length-list-prim expects (Type, list)"

primAppend :: [Value] -> IO Value
primAppend [ty, VList xs, VList ys] = do
  expectTypeArg ty
  pure $ VList (xs ++ ys)
primAppend _ = error "append-prim expects (Type, list, list)"

primMap :: [Value] -> IO Value
primMap [tyA, tyB, fn, VList xs] = do
  expectTypeArg tyA
  expectTypeArg tyB
  VList <$> mapM (\v -> apply fn [v]) xs
primMap _ = error "map-prim expects (Type, Type, fn, list)"

primNot :: [Value] -> IO Value
primNot [VBoolean b] = pure $ VBoolean (not b)
primNot [v] = pure $ VBoolean (not (isTruthy v))
primNot _ = error "not-prim expects 1 arg"

primIfBool :: [Value] -> IO Value
primIfBool [ty, cond, t, f] = do
  expectTypeArg ty
  pure $ if isTruthy cond then t else f
primIfBool _ = error "if-bool-prim expects (Type, cond, then, else)"

primCompareNat :: (Integer -> Integer -> Bool) -> [Value] -> IO Value
primCompareNat op [a, b] = do
  a' <- expectNat a
  b' <- expectNat b
  pure $ VBoolean (a' `op` b')
primCompareNat _ _ = error "comparison expects 2 args"

-- Arithmetic operators

primMulNat :: [Value] -> IO Value
primMulNat vals = do
  ints <- mapM expectNat vals
  pure $ VNatural (product ints)

primDivNat :: [Value] -> IO Value
primDivNat [a, b] = do
  a' <- expectNat a
  b' <- expectNat b
  if b' == 0
    then error "div-nat-prim: division by zero"
    else pure $ VNatural (a' `div` b')
primDivNat _ = error "div-nat-prim expects 2 args"

primModNat :: [Value] -> IO Value
primModNat [a, b] = do
  a' <- expectNat a
  b' <- expectNat b
  if b' == 0
    then error "mod-nat-prim: modulo by zero"
    else pure $ VNatural (a' `mod` b')
primModNat _ = error "mod-nat-prim expects 2 args"

-- List operations

primNth :: [Value] -> IO Value
primNth [ty, VNatural idx, VList xs] = do
  expectTypeArg ty
  let i = fromInteger idx
  if i < 0 || i >= length xs
    then error $ "nth-prim: index " ++ show idx ++ " out of bounds for list of length " ++ show (length xs)
    else pure $ xs !! i
primNth _ = error "nth-prim expects (Type, index, list)"

primTake :: [Value] -> IO Value
primTake [ty, VNatural n, VList xs] = do
  expectTypeArg ty
  let count = fromInteger n
  pure $ VList (take count xs)
primTake _ = error "take-prim expects (Type, count, list)"

primDrop :: [Value] -> IO Value
primDrop [ty, VNatural n, VList xs] = do
  expectTypeArg ty
  let count = fromInteger n
  pure $ VList (drop count xs)
primDrop _ = error "drop-prim expects (Type, count, list)"

-- String operations

primSubstring :: [Value] -> IO Value
primSubstring [VNatural start, VNatural len, VString s] = do
  let startIdx = fromInteger start
      lenVal = fromInteger len
      result = T.take lenVal (T.drop startIdx s)
  pure $ VString result
primSubstring _ = error "substring-prim expects (start, length, string)"

primCharAt :: [Value] -> IO Value
primCharAt [VNatural idx, VString s] = do
  let i = fromInteger idx
  if i < 0 || i >= T.length s
    then error $ "char-at-prim: index " ++ show idx ++ " out of bounds for string of length " ++ show (T.length s)
    else pure $ VString (T.singleton (T.index s i))
primCharAt _ = error "char-at-prim expects (index, string)"

primContains :: [Value] -> IO Value
primContains [VString needle, VString haystack] =
  pure $ VBoolean (needle `T.isInfixOf` haystack)
primContains _ = error "contains-prim expects (needle, haystack)"

primStartsWith :: [Value] -> IO Value
primStartsWith [VString prefix, VString s] =
  pure $ VBoolean (prefix `T.isPrefixOf` s)
primStartsWith _ = error "starts-with-prim expects (prefix, string)"

primEndsWith :: [Value] -> IO Value
primEndsWith [VString suffix, VString s] =
  pure $ VBoolean (suffix `T.isSuffixOf` s)
primEndsWith _ = error "ends-with-prim expects (suffix, string)"

primIndexOf :: [Value] -> IO Value
primIndexOf [VString needle, VString haystack] =
  case T.breakOn needle haystack of
    (before, after) | T.null after -> pure $ VNatural (fromIntegral (T.length haystack))
                    | otherwise -> pure $ VNatural (fromIntegral (T.length before))
primIndexOf _ = error "index-of-prim expects (needle, haystack)"

primReverseString :: [Value] -> IO Value
primReverseString [VString s] = pure $ VString (T.reverse s)
primReverseString _ = error "reverse-string-prim expects 1 string arg"

primLast :: [Value] -> IO Value
primLast [ty, VList xs] = do
  expectTypeArg ty
  if null xs
    then pure (VList [])
    else pure (last xs)
primLast _ = error "last-prim expects (Type, list)"

primInit :: [Value] -> IO Value
primInit [ty, VList xs] = do
  expectTypeArg ty
  if null xs
    then pure (VList [])
    else pure (VList (init xs))
primInit _ = error "init-prim expects (Type, list)"

primError :: [Value] -> IO Value
primError [ty, VString msg] = do
  expectTypeArg ty
  error (T.unpack msg)
primError _ = error "error-prim expects (Type, string)"

primNatToString :: [Value] -> IO Value
primNatToString [VNatural n] = pure $ VString (T.pack (show n))
primNatToString _ = error "nat-to-string-prim expects 1 Natural arg"

primAssertEqBool :: [Value] -> IO Value
primAssertEqBool [VBoolean a, VBoolean b] = do
  if a == b
    then pure (VComp (modifyIORef' assertionCounter (+1) >> pure VUnit))
    else pure (VComp (error $ "assert-eq-bool failed: " ++ show a ++ " /= " ++ show b))
primAssertEqBool _ = error "assert-eq-bool-prim expects 2 Boolean args"

isTypeValue :: Value -> Bool
isTypeValue v = case v of
  VTypeConst _ -> True
  VTypeUniverse _ -> True
  VTypeForAll _ _ _ -> True
  VTypeThereExists _ _ _ -> True
  VTypeComp _ -> True
  VTypeLift _ _ _ -> True
  VTypeApp _ _ -> True
  _ -> False

expectTypeArg :: Value -> IO ()
expectTypeArg v =
  if isTypeValue v
    then pure ()
    else error $ "expected Type argument, got " ++ show v

expectNat :: Value -> IO Integer
expectNat (VNatural n) = pure n
expectNat v        = error $ "expected Nat, got " ++ show v

expectString :: Value -> IO Text
expectString (VString s) = pure s
expectString v           = error $ "expected String, got " ++ show v

primDropUntil :: [Value] -> IO Value
primDropUntil [target, VList xs] = do
  tgt <- expectString target
  pure $ VList (dropWhile (\v -> case v of
                            VString s -> s /= tgt
                            _ -> True) xs)
primDropUntil _ = pure (VList [])
isTruthy :: Value -> Bool
isTruthy VUnit = True
isTruthy (VBoolean b) = b
isTruthy (VString s) = not (T.null s)
isTruthy (VNatural n) = n /= 0
isTruthy (VList xs) = not (null xs)
isTruthy _ = False

-- Build environment from module definitions atop an existing env (imports/prims)
-- Uses lazy evaluation to tie-the-knot: all definitions capture the final environment
bindModule :: Module -> Env -> Env
bindModule (Module _modName _ _ defs) base =
  let env = foldl addDef base defs
      addDef e (Definition _ name body) =
        Map.insert name (BValueExpr env body) e
  in env

runModuleMain :: FilePath -> Module -> IO Int
runModuleMain projectRoot m@(Module _modName _ _ _) = do
  -- Reset assertion counter
  writeIORef assertionCounter 0

  envImports <- loadImports projectRoot m
  let env = bindModule m envImports
  case Map.lookup (T.pack "main") env of
    Just b -> do
      val <- resolveBinding env b
      case val of
        VComp action -> do _ <- action; readIORef assertionCounter
        VPrim f -> do _ <- f []; readIORef assertionCounter
        _ -> error "main must evaluate to a computation value"
    _ -> error "No main computation found"

evalExpr :: Env -> Expr -> IO Value
evalExpr env expr = case expr of
  EVar t -> case Map.lookup t env of
    Just b  -> resolveBinding env b
    Nothing ->
      let candidates = Map.keys env
          matches = findFuzzyMatches t candidates
          suggestion = case matches of
                        [] -> T.empty
                        (x:_) -> " (did you mean '" <> x <> "'?)"
      in error $ T.unpack ("Unknown variable: " <> t <> suggestion)
  ELit lit -> pure $ case lit of
    LNatural n    -> VNatural n
    LString s -> VString s
    LBoolean b   -> VBoolean b
    LUnit     -> VUnit
  ETypeConst tc -> pure (VTypeConst tc)
  ETypeUniverse n -> pure (VTypeUniverse n)
  EForAll v dom cod -> pure (VTypeForAll v dom cod)
  EThereExists v dom cod -> pure (VTypeThereExists v dom cod)
  ECompType t -> pure (VTypeComp t)
  ELift ty fromLevel toLevel -> pure (VTypeLift ty fromLevel toLevel)
  EUp _ _ _ body -> evalExpr env body
  EDown _ _ _ body -> evalExpr env body
  EPack _ _ _ witness body -> do
    witness' <- evalExpr env witness
    body' <- evalExpr env body
    pure (VPair witness' body')
  EUnpack packed x y body -> do
    packedVal <- evalExpr env packed
    case packedVal of
      VPair a b ->
        let env' = Map.insert y (BVal b) (Map.insert x (BVal a) env)
        in evalExpr env' body
      _ -> error "unpack expects a packed value"
  EFunction params _constraints _retTy body ->
    pure $ VClosure env (map paramName params) body
  ELet name val body -> do
    val' <- evalExpr env val
    let env' = Map.insert name (BVal val') env
    evalExpr env' body
  ECompute comp -> pure $ VComp (runComp env comp)
  EMatch scrut _scrutTy scrutName _retTy cases -> evalMatch env scrut scrutName cases
  EAnnot annExpr _ty -> evalExpr env annExpr  -- Type annotation ignored in evaluation
  ETyped typedExpr _ty -> evalExpr env typedExpr  -- Inferred type ignored in evaluation
  EApp f args -> do
    vf <- evalExpr env f
    vs <- mapM (evalExpr env) args
    apply vf vs
  EDict className impls -> do
    -- Evaluate each method implementation
    implVals <- mapM (\(name, implExpr) -> do
      val <- evalExpr env implExpr
      pure (name, val)) impls
    pure (VDict className (Map.fromList implVals))
  EDictAccess dictExpr methodName -> do
    dictVal <- evalExpr env dictExpr
    case dictVal of
      VDict _ methods ->
        case Map.lookup methodName methods of
          Just val -> pure val
          Nothing -> error $ "Method not found in dictionary: " ++ T.unpack methodName
      _ -> error "Expected dictionary value"
  ETypeClass _ _ -> error "Typeclass nodes are not evaluable"
  EInstance _ _ _ -> error "Instance nodes are not evaluable"

resolveBinding :: Env -> Binding -> IO Value
resolveBinding _outerEnv b = case b of
  BVal v               -> pure v
  BValueExpr env e     -> evalExpr env e  -- Use captured environment

apply :: Value -> [Value] -> IO Value
apply v [] = pure v
apply (VPrim f) args = f args
apply (VTypeConst tc) args = pure (VTypeApp (VTypeConst tc) args)
apply (VTypeLift ty fromLevel toLevel) args =
  pure (VTypeApp (VTypeLift ty fromLevel toLevel) args)
apply (VTypeApp f existing) args = pure (VTypeApp f (existing ++ args))
apply (VClosure cenv params body) args = do
  let (used, remaining) = splitAt (length params) args
  if length used < length params
    then do
      let env' = bindParams cenv (zip params used)
          leftoverParams = drop (length used) params
      pure (VClosure env' leftoverParams body)
    else do
      let env' = bindParams cenv (zip params used)
      val <- evalFunctionBody env' body
      if null remaining then pure val else apply val remaining
apply v _            = error $ "Cannot apply non-function value: " ++ show v

bindParams :: Env -> [(Text, Value)] -> Env
bindParams = foldl (\e (name, val) -> Map.insert name (BVal val) e)

evalFunctionBody :: Env -> FunctionBody -> IO Value
evalFunctionBody env body = case body of
  FunctionValue expr -> evalExpr env expr
  FunctionCompute comp -> pure (VComp (runComp env comp))

evalMatch :: Env -> Expr -> Text -> [MatchCase] -> IO Value
evalMatch env scrut scrutName cases = do
  scrutVal <- evalExpr env scrut
  let envScrut = Map.insert scrutName (BVal scrutVal) env
  case scrutVal of
    VList [] ->
      case [body | MatchEmpty body <- cases] of
        (body:_) -> evalExpr envScrut body
        [] -> error "match: missing empty-case"
    VList (h:t) ->
      case [(hName, tName, body) | MatchCons hName _ tName _ body <- cases] of
        ((hName, tName, body):_) -> do
          let env' = Map.insert tName (BVal (VList t)) (Map.insert hName (BVal h) envScrut)
          evalExpr env' body
        [] -> error "match: missing cons-case"
    VBoolean False ->
      case [body | MatchFalse body <- cases] of
        (body:_) -> evalExpr envScrut body
        [] -> error "match: missing false-case"
    VBoolean True ->
      case [body | MatchTrue body <- cases] of
        (body:_) -> evalExpr envScrut body
        [] -> error "match: missing true-case"
    VPair a b ->
      case [(aName, bName, body) | MatchPair aName _ bName _ body <- cases] of
        ((aName, bName, body):_) -> do
          let env' = Map.insert bName (BVal b) (Map.insert aName (BVal a) envScrut)
          evalExpr env' body
        [] -> error "match: missing pair-case"
    _ -> error "match: unsupported scrutinee value"

runComp :: Env -> Comp -> IO Value
runComp env comp = case comp of
  CReturn e    -> evalExpr env e
  CBind v c1 c2 -> do
    val <- runComp env c1
    let env' = Map.insert v (BVal val) env
    runComp env' c2
  CPerform e   -> do
    val <- evalExpr env e
    case val of
      VComp action -> action
      _ -> error "perform expects a computation value"
  CSeq c1 c2 -> do
    _ <- runComp env c1
    runComp env c2

-- Import loading

loadImports :: FilePath -> Module -> IO Env
loadImports projectRoot (Module _ imports _ _) = do
  envs <- mapM (loadImport projectRoot) imports
  pure $ Map.unions (primEnv : envs)

loadImport :: FilePath -> Import -> IO Env
loadImport projectRoot (Import modName alias) = do
  let modPath = modNameToPath modName
      -- If module path starts with "test/", use projectRoot directly
      -- Otherwise, use projectRoot </> "lib"
      basePath = if "test/" `isPrefixOf` modPath
                 then projectRoot </> modPath
                 else projectRoot </> "lib" </> modPath
      lqPath = basePath <.> "lq"
      lqsPath = basePath <.> "lqs"

  -- Try .lq first, fall back to .lqs if not found
  (path, contents) <- tryLoadFile lqPath `catch` \(_ :: IOException) -> tryLoadFile lqsPath

  -- Parse based on file extension
  parsed <- case takeExtension path of
    ".lq"  -> case parseMExprFile path contents of
      Left err -> error err
      Right m -> pure m
    ".lqs" -> case parseModuleFile path contents of
      Left err -> error err
      Right m -> pure m
    _      -> error $ "Unknown file extension: " ++ path

  -- Type check the imported module (native typeclass support on original module)
  tcResult <- TC.typeCheckModuleWithImports projectRoot contents parsed
  annotated <- case tcResult of
    Left tcErr -> error $ "Type error in import " ++ T.unpack modName ++ ": " ++ show tcErr
    Right env -> do
      transformed <- transformModuleWithEnvs projectRoot parsed
      case TC.annotateModule env transformed of
        Left annErr -> error $ "Annotation error in import " ++ T.unpack modName ++ ": " ++ show annErr
        Right m -> pure m

  let Module _modName _ opens defs = annotated
  envImports <- loadImports projectRoot annotated
  -- Process opens to bring in unqualified names from open statements
  let envWithOpens = processOpens opens envImports

  -- Bind the module to get all definitions
  let envSelf = bindModule annotated envWithOpens
      -- Add qualified names for each definition using the alias
      defNames = map defName defs
      envFinal = foldl (insertQualified alias envSelf) envWithOpens defNames

  pure envFinal
  where
    tryLoadFile p = do
      c <- TIO.readFile p
      pure (p, c)

    -- Insert qualified name for a definition (if it exists in envSelf)
    insertQualified :: Text -> Env -> Env -> Text -> Env
    insertQualified aliasPrefix envSelf env name =
      case Map.lookup name envSelf of
        Just binding ->
          let qualifiedName = qualifyName aliasPrefix name
          in Map.insert qualifiedName binding env
        Nothing -> env  -- Definition not in environment

-- Process open statements to bring unqualified names into scope
processOpens :: [Open] -> Env -> Env
processOpens opens env = foldl processOneOpen env opens
  where
    processOneOpen :: Env -> Open -> Env
    processOneOpen e (Open modAlias nameList) =
      foldl (openOneName modAlias) e nameList

    openOneName :: Text -> Env -> Text -> Env
    openOneName modAlias e name =
      let qualifiedName = qualifyName modAlias name
      in case Map.lookup qualifiedName e of
           Just binding -> Map.insert name binding e
           Nothing -> e
