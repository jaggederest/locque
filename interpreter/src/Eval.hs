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

-- Global assertion counter (reset at the start of each test run)
{-# NOINLINE assertionCounter #-}
assertionCounter :: IORef Int
assertionCounter = unsafePerformIO (newIORef 0)

-- Runtime values

data Value
  = VNat Integer
  | VString Text
  | VList [Value]
  | VUnit
  | VBool Bool
  | VPair Value Value
  | VClosure Env Text Expr
  | VPrim ([Value] -> IO Value)
  | VDict Text (Map.Map Text Value)  -- className, method implementations

instance Show Value where
  show (VNat n)    = show n
  show (VString s) = show s
  show (VList xs)  = "[" ++ inner xs ++ "]"
    where inner = concat . map ((++ ",") . show)
  show VUnit       = "tt"
  show (VBool b)   = if b then "true" else "false"
  show (VPair a b) = "(" ++ show a ++ ", " ++ show b ++ ")"
  show (VClosure _ _ _) = "<closure>"
  show (VPrim _)   = "<prim>"
  show (VDict className _) = "<dict:" ++ T.unpack className ++ ">"

data Binding
  = BVal Value
  | BValueExpr Env Expr  -- Capture environment for lazy evaluation (module-local scope)
  | BCompExpr Env Comp   -- Capture environment for lazy evaluation (module-local scope)

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
  , (T.pack "nil-prim", BVal (VList []))
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
  , (T.pack "match-prim", BVal (VPrim primMatch))
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
  ]

primAdd :: [Value] -> IO Value
primAdd vals = do
  ints <- mapM expectNat vals
  pure $ VNat (sum ints)

primSub :: [Value] -> IO Value
primSub [a,b] = do
  a' <- expectNat a
  b' <- expectNat b
  pure $ VNat (max 0 (a' - b'))
primSub _ = error "sub-nat-prim expects 2 args"

primEqNat :: [Value] -> IO Value
primEqNat [a,b] = do
  a' <- expectNat a
  b' <- expectNat b
  pure $ VBool (a' == b')
primEqNat _ = error "eq-nat-prim expects 2 args"

primEqString :: [Value] -> IO Value
primEqString [a,b] = do
  a' <- expectString a
  b' <- expectString b
  pure $ VBool (a' == b')
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
primFilter [VPrim f, VList xs] = do
  kept <- filterM (\v -> isTruthy <$> f [v]) xs
  pure $ VList kept
primFilter [VClosure env param body, VList xs] = do
  kept <- filterM (\v -> do
                      let env' = Map.insert param (BVal v) env
                      res <- evalExpr env' body
                      pure (isTruthy res)) xs
  pure $ VList kept
primFilter _ = error "filter-prim expects (predicate, list)"

primFold :: [Value] -> IO Value
primFold [fn, z, VList xs] = foldM step z xs
  where
    step acc v = apply fn [acc, v]
primFold _ = error "fold-prim expects (fn, init, list)"

primMatch :: [Value] -> IO Value
primMatch [v, c1, c2] = case v of
  VList []      -> apply c1 [VUnit]
  VList (h:t)   -> apply c2 [h, VList t]
  VBool False   -> apply c1 [VUnit]
  VBool True    -> apply c2 [VUnit]
  VPair a b     -> apply c2 [a, b]
  _             -> error "match-prim unsupported value"
primMatch _ = error "match-prim expects (value, case1, case2)"

primMatchList :: [Value] -> IO Value
primMatchList [VList [], c1, _c2] = apply c1 [VUnit]
primMatchList [VList (h:t), _c1, c2] = apply c2 [h, VList t]
primMatchList [v, _, _] = error $ "match-list-prim expects List, got " ++ show v
primMatchList _ = error "match-list-prim expects (List, empty-handler, cons-handler)"

primMatchBool :: [Value] -> IO Value
primMatchBool [VBool False, c1, _c2] = apply c1 [VUnit]
primMatchBool [VBool True, _c1, c2] = apply c2 [VUnit]
primMatchBool [v, _, _] = error $ "match-bool-prim expects Bool, got " ++ show v
primMatchBool _ = error "match-bool-prim expects (Bool, false-handler, true-handler)"

primMatchPair :: [Value] -> IO Value
primMatchPair [VPair a b, _c1, c2] = apply c2 [a, b]
primMatchPair [v, _, _] = error $ "match-pair-prim expects Pair, got " ++ show v
primMatchPair _ = error "match-pair-prim expects (Pair, unreachable-handler, pair-handler)"

primPair :: [Value] -> IO Value
primPair [a,b] = pure $ VPair a b
primPair _ = error "pair-prim expects 2 args"

primFst :: [Value] -> IO Value
primFst [VPair a _] = pure a
primFst _ = error "fst-prim expects pair"

primSnd :: [Value] -> IO Value
primSnd [VPair _ b] = pure b
primSnd _ = error "snd-prim expects pair"

primPairToList :: [Value] -> IO Value
primPairToList [VPair a b] = pure $ VList [a,b]
primPairToList _ = error "pair-to-list-prim expects pair"

primValidate :: [Value] -> IO Value
primValidate [VString s] = do
  -- Add trailing newline if missing (parser requires it)
  let s' = if T.isSuffixOf (T.pack "\n") s then s else s <> T.pack "\n"
  case checkParens "<inline>" s' of
    Left _ -> pure (VBool False)
    Right _ -> case parseModuleFile "<inline>" s' of
      Left _ -> pure (VBool False)
      Right m -> case validateModule m of
        Left _ -> pure (VBool False)
        Right _ -> pure (VBool True)
primValidate _ = error "validate-prim expects 1 string"

primLengthString :: [Value] -> IO Value
primLengthString [a] = do
  s <- expectString a
  pure $ VNat (fromIntegral (T.length s))
primLengthString _ = error "length-string-prim expects 1 arg"

primPrint :: [Value] -> IO Value
primPrint [v] = do
  case v of
    VString s -> putStrLn (T.unpack s)
    other     -> putStrLn (show other)
  pure VUnit
primPrint _ = error "print-prim expects 1 arg"

primReadFile :: [Value] -> IO Value
primReadFile [VString path] = VString <$> TIO.readFile (T.unpack path)
primReadFile _ = error "read-file-prim expects 1 string arg"

primWriteFile :: [Value] -> IO Value
primWriteFile [VString path, VString contents] = do
  TIO.writeFile (T.unpack path) contents
  pure VUnit
primWriteFile _ = error "write-file-prim expects (path, contents)"

primShell :: [Value] -> IO Value
primShell [VString cmd] = do
  (_exitCode, stdout, stderr) <- readCreateProcessWithExitCode (shell (T.unpack cmd)) ""
  pure $ VString (T.pack (stdout ++ stderr))
primShell _ = error "shell-prim expects 1 string arg (command)"

primAssertEqNat :: [Value] -> IO Value
primAssertEqNat [a,b] = do
  a' <- expectNat a
  b' <- expectNat b
  if a' == b'
    then do
      modifyIORef' assertionCounter (+1)
      pure VUnit
    else error $ "assert-eq-nat failed: " ++ show a' ++ " /= " ++ show b'
primAssertEqNat _ = error "assert-eq-nat expects 2 args"

primAssertEqString :: [Value] -> IO Value
primAssertEqString [a,b] = do
  a' <- expectString a
  b' <- expectString b
  if a' == b'
    then do
      modifyIORef' assertionCounter (+1)
      pure VUnit
    else error $ "assert-eq-string failed: " ++ show a' ++ " /= " ++ show b'
primAssertEqString _ = error "assert-eq-string expects 2 args"

primCons :: [Value] -> IO Value
primCons [h, VList t] = pure $ VList (h:t)
primCons _ = error "cons-prim expects head and list"

primHead :: [Value] -> IO Value
primHead [VList (h:_)] = pure h
primHead [VList []]    = pure (VList [])
primHead _ = error "head-prim expects non-empty list"

primTail :: [Value] -> IO Value
primTail [VList (_:t)] = pure (VList t)
primTail [VList []]    = pure (VList [])
primTail _ = error "tail-prim expects non-empty list"

primLengthList :: [Value] -> IO Value
primLengthList [VList xs] = pure $ VNat (fromIntegral (length xs))
primLengthList _ = error "length-list-prim expects 1 list arg"

primAppend :: [Value] -> IO Value
primAppend [VList xs, VList ys] = pure $ VList (xs ++ ys)
primAppend _ = error "append-prim expects (list, list)"

primMap :: [Value] -> IO Value
primMap [fn, VList xs] = VList <$> mapM (\v -> apply fn [v]) xs
primMap _ = error "map-prim expects (fn, list)"

primNot :: [Value] -> IO Value
primNot [VBool b] = pure $ VBool (not b)
primNot [v] = pure $ VBool (not (isTruthy v))
primNot _ = error "not-prim expects 1 arg"

primIfBool :: [Value] -> IO Value
primIfBool [cond, t, f] = pure $ if isTruthy cond then t else f
primIfBool _ = error "if-bool-prim expects (cond, then, else)"

primCompareNat :: (Integer -> Integer -> Bool) -> [Value] -> IO Value
primCompareNat op [a, b] = do
  a' <- expectNat a
  b' <- expectNat b
  pure $ VBool (a' `op` b')
primCompareNat _ _ = error "comparison expects 2 args"

-- Arithmetic operators

primMulNat :: [Value] -> IO Value
primMulNat vals = do
  ints <- mapM expectNat vals
  pure $ VNat (product ints)

primDivNat :: [Value] -> IO Value
primDivNat [a, b] = do
  a' <- expectNat a
  b' <- expectNat b
  if b' == 0
    then error "div-nat-prim: division by zero"
    else pure $ VNat (a' `div` b')
primDivNat _ = error "div-nat-prim expects 2 args"

primModNat :: [Value] -> IO Value
primModNat [a, b] = do
  a' <- expectNat a
  b' <- expectNat b
  if b' == 0
    then error "mod-nat-prim: modulo by zero"
    else pure $ VNat (a' `mod` b')
primModNat _ = error "mod-nat-prim expects 2 args"

-- List operations

primNth :: [Value] -> IO Value
primNth [VNat idx, VList xs] = do
  let i = fromInteger idx
  if i < 0 || i >= length xs
    then error $ "nth-prim: index " ++ show idx ++ " out of bounds for list of length " ++ show (length xs)
    else pure $ xs !! i
primNth _ = error "nth-prim expects (index, list)"

primTake :: [Value] -> IO Value
primTake [VNat n, VList xs] = do
  let count = fromInteger n
  pure $ VList (take count xs)
primTake _ = error "take-prim expects (count, list)"

primDrop :: [Value] -> IO Value
primDrop [VNat n, VList xs] = do
  let count = fromInteger n
  pure $ VList (drop count xs)
primDrop _ = error "drop-prim expects (count, list)"

-- String operations

primSubstring :: [Value] -> IO Value
primSubstring [VNat start, VNat len, VString s] = do
  let startIdx = fromInteger start
      lenVal = fromInteger len
      result = T.take lenVal (T.drop startIdx s)
  pure $ VString result
primSubstring _ = error "substring-prim expects (start, length, string)"

primCharAt :: [Value] -> IO Value
primCharAt [VNat idx, VString s] = do
  let i = fromInteger idx
  if i < 0 || i >= T.length s
    then error $ "char-at-prim: index " ++ show idx ++ " out of bounds for string of length " ++ show (T.length s)
    else pure $ VString (T.singleton (T.index s i))
primCharAt _ = error "char-at-prim expects (index, string)"

primContains :: [Value] -> IO Value
primContains [VString needle, VString haystack] =
  pure $ VBool (needle `T.isInfixOf` haystack)
primContains _ = error "contains-prim expects (needle, haystack)"

primStartsWith :: [Value] -> IO Value
primStartsWith [VString prefix, VString s] =
  pure $ VBool (prefix `T.isPrefixOf` s)
primStartsWith _ = error "starts-with-prim expects (prefix, string)"

primEndsWith :: [Value] -> IO Value
primEndsWith [VString suffix, VString s] =
  pure $ VBool (suffix `T.isSuffixOf` s)
primEndsWith _ = error "ends-with-prim expects (suffix, string)"

primIndexOf :: [Value] -> IO Value
primIndexOf [VString needle, VString haystack] =
  case T.breakOn needle haystack of
    (before, after) | T.null after -> pure $ VNat (fromIntegral (T.length haystack))
                    | otherwise -> pure $ VNat (fromIntegral (T.length before))
primIndexOf _ = error "index-of-prim expects (needle, haystack)"

primReverseString :: [Value] -> IO Value
primReverseString [VString s] = pure $ VString (T.reverse s)
primReverseString _ = error "reverse-string-prim expects 1 string arg"

primLast :: [Value] -> IO Value
primLast [VList xs] =
  if null xs
    then pure (VList [])
    else pure (last xs)
primLast _ = error "last-prim expects 1 list arg"

primInit :: [Value] -> IO Value
primInit [VList xs] =
  if null xs
    then pure (VList [])
    else pure (VList (init xs))
primInit _ = error "init-prim expects 1 list arg"

primError :: [Value] -> IO Value
primError [VString msg] = error (T.unpack msg)
primError _ = error "error-prim expects 1 string arg"

expectNat :: Value -> IO Integer
expectNat (VNat n) = pure n
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
isTruthy (VBool b) = b
isTruthy (VString s) = not (T.null s)
isTruthy (VNat n) = n /= 0
isTruthy (VList xs) = not (null xs)
isTruthy _ = False

-- Build environment from module definitions atop an existing env (imports/prims)
-- Uses lazy evaluation to tie-the-knot: all definitions capture the final environment
bindModule :: Module -> Env -> Env
bindModule (Module _modName _ _ defs) base =
  let env = foldl addDef base defs
      addDef e (Definition _ name kind _mType body) =
        case (kind, body) of
          (ValueDef, ValueBody expr)        -> Map.insert name (BValueExpr env expr) e
          (ComputationDef, ComputationBody comp) -> Map.insert name (BCompExpr env comp) e
          -- Type families, type classes, and instances are handled at type-check time
          _                                 -> e
  in env

runModuleMain :: FilePath -> Module -> IO Int
runModuleMain projectRoot m@(Module _modName _ _ _) = do
  -- Reset assertion counter
  writeIORef assertionCounter 0

  envImports <- loadImports projectRoot m
  let env = bindModule m envImports
  case Map.lookup (T.pack "main") env of
    Just (BCompExpr _localEnv c)    -> do _ <- runComp env c; readIORef assertionCounter
    Just (BVal (VPrim f)) -> do _ <- f []; readIORef assertionCounter
    Just (BValueExpr _localEnv e)   -> do _ <- evalExpr env e; readIORef assertionCounter
    _                     -> error "No main computation found"

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
    LNat n    -> VNat n
    LString s -> VString s
    LBool b   -> VBool b
  ELam v _mType body -> pure $ VClosure env v body  -- Type annotation ignored in evaluation
  ELamMulti params _mType body -> pure $ makeNestedClosures env params body
  EAnnot expr _ty -> evalExpr env expr  -- Type annotation ignored in evaluation
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

-- | Create nested closures from multi-parameter lambda
makeNestedClosures :: Env -> [Text] -> Expr -> Value
makeNestedClosures env [] body = error "makeNestedClosures called with empty parameter list"
makeNestedClosures env [p] body = VClosure env p body
makeNestedClosures env (p:ps) body = VClosure env p (ELamMulti ps Nothing body)

resolveBinding :: Env -> Binding -> IO Value
resolveBinding _outerEnv b = case b of
  BVal v               -> pure v
  BValueExpr env e     -> evalExpr env e  -- Use captured environment
  BCompExpr env c      -> runComp env c   -- Use captured environment

apply :: Value -> [Value] -> IO Value
apply v [] = pure v
apply (VPrim f) args = f args
apply (VClosure cenv param body) (arg:rest) = do
  let env' = Map.insert param (BVal arg) cenv
  val <- evalExpr env' body
  if null rest then pure val else apply val rest
apply v _            = error $ "Cannot apply non-function value: " ++ show v

runComp :: Env -> Comp -> IO Value
runComp env comp = case comp of
  CReturn e    -> evalExpr env e
  CBind v c1 c2 -> do
    val <- runComp env c1
    let env' = Map.insert v (BVal val) env
    runComp env' c2
  CPerform e   -> evalExpr env e
  CVar t -> case Map.lookup t env of
    Just b -> resolveBinding env b
    Nothing ->
      let candidates = Map.keys env
          matches = findFuzzyMatches t candidates
          suggestion = case matches of
                        [] -> T.empty
                        (x:_) -> " (did you mean '" <> x <> "'?)"
      in error $ T.unpack ("Unknown computation: " <> t <> suggestion)
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

  -- Apply dictionary pass to transform typeclass method calls
  let transformed = transformModuleWithEnvs parsed
  let Module _modName _ opens defs = transformed
  envImports <- loadImports projectRoot transformed
  -- Process opens to bring in unqualified names from open statements
  let envWithOpens = processOpens opens envImports

  -- Bind the module to get all definitions
  let envSelf = bindModule transformed envWithOpens
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

    defName :: Definition -> Text
    defName (Definition _ name _ _ _) = name

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
