module Eval
  ( runModuleMain
  ) where

import           AST
import           Control.Monad (foldM)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           System.FilePath ((</>), (<.>))
import qualified Data.Text.IO as TIO
import           Parser (parseModuleFile)

-- Runtime values

data Value
  = VNat Integer
  | VString Text
  | VList [Value]
  | VUnit
  | VPrim ([Value] -> IO Value)

instance Show Value where
  show (VNat n)    = show n
  show (VString s) = show s
  show (VList xs)  = "[" ++ inner xs ++ "]"
    where inner = concat . map ((++ ",") . show)
  show VUnit       = "tt"
  show (VPrim _)   = "<prim>"

data Binding
  = BVal Value
  | BValueExpr Expr
  | BCompExpr Comp

type Env = Map.Map Text Binding

toVal :: Binding -> Maybe Value
toVal (BVal v) = Just v
toVal _        = Nothing

primEnv :: Env
primEnv = Map.fromList
  [ (T.pack "add-nat-prim", BVal (VPrim primAdd))
  , (T.pack "add-nat", BVal (VPrim primAdd))
  , (T.pack "sub-nat-prim", BVal (VPrim primSub))
  , (T.pack "eq-nat-prim", BVal (VPrim primEqNat))
  , (T.pack "eq-string-prim", BVal (VPrim primEqString))
  , (T.pack "concat-string-prim", BVal (VPrim primConcatString))
  , (T.pack "length-string-prim", BVal (VPrim primLengthString))
  , (T.pack "print-prim", BVal (VPrim primPrint))
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
  pure $ if a' == b' then VUnit else error "eq-nat-prim failed"
primEqNat _ = error "eq-nat-prim expects 2 args"

primEqString :: [Value] -> IO Value
primEqString [a,b] = do
  a' <- expectString a
  b' <- expectString b
  pure $ if a' == b' then VUnit else error "eq-string-prim failed"
primEqString _ = error "eq-string-prim expects 2 args"

primConcatString :: [Value] -> IO Value
primConcatString vals = do
  ss <- mapM expectString vals
  pure $ VString (mconcat ss)

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

primAssertEqNat :: [Value] -> IO Value
primAssertEqNat [a,b] = do
  a' <- expectNat a
  b' <- expectNat b
  if a' == b'
    then pure VUnit
    else error $ "assert-eq-nat failed: " ++ show a' ++ " /= " ++ show b'
primAssertEqNat _ = error "assert-eq-nat expects 2 args"

primAssertEqString :: [Value] -> IO Value
primAssertEqString [a,b] = do
  a' <- expectString a
  b' <- expectString b
  if a' == b'
    then pure VUnit
    else error $ "assert-eq-string failed: " ++ show a' ++ " /= " ++ show b'
primAssertEqString _ = error "assert-eq-string expects 2 args"

primCons :: [Value] -> IO Value
primCons [h, VList t] = pure $ VList (h:t)
primCons _ = error "cons-prim expects head and list"

primHead :: [Value] -> IO Value
primHead [VList (h:_)] = pure h
primHead _ = error "head-prim expects non-empty list"

primTail :: [Value] -> IO Value
primTail [VList (_:t)] = pure (VList t)
primTail _ = error "tail-prim expects non-empty list"

primLengthList :: [Value] -> IO Value
primLengthList [VList xs] = pure $ VNat (fromIntegral (length xs))
primLengthList _ = error "length-list-prim expects 1 list arg"

expectNat :: Value -> IO Integer
expectNat (VNat n) = pure n
expectNat v        = error $ "expected Nat, got " ++ show v

expectString :: Value -> IO Text
expectString (VString s) = pure s
expectString v           = error $ "expected String, got " ++ show v

expectList :: Value -> IO [Value]
expectList (VList xs) = pure xs
expectList v          = error $ "expected List, got " ++ show v

-- Build environment from module definitions atop an existing env (imports/prims)
bindModule :: Module -> Env -> Env
bindModule (Module _ _ defs) base = foldl addDef base defs
  where
    addDef env (Definition _ name kind body) =
      case (kind, body) of
        (ValueDef, Left e)        -> Map.insert name (BValueExpr e) env
        (ComputationDef, Right c) -> Map.insert name (BCompExpr c) env
        _                         -> env

runModuleMain :: FilePath -> Module -> IO ()
runModuleMain projectRoot m@(Module _ _ _) = do
  envImports <- loadImports projectRoot m
  let env = bindModule m envImports
  case Map.lookup (T.pack "main") env of
    Just (BCompExpr c)    -> do _ <- runComp env c; pure ()
    Just (BVal (VPrim f)) -> do _ <- f []; pure ()
    Just (BValueExpr e)   -> do _ <- evalExpr env e; pure ()
    _                     -> error "No main computation found"

evalExpr :: Env -> Expr -> IO Value
evalExpr env expr = case expr of
  EVar t -> case Map.lookup t env of
    Just b  -> resolveBinding env b
    Nothing -> error $ "Unknown variable: " ++ T.unpack t ++ " in env keys " ++ show (Map.keys env)
  ELit lit -> pure $ case lit of
    LNat n    -> VNat n
    LString s -> VString s
  EApp f args -> do
    vf <- evalExpr env f
    vs <- mapM (evalExpr env) args
    apply vf vs

resolveBinding :: Env -> Binding -> IO Value
resolveBinding env b = case b of
  BVal v        -> pure v
  BValueExpr e  -> evalExpr env e
  BCompExpr c   -> runComp env c

apply :: Value -> [Value] -> IO Value
apply (VPrim f) args = f args
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
    Just (BCompExpr c) -> runComp env c
    Just b             -> resolveBinding env b
    Nothing            -> error $ "Unknown computation: " ++ T.unpack t
  CSeq c1 c2 -> do
    _ <- runComp env c1
    runComp env c2

-- Import loading

loadImports :: FilePath -> Module -> IO Env
loadImports projectRoot (Module _ imports _) = do
  envs <- mapM (loadImport projectRoot) imports
  pure $ Map.unions (primEnv : envs)

loadImport :: FilePath -> Import -> IO Env
loadImport projectRoot (Import modName alias) = do
  let path = projectRoot </> "lib" </> modNameToPath modName <.> "lqs"
  contents <- TIO.readFile path
  case parseModuleFile path contents of
    Left err -> error err
    Right m@(Module name _ defs) -> do
      envImports <- loadImports projectRoot m
      let envSelf = foldl (insertDef alias name) envImports defs
      pure envSelf

insertDef :: Text -> Text -> Env -> Definition -> Env
insertDef alias modName env (Definition _ name kind body) =
  let base = case (kind, body) of
        (ValueDef, Left e)        -> Just (BValueExpr e)
        (ComputationDef, Right c) -> Just (BCompExpr c)
        _                         -> Nothing
      names = [aliasPref alias name, aliasPref modName name]
   in case base of
        Nothing -> env
        Just b  -> foldl (\acc n -> Map.insert n b acc) env names

aliasPref :: Text -> Text -> Text
aliasPref prefix n = prefix <> T.pack "." <> n

modNameToPath :: Text -> FilePath
modNameToPath t =
  let segs = T.splitOn (T.pack "::") t
      loweredInit = map T.toLower (init segs)
      lastSeg = case reverse segs of
        []    -> ""
        (x:_) -> T.unpack x
   in T.unpack (T.intercalate (T.pack "/") (loweredInit ++ [T.pack lastSeg]))
