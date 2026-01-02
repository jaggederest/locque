{-# LANGUAGE OverloadedStrings #-}
module Eval
  ( CtorArityMap
  , ctorArityMap
  , runModuleMain
  ) where

import           AST
import           Control.Exception (catch, IOException, SomeException, throwIO, try, fromException)
import           Control.Monad (when, void)
import           Control.Concurrent (ThreadId, forkIO, myThreadId, throwTo, threadDelay)
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import           GHC.Conc (getNumCapabilities, setNumCapabilities)
import           Control.Concurrent.STM (atomically)
import           Data.IORef
import           Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Char (ord, chr)
import qualified Data.ByteString as BS
import           System.FilePath ((</>), (<.>), takeExtension)
import           System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , doesPathExist
  , getCurrentDirectory
  , getFileSize
  , getModificationTime
  , listDirectory
  , removeDirectoryRecursive
  , removeFile
  , renameDirectory
  , renameFile
  )
import           Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import qualified Data.Text.IO as TIO
import           System.Environment (getArgs)
import           System.Exit (die, ExitCode(..))
import           System.Posix.Process (exitImmediately)
import           System.IO.Unsafe (unsafePerformIO)
import           System.IO (hPutStrLn, hFlush, stderr)
import           System.Process (readCreateProcessWithExitCode, shell)
import qualified System.Posix.Signals as Signals
import           System.Timeout (timeout)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import           Parser (parseModuleFile, parseMExprFile)
import           Validator (checkParens, validateModule)
import           ErrorMsg (findFuzzyMatches)
import           Utils (modNameToPath, qualifyName)
import           DictPass (transformModuleWithEnvs)
import qualified Type as Type
import qualified TypeChecker as TC

-- Global assertion counter (reset at the start of each test run)
assertionCounter :: IORef Int
assertionCounter = unsafePerformIO (newIORef 0)
{-# NOINLINE assertionCounter #-}

-- Captured output stack (top of stack is current capture, reversed order).
outputCapture :: IORef [[Text]]
outputCapture = unsafePerformIO (newIORef [])
{-# NOINLINE outputCapture #-}


-- Runtime values

data Value
  = VNatural Integer
  | VString Text
  | VList [Value]
  | VUnit
  | VBoolean Bool
  | VPair Value Value
  | VDictionary (Map.Map Integer [(Value, Value)]) Int
  | VListener NS.Socket
  | VSocket NS.Socket
  | VTypeConst TypeConst
  | VTypeData Text
  | VTypeUniverse Int
  | VTypeForAll Text Expr Expr
  | VTypeThereExists Text Expr Expr
  | VTypeComp Expr
  | VTypeLift Expr Int Int
  | VTypeEqual Value Value Value
  | VTypeApp Value [Value]
  | VConstructor Text Int Int [Value]
  | VData Text [Value]
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
  show (VDictionary _ size) = "<dictionary:" ++ show size ++ ">"
  show (VListener _) = "<listener>"
  show (VSocket _) = "<socket>"
  show (VTypeConst tc) = T.unpack (Type.typeConstName tc)
  show (VTypeData name) = T.unpack name
  show (VTypeUniverse n) = "Type" ++ show n
  show (VTypeForAll _ _ _) = "<for-all>"
  show (VTypeThereExists _ _ _) = "<there-exists>"
  show (VTypeComp _) = "<computation-type>"
  show (VTypeLift _ _ _) = "<lift-type>"
  show (VTypeEqual _ _ _) = "<equal-type>"
  show (VTypeApp _ _) = "<type-app>"
  show (VConstructor name _ _ _) = "<constructor:" ++ T.unpack name ++ ">"
  show (VData name _) = "<data:" ++ T.unpack name ++ ">"
  show (VClosure _ _ _) = "<closure>"
  show (VPrim _)   = "<prim>"
  show (VComp _)   = "<computation>"
  show (VDict className _) = "<dict:" ++ T.unpack className ++ ">"

data Binding
  = BVal Value
  | BValueExpr Env Expr  -- Capture environment for lazy evaluation (module-local scope)

type Env = Map.Map Text Binding
type CtorArityMap = Map.Map Text (Int, Int)

primEnv :: Env
primEnv = Map.fromList
  [ (T.pack "add-nat-prim", BVal (VPrim primAdd))
  , (T.pack "sub-nat-prim", BVal (VPrim primSub))
  , (T.pack "eq-nat-prim", BVal (VPrim primEqNat))
  , (T.pack "decide-eq-nat-prim", BVal (VPrim primDecideEqNat))
  , (T.pack "eq-string-prim", BVal (VPrim primEqString))
  , (T.pack "decide-eq-string-prim", BVal (VPrim primDecideEqString))
  , (T.pack "decide-eq-bool-prim", BVal (VPrim primDecideEqBool))
  , (T.pack "decide-eq-pair-prim", BVal (VPrim primDecideEqPair))
  , (T.pack "decide-eq-list-prim", BVal (VPrim primDecideEqList))
  , (T.pack "concat-string-prim", BVal (VPrim primConcatString))
  , (T.pack "char-code-prim", BVal (VPrim primCharCode))
  , (T.pack "char-from-code-prim", BVal (VPrim primCharFromCode))
  , (T.pack "print-prim", BVal (VPrim primPrint))
  , (T.pack "capture-output-prim", BVal (VPrim primCaptureOutput))
  , (T.pack "forever-prim", BVal (VPrim primForever))
  , (T.pack "on-signal-prim", BVal (VPrim primOnSignal))
  , (T.pack "tcp-listen-prim", BVal (VPrim primTcpListen))
  , (T.pack "tcp-accept-prim", BVal (VPrim primTcpAccept))
  , (T.pack "tcp-recv-prim", BVal (VPrim primTcpRecv))
  , (T.pack "tcp-send-prim", BVal (VPrim primTcpSend))
  , (T.pack "tcp-close-prim", BVal (VPrim primTcpClose))
  , (T.pack "tcp-close-listener-prim", BVal (VPrim primTcpCloseListener))
  , (T.pack "tcp-select-listener-prim", BVal (VPrim primTcpSelectListener))
  , (T.pack "tcp-select-socket-prim", BVal (VPrim primTcpSelectSocket))
  , (T.pack "sleep-prim", BVal (VPrim primSleep))
  , (T.pack "timeout-prim", BVal (VPrim primTimeout))
  , (T.pack "panic-prim", BVal (VPrim primPanic))
  , (T.pack "assert-hit-prim", BVal (VComp primAssertHit))
  , (T.pack "get-line-prim", BVal (VComp primGetLine))
  , (T.pack "cli-args-prim", BVal (VComp primCliArgs))
  , (T.pack "current-directory-prim", BVal (VComp primCurrentDirectory))
  , (T.pack "time-now-prim", BVal (VComp primTimeNow))
  , (T.pack "read-file-prim", BVal (VPrim primReadFile))
  , (T.pack "write-file-prim", BVal (VPrim primWriteFile))
  , (T.pack "shell-prim", BVal (VPrim primShell))
  , (T.pack "list-dir-prim", BVal (VPrim primListDir))
  , (T.pack "path-exists-prim", BVal (VPrim primPathExists))
  , (T.pack "is-directory-prim", BVal (VPrim primIsDirectory))
  , (T.pack "is-file-prim", BVal (VPrim primIsFile))
  , (T.pack "make-directory-prim", BVal (VPrim primMakeDirectory))
  , (T.pack "remove-file-prim", BVal (VPrim primRemoveFile))
  , (T.pack "remove-directory-prim", BVal (VPrim primRemoveDirectory))
  , (T.pack "append-file-prim", BVal (VPrim primAppendFile))
  , (T.pack "copy-file-prim", BVal (VPrim primCopyFile))
  , (T.pack "copy-tree-prim", BVal (VPrim primCopyTree))
  , (T.pack "rename-path-prim", BVal (VPrim primRenamePath))
  , (T.pack "walk-prim", BVal (VPrim primWalk))
  , (T.pack "stat-prim", BVal (VPrim primStat))
  , (T.pack "validate-prim", BVal (VPrim primValidate))
  , (T.pack "List::empty", BVal (VPrim primNil))
  , (T.pack "List::cons", BVal (VPrim primCons))
  , (T.pack "Pair::pair", BVal (VPrim primPair))
  , (T.pack "dictionary-empty-prim", BVal (VPrim primDictionaryEmpty))
  , (T.pack "dictionary-insert-prim", BVal (VPrim primDictionaryInsert))
  , (T.pack "dictionary-lookup-prim", BVal (VPrim primDictionaryLookup))
  , (T.pack "dictionary-remove-prim", BVal (VPrim primDictionaryRemove))
  , (T.pack "dictionary-size-prim", BVal (VPrim primDictionarySize))
  , (T.pack "dictionary-to-list-prim", BVal (VPrim primDictionaryToList))
  , (T.pack "Boolean::false", BVal (VBoolean False))
  , (T.pack "Boolean::true", BVal (VBoolean True))
  , (T.pack "Unit::tt", BVal VUnit)
  , (T.pack "Boolean::true", BVal (VBoolean True))
  , (T.pack "Boolean::false", BVal (VBoolean False))
  , (T.pack "Unit::tt", BVal VUnit)
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
  , (T.pack "natural-to-peano-prim", BVal (VPrim primNaturalToPeano))
  -- String operations
  , (T.pack "string-to-list-prim", BVal (VPrim primStringToList))
  -- Error handling
  , (T.pack "error-prim", BVal (VPrim primError))
  -- Display/conversion
  , (T.pack "nat-to-string-prim", BVal (VPrim primNatToString))
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

notProofValue :: Value
notProofValue =
  VPrim $ \args -> case args of
    [_] -> pure VUnit
    _ -> error "not-proof expects 1 arg"

decisionResult :: Bool -> Value
decisionResult ok =
  VPair (VBoolean ok) (if ok then VUnit else notProofValue)

expectDecisionBool :: Value -> IO Bool
expectDecisionBool v = case v of
  VPair (VBoolean ok) _ -> pure ok
  _ -> error "decidable result expects (Boolean, proof)"

primDecideEqNat :: [Value] -> IO Value
primDecideEqNat [a, b] = do
  a' <- expectNat a
  b' <- expectNat b
  pure (decisionResult (a' == b'))
primDecideEqNat _ = error "decide-eq-nat-prim expects 2 args"

primDecideEqString :: [Value] -> IO Value
primDecideEqString [a, b] = do
  a' <- expectString a
  b' <- expectString b
  pure (decisionResult (a' == b'))
primDecideEqString _ = error "decide-eq-string-prim expects 2 args"

primDecideEqBool :: [Value] -> IO Value
primDecideEqBool [a, b] = do
  a' <- expectBool a
  b' <- expectBool b
  pure (decisionResult (a' == b'))
primDecideEqBool _ = error "decide-eq-bool-prim expects 2 args"

primDecideEqPair :: [Value] -> IO Value
primDecideEqPair [tyA, tyB, eqA, eqB, VPair a1 b1, VPair a2 b2] = do
  expectTypeArg tyA
  expectTypeArg tyB
  okA <- expectDecisionBool =<< apply eqA [a1, a2]
  if not okA
    then pure (decisionResult False)
    else do
      okB <- expectDecisionBool =<< apply eqB [b1, b2]
      pure (decisionResult okB)
primDecideEqPair _ =
  error "decide-eq-pair-prim expects (Type, Type, eqA, eqB, pair, pair)"

primDecideEqList :: [Value] -> IO Value
primDecideEqList [tyA, eqA, VList xs, VList ys] = do
  expectTypeArg tyA
  let go [] [] = pure True
      go (x:xt) (y:yt) = do
        ok <- expectDecisionBool =<< apply eqA [x, y]
        if ok then go xt yt else pure False
      go _ _ = pure False
  ok <- go xs ys
  pure (decisionResult ok)
primDecideEqList _ =
  error "decide-eq-list-prim expects (Type, eqA, list, list)"
primConcatString :: [Value] -> IO Value
primConcatString vals = do
  ss <- mapM expectString vals
  pure $ VString (mconcat ss)

primCharCode :: [Value] -> IO Value
primCharCode [VString s] =
  case T.unpack s of
    [c] -> pure $ VNatural (toInteger (ord c))
    _ -> error "char-code-prim expects a single-character string"
primCharCode _ = error "char-code-prim expects 1 string arg"

primCharFromCode :: [Value] -> IO Value
primCharFromCode [VNatural n]
  | n <= 0x10FFFF = pure $ VString (T.singleton (chr (fromInteger n)))
  | otherwise = error "char-from-code-prim expects a valid Unicode codepoint"
primCharFromCode _ = error "char-from-code-prim expects 1 nat arg"

primNil :: [Value] -> IO Value
primNil [ty] = do
  expectTypeArg ty
  pure (VList [])
primNil _ = error "List::empty expects (Type)"

primPair :: [Value] -> IO Value
primPair [tyA, tyB, a, b] = do
  expectTypeArg tyA
  expectTypeArg tyB
  pure $ VPair a b
primPair _ = error "Pair::pair expects (Type, Type, a, b)"

optionNone :: Value
optionNone = VData "Option::none" []

optionSome :: Value -> Value
optionSome v = VData "Option::some" [v]

expectDictionary :: Value -> IO (Map.Map Integer [(Value, Value)], Int)
expectDictionary v = case v of
  VDictionary buckets size -> pure (buckets, size)
  _ -> error $ "expected Dictionary, got " ++ show v

insertBucket :: Value -> Value -> Value -> [(Value, Value)] -> IO (Bool, [(Value, Value)])
insertBucket eqFn key val entries = case entries of
  [] -> pure (False, [(key, val)])
  (k, v):rest -> do
    same <- expectBool =<< apply eqFn [key, k]
    if same
      then pure (True, (key, val) : rest)
      else do
        (found, rest') <- insertBucket eqFn key val rest
        pure (found, (k, v) : rest')

lookupBucket :: Value -> Value -> [(Value, Value)] -> IO (Maybe Value)
lookupBucket eqFn key entries = case entries of
  [] -> pure Nothing
  (k, v):rest -> do
    same <- expectBool =<< apply eqFn [key, k]
    if same
      then pure (Just v)
      else lookupBucket eqFn key rest

removeBucket :: Value -> Value -> [(Value, Value)] -> IO (Bool, [(Value, Value)])
removeBucket eqFn key entries = case entries of
  [] -> pure (False, [])
  (k, v):rest -> do
    same <- expectBool =<< apply eqFn [key, k]
    if same
      then pure (True, rest)
      else do
        (found, rest') <- removeBucket eqFn key rest
        pure (found, (k, v) : rest')

primDictionaryEmpty :: [Value] -> IO Value
primDictionaryEmpty [tyK, tyV] = do
  expectTypeArg tyK
  expectTypeArg tyV
  pure (VDictionary Map.empty 0)
primDictionaryEmpty _ = error "dictionary-empty-prim expects (Type, Type)"

primDictionaryInsert :: [Value] -> IO Value
primDictionaryInsert [tyK, tyV, hashFn, eqFn, key, val, dictVal] = do
  expectTypeArg tyK
  expectTypeArg tyV
  (buckets, size) <- expectDictionary dictVal
  h <- expectNat =<< apply hashFn [key]
  let bucket = Map.findWithDefault [] h buckets
  (found, bucket') <- insertBucket eqFn key val bucket
  let size' = if found then size else size + 1
      buckets' = Map.insert h bucket' buckets
  pure (VDictionary buckets' size')
primDictionaryInsert _ =
  error "dictionary-insert-prim expects (Type, Type, hash, eq, key, value, dict)"

primDictionaryLookup :: [Value] -> IO Value
primDictionaryLookup [tyK, tyV, hashFn, eqFn, key, dictVal] = do
  expectTypeArg tyK
  expectTypeArg tyV
  (buckets, _) <- expectDictionary dictVal
  h <- expectNat =<< apply hashFn [key]
  let bucket = Map.findWithDefault [] h buckets
  result <- lookupBucket eqFn key bucket
  pure (maybe optionNone optionSome result)
primDictionaryLookup _ =
  error "dictionary-lookup-prim expects (Type, Type, hash, eq, key, dict)"

primDictionaryRemove :: [Value] -> IO Value
primDictionaryRemove [tyK, tyV, hashFn, eqFn, key, dictVal] = do
  expectTypeArg tyK
  expectTypeArg tyV
  (buckets, size) <- expectDictionary dictVal
  h <- expectNat =<< apply hashFn [key]
  let bucket = Map.findWithDefault [] h buckets
  (removed, bucket') <- removeBucket eqFn key bucket
  let size' = if removed then max 0 (size - 1) else size
      buckets' = if null bucket'
        then Map.delete h buckets
        else Map.insert h bucket' buckets
  pure (VDictionary buckets' size')
primDictionaryRemove _ =
  error "dictionary-remove-prim expects (Type, Type, hash, eq, key, dict)"

primDictionarySize :: [Value] -> IO Value
primDictionarySize [tyK, tyV, dictVal] = do
  expectTypeArg tyK
  expectTypeArg tyV
  (_, size) <- expectDictionary dictVal
  pure (VNatural (toInteger size))
primDictionarySize _ = error "dictionary-size-prim expects (Type, Type, dict)"

primDictionaryToList :: [Value] -> IO Value
primDictionaryToList [tyK, tyV, dictVal] = do
  expectTypeArg tyK
  expectTypeArg tyV
  (buckets, _) <- expectDictionary dictVal
  let entries = concatMap snd (Map.toList buckets)
      pairs = map (\(k, v) -> VPair k v) entries
  pure (VList pairs)
primDictionaryToList _ = error "dictionary-to-list-prim expects (Type, Type, dict)"

primValidate :: [Value] -> IO Value
primValidate [VString s] = do
  -- Add trailing newline if missing (parser requires it)
  let s' = if T.isSuffixOf (T.pack "\n") s then s else s <> T.pack "\n"
  case checkParens "<inline>" s' of
    Left _ -> pure (VBoolean False)
    Right _ -> case parseMExprFile "<inline>" s' of
      Left _ -> pure (VBoolean False)
      Right m -> case validateModule m of
        Left _ -> pure (VBoolean False)
        Right _ -> pure (VBoolean True)
primValidate _ = error "validate-prim expects 1 string"


primPrint :: [Value] -> IO Value
primPrint [v] = do
  let line = case v of
        VString s -> s
        other     -> T.pack (show other)
  pure (VComp (writeOutput line >> pure VUnit))
primPrint _ = error "print-prim expects 1 arg"

primCaptureOutput :: [Value] -> IO Value
primCaptureOutput [ty, VComp action] = do
  expectTypeArg ty
  pure $ VComp $ do
    modifyIORef' outputCapture ([]:)
    resultOrEx <- try action
    stack <- readIORef outputCapture
    case stack of
      [] -> error "capture-output-prim stack empty"
      (buf:rest) -> do
        writeIORef outputCapture rest
        case resultOrEx of
          Left (ex :: SomeException) -> throwIO ex
          Right result ->
            pure (VPair (VList (map VString (reverse buf))) result)
primCaptureOutput _ = error "capture-output-prim expects (Type, computation)"

primForever :: [Value] -> IO Value
primForever [VComp action] =
  pure $ VComp $ let loop = action >> loop in loop
primForever _ = error "forever-prim expects (computation Unit)"

signalFromName :: Text -> Maybe Signals.Signal
signalFromName name =
  case T.toUpper (T.strip name) of
    "INT" -> Just Signals.sigINT
    "SIGINT" -> Just Signals.sigINT
    "TERM" -> Just Signals.sigTERM
    "SIGTERM" -> Just Signals.sigTERM
    "HUP" -> Just Signals.sigHUP
    "SIGHUP" -> Just Signals.sigHUP
    "QUIT" -> Just Signals.sigQUIT
    "SIGQUIT" -> Just Signals.sigQUIT
    "PIPE" -> Just Signals.sigPIPE
    "SIGPIPE" -> Just Signals.sigPIPE
    "USR1" -> Just Signals.sigUSR1
    "SIGUSR1" -> Just Signals.sigUSR1
    "USR2" -> Just Signals.sigUSR2
    "SIGUSR2" -> Just Signals.sigUSR2
    _ -> Nothing

primOnSignal :: [Value] -> IO Value
primOnSignal [VString name, VComp handler] =
  case signalFromName name of
    Nothing -> error "on-signal-prim expects a known signal name"
    Just sig -> pure $ VComp $ do
      caller <- myThreadId
      hPutStrLn stderr ("[signal] installing handler for " ++ T.unpack name)
      hFlush stderr
      _ <- Signals.installHandler sig (Signals.Catch (runHandler caller sig handler)) Nothing
      pure VUnit
  where
    runHandler caller sig action = do
      hPutStrLn stderr ("[signal] caught " ++ show sig)
      hFlush stderr
      result <- try action
      case result of
        Left (ex :: SomeException) -> do
          throwTo caller ex
          case fromException ex of
            Just code -> exitImmediately code
            Nothing -> exitImmediately (ExitFailure 1)
        Right _ -> pure ()
primOnSignal _ = error "on-signal-prim expects (signal-name, computation Unit)"

primAssertHit :: IO Value
primAssertHit = do
  modifyIORef' assertionCounter (+1)
  pure VUnit

expectListener :: Value -> IO NS.Socket
expectListener v = case v of
  VListener sock -> pure sock
  _ -> error $ "expected Listener, got " ++ show v

expectSocket :: Value -> IO NS.Socket
expectSocket v = case v of
  VSocket sock -> pure sock
  _ -> error $ "expected Socket, got " ++ show v

microsToInt :: Integer -> Int
microsToInt micros =
  let maxInt = toInteger (maxBound :: Int)
  in fromInteger (min micros maxInt)

runBlocking :: IO a -> IO a
runBlocking action = do
  resultVar <- newEmptyMVar
  _ <- forkIO $ do
    result <- try action
    putMVar resultVar result
  result <- takeMVar resultVar
  case result of
    Left (ex :: SomeException) -> throwIO ex
    Right val -> pure val

primTcpListen :: [Value] -> IO Value
primTcpListen [VNatural port] = do
  pure $ VComp $ NS.withSocketsDo $ do
    let hints =
          NS.defaultHints
            { NS.addrFlags = [NS.AI_PASSIVE]
            , NS.addrSocketType = NS.Stream
            }
        service = Just (show port)
    addrInfos <- NS.getAddrInfo (Just hints) Nothing service
    case addrInfos of
      [] -> error "tcp-listen-prim: no addr info"
      (addr:_) -> do
        sock <- NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr)
        NS.setSocketOption sock NS.ReuseAddr 1
        NS.bind sock (NS.addrAddress addr)
        NS.listen sock 128
        pure (VListener sock)
primTcpListen _ = error "tcp-listen-prim expects (port)"

primTcpAccept :: [Value] -> IO Value
primTcpAccept [listenerVal] = do
  pure $ VComp $ NS.withSocketsDo $ do
    listener <- expectListener listenerVal
    (conn, _peer) <- runBlocking (NS.accept listener)
    pure (VSocket conn)
primTcpAccept _ = error "tcp-accept-prim expects (listener)"

primTcpRecv :: [Value] -> IO Value
primTcpRecv [socketVal] = do
  pure $ VComp $ do
    sock <- expectSocket socketVal
    chunk <- runBlocking (NSB.recv sock 4096)
    pure (VString (TE.decodeUtf8 chunk))
primTcpRecv _ = error "tcp-recv-prim expects (socket)"

primTcpSend :: [Value] -> IO Value
primTcpSend [socketVal, VString payload] = do
  pure $ VComp $ do
    sock <- expectSocket socketVal
    runBlocking (NSB.sendAll sock (TE.encodeUtf8 payload))
    pure VUnit
primTcpSend _ = error "tcp-send-prim expects (socket, string)"

primTcpClose :: [Value] -> IO Value
primTcpClose [socketVal] = do
  pure $ VComp $ do
    sock <- expectSocket socketVal
    NS.close sock
    pure VUnit
primTcpClose _ = error "tcp-close-prim expects (socket)"

primTcpCloseListener :: [Value] -> IO Value
primTcpCloseListener [listenerVal] = do
  pure $ VComp $ do
    listener <- expectListener listenerVal
    NS.close listener
    pure VUnit
primTcpCloseListener _ = error "tcp-close-listener-prim expects (listener)"

primTcpSelectListener :: [Value] -> IO Value
primTcpSelectListener [listenerVal, VNatural micros] = do
  pure $ VComp $ do
    listener <- expectListener listenerVal
    let waitMicros = microsToInt micros
    waitStm <- NS.waitReadSocketSTM listener
    ready <- timeout waitMicros (atomically waitStm)
    pure (VBoolean (maybe False (const True) ready))
primTcpSelectListener _ = error "tcp-select-listener-prim expects (listener, micros)"

primTcpSelectSocket :: [Value] -> IO Value
primTcpSelectSocket [socketVal, VNatural micros] = do
  pure $ VComp $ do
    sock <- expectSocket socketVal
    let waitMicros = microsToInt micros
    waitStm <- NS.waitReadSocketSTM sock
    ready <- timeout waitMicros (atomically waitStm)
    pure (VBoolean (maybe False (const True) ready))
primTcpSelectSocket _ = error "tcp-select-socket-prim expects (socket, micros)"

primSleep :: [Value] -> IO Value
primSleep [VNatural micros] =
  pure $ VComp $ do
    threadDelay (microsToInt micros)
    pure VUnit
primSleep _ = error "sleep-prim expects (micros)"

primTimeout :: [Value] -> IO Value
primTimeout [ty, VNatural micros, VComp action] = do
  expectTypeArg ty
  pure $ VComp $ do
    let waitMicros = microsToInt micros
    result <- timeout waitMicros action
    pure (maybe optionNone optionSome result)
primTimeout _ = error "timeout-prim expects (Type, micros, computation)"

primPanic :: [Value] -> IO Value
primPanic [VString msg] =
  pure $ VComp $ die (T.unpack msg)
primPanic _ = error "panic-prim expects (string)"

primGetLine :: IO Value
primGetLine = do
  line <- TIO.getLine
  pure (VString (line <> T.pack "\n"))

writeOutput :: Text -> IO ()
writeOutput line = do
  stack <- readIORef outputCapture
  case stack of
    [] -> TIO.putStrLn line
    (buf:rest) -> writeIORef outputCapture ((line : buf) : rest)

primCliArgs :: IO Value
primCliArgs = do
  args <- getArgs
  pure (VList (map (VString . T.pack) args))

primCurrentDirectory :: IO Value
primCurrentDirectory = do
  dir <- getCurrentDirectory
  pure (VString (T.pack dir))

primTimeNow :: IO Value
primTimeNow = do
  t <- getPOSIXTime
  let micros = floor (t * 1000000)
  pure (VNatural micros)

primReadFile :: [Value] -> IO Value
primReadFile [VString path] =
  pure (VComp (VString <$> TIO.readFile (T.unpack path)))
primReadFile _ = error "read-file-prim expects 1 string arg"

primWriteFile :: [Value] -> IO Value
primWriteFile [VString path, VString contents] = do
  pure (VComp (TIO.writeFile (T.unpack path) contents >> pure VUnit))
primWriteFile _ = error "write-file-prim expects (path, contents)"

primListDir :: [Value] -> IO Value
primListDir [VString path] =
  pure $ VComp $ do
    entries <- listDirectory (T.unpack path)
    pure (VList (map (VString . T.pack) entries))
primListDir _ = error "list-dir-prim expects 1 string arg"

primPathExists :: [Value] -> IO Value
primPathExists [VString path] =
  pure $ VComp $ do
    ok <- doesPathExist (T.unpack path)
    pure (VBoolean ok)
primPathExists _ = error "path-exists-prim expects 1 string arg"

primIsDirectory :: [Value] -> IO Value
primIsDirectory [VString path] =
  pure $ VComp $ do
    ok <- doesDirectoryExist (T.unpack path)
    pure (VBoolean ok)
primIsDirectory _ = error "is-directory-prim expects 1 string arg"

primIsFile :: [Value] -> IO Value
primIsFile [VString path] =
  pure $ VComp $ do
    ok <- doesFileExist (T.unpack path)
    pure (VBoolean ok)
primIsFile _ = error "is-file-prim expects 1 string arg"

primMakeDirectory :: [Value] -> IO Value
primMakeDirectory [VString path] =
  pure $ VComp $ do
    createDirectoryIfMissing True (T.unpack path)
    pure VUnit
primMakeDirectory _ = error "make-directory-prim expects 1 string arg"

primRemoveFile :: [Value] -> IO Value
primRemoveFile [VString path] =
  pure $ VComp $ do
    let path' = T.unpack path
    isDir <- doesDirectoryExist path'
    if isDir
      then error "remove-file-prim expects a file path"
      else do
        exists <- doesFileExist path'
        if exists then removeFile path' else pure ()
        pure VUnit
primRemoveFile _ = error "remove-file-prim expects 1 string arg"

primRemoveDirectory :: [Value] -> IO Value
primRemoveDirectory [VString path] =
  pure $ VComp $ do
    let path' = T.unpack path
    isDir <- doesDirectoryExist path'
    if isDir
      then removeDirectoryRecursive path' >> pure VUnit
      else do
        isFile <- doesFileExist path'
        if isFile
          then error "remove-directory-prim expects a directory path"
          else pure VUnit
primRemoveDirectory _ = error "remove-directory-prim expects 1 string arg"

primAppendFile :: [Value] -> IO Value
primAppendFile [VString path, VString contents] =
  pure (VComp (TIO.appendFile (T.unpack path) contents >> pure VUnit))
primAppendFile _ = error "append-file-prim expects (path, contents)"

primCopyFile :: [Value] -> IO Value
primCopyFile [VString src, VString dest] =
  pure $ VComp $ do
    let srcPath = T.unpack src
        destPath = T.unpack dest
    isDir <- doesDirectoryExist srcPath
    if isDir
      then error "copy-file-prim expects a file source"
      else copyFile srcPath destPath >> pure VUnit
primCopyFile _ = error "copy-file-prim expects (source, destination)"

primRenamePath :: [Value] -> IO Value
primRenamePath [VString src, VString dest] =
  pure $ VComp $ do
    let srcPath = T.unpack src
        destPath = T.unpack dest
    isDir <- doesDirectoryExist srcPath
    if isDir
      then renameDirectory srcPath destPath >> pure VUnit
      else do
        isFile <- doesFileExist srcPath
        if isFile
          then renameFile srcPath destPath >> pure VUnit
          else error "rename-path-prim expects an existing path"
primRenamePath _ = error "rename-path-prim expects (source, destination)"

copyTree :: FilePath -> FilePath -> IO ()
copyTree src dest = do
  isDir <- doesDirectoryExist src
  isFile <- doesFileExist src
  if isDir
    then do
      createDirectoryIfMissing True dest
      entries <- listDirectory src
      let fullPaths = map (src </>) entries
          destPaths = map (dest </>) entries
      mapM_ (uncurry copyTree) (zip fullPaths destPaths)
    else if isFile
      then copyFile src dest
      else error "copy-tree-prim expects an existing path"

primCopyTree :: [Value] -> IO Value
primCopyTree [VString src, VString dest] =
  pure $ VComp $ do
    copyTree (T.unpack src) (T.unpack dest)
    pure VUnit
primCopyTree _ = error "copy-tree-prim expects (source, destination)"

walkFrom :: FilePath -> IO [(FilePath, Bool)]
walkFrom dir = do
  entries <- listDirectory dir
  let fullPaths = map (dir </>) entries
  parts <- mapM walkPath fullPaths
  pure (concat parts)
  where
    walkPath path = do
      isDir <- doesDirectoryExist path
      if isDir
        then do
          rest <- walkFrom path
          pure ((path, True) : rest)
        else do
          isFile <- doesFileExist path
          if isFile
            then pure [(path, False)]
            else pure []

primWalk :: [Value] -> IO Value
primWalk [VString path] =
  pure $ VComp $ do
    let root = T.unpack path
    isDir <- doesDirectoryExist root
    isFile <- doesFileExist root
    entries <- if isDir
      then walkFrom root
      else if isFile
        then pure [(root, False)]
        else error "walk-prim expects an existing path"
    let toValue (p, isDirEntry) =
          VPair (VString (T.pack p)) (VBoolean isDirEntry)
    pure (VList (map toValue entries))
primWalk _ = error "walk-prim expects 1 string arg"

primStat :: [Value] -> IO Value
primStat [VString path] =
  pure $ VComp $ do
    let path' = T.unpack path
    isDir <- doesDirectoryExist path'
    isFile <- doesFileExist path'
    if not (isDir || isFile)
      then error "stat-prim expects an existing path"
      else do
        mtime <- getModificationTime path'
        let mtimeNat = floor (utcTimeToPOSIXSeconds mtime)
        size <- if isFile then getFileSize path' else pure 0
        let kind = if isDir then T.pack "directory" else T.pack "file"
        pure $ VPair (VString kind) (VPair (VNatural size) (VNatural mtimeNat))
primStat _ = error "stat-prim expects 1 string arg"

primShell :: [Value] -> IO Value
primShell [VString cmd] = do
  pure (VComp (do
    (_exitCode, stdout, stderr) <- readCreateProcessWithExitCode (shell (T.unpack cmd)) ""
    pure $ VString (T.pack (stdout ++ stderr))))
primShell _ = error "shell-prim expects 1 string arg (command)"

primCons :: [Value] -> IO Value
primCons [ty, h, VList t] = do
  expectTypeArg ty
  pure $ VList (h:t)
primCons _ = error "List::cons expects (Type, head, list)"

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

primNaturalToPeano :: [Value] -> IO Value
primNaturalToPeano [VNatural n] =
  pure $ VList (replicate (fromInteger n) VUnit)
primNaturalToPeano _ = error "natural-to-peano-prim expects 1 nat arg"

primStringToList :: [Value] -> IO Value
primStringToList [VString s] =
  pure $ VList (map (VString . T.singleton) (T.unpack s))
primStringToList _ = error "string-to-list-prim expects 1 string arg"

primError :: [Value] -> IO Value
primError [ty, VString msg] = do
  expectTypeArg ty
  error (T.unpack msg)
primError _ = error "error-prim expects (Type, string)"

primNatToString :: [Value] -> IO Value
primNatToString [VNatural n] = pure $ VString (T.pack (show n))
primNatToString _ = error "nat-to-string-prim expects 1 Natural arg"

countForAll :: Expr -> Int
countForAll expr = case expr of
  EForAll _ _ body -> 1 + countForAll body
  _ -> 0

isTypeValue :: Value -> Bool
isTypeValue v = case v of
  VTypeConst _ -> True
  VTypeData _ -> True
  VTypeUniverse _ -> True
  VTypeForAll _ _ _ -> True
  VTypeThereExists _ _ _ -> True
  VTypeComp _ -> True
  VTypeLift _ _ _ -> True
  VTypeEqual _ _ _ -> True
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

expectBool :: Value -> IO Bool
expectBool (VBoolean b) = pure b
expectBool v           = error $ "expected Boolean, got " ++ show v

expectString :: Value -> IO Text
expectString (VString s) = pure s
expectString v           = error $ "expected String, got " ++ show v

-- Build environment from module definitions atop an existing env (imports/prims)
-- Uses lazy evaluation to tie-the-knot: all definitions capture the final environment
bindModule :: CtorArityMap -> Module -> Env -> Env
bindModule ctorArity (Module _modName _ _ defs) base =
  let env = foldl addDef base defs
      addDef e (Definition _ name body) = case body of
        EData params _ cases ->
          let e' = Map.insert name (BVal (VTypeData name)) e
              ctorBindings = map (ctorBinding params) cases
          in foldl (\acc (ctorName, ctorVal) -> Map.insert ctorName (BVal ctorVal) acc) e' ctorBindings
        _ ->
          Map.insert name (BValueExpr env body) e
  in env
  where
    ctorBinding params (DataCase ctorName ctorTy) =
      let fallback = (countFreeDataParams params ctorTy, countForAll ctorTy)
          (dataParamCount, termParamCount) = Map.findWithDefault fallback ctorName ctorArity
          total = dataParamCount + termParamCount
          ctorVal = if total == 0
            then VData ctorName []
            else VConstructor ctorName dataParamCount termParamCount []
      in (ctorName, ctorVal)

countFreeDataParams :: [Param] -> Expr -> Int
countFreeDataParams params ctorTy =
  let free = TC.freeVars ctorTy
  in length [() | Param name _ <- params, name `Set.member` free]

runModuleMain :: FilePath -> CtorArityMap -> Module -> IO Int
runModuleMain projectRoot ctorArity m@(Module _modName _ opens _) = do
  caps <- getNumCapabilities
  when (caps < 2) (setNumCapabilities 2)
  -- Reset assertion counter
  writeIORef assertionCounter 0

  envImports <- loadImports projectRoot m
  let envWithOpens = processOpens opens envImports
      env = bindModule ctorArity m envWithOpens
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
  EListLiteral elems -> VList <$> mapM (evalExpr env) elems
  ETypeConst tc -> pure (VTypeConst tc)
  EData _ _ _ -> pure (VTypeData "<data>")
  ETypeUniverse n -> pure (VTypeUniverse n)
  EForAll v dom cod -> pure (VTypeForAll v dom cod)
  EThereExists v dom cod -> pure (VTypeThereExists v dom cod)
  ECompType t -> pure (VTypeComp t)
  EEqual ty lhs rhs ->
    VTypeEqual <$> evalExpr env ty <*> evalExpr env lhs <*> evalExpr env rhs
  EReflexive _ _ -> pure VUnit
  ERewrite _ _ body -> evalExpr env body
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
    let recClosure = VClosure envWithRecur (map paramName params) body
        envWithRecur = Map.insert (T.pack "recur") (BVal recClosure) env
    in pure recClosure
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
  ETypeClass _ _ _ -> error "Typeclass nodes are not evaluable"
  EInstance _ _ _ -> error "Instance nodes are not evaluable"

resolveBinding :: Env -> Binding -> IO Value
resolveBinding _outerEnv b = case b of
  BVal v               -> pure v
  BValueExpr env e     -> evalExpr env e  -- Use captured environment

apply :: Value -> [Value] -> IO Value
apply v [] = pure v
apply (VPrim f) args = f args
apply (VTypeConst tc) args = pure (VTypeApp (VTypeConst tc) args)
apply (VTypeData name) args = pure (VTypeApp (VTypeData name) args)
apply (VTypeLift ty fromLevel toLevel) args =
  pure (VTypeApp (VTypeLift ty fromLevel toLevel) args)
apply (VTypeApp f existing) args = pure (VTypeApp f (existing ++ args))
apply (VConstructor name dataParamCount termParamCount existing) args = do
  let applied = existing ++ args
      total = dataParamCount + termParamCount
  case compare (length applied) total of
    LT -> pure (VConstructor name dataParamCount termParamCount applied)
    EQ -> pure (VData name (drop dataParamCount applied))
    GT -> error ("Constructor applied to too many arguments: " ++ T.unpack name)
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
    VList [] -> matchCtor "List::empty" [] envScrut
    VList (h:t) -> matchCtor "List::cons" [h, VList t] envScrut
    VBoolean False -> matchCtor "Boolean::false" [] envScrut
    VBoolean True -> matchCtor "Boolean::true" [] envScrut
    VPair a b -> matchCtor "Pair::pair" [a, b] envScrut
    VUnit -> matchCtor "Unit::tt" [] envScrut
    VData ctorName args -> matchCtor ctorName args envScrut
    VConstructor ctorName dataParamCount termParamCount args ->
      let total = dataParamCount + termParamCount
      in if length args == total
        then matchCtor ctorName (drop dataParamCount args) envScrut
        else error ("match: scrutinee not fully applied: " ++ T.unpack ctorName)
    _ -> error "match: unsupported scrutinee value"
  where
    matchCtor ctorName args envScrut = do
      case [ (binders, body) | MatchCase caseCtor binders body <- cases, caseCtor == ctorName ] of
        ((binders, body):_) -> do
          when (length binders /= length args) $
            error ("match: case " ++ T.unpack ctorName ++ " expects " ++ show (length binders) ++ " binders")
          let env' = foldl (\e (name, val) -> Map.insert name (BVal val) e)
                          envScrut
                          (zip (map paramName binders) args)
          evalExpr env' body
        [] -> error ("match: missing case " ++ T.unpack ctorName)

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

-- Import loading

loadImports :: FilePath -> Module -> IO Env
loadImports projectRoot (Module _ imports _ _) =
  loadImportsWith projectRoot Set.empty imports

loadImport :: FilePath -> Set.Set Text -> Import -> IO Env
loadImport projectRoot visiting (Import modName alias) = do
  when (modName `Set.member` visiting) $
    error $ "Import cycle detected: " ++ T.unpack modName
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
  normalizedResult <- TC.normalizeModuleWithImports projectRoot contents parsed
  normalized <- case normalizedResult of
    Left tcErr -> error $ "Type error in import " ++ T.unpack modName ++ ": " ++ show tcErr
    Right m -> pure m

  let Module _modName _ opens defs = annotated
      visiting' = Set.insert modName visiting
  envImports <- loadImportsWith projectRoot visiting' (modImports annotated)
  -- Process opens to bring in unqualified names from open statements
  let envWithOpens = processOpens opens envImports

  -- Bind the module to get all definitions
  let envSelf = bindModule (ctorArityMap normalized) annotated envWithOpens
      -- Add qualified names for each definition using the alias
      defNames = map defName defs
      ctorNames = concatMap defCtorNames defs
      envFinal = foldl (insertQualified alias envSelf) envWithOpens (defNames ++ ctorNames)

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

    defCtorNames :: Definition -> [Text]
    defCtorNames def = case defBody def of
      EData _ _ cases -> map dataCaseName cases
      _ -> []

loadImportsWith :: FilePath -> Set.Set Text -> [Import] -> IO Env
loadImportsWith projectRoot visiting imports = do
  envs <- mapM (loadImport projectRoot visiting) imports
  pure $ Map.unions (primEnv : envs)

ctorArityMap :: Module -> CtorArityMap
ctorArityMap (Module _ _ _ defs) =
  Map.fromList (concatMap defCtorArity defs)
  where
    defCtorArity (Definition _ _ body) = case body of
      EData params _ cases ->
        [ (ctorName, (countFreeDataParams params ctorTy, countForAll ctorTy))
        | DataCase ctorName ctorTy <- cases
        ]
      _ -> []

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
