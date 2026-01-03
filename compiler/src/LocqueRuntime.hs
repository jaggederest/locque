{-# LANGUAGE ScopedTypeVariables #-}
module LocqueRuntime
  ( Comp(..)
  , compReturn
  , compBind
  , perform
  , Unit
  , Boolean
  , Natural
  , String
  , Character
  , List
  , Pair
  , Dictionary
  , Listener
  , Socket
  , Option
  , Either
  , Result
  , addNatPrim
  , subNatPrim
  , mulNatPrim
  , divNatPrim
  , modNatPrim
  , ltNatPrim
  , leNatPrim
  , gtNatPrim
  , geNatPrim
  , eqNatPrim
  , decideEqNatPrim
  , eqStringPrim
  , decideEqStringPrim
  , decideEqBoolPrim
  , decideEqPairPrim
  , decideEqListPrim
  , concatStringPrim
  , stringLengthPrim
  , stringToListPrim
  , charCodePrim
  , charFromCodePrim
  , naturalToPeanoPrim
  , natToStringPrim
  , errorPrim
  , printPrim
  , captureOutputPrim
  , foreverPrim
  , assertHitPrim
  , getLinePrim
  , cliArgsPrim
  , currentDirectoryPrim
  , timeNowPrim
  , readFilePrim
  , writeFilePrim
  , shellPrim
  , appendFilePrim
  , copyFilePrim
  , copyTreePrim
  , renamePathPrim
  , listDirPrim
  , pathExistsPrim
  , isDirectoryPrim
  , isFilePrim
  , makeDirectoryPrim
  , removeFilePrim
  , removeDirectoryPrim
  , walkPrim
  , walkFilterPrim
  , statPrim
  , validatePrim
  , dictionaryEmptyPrim
  , dictionaryInsertPrim
  , dictionaryLookupPrim
  , dictionaryRemovePrim
  , dictionarySizePrim
  , dictionaryToListPrim
  , onSignalPrim
  , tcpListenPrim
  , tcpAcceptPrim
  , tcpRecvPrim
  , tcpSendPrim
  , tcpClosePrim
  , tcpCloseListenerPrim
  , tcpSelectListenerPrim
  , tcpSelectSocketPrim
  , sleepPrim
  , timeoutPrim
  , panicPrim
  ) where

import Prelude hiding (Either, String)
import qualified Prelude as P
import Control.Concurrent (forkIO, myThreadId, threadDelay, throwTo)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Exception (SomeException, fromException, try, throwIO)
import Data.Char (chr, ord)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , doesPathExist
  , getCurrentDirectory
  , getFileSize
  , getModificationTime
  , getTemporaryDirectory
  , listDirectory
  , removeDirectoryRecursive
  , removeFile
  , renameDirectory
  , renameFile
  )
import System.Environment (getArgs)
import System.Exit (ExitCode(..))
import System.FilePath (pathSeparator, (</>))
import System.IO.Unsafe (unsafePerformIO)
import System.IO (hClose, openTempFile)
import System.Posix.Process (exitImmediately)
import System.Posix.Signals (Signal)
import qualified System.Posix.Signals as Signals
import System.Process (proc, readCreateProcessWithExitCode, shell)
import System.Timeout (timeout)
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import Unsafe.Coerce (unsafeCoerce)

newtype Comp a = Comp { runComp :: IO a }

type Unit = ()
type Boolean = Bool
type Natural = Integer
type String = Text
type Character = Text
type List a = [a]
type Pair a b = (a, b)
data Dictionary k v = Dictionary (Map.Map Integer [(k, v)]) Int
newtype Listener = Listener NS.Socket
newtype Socket = Socket NS.Socket
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

mulNatPrim :: Natural -> Natural -> Natural
mulNatPrim = (P.*)

divNatPrim :: Natural -> Natural -> Natural
divNatPrim _ 0 = P.error "div-nat-prim: division by zero"
divNatPrim left right = left `P.div` right

modNatPrim :: Natural -> Natural -> Natural
modNatPrim _ 0 = P.error "mod-nat-prim: modulo by zero"
modNatPrim left right = left `P.mod` right

ltNatPrim :: Natural -> Natural -> P.Bool
ltNatPrim = (P.<)

leNatPrim :: Natural -> Natural -> P.Bool
leNatPrim = (P.<=)

gtNatPrim :: Natural -> Natural -> P.Bool
gtNatPrim = (P.>)

geNatPrim :: Natural -> Natural -> P.Bool
geNatPrim = (P.>=)

eqNatPrim :: Natural -> Natural -> P.Bool
eqNatPrim = (P.==)

decideEqNatPrim :: Natural -> Natural -> (P.Bool, proof)
decideEqNatPrim left right = decisionResult (left P.== right)

eqStringPrim :: String -> String -> P.Bool
eqStringPrim = (P.==)

decideEqStringPrim :: String -> String -> (P.Bool, proof)
decideEqStringPrim left right = decisionResult (left P.== right)

decideEqBoolPrim :: P.Bool -> P.Bool -> (P.Bool, proof)
decideEqBoolPrim left right = decisionResult (left P.== right)

decideEqPairPrim
  :: (a -> a -> (P.Bool, proofA))
  -> (b -> b -> (P.Bool, proofB))
  -> (a, b)
  -> (a, b)
  -> (P.Bool, proof)
decideEqPairPrim eqA eqB (a1, b1) (a2, b2) =
  let (okA, _) = eqA a1 a2
   in if okA
        then
          let (okB, _) = eqB b1 b2
           in decisionResult okB
        else decisionResult False

decideEqListPrim
  :: (a -> a -> (P.Bool, proof))
  -> [a]
  -> [a]
  -> (P.Bool, proofOut)
decideEqListPrim eqA = go
  where
    go [] [] = decisionResult True
    go (x:xs) (y:ys) =
      let (ok, _) = eqA x y
       in if ok then go xs ys else decisionResult False
    go _ _ = decisionResult False

concatStringPrim :: String -> String -> String
concatStringPrim = T.append

stringLengthPrim :: String -> Natural
stringLengthPrim = P.fromIntegral . T.length

stringToListPrim :: String -> List String
stringToListPrim = map T.singleton . T.unpack

charCodePrim :: String -> Natural
charCodePrim text =
  case T.unpack text of
    [c] -> P.toInteger (ord c)
    _ -> P.error "char-code-prim expects a single-character string"

charFromCodePrim :: Natural -> String
charFromCodePrim code
  | code < 0 = P.error "char-from-code-prim expects a valid Unicode codepoint"
  | code > 0x10FFFF = P.error "char-from-code-prim expects a valid Unicode codepoint"
  | otherwise = T.singleton (chr (P.fromInteger code))

naturalToPeanoPrim :: Natural -> List Unit
naturalToPeanoPrim n =
  replicate (P.fromInteger n) ()

natToStringPrim :: Natural -> String
natToStringPrim n = T.pack (show n)

errorPrim :: String -> a
errorPrim message = P.error (T.unpack message)

printPrim :: String -> Comp Unit
printPrim message = Comp (writeOutput message)

captureOutputPrim :: Comp a -> Comp (List String, a)
captureOutputPrim (Comp action) = Comp $ do
  modifyIORef' outputCapture ([]:)
  resultOrEx <- try action
  stack <- readIORef outputCapture
  case stack of
    [] -> P.error "capture-output-prim stack empty"
    (buf:rest) -> do
      writeIORef outputCapture rest
      case resultOrEx of
        Left (ex :: SomeException) -> throwIO ex
        Right result -> pure (reverse buf, result)

foreverPrim :: Comp Unit -> Comp Unit
foreverPrim (Comp action) =
  Comp $ let loop = action P.>> loop in loop

assertHitPrim :: Comp Unit
assertHitPrim = Comp $ do
  modifyIORef' assertionCounter (P.+ 1)
  pure ()

getLinePrim :: Comp String
getLinePrim = Comp $ do
  line <- TIO.getLine
  pure (line <> T.pack "\n")

cliArgsPrim :: Comp (List String)
cliArgsPrim = Comp $ do
  args <- getArgs
  pure (map T.pack args)

currentDirectoryPrim :: Comp String
currentDirectoryPrim = Comp $ do
  dir <- getCurrentDirectory
  pure (T.pack dir)

timeNowPrim :: Comp Natural
timeNowPrim = Comp $ do
  t <- getPOSIXTime
  let micros = floor (t * 1000000)
  pure micros

readFilePrim :: String -> Comp String
readFilePrim path =
  Comp (TIO.readFile (T.unpack path))

writeFilePrim :: String -> String -> Comp Unit
writeFilePrim path contents =
  Comp (TIO.writeFile (T.unpack path) contents >> pure ())

shellPrim :: String -> Comp String
shellPrim cmd =
  Comp $ do
    (_exitCode, stdoutText, stderrText) <- readCreateProcessWithExitCode (shell (T.unpack cmd)) ""
    pure (T.pack (stdoutText ++ stderrText))

appendFilePrim :: String -> String -> Comp Unit
appendFilePrim path contents =
  Comp (TIO.appendFile (T.unpack path) contents >> pure ())

copyFilePrim :: String -> String -> Comp Unit
copyFilePrim src dest =
  Comp $ do
    let srcPath = T.unpack src
        destPath = T.unpack dest
    isDir <- doesDirectoryExist srcPath
    if isDir
      then P.error "copy-file-prim expects a file source"
      else copyFile srcPath destPath >> pure ()

copyTreePrim :: String -> String -> Comp Unit
copyTreePrim src dest =
  Comp $ do
    copyTree (T.unpack src) (T.unpack dest)
    pure ()

renamePathPrim :: String -> String -> Comp Unit
renamePathPrim src dest =
  Comp $ do
    let srcPath = T.unpack src
        destPath = T.unpack dest
    isDir <- doesDirectoryExist srcPath
    if isDir
      then renameDirectory srcPath destPath >> pure ()
      else do
        isFile <- doesFileExist srcPath
        if isFile
          then renameFile srcPath destPath >> pure ()
          else P.error "rename-path-prim expects an existing path"

listDirPrim :: String -> Comp (List String)
listDirPrim path =
  Comp $ do
    entries <- listDirectory (T.unpack path)
    pure (map T.pack entries)

pathExistsPrim :: String -> Comp P.Bool
pathExistsPrim path =
  Comp (doesPathExist (T.unpack path))

isDirectoryPrim :: String -> Comp P.Bool
isDirectoryPrim path =
  Comp (doesDirectoryExist (T.unpack path))

isFilePrim :: String -> Comp P.Bool
isFilePrim path =
  Comp (doesFileExist (T.unpack path))

makeDirectoryPrim :: String -> Comp Unit
makeDirectoryPrim path =
  Comp (createDirectoryIfMissing True (T.unpack path) >> pure ())

removeFilePrim :: String -> Comp Unit
removeFilePrim path =
  Comp $ do
    let path' = T.unpack path
    isDir <- doesDirectoryExist path'
    if isDir
      then P.error "remove-file-prim expects a file path"
      else do
        exists <- doesFileExist path'
        if exists then removeFile path' else pure ()
        pure ()

removeDirectoryPrim :: String -> Comp Unit
removeDirectoryPrim path =
  Comp $ do
    let path' = T.unpack path
    isDir <- doesDirectoryExist path'
    if isDir
      then removeDirectoryRecursive path' >> pure ()
      else do
        isFile <- doesFileExist path'
        if isFile
          then P.error "remove-directory-prim expects a directory path"
          else pure ()

walkPrim :: String -> Comp (List (Pair String P.Bool))
walkPrim path =
  Comp $ do
    let root = T.unpack path
    isDir <- doesDirectoryExist root
    isFile <- doesFileExist root
    entries <-
      if isDir
        then walkFrom root
        else if isFile
          then pure [(root, False)]
          else P.error "walk-prim expects an existing path"
    pure (map (\(p, isDirEntry) -> (T.pack p, isDirEntry)) entries)

walkFilterPrim :: String -> String -> List String -> Comp (List String)
walkFilterPrim root suffix skips =
  Comp $ do
    let rootPath = T.unpack root
        suffixStr = T.unpack suffix
        skipPaths = map T.unpack skips
    isDir <- doesDirectoryExist rootPath
    isFile <- doesFileExist rootPath
    entries <-
      if isDir
        then if shouldSkip skipPaths rootPath
          then pure []
          else walkFromFiltered rootPath skipPaths suffixStr
        else if isFile
          then pure (if suffixStr `isSuffixOf` rootPath then [rootPath] else [])
          else P.error "walk-filter-prim expects an existing path"
    pure (map T.pack entries)

statPrim :: String -> Comp (Pair String (Pair Natural Natural))
statPrim path =
  Comp $ do
    let path' = T.unpack path
    isDir <- doesDirectoryExist path'
    isFile <- doesFileExist path'
    if not (isDir || isFile)
      then P.error "stat-prim expects an existing path"
      else do
        mtime <- getModificationTime path'
        let mtimeNat :: Natural
            mtimeNat = floor (utcTimeToPOSIXSeconds mtime)
        size <- if isFile then getFileSize path' else pure 0
        let kind = if isDir then T.pack "directory" else T.pack "file"
        pure (kind, (fromIntegral size, fromIntegral mtimeNat))

onSignalPrim :: String -> Comp Unit -> Comp Unit
onSignalPrim name (Comp handler) =
  case signalFromName name of
    Nothing -> Comp (P.error "on-signal-prim expects a known signal name")
    Just sig ->
      Comp $ do
        caller <- myThreadId
        _ <- Signals.installHandler sig (Signals.Catch (runHandler caller handler)) Nothing
        pure ()
  where
    runHandler caller action = do
      result <- try action
      case result of
        Left (ex :: SomeException) -> do
          throwTo caller ex
          case fromException ex of
            Just code -> exitImmediately code
            Nothing -> exitImmediately (ExitFailure 1)
        Right _ -> pure ()

tcpListenPrim :: Natural -> Comp Listener
tcpListenPrim port =
  Comp $ NS.withSocketsDo $ do
    let hints =
          NS.defaultHints
            { NS.addrFlags = [NS.AI_PASSIVE]
            , NS.addrSocketType = NS.Stream
            }
        service = Just (show port)
    addrInfos <- NS.getAddrInfo (Just hints) Nothing service
    case addrInfos of
      [] -> P.error "tcp-listen-prim: no addr info"
      (addr:_) -> do
        sock <- NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr)
        NS.setSocketOption sock NS.ReuseAddr 1
        NS.bind sock (NS.addrAddress addr)
        NS.listen sock 10
        pure (Listener sock)

tcpAcceptPrim :: Listener -> Comp Socket
tcpAcceptPrim (Listener listener) =
  Comp $ NS.withSocketsDo $ do
    (conn, _peer) <- runBlocking (NS.accept listener)
    pure (Socket conn)

tcpRecvPrim :: Socket -> Comp String
tcpRecvPrim (Socket sock) =
  Comp $ do
    chunk <- runBlocking (NSB.recv sock 4096)
    pure (TE.decodeUtf8 chunk)

tcpSendPrim :: Socket -> String -> Comp Unit
tcpSendPrim (Socket sock) payload =
  Comp $ do
    runBlocking (NSB.sendAll sock (TE.encodeUtf8 payload))
    pure ()

tcpClosePrim :: Socket -> Comp Unit
tcpClosePrim (Socket sock) =
  Comp $ NS.close sock >> pure ()

tcpCloseListenerPrim :: Listener -> Comp Unit
tcpCloseListenerPrim (Listener listener) =
  Comp $ NS.close listener >> pure ()

tcpSelectListenerPrim :: Listener -> Natural -> Comp P.Bool
tcpSelectListenerPrim (Listener listener) micros =
  Comp $ do
    let waitMicros = microsToInt micros
    waitStm <- NS.waitReadSocketSTM listener
    ready <- timeout waitMicros (atomically waitStm)
    pure (maybe False (const True) ready)

tcpSelectSocketPrim :: Socket -> Natural -> Comp P.Bool
tcpSelectSocketPrim (Socket sock) micros =
  Comp $ do
    let waitMicros = microsToInt micros
    waitStm <- NS.waitReadSocketSTM sock
    ready <- timeout waitMicros (atomically waitStm)
    pure (maybe False (const True) ready)

sleepPrim :: Natural -> Comp Unit
sleepPrim micros =
  Comp $ do
    threadDelay (microsToInt micros)
    pure ()

timeoutPrim :: Natural -> Comp a -> Comp (Option a)
timeoutPrim micros (Comp action) =
  Comp $ do
    result <- timeout (microsToInt micros) action
    pure result

panicPrim :: String -> Comp a
panicPrim message =
  Comp (P.ioError (P.userError (T.unpack message)))

decisionResult :: P.Bool -> (P.Bool, proof)
decisionResult ok = (ok, unsafeCoerce ())

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
      else P.error "copy-tree-prim expects an existing path"

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

walkFromFiltered :: FilePath -> [FilePath] -> FilePath -> IO [FilePath]
walkFromFiltered dir skips suffix = do
  entries <- listDirectory dir
  let fullPaths = map (dir </>) entries
  parts <- mapM (walkPathFiltered skips suffix) fullPaths
  pure (concat parts)

walkPathFiltered :: [FilePath] -> FilePath -> FilePath -> IO [FilePath]
walkPathFiltered skips suffix path = do
  let skip = shouldSkip skips path
  isDir <- doesDirectoryExist path
  if isDir
    then if skip then pure [] else walkFromFiltered path skips suffix
    else do
      isFile <- doesFileExist path
      if isFile && suffix `isSuffixOf` path
        then pure [path]
        else pure []

shouldSkip :: [FilePath] -> FilePath -> Bool
shouldSkip skips path = any (\pfx -> isSkipPrefix pfx path) skips
  where
    isSkipPrefix pfx target =
      target == pfx || (pfx ++ [pathSeparator]) `isPrefixOf` target

signalFromName :: Text -> Maybe Signal
signalFromName name =
  case T.unpack (T.toUpper (T.strip name)) of
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

dictionaryEmptyPrim :: Dictionary k v
dictionaryEmptyPrim = Dictionary Map.empty 0

dictionaryInsertPrim
  :: (k -> Natural)
  -> (k -> k -> Bool)
  -> k
  -> v
  -> Dictionary k v
  -> Dictionary k v
dictionaryInsertPrim hashFn eqFn key val (Dictionary buckets size) =
  let h = hashFn key
      bucket = Map.findWithDefault [] h buckets
      (found, bucket') = insertBucket eqFn key val bucket
      size' = if found then size else size + 1
      buckets' = Map.insert h bucket' buckets
  in Dictionary buckets' size'

dictionaryLookupPrim
  :: (k -> Natural)
  -> (k -> k -> Bool)
  -> k
  -> Dictionary k v
  -> Option v
dictionaryLookupPrim hashFn eqFn key (Dictionary buckets _) =
  let h = hashFn key
      bucket = Map.findWithDefault [] h buckets
  in lookupBucket eqFn key bucket

dictionaryRemovePrim
  :: (k -> Natural)
  -> (k -> k -> Bool)
  -> k
  -> Dictionary k v
  -> Dictionary k v
dictionaryRemovePrim hashFn eqFn key (Dictionary buckets size) =
  let h = hashFn key
      bucket = Map.findWithDefault [] h buckets
      (removed, bucket') = removeBucket eqFn key bucket
      size' = if removed then max 0 (size - 1) else size
      buckets' = if null bucket'
        then Map.delete h buckets
        else Map.insert h bucket' buckets
  in Dictionary buckets' size'

dictionarySizePrim :: Dictionary k v -> Natural
dictionarySizePrim (Dictionary _ size) =
  fromIntegral size

dictionaryToListPrim :: Dictionary k v -> List (Pair k v)
dictionaryToListPrim (Dictionary buckets _) =
  concatMap snd (Map.toList buckets)

insertBucket :: (k -> k -> Bool) -> k -> v -> List (Pair k v) -> (Bool, List (Pair k v))
insertBucket eqFn key val entries =
  case entries of
    [] -> (False, [(key, val)])
    (k, v):rest ->
      if eqFn key k
        then (True, (key, val) : rest)
        else
          let (found, rest') = insertBucket eqFn key val rest
          in (found, (k, v) : rest')

lookupBucket :: (k -> k -> Bool) -> k -> List (Pair k v) -> Option v
lookupBucket eqFn key entries =
  case entries of
    [] -> Nothing
    (k, v):rest ->
      if eqFn key k
        then Just v
        else lookupBucket eqFn key rest

removeBucket :: (k -> k -> Bool) -> k -> List (Pair k v) -> (Bool, List (Pair k v))
removeBucket eqFn key entries =
  case entries of
    [] -> (False, [])
    (k, v):rest ->
      if eqFn key k
        then (True, rest)
        else
          let (found, rest') = removeBucket eqFn key rest
          in (found, (k, v) : rest')

validatePrim :: String -> Bool
validatePrim source =
  unsafePerformIO $ do
    let normalized =
          if T.isSuffixOf (T.pack "\n") source
            then source
            else source <> T.pack "\n"
    tempDir <- getTemporaryDirectory
    (path, handle) <- openTempFile tempDir "locque_validate.lq"
    TIO.hPutStr handle normalized
    hClose handle
    let outPath = path <> ".lqs"
    (exitCode, _out, _err) <-
      readCreateProcessWithExitCode
        (proc "locque-interpreter" ["emit-lqs", path, outPath])
        ""
    _ <- try (removeFile path) :: IO (Either SomeException ())
    _ <- try (removeFile outPath) :: IO (Either SomeException ())
    pure (case exitCode of
      ExitSuccess -> True
      ExitFailure _ -> False)

outputCapture :: IORef [List String]
outputCapture = unsafePerformIO (newIORef [])
{-# NOINLINE outputCapture #-}

assertionCounter :: IORef Natural
assertionCounter = unsafePerformIO (newIORef 0)
{-# NOINLINE assertionCounter #-}

writeOutput :: Text -> IO ()
writeOutput line = do
  stack <- readIORef outputCapture
  case stack of
    [] -> TIO.putStrLn line
    (buf:rest) -> writeIORef outputCapture ((line : buf) : rest)
