{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Exception (SomeException, try)
import Data.Char (isDigit, isHexDigit)
import Data.Foldable (toList)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Exit (exitSuccess)
import System.IO (BufferMode(NoBuffering), hFlush, hIsEOF, hSetBinaryMode, hSetBuffering, stdin, stdout)
import System.FilePath ((</>), (<.>))

import qualified Data.Aeson as A
import Data.Aeson ((.=), Value(..), object)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL

import AST (Module(..), Import(..), Open(..))
import Parser (parseMExprFile)
import SmythConfig (findSmythfile)
import qualified TypeChecker as TC
import SourceLoc (SrcLoc(..))
import Utils (modNameToPath)

data ServerState = ServerState
  { stateRoot :: FilePath
  , stateDocs :: Map.Map Text Text
  }

main :: IO ()
main = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  root <- findProjectRoot
  ref <- newIORef (ServerState root Map.empty)
  loop ref

findProjectRoot :: IO FilePath
findProjectRoot = do
  maybeRoot <- findSmythfile
  case maybeRoot of
    Just root -> pure root
    Nothing -> getCurrentDirectory

loop :: IORef ServerState -> IO ()
loop ref = do
  msg <- readMessage
  case msg of
    Nothing -> pure ()
    Just value -> do
      handleMessage ref value
      loop ref

readMessage :: IO (Maybe Value)
readMessage = do
  eof <- hIsEOF stdin
  if eof
    then pure Nothing
    else do
      headers <- readHeaders
      case lookupHeader "content-length" headers of
        Nothing -> pure Nothing
        Just lenText ->
          case readMaybeInt lenText of
            Nothing -> pure Nothing
            Just len -> do
              body <- BS.hGet stdin len
              case A.decodeStrict' body of
                Nothing -> pure Nothing
                Just value -> pure (Just value)

readHeaders :: IO [(Text, Text)]
readHeaders = go []
  where
    go acc = do
      line <- BS8.hGetLine stdin
      let trimmed = BS8.takeWhile (/= '\r') line
      if BS8.null trimmed
        then pure acc
        else case BS8.break (== ':') trimmed of
          (name, rest) ->
            let value = BS8.dropWhile (== ' ') (BS8.drop 1 rest)
                nameText = T.toLower (TE.decodeUtf8 name)
                valueText = TE.decodeUtf8 value
            in go ((nameText, valueText) : acc)

lookupHeader :: Text -> [(Text, Text)] -> Maybe Text
lookupHeader name headers =
  lookup name headers

readMaybeInt :: Text -> Maybe Int
readMaybeInt t =
  if T.all isDigit t && not (T.null t)
    then Just (read (T.unpack t))
    else Nothing

handleMessage :: IORef ServerState -> Value -> IO ()
handleMessage ref value = case value of
  Object obj ->
    case getTextField "method" obj of
      Just "initialize" -> do
        let result = object
              [ "capabilities" .= object
                  [ "textDocumentSync" .= object
                      [ "openClose" .= True
                      , "change" .= (1 :: Int)
                      ]
                  , "definitionProvider" .= True
                  ]
              , "serverInfo" .= object
                  [ "name" .= ("locque-lsp" :: Text)
                  , "version" .= ("0.1.0" :: Text)
                  ]
              ]
        respond obj result
      Just "shutdown" -> respond obj Null
      Just "exit" -> exitSuccess
      Just "textDocument/didOpen" -> handleDidOpen ref obj
      Just "textDocument/didChange" -> handleDidChange ref obj
      Just "textDocument/didClose" -> handleDidClose ref obj
      Just "textDocument/definition" -> handleDefinition ref obj
      Just "textDocument/documentSymbol" -> handleDocumentSymbol ref obj
      _ -> pure ()
  _ -> pure ()

handleDidOpen :: IORef ServerState -> KM.KeyMap Value -> IO ()
handleDidOpen ref obj =
  case getObjectField "params" obj >>= getObjectField "textDocument" of
    Nothing -> pure ()
    Just docObj -> do
      case (getTextField "uri" docObj, getTextField "text" docObj) of
        (Just uri, Just contents) -> do
          updateDoc ref uri contents
          publishDiagnostics ref uri contents
        _ -> pure ()

handleDidChange :: IORef ServerState -> KM.KeyMap Value -> IO ()
handleDidChange ref obj =
  case getObjectField "params" obj of
    Nothing -> pure ()
    Just paramsObj ->
      case getTextField "uri" =<< getObjectField "textDocument" paramsObj of
        Nothing -> pure ()
        Just uri ->
          case getArrayField "contentChanges" paramsObj >>= firstChangeText of
            Nothing -> pure ()
            Just contents -> do
              updateDoc ref uri contents
              publishDiagnostics ref uri contents

handleDidClose :: IORef ServerState -> KM.KeyMap Value -> IO ()
handleDidClose ref obj =
  case getObjectField "params" obj >>= getObjectField "textDocument" >>= getTextField "uri" of
    Nothing -> pure ()
    Just uri -> do
      clearDoc ref uri
      sendDiagnostics uri []

handleDefinition :: IORef ServerState -> KM.KeyMap Value -> IO ()
handleDefinition ref obj =
  case getObjectField "params" obj of
    Nothing -> respond obj (Array mempty)
    Just paramsObj ->
      case ( getTextField "uri" =<< getObjectField "textDocument" paramsObj
           , getPositionField "position" paramsObj
           ) of
        (Just uri, Just pos) -> do
          state <- readIORef ref
          locations <- findDefinitionLocations state uri pos
          respond obj (A.toJSON (map locationToValue locations))
        _ -> respond obj (Array mempty)

handleDocumentSymbol :: IORef ServerState -> KM.KeyMap Value -> IO ()
handleDocumentSymbol ref obj =
  case getObjectField "params" obj >>= getObjectField "textDocument" >>= getTextField "uri" of
    Nothing -> respond obj (Array mempty)
    Just uri -> do
      state <- readIORef ref
      contents <- getDocumentText state uri
      case contents of
        Nothing -> respond obj (Array mempty)
        Just text -> do
          let symbols = documentSymbolsForText text
          respond obj (A.toJSON (map symbolToValue symbols))

updateDoc :: IORef ServerState -> Text -> Text -> IO ()
updateDoc ref uri contents = do
  state <- readIORef ref
  let docs' = Map.insert uri contents (stateDocs state)
  writeIORef ref state { stateDocs = docs' }

clearDoc :: IORef ServerState -> Text -> IO ()
clearDoc ref uri = do
  state <- readIORef ref
  let docs' = Map.delete uri (stateDocs state)
  writeIORef ref state { stateDocs = docs' }

publishDiagnostics :: IORef ServerState -> Text -> Text -> IO ()
publishDiagnostics ref uri contents = do
  state <- readIORef ref
  let filePath = fromMaybe "<memory>" (uriToFilePath uri)
  diags <- computeDiagnostics (stateRoot state) filePath contents
  sendDiagnostics uri diags

computeDiagnostics :: FilePath -> FilePath -> Text -> IO [Value]
computeDiagnostics projectRoot path contents =
  case parseMExprFile path contents of
    Left parseErr ->
      pure [diagnosticValue (Range (Position 0 0) (Position 0 0)) (T.pack parseErr)]
    Right m -> do
      result <- try (TC.typeCheckModuleWithImports projectRoot contents m)
        :: IO (Either SomeException (Either TC.TypeError TC.TypeEnv))
      case result of
        Left err ->
          pure [diagnosticValue (Range (Position 0 0) (Position 0 0)) (T.pack (show err))]
        Right (Left typeErr) ->
          let loc = typeErrorLoc typeErr
              range = locToRange loc
          in pure [diagnosticValue range (T.pack (show typeErr))]
        Right (Right _) ->
          pure []

typeErrorLoc :: TC.TypeError -> SrcLoc
typeErrorLoc err = case err of
  TC.VarNotInScope loc _ _ -> loc
  TC.TypeMismatch loc _ _ -> loc
  TC.CannotApply loc _ _ -> loc
  TC.NotAFunction loc _ -> loc
  TC.NotAComputation loc _ -> loc
  TC.ExpectedType loc _ -> loc
  TC.InvalidLift loc _ _ -> loc
  TC.ExpectedThereExists loc _ -> loc
  TC.ExpectedEquality loc _ -> loc
  TC.MatchCaseError loc _ -> loc
  TC.TypeclassError loc _ -> loc
  TC.RecursionError loc _ -> loc

data Position = Position Int Int
  deriving (Eq, Ord)

data Range = Range Position Position

data Location = Location Text Position Position
  deriving (Eq, Ord)

data Symbol = Symbol Text Int Range Range [Symbol]

locToRange :: SrcLoc -> Range
locToRange loc = case loc of
  NoLoc -> Range (Position 0 0) (Position 0 0)
  SrcLoc _ line col ->
    let line' = max 0 (line - 1)
        col' = max 0 (col - 1)
    in Range (Position line' col') (Position line' col')

diagnosticValue :: Range -> Text -> Value
diagnosticValue range msg =
  object
    [ "range" .= rangeToValue range
    , "severity" .= (1 :: Int)
    , "source" .= ("locque" :: Text)
    , "message" .= msg
    ]

rangeToValue :: Range -> Value
rangeToValue (Range start end) =
  object
    [ "start" .= positionToValue start
    , "end" .= positionToValue end
    ]

positionToValue :: Position -> Value
positionToValue (Position line col) =
  object
    [ "line" .= line
    , "character" .= col
    ]

locationToValue :: Location -> Value
locationToValue (Location uri start end) =
  object
    [ "uri" .= uri
    , "range" .= rangeToValue (Range start end)
    ]

symbolToValue :: Symbol -> Value
symbolToValue (Symbol name kind range selection children) =
  object $
    [ "name" .= name
    , "kind" .= kind
    , "range" .= rangeToValue range
    , "selectionRange" .= rangeToValue selection
    ] ++ if null children then [] else ["children" .= map symbolToValue children]

sendDiagnostics :: Text -> [Value] -> IO ()
sendDiagnostics uri diags = do
  let params = object
        [ "uri" .= uri
        , "diagnostics" .= diags
        ]
      msg = object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "method" .= ("textDocument/publishDiagnostics" :: Text)
        , "params" .= params
        ]
  sendMessage msg

respond :: KM.KeyMap Value -> Value -> IO ()
respond obj result =
  case KM.lookup (K.fromText "id") obj of
    Nothing -> pure ()
    Just reqId -> do
      let msg = object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "id" .= reqId
            , "result" .= result
            ]
      sendMessage msg

sendMessage :: Value -> IO ()
sendMessage value = do
  let payload = A.encode value
      header = "Content-Length: " <> BS8.pack (show (BL.length payload)) <> "\r\n\r\n"
  BS.hPut stdout header
  BL.hPut stdout payload
  hFlush stdout

getObjectField :: Text -> KM.KeyMap Value -> Maybe (KM.KeyMap Value)
getObjectField name obj =
  case KM.lookup (K.fromText name) obj of
    Just (Object inner) -> Just inner
    _ -> Nothing

getArrayField :: Text -> KM.KeyMap Value -> Maybe [Value]
getArrayField name obj =
  case KM.lookup (K.fromText name) obj of
    Just (Array arr) -> Just (toList arr)
    _ -> Nothing

getIntField :: Text -> KM.KeyMap Value -> Maybe Int
getIntField name obj =
  case KM.lookup (K.fromText name) obj of
    Just val -> case A.fromJSON val of
      A.Success n -> Just n
      _ -> Nothing
    _ -> Nothing

getTextField :: Text -> KM.KeyMap Value -> Maybe Text
getTextField name obj =
  case KM.lookup (K.fromText name) obj of
    Just (String t) -> Just t
    _ -> Nothing

getPositionField :: Text -> KM.KeyMap Value -> Maybe Position
getPositionField name obj =
  case getObjectField name obj of
    Nothing -> Nothing
    Just posObj -> do
      line <- getIntField "line" posObj
      col <- getIntField "character" posObj
      pure (Position line col)

firstChangeText :: [Value] -> Maybe Text
firstChangeText changes = case changes of
  [] -> Nothing
  (Object obj : _) -> getTextField "text" obj
  _ -> Nothing

uriToFilePath :: Text -> Maybe FilePath
uriToFilePath uri =
  if "file://" `T.isPrefixOf` uri
    then Just (T.unpack (decodeUriPath (T.drop 7 uri)))
    else Nothing

filePathToUri :: FilePath -> Text
filePathToUri path =
  "file://" <> encodeUriPath (T.pack path)

encodeUriPath :: Text -> Text
encodeUriPath = T.concatMap encodeChar
  where
    encodeChar c
      | isUnreserved c = T.singleton c
      | otherwise = T.pack (percentEncode c)
    isUnreserved c =
      (c >= 'A' && c <= 'Z') ||
      (c >= 'a' && c <= 'z') ||
      (c >= '0' && c <= '9') ||
      c == '-' || c == '.' || c == '_' || c == '~' || c == '/'
    percentEncode c =
      let n = fromEnum c
          hi = hexDigit (n `div` 16)
          lo = hexDigit (n `mod` 16)
      in ['%', hi, lo]
    hexDigit n
      | n < 10 = toEnum (fromEnum '0' + n)
      | otherwise = toEnum (fromEnum 'A' + (n - 10))

decodeUriPath :: Text -> Text
decodeUriPath = go T.empty
  where
    go acc input = case T.uncons input of
      Nothing -> acc
      Just ('%', rest) ->
        let (hex, tailText) = T.splitAt 2 rest
        in case parseHexByte hex of
            Just c -> go (T.snoc acc c) tailText
            Nothing -> go (T.snoc acc '%') rest
      Just (ch, rest) -> go (T.snoc acc ch) rest

parseHexByte :: Text -> Maybe Char
parseHexByte t =
  if T.length t == 2 && T.all isHexDigit t
    then Just (toEnum (hexValue t))
    else Nothing

hexValue :: Text -> Int
hexValue t = case map hexDigitValue (T.unpack t) of
  [a, b] -> a * 16 + b
  _ -> 0

hexDigitValue :: Char -> Int
hexDigitValue c
  | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
  | c >= 'a' && c <= 'f' = 10 + (fromEnum c - fromEnum 'a')
  | c >= 'A' && c <= 'F' = 10 + (fromEnum c - fromEnum 'A')
  | otherwise = 0

documentSymbolsForText :: Text -> [Symbol]
documentSymbolsForText text =
  scanSymbols 0 (T.lines text)
  where
    scanSymbols _ [] = []
    scanSymbols lineNum (line:rest) =
      case parseDefineLine lineNum line rest of
        Nothing -> scanSymbols (lineNum + 1) rest
        Just (sym, consumed, remaining) ->
          sym : scanSymbols (lineNum + 1 + consumed) remaining

parseDefineLine :: Int -> Text -> [Text] -> Maybe (Symbol, Int, [Text])
parseDefineLine lineNum line rest =
  let trimmed = T.stripStart line
  in if "define " `T.isPrefixOf` trimmed
      then case T.words trimmed of
        ("define" : _opacity : name : "as" : afterAs) ->
          let (kind, isData) = classifyDefine afterAs trimmed
              nameCol = findNameColumn name line
              lineLen = T.length line
              selection = Range (Position lineNum nameCol) (Position lineNum (nameCol + T.length name))
              range = Range (Position lineNum 0) (Position lineNum lineLen)
          in if isData
              then
                let (children, consumed, remaining) = parseDataBlock (lineNum + 1) rest
                    sym = Symbol name kind range selection children
                in Just (sym, consumed, remaining)
              else
                let sym = Symbol name kind range selection []
                in Just (sym, 0, rest)
        _ -> Nothing
      else Nothing

classifyDefine :: [Text] -> Text -> (Int, Bool)
classifyDefine afterAs lineText = case afterAs of
  ("data" : _) -> (symbolKindStruct, True)
  ("typeclass" : _) -> (symbolKindInterface, False)
  ("instance" : _) -> (symbolKindClass, False)
  ("function" : _) -> (symbolKindFunction, False)
  _ ->
    if " function " `T.isInfixOf` lineText
      then (symbolKindFunction, False)
      else (symbolKindVariable, False)

parseDataBlock :: Int -> [Text] -> ([Symbol], Int, [Text])
parseDataBlock startLine dataLines =
  go startLine dataLines [] 0
  where
    go _ [] acc consumed = (reverse acc, consumed, [])
    go lineNum (line:rest) acc consumed =
      let trimmed = T.stripStart line
      in if trimmed == "end"
          then (reverse acc, consumed + 1, rest)
          else
            let acc' = case parseCaseLine lineNum line of
                  Nothing -> acc
                  Just sym -> sym : acc
            in go (lineNum + 1) rest acc' (consumed + 1)

parseCaseLine :: Int -> Text -> Maybe Symbol
parseCaseLine lineNum line =
  let trimmed = T.stripStart line
  in if "case " `T.isPrefixOf` trimmed
      then case T.words trimmed of
        ("case" : name : _rest) ->
          let nameCol = findNameColumn name line
              lineLen = T.length line
              selection = Range (Position lineNum nameCol) (Position lineNum (nameCol + T.length name))
              range = Range (Position lineNum 0) (Position lineNum lineLen)
          in Just (Symbol name symbolKindConstructor range selection [])
        _ -> Nothing
      else Nothing

findNameColumn :: Text -> Text -> Int
findNameColumn name line =
  let (prefix, _) = T.breakOn name line
  in T.length prefix

symbolKindStruct :: Int
symbolKindStruct = 23

symbolKindInterface :: Int
symbolKindInterface = 11

symbolKindClass :: Int
symbolKindClass = 5

symbolKindFunction :: Int
symbolKindFunction = 12

symbolKindVariable :: Int
symbolKindVariable = 13

symbolKindConstructor :: Int
symbolKindConstructor = 9

findDefinitionLocations :: ServerState -> Text -> Position -> IO [Location]
findDefinitionLocations state uri pos = do
  contents <- getDocumentText state uri
  case contents of
    Nothing -> pure []
    Just text -> do
      let filePath = fromMaybe "<memory>" (uriToFilePath uri)
      case identifierAt text pos of
        Nothing -> pure []
        Just name -> do
          moduleInfo <- parseModuleInfo (stateRoot state) filePath text
          localLocs <- pure (findInText uri text name)
          qualifiedLocs <- findQualified moduleInfo name
          openLocs <- findOpen moduleInfo name
          pure (dedupeLocations (localLocs ++ qualifiedLocs ++ openLocs))

dedupeLocations :: [Location] -> [Location]
dedupeLocations locs =
  Map.elems (Map.fromList [(loc, loc) | loc <- locs])

getDocumentText :: ServerState -> Text -> IO (Maybe Text)
getDocumentText state uri =
  case Map.lookup uri (stateDocs state) of
    Just text -> pure (Just text)
    Nothing -> case uriToFilePath uri of
      Nothing -> pure Nothing
      Just path -> do
        exists <- doesFileExist path
        if exists
          then Just <$> TIO.readFile path
          else pure Nothing

data ModuleInfo = ModuleInfo
  { infoImports :: Map.Map Text Text
  , infoModules :: [Text]
  , infoOpens :: Map.Map Text [Text]
  , infoRoot :: FilePath
  }

parseModuleInfo :: FilePath -> FilePath -> Text -> IO ModuleInfo
parseModuleInfo projectRoot path text =
  case parseMExprFile path text of
    Left _ -> pure (ModuleInfo Map.empty [] Map.empty projectRoot)
    Right (Module _ imports opens _) ->
      let aliasMap = Map.fromList [(impAlias i, impModule i) | i <- imports]
          openMap = foldl insertOpen Map.empty opens
      in pure (ModuleInfo aliasMap (map impModule imports) openMap projectRoot)
  where
    insertOpen acc open =
      foldl (addName (openModule open)) acc (openNames open)
    addName alias acc name =
      Map.insertWith (++) name [alias] acc

identifierAt :: Text -> Position -> Maybe Text
identifierAt contents (Position line col) = do
  let ls = T.lines contents
  if line < 0 || line >= length ls
    then Nothing
    else do
      let lineText = ls !! line
          len = T.length lineText
      if len == 0
        then Nothing
        else do
          let col' = max 0 (min col len)
              idx =
                if col' == len
                  then len - 1
                  else col'
              idx' =
                if idx >= 0 && idx < len && isIdentChar (T.index lineText idx)
                  then idx
                  else if col' > 0 && isIdentChar (T.index lineText (col' - 1))
                    then col' - 1
                    else -1
          if idx' < 0
            then Nothing
            else
              let start = moveLeft lineText idx'
                  end = moveRight lineText idx' + 1
              in Just (T.take (end - start) (T.drop start lineText))
  where
    isIdentChar c =
      (c >= 'a' && c <= 'z') ||
      (c >= 'A' && c <= 'Z') ||
      (c >= '0' && c <= '9') ||
      c == '_' || c == '-' || c == ':'
    moveLeft lineText i =
      if i > 0 && isIdentChar (T.index lineText (i - 1))
        then moveLeft lineText (i - 1)
        else i
    moveRight lineText i =
      let len = T.length lineText
      in if i + 1 < len && isIdentChar (T.index lineText (i + 1))
          then moveRight lineText (i + 1)
          else i

findInText :: Text -> Text -> Text -> [Location]
findInText uri contents name =
  case findDefinitionInText contents name of
    Nothing -> []
    Just (line, col) ->
      [Location uri (Position line col) (Position line (col + T.length name))]

findDefinitionInText :: Text -> Text -> Maybe (Int, Int)
findDefinitionInText contents name =
  case findLineWith definePatterns of
    Just loc -> Just loc
    Nothing -> findLineWith [casePattern]
  where
    linesList = T.lines contents
    definePatterns =
      [ "define transparent " <> name <> " as"
      , "define opaque " <> name <> " as"
      , "define " <> name <> " as"
      ]
    casePattern = "case " <> name <> " of-type"
    findLineWith pats = go 0 linesList pats
    go _ [] _ = Nothing
    go idx (lineText:rest) pats =
      case firstMatch lineText pats of
        Nothing -> go (idx + 1) rest pats
        Just col -> Just (idx, col)
    firstMatch lineText pats =
      case filter (`T.isInfixOf` lineText) pats of
        [] -> Nothing
        _ ->
          let (prefix, _) = T.breakOn name lineText
          in Just (T.length prefix)

findQualified :: ModuleInfo -> Text -> IO [Location]
findQualified info name =
  case T.splitOn "::" name of
    [_] -> pure []
    segments ->
      case firstResolved (prefixes segments) of
        Nothing -> pure []
        Just (moduleName, targetName) -> findInModule info moduleName targetName
  where
    prefixes segments =
      let headSegments = init segments
          joined = scanl1 (\a b -> a <> "::" <> b) headSegments
      in reverse joined
    firstResolved [] = Nothing
    firstResolved (prefix:rest) =
      case Map.lookup prefix (infoImports info) of
        Just moduleName -> Just (moduleName, dropPrefix prefix name)
        Nothing ->
          if prefix `elem` infoModules info
            then Just (prefix, dropPrefix prefix name)
            else firstResolved rest
    dropPrefix prefix full =
      fromMaybe full (T.stripPrefix (prefix <> "::") full)

findOpen :: ModuleInfo -> Text -> IO [Location]
findOpen info name =
  case T.splitOn "::" name of
    [] -> pure []
    [unqualified] ->
      case Map.lookup unqualified (infoOpens info) of
        Nothing -> pure []
        Just aliases ->
          concat <$> mapM (findOpenAlias unqualified) aliases
    (prefix:_) ->
      case Map.lookup prefix (infoOpens info) of
        Nothing -> pure []
        Just aliases ->
          concat <$> mapM (findOpenAlias name) aliases
  where
    findOpenAlias target alias =
      case Map.lookup alias (infoImports info) of
        Just moduleName -> findInModule info moduleName target
        Nothing -> pure []

findInModule :: ModuleInfo -> Text -> Text -> IO [Location]
findInModule info moduleName targetName = do
  mFile <- resolveModuleFile info moduleName
  case mFile of
    Nothing -> pure []
    Just (uri, contents) ->
      pure (findInText uri contents targetName)

resolveModuleFile :: ModuleInfo -> Text -> IO (Maybe (Text, Text))
resolveModuleFile info moduleName = do
  let modPath = modNameToPath moduleName
      basePath =
        if "test/" `isPrefixOf` modPath
          then infoRoot info </> modPath
          else infoRoot info </> "lib" </> modPath
      lqPath = basePath <.> "lq"
      lqsPath = basePath <.> "lqs"
  lqExists <- doesFileExist lqPath
  if lqExists
    then Just . (filePathToUri lqPath,) <$> TIO.readFile lqPath
    else do
      lqsExists <- doesFileExist lqsPath
      if lqsExists
        then Just . (filePathToUri lqsPath,) <$> TIO.readFile lqsPath
        else pure Nothing
