{-# LANGUAGE OverloadedStrings #-}

module Locque.Compiler.Codegen
  ( DebugAnnotation(..)
  , DebugInfo
  , EmitOptions(..)
  , defaultEmitOptions
  , emitModule
  , emitModuleWith
  ) where

import Data.Char (isAlphaNum, isLower, isUpper)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Locque.Compiler.Core
import Locque.Compiler.CoreErased
import Locque.Compiler.Erase

data DebugAnnotation = DebugAnnotation
  { debugSourceFile :: Maybe FilePath
  , debugSourceLine :: Maybe Int
  , debugType :: Maybe Text
  } deriving (Show, Read, Eq)

type DebugInfo = Map.Map Name DebugAnnotation

data EmitOptions = EmitOptions
  { emitDebugInfo :: DebugInfo
  } deriving (Show, Read, Eq)

defaultEmitOptions :: EmitOptions
defaultEmitOptions = EmitOptions
  { emitDebugInfo = Map.empty
  }

emitModule :: CoreModule -> Text
emitModule = emitModuleWith defaultEmitOptions

emitModuleWith :: EmitOptions -> CoreModule -> Text
emitModuleWith options coreModule =
  T.unlines (moduleHeader ++ concatMap (emitDeclWith options) (coreModuleDecls coreModule))
  where
    moduleHeader =
      [ "{-# LANGUAGE OverloadedStrings #-}"
      , "{-# LANGUAGE EmptyCase #-}"
      , "{-# LANGUAGE EmptyDataDecls #-}"
      , "module LocqueGen where"
      , "import Prelude hiding (String)"
      , "import LocqueRuntime"
      , "import Unsafe.Coerce (unsafeCoerce)"
      , ""
      ]

emitDeclWith :: EmitOptions -> CoreDecl -> [Text]
emitDeclWith options decl =
  case decl of
    CoreDef name _ value ->
      emitWithDebug options name
        [ emitValueBinding name (eraseValue value)
        , ""
        ]
    CoreDefComp name _ comp ->
      emitWithDebug options name
        [ emitCompBinding name (eraseComp comp)
        , ""
        ]
    CoreData dataDecl ->
      if isBuiltinDataDecl dataDecl
        then []
        else emitWithDebug options (dataName dataDecl)
          [ emitDataDecl dataDecl
          , ""
          ]

emitWithDebug :: EmitOptions -> Name -> [Text] -> [Text]
emitWithDebug options name rendered =
  emitDebugLines options name <> rendered

emitDebugLines :: EmitOptions -> Name -> [Text]
emitDebugLines options name =
  case Map.lookup name (emitDebugInfo options) of
    Nothing -> []
    Just ann ->
      let typeLine = case debugType ann of
            Nothing -> []
            Just ty -> ["-- locque: " <> unName name <> " : " <> ty]
          linePragma = case (debugSourceFile ann, debugSourceLine ann) of
            (Just file, Just line) -> [renderLinePragma line file]
            _ -> []
          rendered = typeLine <> linePragma
      in if null rendered then [] else rendered

renderLinePragma :: Int -> FilePath -> Text
renderLinePragma line file =
  "{-# LINE " <> T.pack (show line) <> " \"" <> escapeString (T.pack file) <> "\" #-}"

emitValueBinding :: Name -> ErasedValue -> Text
emitValueBinding name value =
  hsVarName name <> " = " <> renderValue value

emitCompBinding :: Name -> ErasedComp -> Text
emitCompBinding name comp =
  hsVarName name <> " = " <> renderComp comp

emitDataDecl :: CoreDataDecl -> Text
emitDataDecl (CoreDataDecl name params ctors) =
  case ctors of
    [] ->
      "data "
        <> hsTypeName name
        <> renderTypeParams params
    _ ->
      "data "
        <> hsTypeName name
        <> renderTypeParams params
        <> " = "
        <> T.intercalate " | " (map emitCtor ctors)
  where
    renderTypeParams [] = ""
    renderTypeParams names =
      " " <> T.intercalate " " (map hsTypeVarName names)

emitCtor :: CoreCtor -> Text
emitCtor (CoreCtor name fields) =
  case fields of
    [] -> hsCtorName name
    _ ->
      hsCtorName name <> " " <> T.intercalate " " (map renderTypeAtom fields)

renderTypeAtom :: CoreType -> Text
renderTypeAtom ty =
  if isAtomicType ty
    then renderType ty
    else "(" <> renderType ty <> ")"

isAtomicType :: CoreType -> Bool
isAtomicType ty =
  case ty of
    TyVar _ -> True
    TyCon _ [] -> True
    TyUnit -> True
    TyBoolean -> True
    TyNatural -> True
    TyString -> True
    TyCharacter -> True
    _ -> False

renderType :: CoreType -> Text
renderType ty =
  case ty of
    TyVar name -> hsTypeVarName name
    TyCon name args ->
      let headName = hsTypeConName name
       in case args of
            [] -> headName
            _ -> headName <> " " <> T.intercalate " " (map renderTypeAtom args)
    TyFun left right ->
      renderTypeAtom left <> " -> " <> renderType right
    TyComp inner ->
      "Comp " <> renderTypeAtom inner
    TyUnit -> "Unit"
    TyBoolean -> "Bool"
    TyNatural -> "Natural"
    TyString -> "String"
    TyCharacter -> "Character"

renderComp :: ErasedComp -> Text
renderComp comp =
  case comp of
    EReturn value -> "compReturn " <> renderValueAtomCoerced value
    EBind name left right ->
      "compBind "
        <> renderCompAtom left
        <> " (\\"
        <> hsVarName name
        <> " -> "
        <> renderComp right
        <> ")"
    EPerform value -> "perform " <> renderValueAtomCoerced value
    EApp fn arg -> renderValueAtom fn <> " " <> renderValueAtomCoerced arg
    ELet name value body ->
      "let "
        <> hsVarName name
        <> " = "
        <> renderValue value
        <> " in "
        <> renderComp body
    EMatch value cases ->
      "case "
        <> renderValueCoerced value
        <> " of "
        <> renderCaseBlock (map renderCase cases)

renderCase :: ErasedCase -> Text
renderCase (ErasedCase ctor binders body) =
  renderPattern ctor binders
    <> " -> "
    <> renderComp body

renderCaseBlock :: [Text] -> Text
renderCaseBlock cases =
  "{ " <> T.intercalate " ; " cases <> " }"

renderValueCase :: ErasedValueCase -> Text
renderValueCase (ErasedValueCase ctor binders body) =
  renderPattern ctor binders
    <> " -> unsafeCoerce ("
    <> renderValue body
    <> ")"

renderPattern :: Name -> [Name] -> Text
renderPattern ctor binders =
  let binderNames = renderBinderNames binders
   in case builtinPattern ctor binderNames of
    Just pat -> pat
    Nothing ->
      T.intercalate " " (hsCtorName ctor : binderNames)

renderBinderNames :: [Name] -> [Text]
renderBinderNames binders =
  reverse (snd (foldl step ([], []) binders))
  where
    step (seen, acc) name =
      let raw = unName name
          rendered = hsVarName name
      in if raw == "ignored" || rendered `elem` seen
          then (seen, "_" : acc)
          else (rendered : seen, rendered : acc)

renderValue :: ErasedValue -> Text
renderValue value =
  case value of
    EVar name -> renderBuiltinVar name
    ELit lit -> renderLiteral lit
    EErased -> "(unsafeCoerce ())"
    ELam name body -> "(\\" <> hsVarName name <> " -> " <> renderValue body <> ")"
    EAppValue fn arg -> renderValueAtom fn <> " " <> renderValueAtomCoerced arg
    EConstructor name args -> renderConstructor name args
    ECompute comp -> renderComp comp
    ELetValue name val body ->
      "let "
        <> hsVarName name
        <> " = "
        <> renderValue val
        <> " in "
        <> renderValue body
    EMatchValue scrut cases ->
      "case "
        <> renderValueCoerced scrut
        <> " of "
        <> renderCaseBlock (map renderValueCase cases)

renderValueAtom :: ErasedValue -> Text
renderValueAtom value =
  if isAtomicValue value
    then renderValue value
    else "(" <> renderValue value <> ")"

renderValueCoerced :: ErasedValue -> Text
renderValueCoerced value =
  "unsafeCoerce (" <> renderValue value <> ")"

renderValueAtomCoerced :: ErasedValue -> Text
renderValueAtomCoerced value =
  "(unsafeCoerce (" <> renderValueAtom value <> "))"

renderCompAtom :: ErasedComp -> Text
renderCompAtom comp =
  "(" <> renderComp comp <> ")"

isAtomicValue :: ErasedValue -> Bool
isAtomicValue value =
  case value of
    EVar _ -> True
    ELit _ -> True
    EErased -> True
    EConstructor name args -> builtinCtorIsAtomic name args
    _ -> False

builtinCtorIsAtomic :: Name -> [ErasedValue] -> Bool
builtinCtorIsAtomic (Name name) args =
  case (ctorBaseName name, args) of
    ("Option::none", []) -> True
    ("List::empty", []) -> True
    ("Boolean::true", []) -> True
    ("Boolean::false", []) -> True
    ("Unit::tt", []) -> True
    _ -> False

renderConstructor :: Name -> [ErasedValue] -> Text
renderConstructor name@(Name raw) args =
  case (ctorBaseName raw, args) of
    ("List::empty", []) -> "[]"
    ("List::cons", []) -> "(:)"
    ("List::cons", [headValue, tailValue]) ->
      "(" <> renderValueAtom headValue <> " : " <> renderValueAtom tailValue <> ")"
    ("Pair::pair", []) -> "(,)"
    ("Pair::pair", [leftValue, rightValue]) ->
      "(" <> renderValueAtom leftValue <> ", " <> renderValueAtom rightValue <> ")"
    ("Option::none", []) -> "Nothing"
    ("Option::some", []) -> "Just"
    ("Option::some", [value]) -> "Just " <> renderValueAtom value
    ("Either::left", []) -> "Left"
    ("Either::left", [value]) -> "Left " <> renderValueAtom value
    ("Either::right", []) -> "Right"
    ("Either::right", [value]) -> "Right " <> renderValueAtom value
    ("Result::ok", []) -> "Right"
    ("Result::ok", [value]) -> "Right " <> renderValueAtom value
    ("Result::err", []) -> "Left"
    ("Result::err", [value]) -> "Left " <> renderValueAtom value
    _ ->
      let ctorName = hsCtorName name
          renderedArgs = map renderValueAtomCoerced args
       in case renderedArgs of
            [] -> ctorName
            _ -> ctorName <> " " <> T.intercalate " " renderedArgs

renderBuiltinVar :: Name -> Text
renderBuiltinVar (Name name) =
  case name of
    "add-nat-prim" -> "addNatPrim"
    "sub-nat-prim" -> "subNatPrim"
    "mul-nat-prim" -> "mulNatPrim"
    "div-nat-prim" -> "divNatPrim"
    "mod-nat-prim" -> "modNatPrim"
    "lt-nat-prim" -> "ltNatPrim"
    "le-nat-prim" -> "leNatPrim"
    "gt-nat-prim" -> "gtNatPrim"
    "ge-nat-prim" -> "geNatPrim"
    "eq-nat-prim" -> "eqNatPrim"
    "decide-eq-nat-prim" -> "decideEqNatPrim"
    "eq-string-prim" -> "eqStringPrim"
    "decide-eq-string-prim" -> "decideEqStringPrim"
    "decide-eq-bool-prim" -> "decideEqBoolPrim"
    "decide-eq-pair-prim" -> "decideEqPairPrim"
    "decide-eq-list-prim" -> "decideEqListPrim"
    "concat-string-prim" -> "concatStringPrim"
    "string-length-prim" -> "stringLengthPrim"
    "string-to-list-prim" -> "stringToListPrim"
    "char-code-prim" -> "charCodePrim"
    "char-from-code-prim" -> "charFromCodePrim"
    "error-prim" -> "errorPrim"
    "print-prim" -> "printPrim"
    "capture-output-prim" -> "captureOutputPrim"
    "forever-prim" -> "foreverPrim"
    "assert-hit-prim" -> "assertHitPrim"
    "assertion-count-prim" -> "assertionCountPrim"
    "get-line-prim" -> "getLinePrim"
    "cli-args-prim" -> "cliArgsPrim"
    "current-directory-prim" -> "currentDirectoryPrim"
    "read-file-prim" -> "readFilePrim"
    "write-file-prim" -> "writeFilePrim"
    "append-file-prim" -> "appendFilePrim"
    "copy-file-prim" -> "copyFilePrim"
    "copy-tree-prim" -> "copyTreePrim"
    "rename-path-prim" -> "renamePathPrim"
    "list-dir-prim" -> "listDirPrim"
    "path-exists-prim" -> "pathExistsPrim"
    "is-directory-prim" -> "isDirectoryPrim"
    "is-file-prim" -> "isFilePrim"
    "make-directory-prim" -> "makeDirectoryPrim"
    "remove-file-prim" -> "removeFilePrim"
    "remove-directory-prim" -> "removeDirectoryPrim"
    "walk-prim" -> "walkPrim"
    "walk-filter-prim" -> "walkFilterPrim"
    "stat-prim" -> "statPrim"
    "natural-to-peano-prim" -> "naturalToPeanoPrim"
    "nat-to-string-prim" -> "natToStringPrim"
    "on-signal-prim" -> "onSignalPrim"
    "tcp-listen-prim" -> "tcpListenPrim"
    "tcp-accept-prim" -> "tcpAcceptPrim"
    "tcp-recv-prim" -> "tcpRecvPrim"
    "tcp-send-prim" -> "tcpSendPrim"
    "tcp-close-prim" -> "tcpClosePrim"
    "tcp-close-listener-prim" -> "tcpCloseListenerPrim"
    "tcp-select-listener-prim" -> "tcpSelectListenerPrim"
    "tcp-select-socket-prim" -> "tcpSelectSocketPrim"
    "sleep-prim" -> "sleepPrim"
    "timeout-prim" -> "timeoutPrim"
    "dictionary-empty-prim" -> "dictionaryEmptyPrim"
    "dictionary-insert-prim" -> "dictionaryInsertPrim"
    "dictionary-lookup-prim" -> "dictionaryLookupPrim"
    "dictionary-remove-prim" -> "dictionaryRemovePrim"
    "dictionary-size-prim" -> "dictionarySizePrim"
    "dictionary-to-list-prim" -> "dictionaryToListPrim"
    "dict-access-prim" -> "dictAccessPrim"
    "shell-prim" -> "shellPrim"
    "process-run-prim" -> "processRunPrim"
    "time-now-prim" -> "timeNowPrim"
    "validate-prim" -> "validatePrim"
    "panic-prim" -> "panicPrim"
    _ ->
      case ctorBaseName name of
        "Boolean::true" -> "True"
        "Boolean::false" -> "False"
        "Unit::tt" -> "()"
        "List::empty" -> "[]"
        "List::cons" -> "(:)"
        "Pair::pair" -> "(,)"
        "Option::none" -> "Nothing"
        "Option::some" -> "Just"
        "Either::left" -> "Left"
        "Either::right" -> "Right"
        "Result::ok" -> "Right"
        "Result::err" -> "Left"
        _ -> hsVarName (Name name)

renderLiteral :: CoreLiteral -> Text
renderLiteral lit =
  case lit of
    LitNatural nat -> T.pack (show nat)
    LitString text -> "(\"" <> escapeString text <> "\" :: String)"
    LitBoolean value -> if value then "True" else "False"
    LitUnit -> "()"

escapeString :: Text -> Text
escapeString = T.concatMap escapeChar

escapeChar :: Char -> Text
escapeChar char =
  case char of
    '\\' -> "\\\\"
    '"' -> "\\\""
    '\n' -> "\\n"
    '\t' -> "\\t"
    '\r' -> "\\r"
    _ -> T.singleton char

hsTypeConName :: Name -> Text
hsTypeConName name@(Name raw) =
  case baseName raw of
    "List" -> "List"
    "Pair" -> "Pair"
    "Dictionary" -> "Dictionary"
    "Option" -> "Option"
    "Either" -> "Either"
    "Result" -> "Result"
    "Boolean" -> "Boolean"
    _ -> hsTypeName name

hsTypeName :: Name -> Text
hsTypeName (Name raw) =
  let sanitized = sanitizeName raw
   in ensureUpper (avoidReserved sanitized)

hsTypeVarName :: Name -> Text
hsTypeVarName (Name raw) =
  let sanitized = T.toLower (sanitizeName raw)
      base = avoidReserved sanitized
   in ensureLower base

hsCtorName :: Name -> Text
hsCtorName (Name raw) =
  let sanitized = sanitizeName raw
   in ensureUpper (avoidReserved sanitized)

hsVarName :: Name -> Text
hsVarName (Name raw) =
  let sanitized = sanitizeName raw
      base = avoidReserved sanitized
   in ensureLower base

sanitizeName :: Text -> Text
sanitizeName raw =
  T.map sanitizeChar (T.replace "::" "_" raw)
  where
    sanitizeChar char
      | isAlphaNum char = char
      | char == '_' = char
      | otherwise = '_'

avoidReserved :: Text -> Text
avoidReserved name =
  if name `elem` reservedWords
    then "locque_" <> name
    else name

reservedWords :: [Text]
reservedWords =
  [ "case", "class", "data", "default", "deriving", "do", "else", "if"
  , "import", "in", "infix", "infixl", "infixr", "instance", "let"
  , "module", "newtype", "of", "then", "type", "where"
  ]

ensureUpper :: Text -> Text
ensureUpper text =
  case T.uncons text of
    Nothing -> "Ctor_"
    Just (char, _rest)
      | isUpper char -> text
      | otherwise -> "Ctor_" <> text

ensureLower :: Text -> Text
ensureLower text =
  case T.uncons text of
    Nothing -> "v"
    Just (char, _rest)
      | isLower char -> text
      | otherwise -> "v_" <> text

builtinPattern :: Name -> [Text] -> Maybe Text
builtinPattern (Name name) binders =
  case (ctorBaseName name, binders) of
    ("List::empty", []) -> Just "[]"
    ("List::cons", [headName, tailName]) ->
      Just (headName <> " : " <> tailName)
    ("Pair::pair", [leftName, rightName]) ->
      Just ("(" <> leftName <> ", " <> rightName <> ")")
    ("Option::none", []) -> Just "Nothing"
    ("Option::some", [valueName]) -> Just ("Just " <> valueName)
    ("Either::left", [valueName]) -> Just ("Left " <> valueName)
    ("Either::right", [valueName]) -> Just ("Right " <> valueName)
    ("Result::ok", [valueName]) -> Just ("Right " <> valueName)
    ("Result::err", [valueName]) -> Just ("Left " <> valueName)
    ("Boolean::true", []) -> Just "True"
    ("Boolean::false", []) -> Just "False"
    ("Unit::tt", []) -> Just "()"
    _ -> Nothing

baseName :: Text -> Text
baseName raw =
  case reverse (T.splitOn "::" raw) of
    (name:_) -> name
    [] -> raw

ctorBaseName :: Text -> Text
ctorBaseName raw =
  case reverse (T.splitOn "::" raw) of
    (ctorName:parentName:_) -> parentName <> "::" <> ctorName
    (only:_) -> only
    [] -> raw

isBuiltinDataDecl :: CoreDataDecl -> Bool
isBuiltinDataDecl (CoreDataDecl (Name raw) _ _) =
  baseName raw `elem` ["Option", "Either", "Result"]
