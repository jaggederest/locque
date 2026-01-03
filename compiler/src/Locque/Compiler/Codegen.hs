{-# LANGUAGE OverloadedStrings #-}

module Locque.Compiler.Codegen
  ( emitModule
  ) where

import Data.Char (isAlphaNum, isLower, isUpper)
import Data.Text (Text)
import qualified Data.Text as T

import Locque.Compiler.Core
import Locque.Compiler.CoreErased
import Locque.Compiler.Erase

emitModule :: CoreModule -> Text
emitModule coreModule =
  T.unlines (moduleHeader ++ concatMap emitDecl (coreModuleDecls coreModule))
  where
    moduleHeader =
      [ "{-# LANGUAGE OverloadedStrings #-}"
      , "module LocqueGen where"
      , "import LocqueRuntime"
      , ""
      ]

emitDecl :: CoreDecl -> [Text]
emitDecl decl =
  case decl of
    CoreDef name _ value ->
      [ emitValueBinding name (eraseValue value)
      , ""
      ]
    CoreDefComp name _ comp ->
      [ emitCompBinding name (eraseComp comp)
      , ""
      ]
    CoreData dataDecl ->
      [ emitDataDecl dataDecl
      , ""
      ]

emitValueBinding :: Name -> ErasedValue -> Text
emitValueBinding name value =
  hsVarName name <> " = " <> renderValue value

emitCompBinding :: Name -> ErasedComp -> Text
emitCompBinding name comp =
  hsVarName name <> " = " <> renderComp comp

emitDataDecl :: CoreDataDecl -> Text
emitDataDecl (CoreDataDecl name params ctors) =
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
    EReturn value -> "compReturn " <> renderValueAtom value
    EBind name left right ->
      "compBind "
        <> renderCompAtom left
        <> " (\\"
        <> hsVarName name
        <> " -> "
        <> renderComp right
        <> ")"
    EPerform value -> "perform " <> renderValueAtom value
    EApp fn arg -> renderValueAtom fn <> " " <> renderValueAtom arg
    ELet name value body ->
      "let "
        <> hsVarName name
        <> " = "
        <> renderValue value
        <> " in "
        <> renderComp body
    EMatch value cases ->
      "case "
        <> renderValue value
        <> " of\n"
        <> T.unlines (map renderCase cases)

renderCase :: ErasedCase -> Text
renderCase (ErasedCase ctor binders body) =
  "  "
    <> renderPattern ctor binders
    <> " -> "
    <> renderComp body

renderPattern :: Name -> [Name] -> Text
renderPattern ctor binders =
  case builtinPattern ctor binders of
    Just pat -> pat
    Nothing ->
      T.intercalate " " (hsCtorName ctor : map hsVarName binders)

renderValue :: ErasedValue -> Text
renderValue value =
  case value of
    EVar name -> renderBuiltinVar name
    ELit lit -> renderLiteral lit
    ELam name body -> "(\\" <> hsVarName name <> " -> " <> renderComp body <> ")"
    EConstructor name args -> renderConstructor name args
    ECompute comp -> renderComp comp

renderValueAtom :: ErasedValue -> Text
renderValueAtom value =
  if isAtomicValue value
    then renderValue value
    else "(" <> renderValue value <> ")"

renderCompAtom :: ErasedComp -> Text
renderCompAtom comp =
  "(" <> renderComp comp <> ")"

isAtomicValue :: ErasedValue -> Bool
isAtomicValue value =
  case value of
    EVar _ -> True
    ELit _ -> True
    EConstructor name args -> builtinCtorIsAtomic name args
    _ -> False

builtinCtorIsAtomic :: Name -> [ErasedValue] -> Bool
builtinCtorIsAtomic (Name name) args =
  case (name, args) of
    ("Option::none", []) -> True
    ("List::empty", []) -> True
    ("Boolean::true", []) -> True
    ("Boolean::false", []) -> True
    ("Unit::tt", []) -> True
    _ -> False

renderConstructor :: Name -> [ErasedValue] -> Text
renderConstructor name args =
  case (name, args) of
    (Name "List::empty", []) -> "[]"
    (Name "List::cons", [headValue, tailValue]) ->
      "(" <> renderValueAtom headValue <> " : " <> renderValueAtom tailValue <> ")"
    (Name "Pair::pair", [leftValue, rightValue]) ->
      "(" <> renderValue leftValue <> ", " <> renderValue rightValue <> ")"
    (Name "Option::none", []) -> "Nothing"
    (Name "Option::some", [value]) -> "Just " <> renderValueAtom value
    (Name "Either::left", [value]) -> "Left " <> renderValueAtom value
    (Name "Either::right", [value]) -> "Right " <> renderValueAtom value
    (Name "Result::ok", [value]) -> "Right " <> renderValueAtom value
    (Name "Result::err", [value]) -> "Left " <> renderValueAtom value
    _ ->
      let ctorName = hsCtorName name
          renderedArgs = map renderValueAtom args
       in case renderedArgs of
            [] -> ctorName
            _ -> ctorName <> " " <> T.intercalate " " renderedArgs

renderBuiltinVar :: Name -> Text
renderBuiltinVar (Name name) =
  case name of
    "add-nat-prim" -> "addNatPrim"
    "sub-nat-prim" -> "subNatPrim"
    "eq-nat-prim" -> "eqNatPrim"
    "eq-string-prim" -> "eqStringPrim"
    "concat-string-prim" -> "concatStringPrim"
    "error-prim" -> "errorPrim"
    "print-prim" -> "printPrim"
    "get-line-prim" -> "getLinePrim"
    "panic-prim" -> "panicPrim"
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
    LitString text -> "\"" <> escapeString text <> "\""
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
  case raw of
    "List" -> "List"
    "Pair" -> "Pair"
    "Option" -> "Option"
    "Either" -> "Either"
    "Result" -> "Result"
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

builtinPattern :: Name -> [Name] -> Maybe Text
builtinPattern (Name name) binders =
  case (name, binders) of
    ("List::empty", []) -> Just "[]"
    ("List::cons", [headName, tailName]) ->
      Just (hsVarName headName <> " : " <> hsVarName tailName)
    ("Pair::pair", [leftName, rightName]) ->
      Just ("(" <> hsVarName leftName <> ", " <> hsVarName rightName <> ")")
    ("Option::none", []) -> Just "Nothing"
    ("Option::some", [valueName]) -> Just ("Just " <> hsVarName valueName)
    ("Either::left", [valueName]) -> Just ("Left " <> hsVarName valueName)
    ("Either::right", [valueName]) -> Just ("Right " <> hsVarName valueName)
    ("Result::ok", [valueName]) -> Just ("Right " <> hsVarName valueName)
    ("Result::err", [valueName]) -> Just ("Left " <> hsVarName valueName)
    ("Boolean::true", []) -> Just "True"
    ("Boolean::false", []) -> Just "False"
    ("Unit::tt", []) -> Just "()"
    _ -> Nothing
