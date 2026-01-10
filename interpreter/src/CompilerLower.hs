{-# LANGUAGE OverloadedStrings #-}
module CompilerLower
  ( lowerModule
  ) where

import Data.Char (isUpper)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import qualified AST as AST
import Locque.Compiler.Core

lowerModule :: AST.Module -> CoreModule
lowerModule (AST.Module modName _imports _opens defs) =
  let typeNames = collectTypeNames defs
      ctorNames = collectCtorNames defs
      ctorArity = collectCtorArity defs
      constraintsMap = collectConstraintsMap defs
      lowered = CoreModule (Name modName) (map (lowerDefinition typeNames ctorNames ctorArity constraintsMap Set.empty) defs)
   in simplifyModule lowered

lowerDefinition :: Set.Set T.Text -> Set.Set T.Text -> Map.Map T.Text Int -> Map.Map T.Text [AST.Constraint] -> Set.Set T.Text -> AST.Definition -> CoreDecl
lowerDefinition typeNames ctorNames ctorArity constraintsMap typeVars defn =
  case AST.defBody defn of
    AST.EData params _universe cases ->
      CoreData (lowerDataDecl (AST.defName defn) params cases)
    AST.ETypeClass paramName _kind methods ->
      CoreData (lowerTypeClass (AST.defName defn) paramName methods)
    AST.EInstance className _instType methods ->
      CoreDef
        (Name (AST.defName defn))
        (lowerType Set.empty (getDefType defn))
        (lowerInstance typeNames ctorNames ctorArity constraintsMap typeVars className methods)
    body ->
      let body' = rewriteRecurExpr (AST.defName defn) body
       in CoreDef
            (Name (AST.defName defn))
            (lowerType Set.empty (getDefType defn))
            (lowerExpr typeNames ctorNames ctorArity constraintsMap typeVars body')

getDefType :: AST.Definition -> AST.Expr
getDefType defn =
  case AST.defBody defn of
    AST.ETyped _ t -> t
    AST.EAnnot _ t -> t
    _ -> AST.ETypeUniverse 0 -- Fallback, should not happen in typechecked code

collectTypeNames :: [AST.Definition] -> Set.Set T.Text
collectTypeNames defs =
  let dataNames =
        Set.fromList
          [ AST.defName defn
          | defn <- defs
          , AST.EData {} <- [AST.defBody defn]
          ]
      initial = Set.union dataNames builtinTypeNames
   in growTypeNames defs initial

growTypeNames :: [AST.Definition] -> Set.Set T.Text -> Set.Set T.Text
growTypeNames defs known =
  let aliasNames =
        Set.fromList
          [ AST.defName defn
          | defn <- defs
          , isTypeDefinition known defn
          ]
      next = Set.union known aliasNames
   in if next == known then known else growTypeNames defs next

builtinTypeNames :: Set.Set T.Text
builtinTypeNames =
  Set.fromList
    [ "Natural"
    , "String"
    , "Boolean"
    , "Unit"
    , "Character"
    , "List"
    , "Pair"
    , "Option"
    , "Either"
    , "Result"
    , "Dictionary"
    ]

collectCtorNames :: [AST.Definition] -> Set.Set T.Text
collectCtorNames defs =
  let dataCtors =
        Set.fromList
          [ AST.dataCaseName dataCase
          | defn <- defs
          , AST.EData _ _ cases <- [AST.defBody defn]
          , dataCase <- cases
          ]
   in Set.union dataCtors builtinCtorNames

collectCtorArity :: [AST.Definition] -> Map.Map T.Text Int
collectCtorArity defs =
  Map.fromList
    [ (AST.dataCaseName dataCase, length (collectCtorFields paramNames (AST.dataCaseType dataCase)))
    | defn <- defs
    , AST.EData params _ cases <- [AST.defBody defn]
    , let paramNames = Set.fromList (map AST.paramName params)
    , dataCase <- cases
    ]

builtinCtorNames :: Set.Set T.Text
builtinCtorNames =
  Set.fromList
    [ "List::empty"
    , "List::cons"
    , "Pair::pair"
    ]

isTypeDefinition :: Set.Set T.Text -> AST.Definition -> Bool
isTypeDefinition typeNames defn =
  case AST.defBody defn of
    AST.EData {} -> True
    body ->
      isTypeExpr typeNames Set.empty body
        || isTypeFunctionExpr typeNames body

isTypeFunctionExpr :: Set.Set T.Text -> AST.Expr -> Bool
isTypeFunctionExpr typeNames expr =
  case expr of
    AST.EFunction _params _constraints retTy _body ->
      isUniverseExpr retTy
    AST.EAnnot inner _ -> isTypeFunctionExpr typeNames inner
    AST.ETyped inner _ -> isTypeFunctionExpr typeNames inner
    _ -> False

lowerExpr :: Set.Set T.Text -> Set.Set T.Text -> Map.Map T.Text Int -> Map.Map T.Text [AST.Constraint] -> Set.Set T.Text -> AST.Expr -> CoreValue
lowerExpr typeNames ctorNames ctorArity constraintsMap typeVars expr =
  case expr of
    AST.EVar name ->
      if name `Set.member` ctorNames
        then VConstructor (Name name) []
        else if isTypeExpr typeNames typeVars (AST.EVar name)
          then VErased
          else VVar (Name name)

    AST.ELit lit -> VLit (lowerLiteral lit)
    AST.EListLiteral elems -> lowerListLiteral typeNames ctorNames ctorArity constraintsMap typeVars elems
    AST.ECompute comp -> VCompute (lowerComp typeNames ctorNames ctorArity constraintsMap typeVars comp)
    AST.EAnnot inner _ty -> lowerExpr typeNames ctorNames ctorArity constraintsMap typeVars inner
    AST.ETyped inner _ty -> lowerExpr typeNames ctorNames ctorArity constraintsMap typeVars inner
    AST.EApp fn args ->
      if isTypeExpr typeNames typeVars fn
        then VErased
        else
          case lowerAppArgs typeNames ctorNames ctorArity constraintsMap typeVars args of
            [] -> lowerExpr typeNames ctorNames ctorArity constraintsMap typeVars fn
            args' ->
              case fn of
                AST.EVar name | name `Set.member` ctorNames ->
                  VConstructor
                    (Name name)
                    (trimCtorArgs ctorArity name args')
                _ ->
                  let calleeName = getCalleeName fn
                      dictArgs =
                        case calleeName >>= (\name -> Map.lookup name constraintsMap) of
                          Just constraints ->
                            map (\(AST.Constraint cls _) -> VVar (Name (T.toLower cls <> "-dict"))) constraints
                          Nothing -> []
                      finalArgs = dictArgs ++ args'
                  in foldl VApp (lowerExpr typeNames ctorNames ctorArity constraintsMap typeVars fn) finalArgs

getCalleeName :: AST.Expr -> Maybe T.Text
getCalleeName (AST.EVar name) = Just name
getCalleeName (AST.EApp fn _) = getCalleeName fn
getCalleeName (AST.ETyped e _) = getCalleeName e
getCalleeName (AST.EAnnot e _) = getCalleeName e
getCalleeName (AST.ELet _ _ body) = getCalleeName body
getCalleeName (AST.EMatch _ _ _ _ [cse]) = getCalleeName (AST.matchCaseBody cse)
getCalleeName _ = Nothing
    AST.EFunction params constraints _retTy body ->
      lowerFunction typeNames ctorNames ctorArity constraintsMap typeVars params constraints body
    AST.ELet name value body ->
      if isTypeExpr typeNames typeVars value
        then lowerExpr typeNames ctorNames ctorArity constraintsMap (Set.insert name typeVars) body
        else VLet
          (Name name)
          (lowerExpr typeNames ctorNames ctorArity constraintsMap typeVars value)
          (lowerExpr typeNames ctorNames ctorArity constraintsMap typeVars body)
    AST.EMatch scrut _scrutTy _scrutName retTy cases ->
      if isUniverseExpr retTy
        then VErased
        else VMatch
          (lowerExpr typeNames ctorNames ctorArity constraintsMap typeVars scrut)
          (map (lowerValueMatchCase typeNames ctorNames ctorArity constraintsMap typeVars) cases)
    AST.EData _params _retTy _cases ->
      error "lowerExpr: data declarations are not supported yet"
    AST.ETypeConst _ -> VErased
    AST.ETypeUniverse _ -> VErased
    AST.EForAll {} -> VErased
    AST.EThereExists {} -> VErased
    AST.ECompType {} -> VErased
    AST.EEqual {} -> VErased
    AST.EReflexive {} -> VErased
    AST.ERewrite _family _proof term -> lowerExpr typeNames ctorNames ctorArity constraintsMap typeVars term
    AST.EPack _ _ _ witness body ->
      VConstructor
        (Name "Pair::pair")
        [ lowerExpr typeNames ctorNames ctorArity constraintsMap typeVars witness
        , lowerExpr typeNames ctorNames ctorArity constraintsMap typeVars body
        ]
    AST.EUnpack packed x y body ->
      VMatch
        (lowerExpr typeNames ctorNames ctorArity constraintsMap typeVars packed)
        [ CoreValueCase
            (Name "Pair::pair")
            [Name x, Name y]
            (lowerExpr typeNames ctorNames ctorArity constraintsMap typeVars body)
        ]
    AST.ELift {} -> VErased
    AST.EUp _ty _from _to body -> lowerExpr typeNames ctorNames ctorArity constraintsMap typeVars body
    AST.EDown _ty _from _to body -> lowerExpr typeNames ctorNames ctorArity constraintsMap typeVars body
    AST.ETypeClass {} -> VErased
    AST.EInstance {} -> VErased
    AST.EDict _ impls ->
      lowerDict typeNames ctorNames ctorArity constraintsMap typeVars impls
    AST.EDictAccess dictExpr method ->
      VApp
        (VApp
          (VVar (Name "dict-access-prim"))
          (lowerExpr typeNames ctorNames ctorArity constraintsMap typeVars dictExpr))
        (VLit (LitString method))

lowerFunction :: Set.Set T.Text -> Set.Set T.Text -> Map.Map T.Text Int -> Map.Map T.Text [AST.Constraint] -> Set.Set T.Text -> [AST.Param] -> [AST.Constraint] -> AST.FunctionBody -> CoreValue
lowerFunction typeNames ctorNames ctorArity constraintsMap typeVars params constraints body =
  let paramValue = lowerFunction' typeNames ctorNames ctorArity constraintsMap typeVars params body
   in foldr
        (\(AST.Constraint cls _ty) acc ->
           VLam (Name (T.toLower cls <> "-dict")) TyUnit acc)
        paramValue
        constraints
lowerFunction' :: Set.Set T.Text -> Set.Set T.Text -> Map.Map T.Text Int -> Map.Map T.Text [AST.Constraint] -> Set.Set T.Text -> [AST.Param] -> AST.FunctionBody -> CoreValue
lowerFunction' typeNames ctorNames ctorArity constraintsMap typeVars params body =
  case params of
    [] -> lowerFunctionBody typeNames ctorNames ctorArity constraintsMap typeVars body
    (AST.Param name ty : rest) ->
      if isTypeParam ty
        then lowerFunction' typeNames ctorNames ctorArity constraintsMap (Set.insert name typeVars) rest body
        else VLam (Name name) TyUnit (lowerFunction' typeNames ctorNames ctorArity constraintsMap typeVars rest body)

lowerFunctionBody :: Set.Set T.Text -> Set.Set T.Text -> Map.Map T.Text Int -> Map.Map T.Text [AST.Constraint] -> Set.Set T.Text -> AST.FunctionBody -> CoreValue
lowerFunctionBody typeNames ctorNames ctorArity constraintsMap typeVars body =
  case body of
    AST.FunctionValue expr -> lowerExpr typeNames ctorNames ctorArity constraintsMap typeVars expr
    AST.FunctionCompute comp -> VCompute (lowerComp typeNames ctorNames ctorArity constraintsMap typeVars comp)

lowerComp :: Set.Set T.Text -> Set.Set T.Text -> Map.Map T.Text Int -> Map.Map T.Text [AST.Constraint] -> Set.Set T.Text -> AST.Comp -> CoreComp
lowerComp typeNames ctorNames ctorArity constraintsMap typeVars comp =
  case comp of
    AST.CReturn expr -> CReturn (lowerExpr typeNames ctorNames ctorArity constraintsMap typeVars expr)
    AST.CBind name left right ->
      CBind
        (Name name)
        (lowerComp typeNames ctorNames ctorArity constraintsMap typeVars left)
        (lowerComp typeNames ctorNames ctorArity constraintsMap typeVars right)
    AST.CPerform expr -> CPerform (lowerExpr typeNames ctorNames ctorArity constraintsMap typeVars expr)

lowerAppArgs :: Set.Set T.Text -> Set.Set T.Text -> Map.Map T.Text Int -> Map.Map T.Text [AST.Constraint] -> Set.Set T.Text -> [AST.Expr] -> [CoreValue]
lowerAppArgs typeNames ctorNames ctorArity constraintsMap typeVars args =
  mapMaybe (lowerAppArg typeNames ctorNames ctorArity constraintsMap typeVars) args

lowerAppArg :: Set.Set T.Text -> Set.Set T.Text -> Map.Map T.Text Int -> Map.Map T.Text [AST.Constraint] -> Set.Set T.Text -> AST.Expr -> Maybe CoreValue
lowerAppArg typeNames ctorNames ctorArity constraintsMap typeVars arg
  | isProofTerm arg = Just VErased
  | isTypeArg typeNames typeVars arg = Nothing
  | otherwise = Just (lowerExpr typeNames ctorNames ctorArity constraintsMap typeVars arg)

lowerValueMatchCase :: Set.Set T.Text -> Set.Set T.Text -> Map.Map T.Text Int -> Map.Map T.Text [AST.Constraint] -> Set.Set T.Text -> AST.MatchCase -> CoreValueCase
lowerValueMatchCase typeNames ctorNames ctorArity constraintsMap typeVars (AST.MatchCase ctor params body) =
  CoreValueCase
    (Name ctor)
    (map (Name . AST.paramName) params)
    (lowerExpr typeNames ctorNames ctorArity constraintsMap typeVars body)

lowerListLiteral :: Set.Set T.Text -> Set.Set T.Text -> Map.Map T.Text Int -> Map.Map T.Text [AST.Constraint] -> Set.Set T.Text -> [AST.Expr] -> CoreValue
lowerListLiteral typeNames ctorNames ctorArity constraintsMap typeVars elems =
  foldr
    (\elemValue acc ->
        VConstructor
          (Name "List::cons")
          [lowerExpr typeNames ctorNames ctorArity constraintsMap typeVars elemValue, acc])
    (VConstructor (Name "List::empty") [])
    elems

lowerDict :: Set.Set T.Text
          -> Set.Set T.Text
          -> Map.Map T.Text Int
          -> Map.Map T.Text [AST.Constraint]
          -> Set.Set T.Text
          -> [(T.Text, AST.Expr)]
          -> CoreValue
lowerDict typeNames ctorNames ctorArity constraintsMap typeVars entries =
  foldr
    (\(methodName, implExpr) acc ->
        VConstructor
          (Name "List::cons")
          [ VConstructor
              (Name "Pair::pair")
              [ VLit (LitString methodName)
              , lowerExpr typeNames ctorNames ctorArity constraintsMap typeVars implExpr
              ]
          , acc
          ])
    (VConstructor (Name "List::empty") [])
    entries

lowerLiteral :: AST.Literal -> CoreLiteral
lowerLiteral lit =
  case lit of
    AST.LNatural n -> LitNatural n
    AST.LString s -> LitString s
    AST.LBoolean b -> LitBoolean b
    AST.LUnit -> LitUnit

lowerDataDecl :: T.Text -> [AST.Param] -> [AST.DataCase] -> CoreDataDecl
lowerDataDecl typeName params cases =
  let paramNames = Set.fromList (map AST.paramName params)
   in CoreDataDecl
        (Name typeName)
        (map (Name . AST.paramName) params)
        (map (lowerDataCase paramNames) cases)

lowerDataCase :: Set.Set T.Text -> AST.DataCase -> CoreCtor
lowerDataCase paramNames (AST.DataCase ctorName ctorType) =
  CoreCtor
    (Name ctorName)
    (collectCtorFields paramNames ctorType)

isTypeArg :: Set.Set T.Text -> Set.Set T.Text -> AST.Expr -> Bool
isTypeArg typeNames typeVars expr =
  isTypeExpr typeNames typeVars expr || isTypeVar typeVars expr

isProofTerm :: AST.Expr -> Bool
isProofTerm expr =
  case expr of
    AST.EReflexive {} -> True
    AST.ERewrite {} -> True
    _ -> False

trimCtorArgs :: Map.Map T.Text Int -> T.Text -> [CoreValue] -> [CoreValue]
trimCtorArgs ctorArity ctorName args =
  case Map.lookup ctorName ctorArity of
    Nothing -> args
    Just arity ->
      let extra = length args - arity
      in if extra <= 0 then args else drop extra args

isTypeVar :: Set.Set T.Text -> AST.Expr -> Bool
isTypeVar typeVars expr = case expr of
  AST.EVar name -> name `Set.member` typeVars
  AST.EAnnot inner _ -> isTypeVar typeVars inner
  AST.ETyped inner _ -> isTypeVar typeVars inner
  _ -> False

isTypeExpr :: Set.Set T.Text -> Set.Set T.Text -> AST.Expr -> Bool
isTypeExpr typeNames typeVars expr = case expr of
  AST.ETypeConst _ -> True
  AST.ETypeUniverse _ -> True
  AST.EForAll {} -> True
  AST.EThereExists {} -> True
  AST.ECompType {} -> True
  AST.EEqual {} -> True
  AST.ELift {} -> True
  AST.ETypeClass {} -> True
  AST.EInstance {} -> True
  AST.EFunction _ _ retTy _ ->
    isUniverseExpr retTy
  AST.EVar name ->
    name `Set.member` typeNames
      || name `Set.member` typeVars
      || isTypeNameHeuristic name
      || isPredicateName name
  AST.EApp fn _args ->
    isTypeExpr typeNames typeVars fn
  AST.EAnnot inner _ -> isTypeExpr typeNames typeVars inner
  AST.ETyped inner _ -> isTypeExpr typeNames typeVars inner
  _ -> False

isUniverseExpr :: AST.Expr -> Bool
isUniverseExpr expr =
  case expr of
    AST.ETypeUniverse _ -> True
    AST.EAnnot inner _ -> isUniverseExpr inner
    AST.ETyped inner _ -> isUniverseExpr inner
    _ -> False

isTypeNameHeuristic :: T.Text -> Bool
isTypeNameHeuristic name =
  case T.uncons (lastNameSegment name) of
    Just (c, _) -> isUpper c
    Nothing -> False

isPredicateName :: T.Text -> Bool
isPredicateName name =
  T.isSuffixOf "-predicate" (lastNameSegment name)

isFunctionAliasName :: T.Text -> Bool
isFunctionAliasName name =
  lastNameSegment name == "Function"

isBinaryFunctionAliasName :: T.Text -> Bool
isBinaryFunctionAliasName name =
  lastNameSegment name == "BinaryFunction"

lastNameSegment :: T.Text -> T.Text
lastNameSegment name =
  case reverse (T.splitOn "::" name) of
    (segment : _) -> segment
    [] -> name

rewriteRecurExpr :: T.Text -> AST.Expr -> AST.Expr
rewriteRecurExpr recurName expr =
  case expr of
    AST.EVar name ->
      if name == "recur"
        then AST.EVar recurName
        else expr
    AST.ELit _ -> expr
    AST.EListLiteral elems ->
      AST.EListLiteral (map (rewriteRecurExpr recurName) elems)
    AST.ETypeConst _ -> expr
    AST.ETypeUniverse _ -> expr
    AST.EForAll v dom cod ->
      AST.EForAll
        v
        (rewriteRecurExpr recurName dom)
        (rewriteRecurExpr recurName cod)
    AST.EThereExists v dom cod ->
      AST.EThereExists
        v
        (rewriteRecurExpr recurName dom)
        (rewriteRecurExpr recurName cod)
    AST.ECompType eff ty ->
      AST.ECompType
        (rewriteRecurExpr recurName eff)
        (rewriteRecurExpr recurName ty)
    AST.EEqual ty left right ->
      AST.EEqual
        (rewriteRecurExpr recurName ty)
        (rewriteRecurExpr recurName left)
        (rewriteRecurExpr recurName right)
    AST.EReflexive ty term ->
      AST.EReflexive
        (rewriteRecurExpr recurName ty)
        (rewriteRecurExpr recurName term)
    AST.ERewrite family proof body ->
      AST.ERewrite
        (rewriteRecurExpr recurName family)
        (rewriteRecurExpr recurName proof)
        (rewriteRecurExpr recurName body)
    AST.EPack name ty predicate witness body ->
      AST.EPack
        name
        (rewriteRecurExpr recurName ty)
        (rewriteRecurExpr recurName predicate)
        (rewriteRecurExpr recurName witness)
        (rewriteRecurExpr recurName body)
    AST.EUnpack packed x y body ->
      AST.EUnpack
        (rewriteRecurExpr recurName packed)
        x
        y
        (rewriteRecurExpr recurName body)
    AST.ELift ty lo hi ->
      AST.ELift (rewriteRecurExpr recurName ty) lo hi
    AST.EUp ty lo hi body ->
      AST.EUp
        (rewriteRecurExpr recurName ty)
        lo
        hi
        (rewriteRecurExpr recurName body)
    AST.EDown ty lo hi body ->
      AST.EDown
        (rewriteRecurExpr recurName ty)
        lo
        hi
        (rewriteRecurExpr recurName body)
    AST.EApp fn args ->
      AST.EApp
        (rewriteRecurExpr recurName fn)
        (map (rewriteRecurExpr recurName) args)
    AST.EFunction params constraints retTy body ->
      AST.EFunction
        (map (rewriteRecurParam recurName) params)
        (map (rewriteRecurConstraint recurName) constraints)
        (rewriteRecurExpr recurName retTy)
        (rewriteRecurFunctionBody recurName body)
    AST.ELet name value body ->
      AST.ELet
        name
        (rewriteRecurExpr recurName value)
        (rewriteRecurExpr recurName body)
    AST.ECompute comp ->
      AST.ECompute (rewriteRecurComp recurName comp)
    AST.EMatch scrut scrutTy scrutName retTy cases ->
      AST.EMatch
        (rewriteRecurExpr recurName scrut)
        (rewriteRecurExpr recurName scrutTy)
        scrutName
        (rewriteRecurExpr recurName retTy)
        (map (rewriteRecurMatchCase recurName) cases)
    AST.EData params universe cases ->
      AST.EData
        (map (rewriteRecurParam recurName) params)
        (rewriteRecurExpr recurName universe)
        (map (rewriteRecurDataCase recurName) cases)
    AST.EAnnot inner ty ->
      AST.EAnnot
        (rewriteRecurExpr recurName inner)
        (rewriteRecurExpr recurName ty)
    AST.ETyped inner ty ->
      AST.ETyped
        (rewriteRecurExpr recurName inner)
        (rewriteRecurExpr recurName ty)
    AST.EDict className impls ->
      AST.EDict
        className
        [ (name, rewriteRecurExpr recurName impl)
        | (name, impl) <- impls
        ]
    AST.EDictAccess dictExpr method ->
      AST.EDictAccess (rewriteRecurExpr recurName dictExpr) method
    AST.ETypeClass param kind methods ->
      AST.ETypeClass
        param
        (rewriteRecurExpr recurName kind)
        [ (name, rewriteRecurExpr recurName ty)
        | (name, ty) <- methods
        ]
    AST.EInstance cls instTy methods ->
      AST.EInstance
        cls
        (rewriteRecurExpr recurName instTy)
        [ (name, rewriteRecurExpr recurName impl)
        | (name, impl) <- methods
        ]

rewriteRecurParam :: T.Text -> AST.Param -> AST.Param
rewriteRecurParam recurName (AST.Param name ty) =
  AST.Param name (rewriteRecurExpr recurName ty)

rewriteRecurConstraint :: T.Text -> AST.Constraint -> AST.Constraint
rewriteRecurConstraint recurName (AST.Constraint cls ty) =
  AST.Constraint cls (rewriteRecurExpr recurName ty)

rewriteRecurFunctionBody :: T.Text -> AST.FunctionBody -> AST.FunctionBody
rewriteRecurFunctionBody recurName body =
  case body of
    AST.FunctionValue expr ->
      AST.FunctionValue (rewriteRecurExpr recurName expr)
    AST.FunctionCompute comp ->
      AST.FunctionCompute (rewriteRecurComp recurName comp)

rewriteRecurComp :: T.Text -> AST.Comp -> AST.Comp
rewriteRecurComp recurName comp =
  case comp of
    AST.CReturn expr ->
      AST.CReturn (rewriteRecurExpr recurName expr)
    AST.CBind name left right ->
      AST.CBind
        name
        (rewriteRecurComp recurName left)
        (rewriteRecurComp recurName right)
    AST.CPerform expr ->
      AST.CPerform (rewriteRecurExpr recurName expr)

rewriteRecurMatchCase :: T.Text -> AST.MatchCase -> AST.MatchCase
rewriteRecurMatchCase recurName (AST.MatchCase ctor params body) =
  AST.MatchCase
    ctor
    (map (rewriteRecurParam recurName) params)
    (rewriteRecurExpr recurName body)

rewriteRecurDataCase :: T.Text -> AST.DataCase -> AST.DataCase
rewriteRecurDataCase recurName (AST.DataCase name ty) =
  AST.DataCase name (rewriteRecurExpr recurName ty)

isTypeParam :: AST.Expr -> Bool
isTypeParam expr = case expr of
  AST.ETypeUniverse _ -> True
  AST.EForAll _ _ cod -> isUniverseExpr cod
  AST.EApp (AST.EVar name) _ -> isKindName name
  AST.EAnnot inner _ -> isTypeParam inner
  AST.ETyped inner _ -> isTypeParam inner
  _ -> False

simplifyModule :: CoreModule -> CoreModule
simplifyModule (CoreModule name decls) =
  let nullaryCtors = collectNullaryCtors decls
   in CoreModule name (map (simplifyDecl nullaryCtors) decls)

collectNullaryCtors :: [CoreDecl] -> Set.Set Name
collectNullaryCtors decls =
  let fromData =
        [ ctorName ctor
        | CoreData (CoreDataDecl _ _ ctors) <- decls
        , ctor <- ctors
        , null (ctorFields ctor)
        ]
      builtin =
        [ Name "Boolean::true"
        , Name "Boolean::false"
        , Name "Unit::tt"
        , Name "List::empty"
        ]
   in Set.fromList (fromData ++ builtin)

simplifyDecl :: Set.Set Name -> CoreDecl -> CoreDecl
simplifyDecl nullaryCtors decl =
  case decl of
    CoreDef name ty value -> CoreDef name ty (simplifyValue nullaryCtors value)
    CoreDefComp name ty comp -> CoreDefComp name ty (simplifyComp nullaryCtors comp)
    CoreData _ -> decl

simplifyValue :: Set.Set Name -> CoreValue -> CoreValue
simplifyValue nullaryCtors value =
  case value of
    VVar _ -> value
    VLit _ -> value
    VErased -> value
    VLam name ty body -> VLam name ty (simplifyValue nullaryCtors body)
    VApp fn arg -> VApp (simplifyValue nullaryCtors fn) (simplifyValue nullaryCtors arg)
    VConstructor name args -> VConstructor name (map (simplifyValue nullaryCtors) args)
    VCompute comp -> VCompute (simplifyComp nullaryCtors comp)
    VLet name val body ->
      VLet name (simplifyValue nullaryCtors val) (simplifyValue nullaryCtors body)
    VMatch scrut cases ->
      let scrut' = simplifyValue nullaryCtors scrut
          cases' = map (simplifyValueCase nullaryCtors) cases
       in case selectValueCase nullaryCtors scrut' cases' of
            Just body -> simplifyValue nullaryCtors body
            Nothing -> VMatch scrut' cases'

simplifyValueCase :: Set.Set Name -> CoreValueCase -> CoreValueCase
simplifyValueCase nullaryCtors (CoreValueCase ctor binders body) =
  CoreValueCase ctor binders (simplifyValue nullaryCtors body)

simplifyComp :: Set.Set Name -> CoreComp -> CoreComp
simplifyComp nullaryCtors comp =
  case comp of
    CReturn value -> CReturn (simplifyValue nullaryCtors value)
    CBind name left right ->
      CBind name (simplifyComp nullaryCtors left) (simplifyComp nullaryCtors right)
    CPerform value -> CPerform (simplifyValue nullaryCtors value)
    CApp fn arg -> CApp (simplifyValue nullaryCtors fn) (simplifyValue nullaryCtors arg)
    CLet name val body ->
      CLet name (simplifyValue nullaryCtors val) (simplifyComp nullaryCtors body)
    CMatch scrut cases ->
      let scrut' = simplifyValue nullaryCtors scrut
          cases' = map (simplifyCompCase nullaryCtors) cases
       in case selectCompCase nullaryCtors scrut' cases' of
            Just body -> simplifyComp nullaryCtors body
            Nothing -> CMatch scrut' cases'

simplifyCompCase :: Set.Set Name -> CoreCase -> CoreCase
simplifyCompCase nullaryCtors (CoreCase ctor binders body) =
  CoreCase ctor binders (simplifyComp nullaryCtors body)

selectValueCase :: Set.Set Name -> CoreValue -> [CoreValueCase] -> Maybe CoreValue
selectValueCase nullaryCtors scrut cases =
  case matchCtorArgs nullaryCtors scrut of
    Nothing -> Nothing
    Just (ctor, args) ->
      case [ (binders, body) | CoreValueCase ctor' binders body <- cases, ctor' == ctor ] of
        ((binders, body):_) ->
          if length binders == length args
            then Just (applyValueSubsts binders args body)
            else Nothing
        [] -> Nothing

selectCompCase :: Set.Set Name -> CoreValue -> [CoreCase] -> Maybe CoreComp
selectCompCase nullaryCtors scrut cases =
  case matchCtorArgs nullaryCtors scrut of
    Nothing -> Nothing
    Just (ctor, args) ->
      case [ (binders, body) | CoreCase ctor' binders body <- cases, ctor' == ctor ] of
        ((binders, body):_) ->
          if length binders == length args
            then Just (applyCompSubsts binders args body)
            else Nothing
        [] -> Nothing

matchCtorArgs :: Set.Set Name -> CoreValue -> Maybe (Name, [CoreValue])
matchCtorArgs nullaryCtors value =
  case value of
    VConstructor name args -> Just (name, args)
    VVar name | name `Set.member` nullaryCtors -> Just (name, [])
    VLit (LitBoolean True) -> Just (Name "Boolean::true", [])
    VLit (LitBoolean False) -> Just (Name "Boolean::false", [])
    VLit LitUnit -> Just (Name "Unit::tt", [])
    _ -> Nothing

applyValueSubsts :: [Name] -> [CoreValue] -> CoreValue -> CoreValue
applyValueSubsts binders args body =
  foldl (\acc (name, val) -> substValue name val acc) body (zip binders args)

applyCompSubsts :: [Name] -> [CoreValue] -> CoreComp -> CoreComp
applyCompSubsts binders args body =
  foldl (\acc (name, val) -> substComp name val acc) body (zip binders args)

substValue :: Name -> CoreValue -> CoreValue -> CoreValue
substValue target replacement value =
  case value of
    VVar name | name == target -> replacement
    VVar _ -> value
    VLit _ -> value
    VErased -> value
    VLam name ty body ->
      if name == target
        then VLam name ty body
        else VLam name ty (substValue target replacement body)
    VApp fn arg ->
      VApp (substValue target replacement fn) (substValue target replacement arg)
    VConstructor name args ->
      VConstructor name (map (substValue target replacement) args)
    VCompute comp -> VCompute (substComp target replacement comp)
    VLet name val body ->
      let val' = substValue target replacement val
      in if name == target
        then VLet name val' body
        else VLet name val' (substValue target replacement body)
    VMatch scrut cases ->
      let scrut' = substValue target replacement scrut
          cases' = map (substValueCase target replacement) cases
      in VMatch scrut' cases'

substValueCase :: Name -> CoreValue -> CoreValueCase -> CoreValueCase
substValueCase target replacement (CoreValueCase ctor binders body) =
  if target `elem` binders
    then CoreValueCase ctor binders body
    else CoreValueCase ctor binders (substValue target replacement body)

substComp :: Name -> CoreValue -> CoreComp -> CoreComp
substComp target replacement comp =
  case comp of
    CReturn value -> CReturn (substValue target replacement value)
    CBind name left right ->
      let left' = substComp target replacement left
      in if name == target
        then CBind name left' right
        else CBind name left' (substComp target replacement right)
    CPerform value -> CPerform (substValue target replacement value)
    CApp fn arg -> CApp (substValue target replacement fn) (substValue target replacement arg)
    CLet name val body ->
      let val' = substValue target replacement val
      in if name == target
        then CLet name val' body
        else CLet name val' (substComp target replacement body)
    CMatch scrut cases ->
      let scrut' = substValue target replacement scrut
          cases' = map (substCompCase target replacement) cases
      in CMatch scrut' cases'

substCompCase :: Name -> CoreValue -> CoreCase -> CoreCase
substCompCase target replacement (CoreCase ctor binders body) =
  if target `elem` binders
    then CoreCase ctor binders body
    else CoreCase ctor binders (substComp target replacement body)

isKindName :: T.Text -> Bool
isKindName name =
  name == "TypeFunction"
    || name == "BinaryTypeFunction"
    || T.isSuffixOf "::TypeFunction" name
    || T.isSuffixOf "::BinaryTypeFunction" name

collectCtorFields :: Set.Set T.Text -> AST.Expr -> [CoreType]
collectCtorFields paramNames expr =
  case expr of
    AST.EAnnot inner _ -> collectCtorFields paramNames inner
    AST.ETyped inner _ -> collectCtorFields paramNames inner
    AST.EForAll _ dom cod ->
      lowerType paramNames dom : collectCtorFields paramNames cod
    AST.EApp (AST.EVar name) [dom, cod]
      | isFunctionAliasName name ->
          lowerType paramNames dom : collectCtorFields paramNames cod
    AST.EApp (AST.EVar name) [first, second, third]
      | isBinaryFunctionAliasName name ->
          lowerType paramNames first
            : lowerType paramNames second
            : collectCtorFields paramNames third
    _ -> []

lowerType :: Set.Set T.Text -> AST.Expr -> CoreType
lowerType paramNames expr =
  case expr of
    AST.ETypeConst tc -> lowerTypeConst tc
    AST.ETypeUniverse _ -> TyUnit
    AST.EForAll _ dom cod -> TyFun (lowerType paramNames dom) (lowerType paramNames cod)
    AST.EThereExists _ dom cod -> TyCon (Name "Exists") [lowerType paramNames dom, lowerType paramNames cod]
    AST.ECompType _eff ty -> TyComp (lowerType paramNames ty)
    AST.ELift ty _ _ -> lowerType paramNames ty
    AST.EUp _ _ _ body -> lowerType paramNames body
    AST.EDown _ _ _ body -> lowerType paramNames body
    AST.EApp (AST.EVar name) [dom, cod]
      | isFunctionAliasName name ->
          TyFun (lowerType paramNames dom) (lowerType paramNames cod)
    AST.EApp (AST.EVar name) [first, second, third]
      | isBinaryFunctionAliasName name ->
          TyFun
            (lowerType paramNames first)
            (TyFun (lowerType paramNames second) (lowerType paramNames third))
    AST.EVar name ->
      if name `Set.member` paramNames
        then TyVar (Name name)
        else TyCon (Name name) []
    AST.EApp f args ->
      let headTy = lowerType paramNames f
          argTys = map (lowerType paramNames) args
       in case headTy of
            TyCon name existing -> TyCon name (existing ++ argTys)
            TyVar name -> TyCon name argTys
            _ -> headTy
    _ -> TyUnit

lowerTypeConst :: AST.TypeConst -> CoreType
lowerTypeConst tc =
  case tc of
    AST.TCNatural -> TyNatural
    AST.TCString -> TyString
    AST.TCBoolean -> TyBoolean
    AST.TCUnit -> TyUnit
    AST.TCList -> TyCon (Name "List") []
    AST.TCPair -> TyCon (Name "Pair") []
    AST.TCDictionary -> TyCon (Name "Dictionary") []
    AST.TCListener -> TyCon (Name "Listener") []
    AST.TCSocket -> TyCon (Name "Socket") []

lowerTypeClass :: T.Text -> T.Text -> [(T.Text, AST.Expr)] -> CoreDataDecl
lowerTypeClass className paramName methods =
  CoreDataDecl
    (Name className)
    [Name paramName]
    [CoreCtor (Name (className <> ::dict)) (map (lowerType (Set.singleton paramName) . snd) methods)]

lowerInstance :: Set.Set T.Text
              -> Set.Set T.Text
              -> Map.Map T.Text Int
              -> Map.Map T.Text [AST.Constraint]
              -> Set.Set T.Text
              -> T.Text
              -> [(T.Text, AST.Expr)]
              -> CoreValue
lowerInstance typeNames ctorNames ctorArity constraintsMap typeVars className methods =
  VConstructor
    (Name (className <> ::dict))
    (map (lowerExpr typeNames ctorNames ctorArity constraintsMap typeVars . snd) methods)

getDefConstraints :: AST.Expr -> [AST.Constraint]
getDefConstraints expr =
  case expr of
    AST.EFunction _ c _ _ -> c
    AST.ETyped e _ -> getDefConstraints e
    AST.EAnnot e _ -> getDefConstraints e
    _ -> []

collectConstraintsMap :: [AST.Definition] -> Map.Map T.Text [AST.Constraint]
collectConstraintsMap defs =
  Map.fromList
    [ (AST.defName defn, getDefConstraints (AST.defBody defn))
    | defn <- defs
    ]
