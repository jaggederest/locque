{-# LANGUAGE OverloadedStrings #-}
module DictPass
  ( transformModule
  , transformModuleWithEnvs
  , collectClassEnv
  , collectInstEnv
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import AST
import Type (Type(..))

-- | Environment for dictionary pass
data DictEnv = DictEnv
  { deClassEnv :: Map.Map Text TypeClassBody  -- className -> class definition
  , deInstEnv  :: Map.Map Text [InstanceBody] -- className -> instances
  }

-- | Transform a module by inlining dictionary method calls
transformModule :: Map.Map Text TypeClassBody -> Map.Map Text [InstanceBody] -> Module -> Module
transformModule classEnv instEnv m =
  let env = DictEnv classEnv instEnv
  in m { modDefs = map (transformDef env) (modDefs m) }

-- | Transform a definition
transformDef :: DictEnv -> Definition -> Definition
transformDef env def = case defBody def of
  ValueBody expr -> def { defBody = ValueBody (transformExpr env expr) }
  ComputationBody comp -> def { defBody = ComputationBody (transformComp env comp) }
  _ -> def  -- TypeFamily, TypeClass, Instance definitions unchanged

-- | Transform an expression by inlining typeclass method calls
transformExpr :: DictEnv -> Expr -> Expr
transformExpr env expr = case expr of
  -- Method call application: look for method applied to argument
  -- Also handle ETyped-wrapped function (from annotation pass)
  EApp f args | Just methodName <- extractVarName f ->
    case findMethodClass methodName (deClassEnv env) of
      Just (className, _methodType, _typeParam) ->
        -- This is a method call - try to inline
        case args of
          [arg] ->
            let transformedArg = transformExpr env arg
            in case inferArgType transformedArg of
                 Just concreteType ->
                   -- Find matching instance
                   case findInstance className concreteType (deInstEnv env) of
                     Just inst ->
                       case lookup methodName (instImpls inst) of
                         Just impl -> EApp (transformExpr env impl) [transformedArg]
                         Nothing -> EApp (transformExpr env f) [transformedArg]
                     Nothing -> EApp (transformExpr env f) [transformedArg]
                 Nothing -> EApp (transformExpr env f) [transformedArg]
          _ ->
            -- Multiple arguments or no arguments - transform recursively
            EApp (transformExpr env f) (map (transformExpr env) args)
      Nothing ->
        -- Not a method call - transform recursively
        EApp (transformExpr env f) (map (transformExpr env) args)

  -- General application (no method name found)
  EApp f args -> EApp (transformExpr env f) (map (transformExpr env) args)

  -- Lambda
  ELam param mType body -> ELam param mType (transformExpr env body)
  ELamMulti params mType body -> ELamMulti params mType (transformExpr env body)

  -- Annotation
  EAnnot e ty -> EAnnot (transformExpr env e) ty

  -- Typed expression (from type checker)
  ETyped e ty -> ETyped (transformExpr env e) ty

  -- Dictionary nodes (shouldn't exist yet, but handle them)
  EDict className impls -> EDict className [(n, transformExpr env e) | (n, e) <- impls]
  EDictAccess d method -> EDictAccess (transformExpr env d) method

  -- Leaves unchanged
  EVar _ -> expr
  ELit _ -> expr

-- | Transform a computation
transformComp :: DictEnv -> Comp -> Comp
transformComp env comp = case comp of
  CReturn e -> CReturn (transformExpr env e)
  CBind v c1 c2 -> CBind v (transformComp env c1) (transformComp env c2)
  CPerform e -> CPerform (transformExpr env e)
  CSeq c1 c2 -> CSeq (transformComp env c1) (transformComp env c2)
  CVar _ -> comp

-- | Find which class a method belongs to
findMethodClass :: Text -> Map.Map Text TypeClassBody -> Maybe (Text, Type, Text)
findMethodClass methodName classEnv =
  foldr checkClass Nothing (Map.toList classEnv)
  where
    checkClass (className, TypeClassBody typeParam methods) acc =
      case lookup methodName methods of
        Just methodType -> Just (className, methodType, typeParam)
        Nothing -> acc

-- | Extract variable name from expression, unwrapping ETyped wrappers
extractVarName :: Expr -> Maybe Text
extractVarName (EVar name) = Just name
extractVarName (ETyped e _) = extractVarName e
extractVarName (EAnnot e _) = extractVarName e
extractVarName _ = Nothing

-- | Infer type from an expression (uses ETyped wrapper if available, falls back to literals)
inferArgType :: Expr -> Maybe Type
inferArgType expr = case expr of
  ETyped _ ty -> Just ty  -- Use type from type checker annotation
  ELit lit -> Just $ case lit of
    LNatural _    -> TNatural
    LString _ -> TString
    LBoolean _   -> TBoolean
  _ -> Nothing  -- Can't infer for non-literals without type info

-- | Find instance matching a concrete type
findInstance :: Text -> Type -> Map.Map Text [InstanceBody] -> Maybe InstanceBody
findInstance className concreteType instEnv =
  case Map.lookup className instEnv of
    Nothing -> Nothing
    Just instances -> findMatch concreteType instances

-- | Find first instance that matches the concrete type
findMatch :: Type -> [InstanceBody] -> Maybe InstanceBody
findMatch _ty [] = Nothing
findMatch ty (inst:rest) =
  if matchType (instType inst) ty
    then Just inst
    else findMatch ty rest

-- | Simple type matching (pattern type against concrete type)
matchType :: Type -> Type -> Bool
matchType pattern target = case (pattern, target) of
  (TVar _, _) -> True  -- Type variable matches anything
  (TNatural, TNatural) -> True
  (TString, TString) -> True
  (TBoolean, TBoolean) -> True
  (TUnit, TUnit) -> True
  (TList p, TList t) -> matchType p t
  (TPair p1 p2, TPair t1 t2) -> matchType p1 t1 && matchType p2 t2
  (TFun p1 p2, TFun t1 t2) -> matchType p1 t1 && matchType p2 t2
  _ -> False

-- | Collect typeclass definitions from a module
collectClassEnv :: Module -> Map.Map Text TypeClassBody
collectClassEnv m = Map.fromList
  [ (defName def, body)
  | def <- modDefs m
  , ClassBody body <- [defBody def]
  ]

-- | Collect instance definitions from a module (grouped by class name)
collectInstEnv :: Module -> Map.Map Text [InstanceBody]
collectInstEnv m =
  let instances = [ body | def <- modDefs m, InstBody body <- [defBody def] ]
  in foldr addInstance Map.empty instances
  where
    addInstance inst acc =
      Map.insertWith (++) (instClassName inst) [inst] acc

-- | Transform a module (self-contained version that collects envs from the module)
transformModuleWithEnvs :: Module -> Module
transformModuleWithEnvs m =
  let classEnv = collectClassEnv m
      instEnv = collectInstEnv m
  in transformModule classEnv instEnv m
