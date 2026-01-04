{-# LANGUAGE TypeApplications #-}
module CompilerPipeline
  ( loadAnnotatedModule
  , loadCoreModule
  ) where

import Control.Exception (SomeException, try, evaluate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath (takeExtension)

import AST
import CompilerLower (lowerModule)
import DictPass (transformModuleWithEnvsScope)
import Eval (ctorArityMap)
import ImportResolver (ImportScope(..), ResolvedModule(..), resolveModulePath)
import Parser (parseMExprFile, parseModuleFile)
import Recursor (recursorDefs, insertRecursors)
import SmythConfig (SmythConfig(..))
import Utils (qualifyName)
import Validator (checkParens)
import qualified RunCache as RC
import qualified TypeChecker as TC

import qualified Locque.Compiler.Core as Core

loadCoreModule :: SmythConfig -> FilePath -> IO (Either String Core.CoreModule)
loadCoreModule config file = do
  annotated <- loadAnnotatedModuleWithImports config file
  pure (fmap lowerModule annotated)

loadAnnotatedModule :: SmythConfig -> FilePath -> IO (Either String Module)
loadAnnotatedModule config file =
  loadAnnotatedModuleWithScope config ProjectScope file

loadAnnotatedModuleWithScope :: SmythConfig -> ImportScope -> FilePath -> IO (Either String Module)
loadAnnotatedModuleWithScope config scope file = do
  contents <- TIO.readFile file
  case parseAny file contents of
    Left err -> pure (Left err)
    Right m -> do
      digestAttempt <- try @SomeException
        (TC.moduleDigestWithImportsScope (projectRoot config) scope contents m)
      case digestAttempt of
        Left err -> pure (Left ("Type check phase error: " ++ show err))
        Right (digest, importedEnv) -> do
          cached <- RC.readRunCache (projectRoot config) file digest
          case cached of
            Just entry -> pure (Right (RC.cacheAnnotated entry))
            Nothing -> do
              tcAttempt <- try (evaluate (TC.typeCheckAndNormalizeWithEnv importedEnv m))
                :: IO (Either SomeException (Either TC.TypeError (TC.TypeEnv, Module)))
              case tcAttempt of
                Left err -> pure (Left ("Type check phase error: " ++ show err))
                Right (Left tcErr) -> pure (Left ("Type error: " ++ show tcErr))
                Right (Right (env, normalized)) -> do
                  let arity = ctorArityMap normalized
                      recDefs = recursorDefs normalized
                  prepared <- case insertRecursors m recDefs of
                    Left msg -> pure (Left ("Transform error: " ++ msg))
                    Right ok -> pure (Right ok)
                  case prepared of
                    Left msg -> pure (Left msg)
                    Right preparedModule -> do
                      transformed <- transformModuleWithEnvsScope (projectRoot config) scope preparedModule
                      case TC.annotateModule env transformed of
                        Left annotErr -> pure (Left ("Annotation error: " ++ show annotErr))
                        Right annotatedM -> do
                          let entry = RC.RunCache
                                { RC.cacheVersion = RC.cacheVersionCurrent
                                , RC.cacheDigest = digest
                                , RC.cacheAnnotated = annotatedM
                                , RC.cacheCtorArity = arity
                                }
                          RC.writeRunCache (projectRoot config) file entry
                          pure (Right annotatedM)

parseAny :: FilePath -> T.Text -> Either String Module
parseAny file contents =
  case takeExtension file of
    ".lq"  -> parseMExprFile file contents
    ".lqs" -> do
      _ <- checkParens file contents
      parseModuleFile file contents
    _      -> Left ("Unsupported file extension (expected .lq or .lqs): " ++ file)

loadAnnotatedModuleWithImports :: SmythConfig -> FilePath -> IO (Either String Module)
loadAnnotatedModuleWithImports config file = do
  entry <- loadAnnotatedModule config file
  case entry of
    Left err -> pure (Left err)
    Right modEntry -> do
      defsResult <- flattenModule config ProjectScope Set.empty modEntry
      case defsResult of
        Left err -> pure (Left err)
        Right defs ->
          let combined = Module (modName modEntry) [] [] (dedupeDefs defs)
          in pure (Right combined)

flattenModule :: SmythConfig -> ImportScope -> Set.Set T.Text -> Module -> IO (Either String [Definition])
flattenModule config scope visiting modEntry = do
  depsResult <- loadImports config scope visiting (modImports modEntry)
  case depsResult of
    Left err -> pure (Left err)
    Right deps -> do
      let locals = renameModuleDefs Nothing modEntry
      pure (Right (deps ++ locals))

loadImports :: SmythConfig -> ImportScope -> Set.Set T.Text -> [Import] -> IO (Either String [Definition])
loadImports config scope visiting imports = do
  results <- mapM (loadImport config scope visiting) imports
  pure (concatEither results)

loadImport :: SmythConfig -> ImportScope -> Set.Set T.Text -> Import -> IO (Either String [Definition])
loadImport config scope visiting (Import modName alias) = do
  if modName `Set.member` visiting
    then pure (Left ("Import cycle detected: " ++ T.unpack modName))
    else do
      ResolvedModule modPath nextScope <-
        resolveModulePath (projectRoot config) (libRoot config) scope modName
      imported <- loadAnnotatedModuleWithScope config nextScope modPath
      case imported of
        Left err -> pure (Left err)
        Right modImported -> do
          let visiting' = Set.insert modName visiting
          depsResult <- loadImports config nextScope visiting' (modImports modImported)
          case depsResult of
            Left err -> pure (Left err)
            Right deps -> do
              let qualified = renameModuleDefs (Just alias) modImported
              pure (Right (deps ++ qualified))

dedupeDefs :: [Definition] -> [Definition]
dedupeDefs defs =
  Map.elems (foldl insertDef Map.empty defs)
  where
    insertDef acc def = Map.insert (defName def) def acc

renameModuleDefs :: Maybe T.Text -> Module -> [Definition]
renameModuleDefs maybeAlias modEntry =
  map renameDef (modDefs modEntry)
  where
    localNamesList = defNames modEntry ++ ctorNames modEntry
    localNames = Set.fromList localNamesList
    renameMap = case maybeAlias of
      Nothing -> Map.empty
      Just alias ->
        Map.fromList [ (name, qualifyName alias name) | name <- localNamesList ]
    openMap = Map.fromList
      [ (name, qualifyName (openModule open) name)
      | open <- modOpens modEntry
      , name <- openNames open
      ]
    renameDef (Definition tr name body) =
      let name' = case maybeAlias of
            Nothing -> name
            Just alias -> qualifyName alias name
          body' = renameExpr renameMap openMap localNames Set.empty body
      in Definition tr name' body'

defNames :: Module -> [T.Text]
defNames (Module _ _ _ defs) = map defName defs

ctorNames :: Module -> [T.Text]
ctorNames (Module _ _ _ defs) =
  concatMap defCtorNames defs

defCtorNames :: Definition -> [T.Text]
defCtorNames defn = case defBody defn of
  EData _ _ cases -> map dataCaseName cases
  _ -> []

renameExpr :: Map.Map T.Text T.Text -> Map.Map T.Text T.Text -> Set.Set T.Text -> Set.Set T.Text -> Expr -> Expr
renameExpr renameMap openMap localNames bound expr =
  case expr of
    EVar name -> EVar (renameName name)
    ELit _ -> expr
    EListLiteral elems -> EListLiteral (map (renameExpr renameMap openMap localNames bound) elems)
    ETypeConst _ -> expr
    ETypeUniverse _ -> expr
    EForAll v dom cod ->
      let dom' = renameExpr renameMap openMap localNames bound dom
          bound' = Set.insert v bound
          cod' = renameExpr renameMap openMap localNames bound' cod
      in EForAll v dom' cod'
    EThereExists v dom cod ->
      let dom' = renameExpr renameMap openMap localNames bound dom
          bound' = Set.insert v bound
          cod' = renameExpr renameMap openMap localNames bound' cod
      in EThereExists v dom' cod'
    ECompType eff t ->
      ECompType
        (renameExpr renameMap openMap localNames bound eff)
        (renameExpr renameMap openMap localNames bound t)
    EEqual ty lhs rhs ->
      EEqual
        (renameExpr renameMap openMap localNames bound ty)
        (renameExpr renameMap openMap localNames bound lhs)
        (renameExpr renameMap openMap localNames bound rhs)
    EReflexive ty term ->
      EReflexive
        (renameExpr renameMap openMap localNames bound ty)
        (renameExpr renameMap openMap localNames bound term)
    ERewrite family proof body ->
      ERewrite
        (renameExpr renameMap openMap localNames bound family)
        (renameExpr renameMap openMap localNames bound proof)
        (renameExpr renameMap openMap localNames bound body)
    EPack v dom cod witness body ->
      let dom' = renameExpr renameMap openMap localNames bound dom
          bound' = Set.insert v bound
          cod' = renameExpr renameMap openMap localNames bound' cod
          witness' = renameExpr renameMap openMap localNames bound witness
          body' = renameExpr renameMap openMap localNames bound body
      in EPack v dom' cod' witness' body'
    EUnpack packed x y body ->
      let packed' = renameExpr renameMap openMap localNames bound packed
          bound' = Set.insert x (Set.insert y bound)
          body' = renameExpr renameMap openMap localNames bound' body
      in EUnpack packed' x y body'
    ELift ty fromLevel toLevel ->
      ELift (renameExpr renameMap openMap localNames bound ty) fromLevel toLevel
    EUp ty fromLevel toLevel body ->
      EUp
        (renameExpr renameMap openMap localNames bound ty)
        fromLevel
        toLevel
        (renameExpr renameMap openMap localNames bound body)
    EDown ty fromLevel toLevel body ->
      EDown
        (renameExpr renameMap openMap localNames bound ty)
        fromLevel
        toLevel
        (renameExpr renameMap openMap localNames bound body)
    EApp f args ->
      EApp
        (renameExpr renameMap openMap localNames bound f)
        (map (renameExpr renameMap openMap localNames bound) args)
    EFunction params constraints ret body ->
      let params' = map (renameParam renameMap openMap localNames bound) params
          bound' = bound `Set.union` Set.fromList (map paramName params)
          constraints' = map (renameConstraint renameMap openMap localNames bound') constraints
          ret' = renameExpr renameMap openMap localNames bound' ret
          body' = renameFunctionBody renameMap openMap localNames bound' body
      in EFunction params' constraints' ret' body'
    ELet name value body ->
      let value' = renameExpr renameMap openMap localNames bound value
          bound' = Set.insert name bound
          body' = renameExpr renameMap openMap localNames bound' body
      in ELet name value' body'
    ECompute comp ->
      ECompute (renameComp renameMap openMap localNames bound comp)
    EMatch scrut scrutTy scrutName retTy cases ->
      let scrut' = renameExpr renameMap openMap localNames bound scrut
          scrutTy' = renameExpr renameMap openMap localNames bound scrutTy
          bound' = Set.insert scrutName bound
          retTy' = renameExpr renameMap openMap localNames bound' retTy
          cases' = map (renameMatchCase renameMap openMap localNames bound') cases
      in EMatch scrut' scrutTy' scrutName retTy' cases'
    EData params universe cases ->
      let params' = map (renameParam renameMap openMap localNames bound) params
          bound' = bound `Set.union` Set.fromList (map paramName params)
          universe' = renameExpr renameMap openMap localNames bound universe
          cases' = map (renameDataCase renameMap openMap localNames bound') cases
      in EData params' universe' cases'
    EAnnot inner ty ->
      EAnnot
        (renameExpr renameMap openMap localNames bound inner)
        (renameExpr renameMap openMap localNames bound ty)
    ETyped inner ty ->
      ETyped
        (renameExpr renameMap openMap localNames bound inner)
        (renameExpr renameMap openMap localNames bound ty)
    EDict className impls ->
      let className' = renameName className
          impls' = [ (n, renameExpr renameMap openMap localNames bound e) | (n, e) <- impls ]
      in EDict className' impls'
    EDictAccess d method ->
      EDictAccess (renameExpr renameMap openMap localNames bound d) (renameName method)
    ETypeClass param kind methods ->
      let kind' = renameExpr renameMap openMap localNames bound kind
          bound' = Set.insert param bound
          methods' = [ (n, renameExpr renameMap openMap localNames bound' ty) | (n, ty) <- methods ]
      in ETypeClass param kind' methods'
    EInstance cls instTy methods ->
      let cls' = renameName cls
          instTy' = renameExpr renameMap openMap localNames bound instTy
          methods' = [ (n, renameExpr renameMap openMap localNames bound e) | (n, e) <- methods ]
      in EInstance cls' instTy' methods'
  where
    renameName name
      | name `Set.member` bound = name
      | Just qualified <- Map.lookup name renameMap = qualified
      | name `Set.member` localNames = name
      | Just opened <- Map.lookup name openMap = opened
      | otherwise = name

renameParam :: Map.Map T.Text T.Text -> Map.Map T.Text T.Text -> Set.Set T.Text -> Set.Set T.Text -> Param -> Param
renameParam renameMap openMap localNames bound (Param name ty) =
  Param name (renameExpr renameMap openMap localNames bound ty)

renameConstraint :: Map.Map T.Text T.Text -> Map.Map T.Text T.Text -> Set.Set T.Text -> Set.Set T.Text -> Constraint -> Constraint
renameConstraint renameMap openMap localNames bound (Constraint cls ty) =
  let cls' = renameName cls
      ty' = renameExpr renameMap openMap localNames bound ty
  in Constraint cls' ty'
  where
    renameName name
      | name `Set.member` bound = name
      | Just qualified <- Map.lookup name renameMap = qualified
      | name `Set.member` localNames = name
      | Just opened <- Map.lookup name openMap = opened
      | otherwise = name

renameFunctionBody :: Map.Map T.Text T.Text -> Map.Map T.Text T.Text -> Set.Set T.Text -> Set.Set T.Text -> FunctionBody -> FunctionBody
renameFunctionBody renameMap openMap localNames bound body = case body of
  FunctionValue e -> FunctionValue (renameExpr renameMap openMap localNames bound e)
  FunctionCompute c -> FunctionCompute (renameComp renameMap openMap localNames bound c)

renameComp :: Map.Map T.Text T.Text -> Map.Map T.Text T.Text -> Set.Set T.Text -> Set.Set T.Text -> Comp -> Comp
renameComp renameMap openMap localNames bound comp = case comp of
  CReturn e -> CReturn (renameExpr renameMap openMap localNames bound e)
  CPerform e -> CPerform (renameExpr renameMap openMap localNames bound e)
  CBind name left right ->
    let left' = renameComp renameMap openMap localNames bound left
        bound' = Set.insert name bound
        right' = renameComp renameMap openMap localNames bound' right
    in CBind name left' right'

renameMatchCase :: Map.Map T.Text T.Text -> Map.Map T.Text T.Text -> Set.Set T.Text -> Set.Set T.Text -> MatchCase -> MatchCase
renameMatchCase renameMap openMap localNames bound (MatchCase ctor binders body) =
  let binders' = map (renameParam renameMap openMap localNames bound) binders
      bound' = bound `Set.union` Set.fromList (map paramName binders)
      body' = renameExpr renameMap openMap localNames bound' body
  in MatchCase (renameCtor ctor) binders' body'
  where
    renameCtor name =
      case Map.lookup name renameMap of
        Just qualified -> qualified
        Nothing ->
          case Map.lookup name openMap of
            Just opened -> opened
            Nothing -> name

renameDataCase :: Map.Map T.Text T.Text -> Map.Map T.Text T.Text -> Set.Set T.Text -> Set.Set T.Text -> DataCase -> DataCase
renameDataCase renameMap openMap localNames bound (DataCase ctor ty) =
  let ctor' =
        case Map.lookup ctor renameMap of
          Just qualified -> qualified
          Nothing -> ctor
      ty' = renameExpr renameMap openMap localNames bound ty
  in DataCase ctor' ty'

concatEither :: [Either String [Definition]] -> Either String [Definition]
concatEither results =
  foldr combine (Right []) results
  where
    combine next acc = case (next, acc) of
      (Left err, _) -> Left err
      (_, Left err) -> Left err
      (Right defs, Right rest) -> Right (defs ++ rest)
