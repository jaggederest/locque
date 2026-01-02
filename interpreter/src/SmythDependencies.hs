{-# LANGUAGE OverloadedStrings #-}
module SmythDependencies
  ( runDependencies
  ) where

import Control.Monad (foldM, forM, when)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeExtension)

import AST (Import(..), Module(..))
import Parser (parseMExprFile)
import SmythConfig (SmythConfig(..))

runDependencies :: SmythConfig -> [String] -> IO ()
runDependencies config args = do
  when (not (null args)) $ do
    putStrLn "Error: 'smyth dependencies' does not take arguments"
    putStrLn "Usage: smyth dependencies"
    exitFailure
  let root = projectRoot config
      libDir = root </> libRoot config
  modules <- loadModules libDir
  let graph = buildGraph modules
      libNames = Set.fromList (Map.keys graph)
      roots = findRoots graph libNames
      roots' = if null roots then List.sort (Map.keys graph) else List.sort roots
  putStrLn ("Dependencies (" ++ libRoot config ++ "/)")
  _ <- printForest graph libNames roots'
  pure ()

loadModules :: FilePath -> IO [Module]
loadModules libDir = do
  exists <- doesDirectoryExist libDir
  if not exists
    then pure []
    else do
      files <- listLqFiles libDir
      mapM parseModule files

listLqFiles :: FilePath -> IO [FilePath]
listLqFiles dir = do
  entries <- listDirectory dir
  paths <- forM entries $ \entry -> do
    let path = dir </> entry
    isDir <- doesDirectoryExist path
    if isDir
      then listLqFiles path
      else if takeExtension path == ".lq"
        then pure [path]
        else pure []
  pure (concat paths)

parseModule :: FilePath -> IO Module
parseModule path = do
  contents <- TIO.readFile path
  case parseMExprFile path contents of
    Left err -> fail err
    Right m -> pure m

buildGraph :: [Module] -> Map.Map T.Text [T.Text]
buildGraph mods =
  let pairs = map moduleEntry mods
  in foldr insertModule Map.empty pairs
  where
    moduleEntry m = (modName m, map impModule (modImports m))

    insertModule (name, deps) acc =
      case Map.lookup name acc of
        Nothing -> Map.insert name (List.sort deps) acc
        Just _ -> error ("Duplicate module name: " ++ T.unpack name)

findRoots :: Map.Map T.Text [T.Text] -> Set.Set T.Text -> [T.Text]
findRoots graph libNames =
  let deps = concatMap (filter (`Set.member` libNames)) (Map.elems graph)
      incoming = Set.fromList deps
  in filter (`Set.notMember` incoming) (Map.keys graph)

printForest
  :: Map.Map T.Text [T.Text]
  -> Set.Set T.Text
  -> [T.Text]
  -> IO (Set.Set T.Text)
printForest graph libNames roots =
  foldM (printRoot graph libNames) Set.empty roots

printRoot
  :: Map.Map T.Text [T.Text]
  -> Set.Set T.Text
  -> Set.Set T.Text
  -> T.Text
  -> IO (Set.Set T.Text)
printRoot graph libNames seen name =
  printTree graph libNames Set.empty seen "" True name

printTree
  :: Map.Map T.Text [T.Text]
  -> Set.Set T.Text
  -> Set.Set T.Text
  -> Set.Set T.Text
  -> String
  -> Bool
  -> T.Text
  -> IO (Set.Set T.Text)
printTree graph libNames path seen prefix isLast name = do
  let (label, isTerminal, seen') = classifyNode libNames path seen name
      line = if null prefix
        then T.unpack label
        else prefix ++ (if isLast then "`- " else "|- ") ++ T.unpack label
  putStrLn line
  if isTerminal
    then pure seen'
    else case Map.lookup name graph of
        Nothing -> pure seen'
        Just deps -> do
          let deps' = List.sort deps
              nextPrefix = prefix ++ (if isLast then "   " else "|  ")
              path' = Set.insert name path
          foldM (printChild graph libNames path' nextPrefix) seen' (annotateLast deps')

printChild
  :: Map.Map T.Text [T.Text]
  -> Set.Set T.Text
  -> Set.Set T.Text
  -> String
  -> Set.Set T.Text
  -> (T.Text, Bool)
  -> IO (Set.Set T.Text)
printChild graph libNames path prefix seen (name, isLast) =
  printTree graph libNames path seen prefix isLast name

annotateLast :: [a] -> [(a, Bool)]
annotateLast xs =
  let total = length xs
  in zip xs [i == total | i <- [1..total]]

classifyNode
  :: Set.Set T.Text
  -> Set.Set T.Text
  -> Set.Set T.Text
  -> T.Text
  -> (T.Text, Bool, Set.Set T.Text)
classifyNode libNames path seen name
  | name `Set.member` path =
      (name <> " (cycle)", True, seen)
  | name `Set.notMember` libNames =
      (name <> " (external)", True, seen)
  | name `Set.member` seen =
      (name <> " (shared)", True, seen)
  | otherwise =
      (name, False, Set.insert name seen)
