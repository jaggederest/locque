{-# LANGUAGE OverloadedStrings #-}
module SmythConfig
  ( SmythConfig(..)
  , findSmythfile
  , loadSmythConfig
  , defaultConfig
  ) where

import AST
import Parser (parseMExprFile)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (find)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath ((</>), takeDirectory)

data SmythConfig = SmythConfig
  { projectName   :: T.Text
  , projectRoot   :: FilePath
  , testRoot      :: FilePath   -- relative to projectRoot
  , libRoot       :: FilePath   -- relative to projectRoot
  , sourceRoot    :: FilePath   -- relative to projectRoot
  , logLevel      :: T.Text     -- ERROR, WARN, INFO, DEBUG, TRACE
  , colorMode     :: T.Text     -- auto, always, never
  , errorTests    :: [(FilePath, T.Text)]
  } deriving (Show)

defaultConfig :: FilePath -> SmythConfig
defaultConfig root = SmythConfig
  { projectName = "locque"
  , projectRoot = root
  , testRoot = "test"
  , libRoot = "lib"
  , sourceRoot = "interpreter/src"
  , logLevel = "ERROR"
  , colorMode = "auto"
  , errorTests = []
  }

-- | Find Smythfile.lq by recursing up from current directory
findSmythfile :: IO (Maybe FilePath)
findSmythfile = do
  cwd <- getCurrentDirectory
  search cwd
  where
    search dir = do
      let smythfile = dir </> "Smythfile.lq"
      exists <- doesFileExist smythfile
      if exists
        then pure (Just dir)
        else do
          let parent = takeDirectory dir
          if parent == dir  -- reached root
            then pure Nothing
            else search parent

loadSmythConfig :: FilePath -> IO SmythConfig
loadSmythConfig projectRoot = do
  let smythfile = projectRoot </> "Smythfile.lq"
  exists <- doesFileExist smythfile
  if not exists
    then pure (defaultConfig projectRoot)
    else do
      contents <- TIO.readFile smythfile
      case parseMExprFile smythfile contents of
        Left err -> error err
        Right (Module _ _ _ defs) ->
          case applyDefs smythfile (defaultConfig projectRoot) defs of
            Left msg -> error msg
            Right cfg -> pure cfg

applyDefs :: FilePath -> SmythConfig -> [Definition] -> Either String SmythConfig
applyDefs path cfg defs = do
  cfg1 <- applyStringDef path defs "project-name" (\v c -> c { projectName = v }) cfg
  cfg2 <- applyStringDef path defs "test-root" (\v c -> c { testRoot = T.unpack v }) cfg1
  cfg3 <- applyStringDef path defs "lib-root" (\v c -> c { libRoot = T.unpack v }) cfg2
  cfg4 <- applyStringDef path defs "source-root" (\v c -> c { sourceRoot = T.unpack v }) cfg3
  cfg5 <- applyStringDef path defs "default-log-level" (\v c -> c { logLevel = v }) cfg4
  cfg6 <- applyStringDef path defs "color-mode" (\v c -> c { colorMode = v }) cfg5
  case lookupDef "error-tests" defs of
    Nothing -> Right cfg6
    Just expr -> do
      tests <- parseErrorTests path expr
      Right cfg6 { errorTests = tests }

applyStringDef
  :: FilePath
  -> [Definition]
  -> T.Text
  -> (T.Text -> SmythConfig -> SmythConfig)
  -> SmythConfig
  -> Either String SmythConfig
applyStringDef path defs name setter cfg =
  case lookupDef name defs of
    Nothing -> Right cfg
    Just expr -> case asString expr of
      Just val -> Right (setter val cfg)
      Nothing -> Left (path ++ ": " ++ T.unpack name ++ " must be a string literal")

lookupDef :: T.Text -> [Definition] -> Maybe Expr
lookupDef name defs = defBody <$> find (\d -> defName d == name) defs

parseErrorTests :: FilePath -> Expr -> Either String [(FilePath, T.Text)]
parseErrorTests path expr = do
  items <- withPath (parseList expr)
  mapM parseErrorPair items
  where
    withPath (Left msg) = Left (path ++ ": " ++ msg)
    withPath (Right val) = Right val

    parseErrorPair e = do
      (fileTxt, msgTxt) <- parsePair e
      pure (T.unpack fileTxt, msgTxt)

    parsePair e = case stripExpr e of
      EApp f args -> case stripExpr f of
        EVar name
          | isPairName name -> case args of
              [a, b] -> pairFrom a b
              [_, _, a, b] -> pairFrom a b
              _ -> Left (path ++ ": error-tests pair expects 2 or 4 args")
        _ -> Left (path ++ ": error-tests must contain pair values")
      _ -> Left (path ++ ": error-tests must contain pair values")

    pairFrom a b = case (asString a, asString b) of
      (Just fileTxt, Just msgTxt) -> Right (fileTxt, msgTxt)
      _ -> Left (path ++ ": error-tests pair values must be string literals")

parseList :: Expr -> Either String [Expr]
parseList expr = case stripExpr expr of
  EListLiteral elems -> Right elems
  EVar name
    | isNilName name -> Right []
  EApp f args -> case stripExpr f of
    EVar name
      | isNilName name ->
          case args of
            [_] -> Right []
            _ -> Left "error-tests nil expects 1 type arg"
      | isConsName name -> case args of
          [headExpr, tailExpr] -> do
            rest <- parseList tailExpr
            Right (headExpr : rest)
          [_, headExpr, tailExpr] -> do
            rest <- parseList tailExpr
            Right (headExpr : rest)
          _ -> Left "error-tests list expects cons with 2 or 3 args"
    _ -> Left "error-tests must be a list"
  _ -> Left "error-tests must be a list"

asString :: Expr -> Maybe T.Text
asString expr = case stripExpr expr of
  ELit (LString s) -> Just s
  _ -> Nothing

stripExpr :: Expr -> Expr
stripExpr expr = case expr of
  EAnnot e _ -> stripExpr e
  ETyped e _ -> stripExpr e
  _ -> expr

isConsName :: T.Text -> Bool
isConsName name = name == "cons" || T.isSuffixOf "::cons" name

isNilName :: T.Text -> Bool
isNilName name = name == "nil" || T.isSuffixOf "::nil" name

isPairName :: T.Text -> Bool
isPairName name = name == "pair" || T.isSuffixOf "::pair" name
