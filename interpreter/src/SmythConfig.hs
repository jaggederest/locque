{-# LANGUAGE OverloadedStrings #-}
module SmythConfig
  ( SmythConfig(..)
  , findSmythfile
  , loadSmythConfig
  , defaultConfig
  ) where

import qualified Data.Text as T
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

-- | Load configuration from Smythfile.lq
-- For now, just return default config (parsing Smythfile.lq comes later)
loadSmythConfig :: FilePath -> IO SmythConfig
loadSmythConfig projectRoot = pure (defaultConfig projectRoot)
