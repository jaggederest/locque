{-# LANGUAGE OverloadedStrings #-}
module RunCache
  ( RunCache(..)
  , cacheVersionCurrent
  , cachePathFor
  , readRunCache
  , writeRunCache
  ) where

import AST (Module)
import CtorArity (CtorArityMap)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Bits (xor)
import Data.Char (ord)
import Data.List (isPrefixOf)
import Data.Word (Word64)
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), (<.>), makeRelative, takeDirectory, isAbsolute)
import Text.Read (readMaybe)

data RunCache = RunCache
  { cacheVersion :: Int
  , cacheDigest :: String
  , cacheAnnotated :: Module
  , cacheCtorArity :: CtorArityMap
  } deriving (Show, Read)

cacheVersionCurrent :: Int
cacheVersionCurrent = 5

cacheRoot :: FilePath -> FilePath
cacheRoot projectRoot = projectRoot </> "tmp" </> "locque" </> "cache"

hashString :: String -> String
hashString str =
  let fnvOffset :: Word64
      fnvOffset = 14695981039346656037
      fnvPrime :: Word64
      fnvPrime = 1099511628211
      step acc c = (acc `xor` fromIntegral (ord c)) * fnvPrime
      hashValue = foldl step fnvOffset str
  in showHex hashValue ""

cachePathFor :: FilePath -> FilePath -> FilePath
cachePathFor projectRoot sourcePath =
  let rel = makeRelative projectRoot sourcePath
      safeRel =
        if isAbsolute rel || ".." `isPrefixOf` rel
          then hashString sourcePath
          else rel
  in cacheRoot projectRoot </> safeRel <.> "cache"

readRunCache :: FilePath -> FilePath -> String -> IO (Maybe RunCache)
readRunCache projectRoot sourcePath digest = do
  let cachePath = cachePathFor projectRoot sourcePath
  exists <- doesFileExist cachePath
  if not exists
    then pure Nothing
    else do
      raw <- BS.readFile cachePath
      case readMaybe (BS8.unpack raw) of
        Just entry
          | cacheVersion entry == cacheVersionCurrent
          , cacheDigest entry == digest -> pure (Just entry)
        _ -> pure Nothing

writeRunCache :: FilePath -> FilePath -> RunCache -> IO ()
writeRunCache projectRoot sourcePath entry = do
  let cachePath = cachePathFor projectRoot sourcePath
  createDirectoryIfMissing True (takeDirectory cachePath)
  writeFile cachePath (show entry)
