module Main where

import qualified Data.Text.IO as T
import           System.Environment (getArgs)
import           System.Exit (die)
import           System.Directory (getCurrentDirectory)
import           System.FilePath (takeDirectory, (</>))

import           Eval
import           Parser

main :: IO ()
main = do
  args <- getArgs
  let file = case args of
        []    -> "../examples/00_hello_world.lqs"
        (x:_) -> x
  cwd <- getCurrentDirectory
  let projectRoot = takeDirectory cwd
  contents <- T.readFile file
  case parseModuleFile file contents of
    Left err -> die err
    Right m  -> runModuleMain projectRoot m
