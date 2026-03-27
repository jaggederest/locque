{-# LANGUAGE OverloadedStrings #-}
module SmythNew
  ( runNew
  ) where

import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>))
import System.Exit (exitFailure)

runNew :: [String] -> IO ()
runNew args = case args of
  [name] -> scaffoldProject name
  _ -> do
    putStrLn "Usage: smyth new <name>"
    putStrLn "  Creates a new locque project scaffold in ./<name>/"
    exitFailure

scaffoldProject :: String -> IO ()
scaffoldProject name = do
  exists <- doesDirectoryExist name
  if exists
    then do
      putStrLn ("Error: directory '" ++ name ++ "' already exists")
      exitFailure
    else do
      createDirectoryIfMissing True (name </> "lib")
      createDirectoryIfMissing True (name </> "test")
      writeFile (name </> "Smythfile.lq")        (smythfileContent name)
      writeFile (name </> "lib" </> "hello.lq")  helloLibContent
      writeFile (name </> "test" </> "hello.lq") helloTestContent
      writeFile (name </> "test" </> "main.lq")  testMainContent
      putStrLn ("Created project '" ++ name ++ "'/")
      putStrLn ("  " ++ name ++ "/Smythfile.lq")
      putStrLn ("  " ++ name ++ "/lib/hello.lq")
      putStrLn ("  " ++ name ++ "/test/hello.lq")
      putStrLn ("  " ++ name ++ "/test/main.lq")
      putStrLn ""
      putStrLn ("Run: cd " ++ name ++ " && smyth test")

smythfileContent :: String -> String
smythfileContent name = unlines
  [ "module SmythConfig contains"
  , "  define transparent project-name as \"" ++ name ++ "\""
  , "end"
  ]

helloLibContent :: String
helloLibContent = unlines
  [ "module hello contains"
  , "  define transparent greeting as \"Hello, World!\""
  , "end"
  ]

helloTestContent :: String
helloTestContent = unlines
  [ "import prelude as P"
  , "import assert as A"
  , "import hello as H"
  , "import string as S"
  , "import typeclass::equality as Eq"
  , ""
  , "open A exposing assert-eq end"
  , "open Eq exposing Equality end"
  , ""
  , "module test::hello contains"
  , "  define transparent main as compute"
  , "    perform (assert-eq String H::greeting \"Hello, World!\")"
  , "  end"
  , "end"
  ]

testMainContent :: String
testMainContent = unlines
  [ "import prelude as P"
  , "import test::hello as test_hello"
  , ""
  , "module test::main contains"
  , "  define transparent main as compute"
  , "    perform test_hello::main"
  , "  end"
  , "end"
  ]
