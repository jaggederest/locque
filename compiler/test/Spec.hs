{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (evaluate)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Locque.Compiler.Core
import Locque.Compiler.CoreErased as Erased
import Locque.Compiler.Erase
import Locque.Compiler.CoreParse
import Locque.Compiler.CorePretty
import Locque.Compiler.Codegen
import Locque.Compiler.Emit
import qualified LocqueRuntime as RT

main :: IO ()
main = hspec $ do
  describe "Core pretty/parse round-trips" $ do
    it "round-trips a lambda value" $ do
      let value =
            VLam
              (Name "x")
              TyNatural
              (VVar (Name "x"))
      parseCoreValue (renderCoreValue value) `shouldBe` Right value

    it "round-trips an application value" $ do
      let value =
            VApp
              (VVar (Name "f"))
              (VLit (LitNatural 1))
      parseCoreValue (renderCoreValue value) `shouldBe` Right value

    it "round-trips a constructor value" $ do
      let value =
            VConstructor
              (Name "Option::some")
              [VLit (LitNatural 1)]
      parseCoreValue (renderCoreValue value) `shouldBe` Right value

    it "round-trips a let value" $ do
      let value =
            VLet
              (Name "x")
              (VLit (LitNatural 1))
              (VVar (Name "x"))
      parseCoreValue (renderCoreValue value) `shouldBe` Right value

    it "round-trips a match value" $ do
      let value =
            VMatch
              (VVar (Name "flag"))
              [ CoreValueCase
                  (Name "Boolean::true")
                  []
                  (VLit (LitNatural 1))
              , CoreValueCase
                  (Name "Boolean::false")
                  []
                  (VLit (LitNatural 0))
              ]
      parseCoreValue (renderCoreValue value) `shouldBe` Right value

    it "round-trips a match computation" $ do
      let comp =
            CMatch
              (VVar (Name "xs"))
              [ CoreCase
                  (Name "List::empty")
                  []
                  (CReturn (VLit LitUnit))
              , CoreCase
                  (Name "List::cons")
                  [Name "head", Name "tail"]
                  (CReturn (VVar (Name "head")))
              ]
      parseCoreComp (renderCoreComp comp) `shouldBe` Right comp

    prop "round-trips small values" $
      forAll (genCoreValue 3) $ \value ->
        parseCoreValue (renderCoreValue value) === Right value

    prop "round-trips small computations" $
      forAll (genCoreComp 3) $ \comp ->
        parseCoreComp (renderCoreComp comp) === Right comp

  describe "Core erasure" $ do
    it "erases lambda types" $ do
      let input =
            VLam
              (Name "x")
              TyNatural
              (VVar (Name "x"))
      eraseValue input
        `shouldBe` ELam (Name "x") (EVar (Name "x"))

    it "erases explicit erased values" $ do
      eraseValue VErased `shouldBe` EErased

    it "erases data field types to ctor arity" $ do
      let dataDecl =
            CoreDataDecl
              (Name "Option")
              [Name "A"]
              [ CoreCtor (Name "Option::some") [TyVar (Name "A")]
              , CoreCtor (Name "Option::none") []
              ]
      eraseDataDecl dataDecl
        `shouldBe` ErasedDataDecl
          (Name "Option")
          [ ErasedCtor (Name "Option::some") 1
          , ErasedCtor (Name "Option::none") 0
          ]

    it "preserves match binder names" $ do
      let comp =
            CMatch
              (VVar (Name "xs"))
              [ CoreCase
                  (Name "List::cons")
                  [Name "head", Name "tail"]
                  (CReturn (VVar (Name "head")))
              ]
      eraseComp comp
        `shouldBe` EMatch
          (EVar (Name "xs"))
          [ ErasedCase
              (Name "List::cons")
              [Name "head", Name "tail"]
              (EReturn (EVar (Name "head")))
          ]

    it "preserves module name" $ do
      let input = CoreModule (Name "Example") []
      eraseModule input `shouldBe` ErasedModule (Name "Example") []

  describe "LocqueRuntime" $ do
    it "binds computations in order" $ do
      let comp =
            RT.compBind (RT.compReturn (1 :: RT.Natural)) $ \value ->
              RT.compReturn (RT.addNatPrim value 1)
      RT.runComp comp `shouldReturn` (2 :: RT.Natural)

    it "clamps natural subtraction to zero" $ do
      RT.subNatPrim 2 5 `shouldBe` (0 :: RT.Natural)

    it "compares and concatenates strings" $ do
      let hello = RT.concatStringPrim "he" "llo"
      RT.eqStringPrim hello "hello" `shouldBe` True
      RT.stringLengthPrim hello `shouldBe` (5 :: RT.Natural)

    it "looks up dictionary entries by name" $ do
      let entries = [("alpha" :: RT.String, (10 :: RT.Natural))]
      RT.dictAccessPrim entries "alpha" `shouldBe` (10 :: RT.Natural)

    it "fails dictionary lookup when missing" $ do
      let entries = [("alpha" :: RT.String, (10 :: RT.Natural))]
      evaluate (RT.dictAccessPrim entries "missing")
        `shouldThrow` anyErrorCall

  describe "Core → Haskell codegen" $ do
    it "emits a module header and runtime import" $ do
      let rendered = emitModule (CoreModule (Name "Test") [])
      rendered `shouldSatisfy` T.isInfixOf "module LocqueGen where"
      rendered `shouldSatisfy` T.isInfixOf "import LocqueRuntime"

    it "maps builtins for constructors and lists" $ do
      let decl =
            CoreDef
              (Name "value")
              TyNatural
              (VConstructor
                (Name "Option::some")
                [VLit (LitNatural 1)])
      let listDecl =
            CoreDef
              (Name "list")
              (TyCon (Name "List") [TyNatural])
              (VConstructor
                (Name "List::cons")
                [VLit (LitNatural 1), VConstructor (Name "List::empty") []])
      let rendered = emitModule (CoreModule (Name "Test") [decl, listDecl])
      rendered `shouldSatisfy` T.isInfixOf "Just 1"
      rendered `shouldSatisfy` T.isInfixOf "["
      rendered `shouldSatisfy` T.isInfixOf ":"

    it "annotates string literals with String" $ do
      let decl =
            CoreDef
              (Name "value")
              TyString
              (VLit (LitString "hello"))
      let rendered = emitModule (CoreModule (Name "Test") [decl])
      rendered `shouldSatisfy` T.isInfixOf "(\"hello\" :: String)"

    it "maps dict-access-prim in codegen" $ do
      let decl =
            CoreDef
              (Name "value")
              TyUnit
              (VApp
                (VApp
                  (VVar (Name "dict-access-prim"))
                  (VVar (Name "dict")))
                (VLit (LitString "method")))
      let rendered = emitModule (CoreModule (Name "Test") [decl])
      rendered `shouldSatisfy` T.isInfixOf "dictAccessPrim"

    it "renders ignored binders as underscores" $ do
      let comp =
            CoreDefComp
              (Name "value")
              TyUnit
              (CMatch
                (VVar (Name "pair"))
                [ CoreCase
                    (Name "Pair::pair")
                    [Name "ignored", Name "ignored"]
                    (CReturn (VLit LitUnit))
                ])
      let rendered = emitModule (CoreModule (Name "Test") [comp])
      rendered `shouldSatisfy` T.isInfixOf "(_, _)"

    it "renders duplicate binders with underscores" $ do
      let comp =
            CoreDefComp
              (Name "value")
              TyUnit
              (CMatch
                (VVar (Name "pair"))
                [ CoreCase
                    (Name "Pair::pair")
                    [Name "x", Name "x"]
                    (CReturn (VLit LitUnit))
                ])
      let rendered = emitModule (CoreModule (Name "Test") [comp])
      rendered `shouldSatisfy` T.isInfixOf "(x, _)"

    it "renders bare list cons constructor as (:)" $ do
      let decl =
            CoreDef
              (Name "value")
              TyUnit
              (VConstructor (Name "List::cons") [])
      let rendered = emitModule (CoreModule (Name "Test") [decl])
      rendered `shouldSatisfy` T.isInfixOf "(:)"

    it "escapes string literals in codegen" $ do
      let decl =
            CoreDef
              (Name "value")
              TyString
              (VLit (LitString "line\n\"quote\""))
      let rendered = emitModule (CoreModule (Name "Test") [decl])
      rendered `shouldSatisfy` T.isInfixOf "\\n"
      rendered `shouldSatisfy` T.isInfixOf "\\\""

    it "emits data declarations with constructor fields" $ do
      let dataDecl =
            CoreDataDecl
              (Name "Box")
              [Name "A"]
              [CoreCtor (Name "Box::box") [TyVar (Name "A")]]
      let rendered = emitModule (CoreModule (Name "Test") [CoreData dataDecl])
      rendered `shouldSatisfy` T.isInfixOf "data Box a"
      rendered `shouldSatisfy` T.isInfixOf "Box_box a"

  describe "Emit path helpers" $ do
    it "writes next to input by default" $ do
      emitHsPath Nothing "src/example.lq" `shouldBe` "src/example.hs"

    it "writes to output directory when provided" $ do
      emitHsPath (Just "tmp/locque/gen") "src/example.lq"
        `shouldBe` "tmp/locque/gen/example.hs"

-- Generators

genCoreValue :: Int -> Gen CoreValue
genCoreValue size
  | size <= 0 =
      oneof
        [ VVar <$> genName
        , VLit <$> genLiteral
        , pure VErased
        ]
  | otherwise =
      frequency
        [ (3, VVar <$> genName)
        , (3, VLit <$> genLiteral)
        , (1, pure VErased)
        , (2, VConstructor <$> genName <*> genValueList)
        , (1, VLam <$> genName <*> genType (size - 1) <*> genCoreValue (size - 1))
        , (1, VApp <$> genCoreValue (size - 1) <*> genCoreValue (size - 1))
        , (1, VCompute <$> genCoreComp (size - 1))
        ]
  where
    genValueList = resize 3 (listOf (genCoreValue (size - 1)))


genCoreComp :: Int -> Gen CoreComp
genCoreComp size
  | size <= 0 = CReturn <$> genCoreValue 0
  | otherwise =
      frequency
        [ (3, CReturn <$> genCoreValue (size - 1))
        , (2, CPerform <$> genCoreValue (size - 1))
        , (2, CApp <$> genCoreValue (size - 1) <*> genCoreValue (size - 1))
        , (2, CBind <$> genName <*> genCoreComp (size - 1) <*> genCoreComp (size - 1))
        , (2, CLet <$> genName <*> genCoreValue (size - 1) <*> genCoreComp (size - 1))
        ]


genLiteral :: Gen CoreLiteral
genLiteral =
  oneof
    [ LitNatural <$> chooseInteger (0, 9)
    , LitBoolean <$> elements [True, False]
    , LitString <$> elements ["", "hi", "locque"]
    , pure LitUnit
    ]


genType :: Int -> Gen CoreType
genType size
  | size <= 0 = elements [TyUnit, TyBoolean, TyNatural, TyString, TyCharacter]
  | otherwise =
      frequency
        [ (3, elements [TyUnit, TyBoolean, TyNatural, TyString, TyCharacter])
        , (2, TyVar <$> genName)
        , (2, TyCon <$> genName <*> resize 2 (listOf (genType (size - 1))))
        , (2, TyFun <$> genType (size - 1) <*> genType (size - 1))
        , (2, TyComp <$> genType (size - 1))
        ]


genName :: Gen Name
genName = Name <$> elements
  [ "x"
  , "y"
  , "f"
  , "g"
  , "List::empty"
  , "List::cons"
  , "Option::some"
  ]
