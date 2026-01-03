{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Locque.Compiler.Core
import Locque.Compiler.CoreErased as Erased
import Locque.Compiler.Erase
import Locque.Compiler.CoreParse
import Locque.Compiler.CorePretty
import qualified LocqueRuntime as RT

main :: IO ()
main = hspec $ do
  describe "Core pretty/parse round-trips" $ do
    it "round-trips a lambda value" $ do
      let value =
            VLam
              (Name "x")
              TyNatural
              (CReturn (VVar (Name "x")))
      parseCoreValue (renderCoreValue value) `shouldBe` Right value

    it "round-trips a constructor value" $ do
      let value =
            VConstructor
              (Name "Option::some")
              [VLit (LitNatural 1)]
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
              (CReturn (VVar (Name "x")))
      eraseValue input
        `shouldBe` ELam (Name "x") (EReturn (EVar (Name "x")))

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

-- Generators

genCoreValue :: Int -> Gen CoreValue
genCoreValue size
  | size <= 0 =
      oneof
        [ VVar <$> genName
        , VLit <$> genLiteral
        ]
  | otherwise =
      frequency
        [ (3, VVar <$> genName)
        , (3, VLit <$> genLiteral)
        , (2, VConstructor <$> genName <*> genValueList)
        , (1, VLam <$> genName <*> genType (size - 1) <*> genCoreComp (size - 1))
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
