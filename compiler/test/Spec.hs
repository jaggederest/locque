{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Locque.Compiler.Core
import Locque.Compiler.CoreParse
import Locque.Compiler.CorePretty

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
