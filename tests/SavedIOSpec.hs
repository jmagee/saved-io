-- SavedIO unit test spec
{-# LANGUAGE OverloadedStrings #-}

module SavedIOSpec (
  spec
) where

import            SavedIO
import            SavedIO.Internal
import            SavedIO.Types
import            SavedIO.Util

import            Data.Optional                   (Optional(..))
import            Data.Text
import            Data.Time                       (fromGregorian)
import qualified  System.Console.ANSI     as      CS
import            Test.Hspec
import            Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "ppSavedIOError" $ do
    it "prints message with prefix" $
      ppSavedIOError (SavedIOResponse True "abc 123") `shouldBe` pack "Saved.io error: abc 123"
    it "prints empty with prefix" $
      ppSavedIOError (SavedIOResponse False "") `shouldBe` pack "Saved.io error: "

  describe "defBookColors" $
    it "is full of pretty colors" $
      defBookColors `shouldBe` [ ("id", CS.Cyan)
                               , ("title", CS.Green)
                               , ("url", CS.Blue)
                               , ("note", CS.Yellow)
                               , ("creation", CS.Red)
                               ]

  describe "defBookKeys" $
    it "has the expected keys" $
      defBookKeys `shouldBe` "title,url"

  describe "defBookmarkConfig" $
    it "is expected" $
      defBookmarkConfig `shouldBe` BookmarkConfig defBookKeys Nothing

  describe "retrieveBookmarksQ" $ do
    it "produces a correctly formatted query" $
      retrieveBookmarksQ "cafebabe"
                         (Specific "aardvark")
                         (Specific 10)
        `shouldBe` "?&&devkey=9n7OFeRlp0OfsXycY0IMgX8k79D60vnu&key=cafebabe&limit=10&list=aardvark"
    it "handles optional arguments" $
      retrieveBookmarksQ "deadcafe" Default Default
        `shouldBe` "?&&devkey=9n7OFeRlp0OfsXycY0IMgX8k79D60vnu&key=deadcafe"
    it "handles a mix of optional arguments" $
      retrieveBookmarksQ "deadcafe" (Specific "foo") (Specific 1984)
        `shouldBe` "?&&devkey=9n7OFeRlp0OfsXycY0IMgX8k79D60vnu&key=deadcafe&limit=1984&list=foo"

  describe "createBookmarkQ" $ do
    it "produces a correctly formatted query" $
      createBookmarkQ "toktok" "Everything" "http://haskell.org" Default
        `shouldBe` "devkey=9n7OFeRlp0OfsXycY0IMgX8k79D60vnu&key=toktok&title=Everything&url=http://haskell.org"
    it "produces a correctly formatted query with group" $
      createBookmarkQ "toktok" "Everything" "http://haskell.org" "hask"
        `shouldBe` "devkey=9n7OFeRlp0OfsXycY0IMgX8k79D60vnu&key=toktok&title=Everything&url=http://haskell.org&list=hask"

  describe "deleteBookmarkQ" $
    it "produces a correctly formatted query" $
      deleteBookmarkQ "took" "123456789" `shouldBe` "devkey=9n7OFeRlp0OfsXycY0IMgX8k79D60vnu&key=took&id=123456789"

  describe ">&&<" $ do
    it "the cat smiles"  $ "foo" >&&< "bar" `shouldBe` "foo&bar"
    it "the cat frowns " $ "foo" >&&< ""    `shouldBe` "foo"
    it "the cat meows"   $ ""    >&&< "bar" `shouldBe` "bar"
    it "the cat vomits"  $ ""    >&&< ""    `shouldBe` ""
    prop "is associative" $
      \x y z -> x >&&< (y >&&< z) == (x >&&< y) >&&< z

  describe "+?+" $ do
    prop "s +?+ Default == s" $
      \s -> s +?+ Default == s
    prop "s +?+ (Specific t) == s ++ t" $
      \s t -> s +?+ Specific t == s ++ t

  describe "epochTime" $ do
    it "knows how long since 2016-04-08" $
      epochTime (fromGregorian 2016 04 09)  == "1460160000"
    it "knows how long since 1970-01-01" $
      epochTime (fromGregorian 1970 01 01)  == "0"

  describe "formatParam" $ do
    prop "s `formatParam` Default == empty string" $
      \s -> formatParam s Default == ""
    prop "s `formatParamt` (Specific t) == s ++ t" $
      \s t -> formatParam s (Specific t) == s ++ t

  describe "tokenStr" $
    prop "tokenStr s == \"devkey=<x>&key=s\"" $
      \s -> tokenStr s == "devkey=9n7OFeRlp0OfsXycY0IMgX8k79D60vnu&key=" ++ s

  describe "if'" $ do
    prop "if' == if-then-else :: Int" $
      \x y z -> if' x y z == if x then y else z :: Int
    prop "if' == if-then-else :: Char" $
      \x y z -> if' x y z == if x then y else z :: Char
    prop "if' == if-then-else :: [Float]) " $
      \x y z -> if' x y z == if x then y else z :: [Float]

  describe "?" $ do
    prop "? == if-then-else :: Int" $
      \x y z -> (x ? y $ z) == if x then y else z :: Int
    prop "? == if-then-else :: Char" $
      \x y z -> (x ? y $ z) == if x then y else z :: Char
    prop "? == if-then-else :: [Float]) " $
      \x y z -> (x ? y $ z) == if x then y else z :: [Float]
