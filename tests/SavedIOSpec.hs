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
import            Test.Hspec
import            Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "ppSavedIOError" $ do
    it "prints message with prefix" $
      ppSavedIOError (SavedIOResponse True "abc 123") `shouldBe` pack "Saved.io error: abc 123"
    it "prints empty with prefix" $
      ppSavedIOError (SavedIOResponse False "") `shouldBe` pack "Saved.io error: "

  {-let defaultShowy = ShowyField False True True False True False-}
  {-describe "extractShowy" $ do-}
    {-it "has the expected default" $-}
      {-extractShowy Default `shouldBe` defaultShowy-}
    {-it "handles garbage" $-}
      {-extractShowy (Specific "sdjfklsdfhhwherklasdf jkladsf jk $%$&%*") `shouldBe` defaultShowy-}
    {-it "understands bid" $-}
      {-extractShowy (Specific "bid") `shouldBe` ShowyField True False False False False False-}
    {-it "doesn't overmatch on id" $-}
      {-extractShowy (Specific "id") `shouldBe` defaultShowy-}
    {-it "understands url" $-}
      {-extractShowy (Specific "url") `shouldBe` ShowyField False True False False False False-}
    {-it "understands title" $-}
      {-extractShowy (Specific "title") `shouldBe` ShowyField False False True False False False-}
    {-it "understands listid" $-}
      {-extractShowy (Specific "groupid") `shouldBe` ShowyField False False False True False False-}
    {-it "understands listname" $-}
      {-extractShowy (Specific "groupname") `shouldBe` ShowyField False False False False True False-}
    {-it "understands creation" $-}
      {-extractShowy (Specific "creation") `shouldBe` ShowyField False False False False False True-}
    {-it "understands all" $-}
      {-extractShowy (Specific "bid,url,title,groupid,groupname,creation")-}
      {-`shouldBe` ShowyField True True True True True True-}

  describe "retrieveBookmarksQ" $ do
    it "produces a correctly formatted query" $
      retrieveBookmarksQ "cafebabe"
                         (Specific "aardvark")
                         (Specific (fromGregorian 2015 02 19))
                         (Specific (fromGregorian 2016 02 19))
                         (Specific 10)
        `shouldBe` "bookmarks/aardvark&&token=cafebabe&from=1424304000&to=1455840000&limit=10"
    it "handles optional arguments" $
      retrieveBookmarksQ "deadcafe" Default Default Default Default
        `shouldBe` "bookmarks/&&token=deadcafe"
    it "handles a mix of optional arguments" $
      retrieveBookmarksQ "deadcafe" (Specific "foo") Default Default (Specific 1984)
        `shouldBe` "bookmarks/foo&&token=deadcafe&limit=1984"

  describe "retrieveGroupsQ" $
    it "puts the lotion on its skin or else it gets the hose again" $
      retrieveGroupsQ "cafecafe" `shouldBe` "lists&token=cafecafe"

  describe "createBookmarkQ" $ do
    it "produces a correctly formatted query" $
      createBookmarkQ "toktok" "Everything" "http://haskell.org" Default
        `shouldBe` "token=toktok&title=Everything&url=http://haskell.org"
    it "produces a correctly formatted query with group" $
      createBookmarkQ "toktok" "Everything" "http://haskell.org" "hask"
        `shouldBe` "token=toktok&title=Everything&url=http://haskell.org&list=hask"

  describe "deleteBookmarkQ" $
    it "produces a correctly formatted query" $
      deleteBookmarkQ "took" 123456789 `shouldBe` "token=took&bk_id=123456789"

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
    prop "tokenStr s == \"token=s\"" $
      \s -> tokenStr s == "token=" ++ s

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
