-- SavedIO unit test spec
{-# LANGUAGE OverloadedStrings #-}

module SavedIOSpec (
  spec
) where

import           SavedIO
import           SavedIO.Query
import           SavedIO.Types
import           SavedIO.Util

import           Data.Optional             (Optional (..))
import           Data.Text
import           Data.Time                 (Day, fromGregorian)
import qualified System.Console.ANSI       as CS
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances

instance Arbitrary Token where
  arbitrary = Token <$> arbitrary <*> arbitrary

spec :: Spec
spec = do

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

  let token = Token "babecafe" "cafebabe"
  describe "retrieveBookmarksQ" $ do
    it "produces a correctly formatted query" $
      retrieveBookmarksQ token
                         (Specific "aardvark")
                         (Specific 10)
        `shouldBe` "?&&devkey=babecafe&key=cafebabe&limit=10&list=aardvark"
    it "handles optional arguments" $
      retrieveBookmarksQ token Default Default
        `shouldBe` "?&&devkey=babecafe&key=cafebabe"
    it "handles a mix of optional arguments" $
      retrieveBookmarksQ token (Specific "foo") (Specific 1984)
        `shouldBe` "?&&devkey=babecafe&key=cafebabe&limit=1984&list=foo"

  let token = Token "babebabe" "cafecafe"
  describe "getBookmarkQ" $
    it "produces a correctly formatted query" $
      getBookmarkQ token"XyyZ"
        `shouldBe` "/XyyZ?devkey=babebabe&key=cafecafe"

  let token = Token "babe" "cafe"
  describe "createBookmarkQ" $ do
    it "produces a correctly formatted query" $
      createBookmarkQ token "Everything" "http://haskell.org" Default
        `shouldBe` "devkey=babe&key=cafe&title=Everything&url=http://haskell.org"
    it "produces a correctly formatted query with group" $
      createBookmarkQ token "Everything" "http://haskell.org" "hask"
        `shouldBe` "devkey=babe&key=cafe&title=Everything&url=http://haskell.org&list=hask"

  let token = Token "cafe" "babe"
  describe "deleteBookmarkQ" $
    it "produces a correctly formatted query" $
      deleteBookmarkQ token "123456789" `shouldBe` "devkey=cafe&key=babe&id=123456789"

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
    prop "s +?+ (Specific t) == s `append` t" $
      \s t -> s +?+ Specific t == s `append` t

  describe "epochTime" $ do
    it "knows how long since 2016-04-08" $
      epochTime (fromGregorian 2016 04 09)  == "1460160000"
    it "knows how long since 1970-01-01" $
      epochTime (fromGregorian 1970 01 01)  == "0"

  describe "dateFromString" $ do
    it "converts 2016-04-08 12:00:00" $
      dateFromString "2016-04-08 12:00:00" == (read "2016-04-08" :: Day)
    it "converts 2016-4-8 12:00:00" $
      dateFromString "2016-4-8 12:00:00" == (read "2016-04-08" :: Day)

  describe "formatParam" $ do
    prop "s `formatParam` Default == empty string" $
      \s -> formatParam s Default == ""
    prop "s `formatParamt` (Specific t) == s `append` t" $
      \s t -> formatParam s (Specific t) == s `append` t

  describe "tokenStr" $
    prop "tokenStr s == \"devkey=<x>&key=s\"" $
      \s -> tokenStr s == "devkey=" `append` _devKey s `append` "&key=" `append` _userKey s

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

  describe "mkToken" $
    prop "mkToken a b == Token a b" $
      \a b -> mkToken a b == Token a b

  describe "Token" $
    prop "(Token a b == Token c d) == (a == c && b == d)" $
      \a b c d -> let ab = mkToken a b
                      cd = mkToken c d
                  in (ab == cd) == (a == c && b == d)

  describe "Bookmark" $ do
    prop "_id Bookmark { id, url, title, note, creation } ==  id" $
      \id url title note creation -> let mark = Bookmark id url title note creation
                                     in _id mark == id
    prop "_url Bookmark { id, url, title, note, creation } == url" $
      \id url title note creation -> let mark = Bookmark id url title note creation
                                     in _url mark == url
    prop "_title Bookmark { id, url, title, note, creation } == title" $
      \id url title note creation -> let mark = Bookmark id url title note creation
                                     in _title mark == title
    prop "_note Bookmark { id, url, title, note, creation } == note " $
      \id url title note creation -> let mark = Bookmark id url title note creation
                                     in _note mark == note
    prop "_creation Bookmark { id, url, title, note, creation } == creation" $
      \id url title note creation -> let mark = Bookmark id url title note creation
                                     in _creation mark == creation

  describe "perhaps" $ do
    prop "perhaps x id Default == x :: Int" $
      \x -> perhaps x id Default == (x :: Int)
    prop "perhaps x id (Specific y) == y :: Int" $
      \x y -> perhaps x id (Specific y) == (y :: Int)
    prop "perhaps x id Default == x :: Float" $
      \x -> perhaps x id Default == (x :: Float)
    prop "perhaps x id (Specific y) == y :: Float" $
      \x y -> perhaps x id (Specific y) == (y :: Float)
    prop "perhaps x id Default == x :: String" $
      \x -> perhaps x id Default == (x :: String)
    prop "perhaps x id (Specific y) == y :: String" $
      \x y -> perhaps x id (Specific y) == (y :: String)
