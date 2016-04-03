-- SavedIO unit test spec
{-# LANGUAGE OverloadedStrings #-}

module SavedIOSpec (
  spec
) where

import            SavedIO
import            SavedIO.Types

import            Data.Optional                   (Optional(..))
import            Data.Text
import            Test.Hspec
import            Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "ppSavedIOError" $ do
    it "prints message with prefix" $
      ppSavedIOError (SavedIOResponse True "abc 123") `shouldBe` pack "Saved.io error: abc 123"
    it "prints empty with prefix" $
      ppSavedIOError (SavedIOResponse False "") `shouldBe` pack "Saved.io error: "

  let defaultShowy = ShowyField False True True False True False
  describe "extractShowy" $ do
    it "has the expected default" $
      extractShowy Default `shouldBe` defaultShowy
    it "handles garbage" $
      extractShowy (Specific "sdjfklsdfhhwherklasdf jkladsf jk $%$&%*") `shouldBe` defaultShowy
    it "understands bid" $
      extractShowy (Specific "bid") `shouldBe` ShowyField True False False False False False
    it "doesn't overmatch on id" $
      extractShowy (Specific "id") `shouldBe` defaultShowy
    it "understands url" $
      extractShowy (Specific "url") `shouldBe` ShowyField False True False False False False
    it "understands title" $
      extractShowy (Specific "title") `shouldBe` ShowyField False False True False False False
    it "understands listid" $
      extractShowy (Specific "groupid") `shouldBe` ShowyField False False False True False False
    it "understands listname" $
      extractShowy (Specific "groupname") `shouldBe` ShowyField False False False False True False
    it "understands creation" $
      extractShowy (Specific "creation") `shouldBe` ShowyField False False False False False True
    it "understands all" $
      extractShowy (Specific "bid,url,title,groupid,groupname,creation")
      `shouldBe` ShowyField True True True True True True


