-- | Command line client for saved-io
{-# LANGUAGE OverloadedStrings #-}

module Main where

import            CLOpts
import            SavedIO

import            Data.Text               hiding  (group)

printTextList :: [Text] -> IO ()
printTextList = putStrLn . unpack . Data.Text.concat

run :: CLOpts.Options -> IO ()
run (CLOpts.Options token cmd) =
  case cmd of
    Listing group format -> do
      print format
      print $ extractShowy format
      bm <- retrieveBookmarks token group Nothing Nothing Nothing
      case bm of
        Left err -> putStrLn err
        Right marks -> printTextList $ (`ppBookmark` False) <$> marks
    Search query  -> undefined
    ShowLists     -> do
      bmlist <- retrieveLists token
      case bmlist of
        Left err -> putStrLn err
        Right l  -> printTextList $ ppBMList <$> l

main :: IO ()
main = run =<< execParser (parseOptions `withInfo` "Command Line Interface to saved.io")
