-- | Command line client for saved-io
{-# LANGUAGE OverloadedStrings #-}

module Main where

import            CLOpts
import            SavedIO

import            Data.Text               hiding  (group)

run :: CLOpts.Options -> IO ()
run (CLOpts.Options token cmd) =
  case cmd of
    Listing group -> do
      bm <- retrieveBookmarks token group Nothing Nothing Nothing
      case bm of
        Left err -> putStrLn err
        Right marks -> putStrLn . unpack . Data.Text.concat $ (`ppBookmark` False) <$> marks
    Search query  -> undefined

main :: IO ()
main = run =<< execParser (parseOptions `withInfo` "Command Line Interface to saved.io")
