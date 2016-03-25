-- | Command line client for saved-io
{-# LANGUAGE OverloadedStrings #-}

module Main where

import            CLOpts
import            SavedIO

import            Data.Text                       (Text)
import qualified  Data.Text               as      T
import qualified  Data.Text.IO            as      T
import            System.IO                       (hSetEncoding, stdout, utf8)

main :: IO ()
main = hSetEncoding stdout utf8 -- Hack for Windows to avoid "commitBuffer: invalid argument"
    >> execParser (parseOptions `withInfo` "Command Line Interface to saved.io")
   >>= run

printTextList :: [Text] -> IO ()
printTextList = T.putStrLn . T.concat

run :: CLOpts.Options -> IO ()
run (CLOpts.Options token (Common format color) cmd) =
  case cmd of
    Listing group             ->
      retrieveBookmarks token group Nothing Nothing Nothing
      >>= executeIf (\x -> printTextList $ ppMarkDef <$> x)

    Search query searchFormat ->
      retrieveBookmarks token Nothing Nothing Nothing Nothing
      >>= executeIf (\x -> printTextList $ ppMarkDef <$>
                           searchBookmarks (extractSearchKey searchFormat query) x)

    ShowLists                 ->
      retrieveLists token >>= executeIf (\x -> printTextList $ ppBMList <$> x)

    AddMark title url group   ->
      createBookmark token title url group >>= executeIf (\_ -> putStrLn "Success!")

    where
      executeIf _ (Left err) = putStrLn $ "Error: " ++ err
      executeIf f (Right x)  = f x
      maybeColor Nothing  = False
      maybeColor (Just x) = x
      ppMarkDef = ppBookmark (extractShowy format) (maybeColor color)
