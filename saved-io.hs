-- | Command line client for saved-io
{-# LANGUAGE OverloadedStrings #-}

module Main where

import            CLOpts
import            SavedIO

import            Data.Function                   (on)
import            Data.List                       (sortBy)
import            Data.Optional                   (Optional(..))
import            Data.Text                       (Text)
import qualified  Data.Text               as      T
import qualified  Data.Text.IO            as      T
import            System.IO                       (hSetEncoding, stdout, utf8)

main :: IO ()
main = hSetEncoding stdout utf8 -- Hack for Windows to avoid "commitBuffer: invalid argument"
     >> execParser (parseOptions `withInfo` "Command Line Interface to saved.io")
     >>= run

-- | Print a list of Text.
printTextList :: [Text] -> IO ()
printTextList = T.putStrLn . T.concat

-- | Sort bookmarks.
-- If no SortMethod is provided then default to an ascending sort by
-- title.
sortMarks :: Optional SortMethod -> [Bookmark] -> [Bookmark]
sortMarks Default           = sortBy (compare `on` _title)
sortMarks (Specific method) = case method of
  SortByTitle d -> case d of
    Ascending   -> sortMarks Default
    Descending  -> reverse . sortMarks Default

run :: CLOpts.Options -> IO ()
run (CLOpts.Options token (Common format color start end limit sort sortMethod) cmd) =
  case cmd of
    Listing group             ->
      retrieveBookmarks token group start end limit
      >>= executeIf (\x -> printTextList $ ppMarkDef <$> sortIf sort sortMethod x)

    Search query searchFormat ->
      retrieveBookmarks token Default start end limit
      >>= executeIf (\x -> printTextList $ ppMarkDef <$>
                           sortIf sort
                                  sortMethod
                                  (searchBookmarks (extractSearchKey searchFormat
                                                                     query)
                                                    x))

    ShowLists                 ->
      retrieveGroups token >>= executeIf (\x -> printTextList $ ppBMList <$> x)

    AddMark title url group   ->
      createBookmark token title url group >>= executeIf (\_ -> putStrLn "Success!")

    DelMark bkid              ->
      deleteBookmark token bkid >>= executeIf (\_ -> pure ())

    where
      executeIf _ (Left err) = putStrLn $ "Error: " ++ err
      executeIf f (Right x)  = f x
      maybeColor Default      = False
      maybeColor (Specific x) = x
      ppMarkDef = ppBookmark (extractShowy format) (maybeColor color)
      sortIf (Specific True) m x = sortMarks m x
      sortIf _ _ x               = x
