-- | Command line client for saved-io
{-# LANGUAGE OverloadedStrings #-}

module Main where

import            CLOpts                  as      CL
import            SavedIO
import            SavedIO.Util

import            Data.Aeson                      (eitherDecode')
import qualified  Data.ByteString.Lazy    as      B
import            Data.Function                   (on)
import            Data.List                       (sortBy)
import            Data.Optional                   (Optional(..))
import            Data.Text                       (Text)
import qualified  Data.Text               as      T
import qualified  Data.Text.IO            as      T
import            System.Directory                (doesFileExist, getHomeDirectory)
import            System.IO                       (hSetEncoding, stdout, utf8)

main :: IO ()
main = hSetEncoding stdout utf8 -- Hack for Windows to avoid "commitBuffer: invalid argument"
     >> getRCDefaults
     >>= execParser . (`withInfo` "Command Line Interface to saved.io") . parseOptions
     >>= run

getRCDefaults :: IO Common
getRCDefaults = do
  rc <- fmap (++ "/.saved-io.rc") getHomeDirectory
  exists <- doesFileExist rc
  exists ? decode rc $ pure comDef
    where
      decode :: FilePath -> IO Common
      decode prefs = do
        d <- (eitherDecode' <$> B.readFile prefs) :: IO (Either String Common)
        warn d
        pure $ defaults d
      warn (Left e)  = putStrLn $ "Warning: " ++ e
      warn _         = pure ()
      defaults (Left _)  = comDef
      defaults (Right x) = x
      comDef = Common Default Default Default Default
                      Default Default Default Default

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

run :: CL.Options -> IO ()
run (CL.Options (Common t format color start end limit sort sortMethod) cmd) =
  case cmd of
    Listing group             ->
      retrieveBookmarks (token t) group start end limit
      >>= executeIf (\x -> printTextList $ ppMarkDef <$> sortIf sort sortMethod x)

    Search query searchFormat ->
      retrieveBookmarks (token t) Default start end limit
      >>= executeIf (\x -> printTextList $ ppMarkDef <$>
                           sortIf sort
                                  sortMethod
                                  (searchBookmarks (extractSearchKey searchFormat
                                                                     query)
                                                    x))

    ShowGroups                ->
      retrieveGroups (token t) >>= executeIf (\x -> printTextList $ ppBMGroup <$> x)

    AddMark title url group   ->
      createBookmark (token t) title url group >>= executeIf (\_ -> putStrLn "Success!")

    DelMark bkid              ->
      deleteBookmark (token t) bkid >>= executeIf (\_ -> pure ())

    where
      executeIf _ (Left err) = putStrLn $ "Error: " ++ err
      executeIf f (Right x)  = f x
      maybeColor Default      = False
      maybeColor (Specific x) = x
      ppMarkDef = ppBookmark (extractShowy format) (maybeColor color)
      sortIf (Specific True) m x = sortMarks m x
      sortIf _ _ x               = x
      token Default              = error "Missing token; -t|--token option required."
      token (Specific x)         = x
