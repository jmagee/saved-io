-- | Command line client for saved-io
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           CLOpts                     as CL
import           SavedIO
import           SavedIO.Util
import           Version

import           Data.Aeson                 (eitherDecode', toJSON)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Function              (on)
import           Data.List                  (sortBy)
import           Data.Optional              (Optional (..))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           System.Directory           (doesFileExist, getHomeDirectory)
import           System.IO                  (hSetEncoding, stdout, utf8)

main :: IO ()
main = hSetEncoding stdout utf8 -- Hack for Windows to avoid "commitBuffer: invalid argument"
     >> getRCDefaults
     >>= execParser . (`withInfo` infoStr) . parseOptions
     >>= run
  where
    infoStr = "Command Line Interface to saved.io " ++ version

-- | Read default settings for Common options from RC file.
-- If there is no RC file or it cannot be decoded then this returns
-- Common of all Default.
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
      defaults (Left _)  = comDef
      defaults (Right x) = x
      comDef = Common Default Default Default
                      Default Default Default

run :: CL.Options -> IO ()
run (CL.Options c@(Common t format color limit sort sortMethod) cmd) =
  case cmd of
    Listing group             ->
      retrieveBookmarks (token t) group limit
      >>= executeIf (\x -> printTextList $ ppMarkDef <$> sortIf sort sortMethod x)

    Search query searchFormat ->
      retrieveBookmarks (token t) Default limit
      >>= executeIf (\x -> printTextList $ ppMarkDef <$>
                           sortIf sort
                                  sortMethod
                                  (searchBookmarks (extractSearchKey searchFormat
                                                                     query)
                                                    x))

    AddMark title url group   ->
      createBookmark (token t) title url group
      >>= executeIf (\x -> putStrLn $ "Success!  Created: " ++ x)

    DelMark bkid              ->
      deleteBookmark (token t) bkid >>= \_ -> pure ()

    GetMark bkid              ->
      getBookmark (token t) bkid >>= executeIf (T.putStrLn . ppMarkDef)

    MakeRC                    -> do
      home <- getHomeDirectory
      putStrLn $ "#Redirect the contents below to " ++ home ++ "/.saved-io.rc"
      B.putStrLn $ encodePretty $ toJSON c

    where
      formatText = perhaps defBookKeys T.pack
      useColor = perhaps Nothing (\x -> x ? Just defBookColors $ Nothing) color
      ppMarkDef = ppBookmark $ BookmarkConfig (formatText format) useColor
      sortIf (Specific True) m x = sortMarks m x
      sortIf _ _ x               = x
      token Default              = error "Missing token; -t|--token option required."
      token (Specific x)         = x

-- | Print a list of Text.
printTextList :: [Text] -> IO ()
printTextList = T.putStrLn . T.intercalate "\n"

-- | Print a warning on failure.
warn :: Either String a -> IO ()
warn (Left e)  = putStrLn $ "Warning: " ++ e
warn _         = pure ()

-- | Execute an IO function on Right, print the error on Left.
executeIf :: (a -> IO ()) -> Either String a -> IO ()
executeIf _ (Left err) = putStrLn $ "Error: " ++ err
executeIf f (Right x)  = f x

-- | Sort bookmarks.
-- If no SortMethod is provided then default to an ascending sort by
-- title.
sortMarks :: Optional SortMethod -> [Bookmark] -> [Bookmark]
sortMarks Default           = sortBy (compare `on` _title)
sortMarks (Specific method) = case method of
  SortByTitle d -> case d of
    Ascending   -> sortMarks Default
    Descending  -> reverse . sortMarks Default
