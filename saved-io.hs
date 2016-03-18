-- | Command line client for saved-io
{-# LANGUAGE OverloadedStrings #-}

module Main where

import            CLOpts
import            SavedIO

import qualified  Data.ByteString.Lazy    as      B
import            Control.Monad                   (mzero)
import            Data.Aeson
import            Data.Aeson.Types        as      AT
import            Data.Time                       (Day, defaultTimeLocale,
                                                   parseTimeOrError)
import            Data.Text               hiding  (foldr)
import            Network.HTTP.Conduit            (simpleHttp)

{--- | Base URL for saved io API.-}
savedIOURL :: String
savedIOURL = "http://devapi.saved.io/v1/"

{--- | Fetch a URL from saved.io.-}
savedIO :: String -> IO B.ByteString
savedIO = simpleHttp . (++) savedIOURL

appendMaybe :: String -> Maybe String -> String
appendMaybe s (Just s2) = s ++ s2
appendMaybe s Nothing = s

-- | Redecode the stream as an SavedIOError to see if there was an API error.
-- This will either print the API error, if it can be obtained, or
-- the generic aeson parse error.
{-handleDecodeError :: IO B.ByteString -> String -> IO ()-}
{-handleDecodeError stream errors = do-}
  {-d <- (eitherDecode <$> stream) :: IO (Either String SavedIOError)-}
  {-case d of-}
    {-Left  unknown   -> putStrLn $ "Unknown errors occured: " ++ errors ++ " " ++ unknown-}
    {-Right helpful   -> putStrLn . unpack $ ppSavedIOError helpful-}

run :: CLOpts.Options -> IO ()
run (CLOpts.Options token cmd) =
  case cmd of
    Listing group -> do
      bm <- retrieveBookmarks token group Nothing Nothing Nothing
      case bm of
        Left err -> putStrLn err
        Right marks -> putStrLn . unpack . Data.Text.concat $ (`ppBookmark` False) <$> marks
    Search query  -> undefined
  {-let stream = savedIO query-}
  {-d <- (eitherDecode <$> stream) :: IO (Either String [Bookmark])-}
  {-case d of-}
    {-Left err    -> handleDecodeError stream err-}
    {-Right marks -> putStrLn . unpack . Data.Text.concat $ (`ppBookmark` False) <$> marks-}
    {-where-}
      {-query    = queryStr ++ tokenStr-}
      {-tokenStr = "&token=" ++ token-}
      {-queryStr = case cmd of-}
        {-Listing group    -> retrieveBookmarks token group Nothing Nothing Nothing-}
          {---"bookmarks/" `appendMaybe` group-}
        {-Search query  -> undefined-}

main :: IO ()
main = run =<< execParser (parseOptions `withInfo` "Command Line Interface to saved.io")
