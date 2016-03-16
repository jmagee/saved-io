-- | Command line client for saved-io
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified  Data.ByteString.Lazy    as  B
import            Control.Monad               (mzero)
import            Data.Aeson
import            Data.Time                   (Day, defaultTimeLocale,
                                               parseTimeOrError)
import            Data.Text
import            Network.HTTP.Conduit        (simpleHttp)

-- | Base URL for saved io API.
savedIOURL :: String 
savedIOURL = "http://devapi.saved.io/v1/"

data Bookmark =
  Bookmark { id       :: Int
           , url      :: Text
           , title    :: Text
           , list     :: Int
           , listName :: Text
           , creation :: Day
           } deriving (Show)

instance FromJSON Bookmark where
  parseJSON (Object v) =
    Bookmark <$> (convert <$> v .: "bk_id")
             <*> v .: "url"
             <*> v .: "title"
             <*> (convert <$> v .: "list")
             <*> v .:? "list_name" .!= "none"
             <*> (dateFromString <$> v .: "creation_date")
  parseJSON _ = mzero

-- | Convert a string to a type with a slightly more elegant error
-- than plain read.
convert :: Read a => String -> a
convert s =
  case reads s of
    [(x, "")]  -> x
    _          -> error $ "Could not convert " ++ s

-- | Convert a string into a Day.
-- We define this instead of using read, because has a friendlier parse.
-- For example, read :: Day, would choke on "2016-1-2", but the solution
-- below parses it as "2016-01-02".
dateFromString :: String -> Day
dateFromString = parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d %H:%M:%S"

-- | Fetch a URL from saved.io.
savedIO :: String -> IO B.ByteString
savedIO = simpleHttp . (++) savedIOURL

main :: IO ()
main = do
  d <- (eitherDecode <$> savedIO "bookmarks&token=4df74d63d3f873031d838b7383e60573") :: IO (Either String [Bookmark])
  case d of
    Left err    -> putStrLn $ "Error parsing " ++ err
    Right marks -> print marks
