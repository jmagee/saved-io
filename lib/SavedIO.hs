-- | Bindings for saved.io, a cloud-based bookmark service.
{-# LANGUAGE OverloadedStrings #-}

module SavedIO
( Token
, BMGroup
, Query
, Bookmark(..)
, SavedIOError(..)
, retrieveBookmarks
, ppSavedIOError
, ppBookmark
) where

import            Control.Monad                   (mzero)
import            Data.Aeson
import qualified  Data.ByteString.Lazy    as      B
import            Data.Text               hiding  (foldr, foldl)
import            Data.Time                       (Day, defaultTimeLocale,
                                                   formatTime,
                                                   parseTimeOrError)
import            Network.HTTP.Conduit            (simpleHttp)

type Token    = String
type BMGroup  = Maybe String
type Query    = String

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

ppBookmark :: Bookmark -> Bool -> Text
ppBookmark (Bookmark _ theUrl theTitle _ theList _) color =
  Prelude.foldr append "\n"
    [ "\nBookmark: ", theTitle
    , "\nURL: ", theUrl
    , "\nList: ", theList
    ]

data SavedIOError =
  SavedIOError { isError  :: Bool
               , message   :: Text
               } deriving (Show)

instance FromJSON SavedIOError where
  parseJSON (Object v) =
    SavedIOError <$> v .: "is_error"
                 <*> v .: "message"
  parseJSON _ = mzero

ppSavedIOError :: SavedIOError -> Text
ppSavedIOError (SavedIOError _ msg) = append "Saved.io error: " msg

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

tokenStr :: Token -> String
tokenStr = (++) "&token="

epochTime :: Day -> String
epochTime = formatTime defaultTimeLocale "%s"

toStr :: Maybe Day -> String
toStr Nothing    = ""
toStr (Just day) = "to:" ++ epochTime day

fromStr :: Maybe Day -> String
fromStr Nothing    = ""
fromStr (Just day) = "from:" ++ epochTime day

limitStr :: Maybe Int -> String
limitStr Nothing    = ""
limitStr (Just x)   = "limit: " ++ show x

(>&&<) :: String -> String -> String
left >&&< right = left ++ "&" ++ right

(+?+) :: String -> Maybe String -> String
s +?+ Nothing   = s
s +?+ (Just s2) = s ++ s2

retrieveBookmarks :: Token     -> -- ^ API Token
                     BMGroup   -> -- ^ Bookmark Group
                     Maybe Day -> -- ^ From timestamp
                     Maybe Day -> -- ^ To timestamp
                     Maybe Int -> -- ^ Limit
                     String -- temporary
retrieveBookmarks token group from to limit =
  foldl (>&&<)
        ("bookmarks/" +?+ group)
        [ tokenStr token
        , fromStr from
        , toStr to
        , limitStr limit
        ]

  {-"bookmarks/" `appendMaybe` group-}
  {->&&< tokenStr token-}
  {->&&< fromStr from-}
  {->&&< toStr to-}
  {->&&< limitStr limit-}

      {-query    = queryStr ++ tokenStr-}
      {-tokenStr = "&token=" ++ token-}
      {-queryStr = case cmd of-}
        {-Listing group    -> "bookmarks/" `appendMaybe` group-}
        {-Search query  -> undefined-}
