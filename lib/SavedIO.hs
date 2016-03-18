-- | Bindings for saved.io, a cloud-based bookmark service.
{-# LANGUAGE OverloadedStrings #-}

module SavedIO
( Token
, BMGroup
, BMFormat
, Query
, Bookmark(..)
, SavedIOError(..)
, ppSavedIOError
, ppBookmark
, ppBMList
, retrieveBookmarks
, retrieveLists
, extractShowy
) where

import            Control.Monad                   (mzero)
import            Data.Aeson
import qualified  Data.ByteString.Lazy    as      B
import qualified  Data.List               as      L
import            Data.Text               hiding  (foldr, foldl, group)
import            Data.Time                       (Day, defaultTimeLocale,
                                                   formatTime,
                                                   parseTimeOrError)
import            Network.HTTP.Conduit            (simpleHttp)

type Token          = String
type BMGroup        = Maybe String
type BMFormat       = Maybe String
type Query          = String

-- | Base URL for saved io API.
savedIOURL :: String
savedIOURL = "http://devapi.saved.io/v1/"

data ShowyField =
  ShowyField { showId       :: Bool
             , showUrl      :: Bool
             , showTitle    :: Bool
             , showList     :: Bool
             , showListName :: Bool
             , showCreation :: Bool
             } deriving (Show)

extractShowy :: BMFormat -> ShowyField
extractShowy Nothing = ShowyField True True True True True True
extractShowy (Just format) =
  ShowyField ("id" `L.isInfixOf` format)
             ("url" `L.isInfixOf` format)
             ("title" `L.isInfixOf` format)
             ("list" `L.isInfixOf` format)
             ("listname" `L.isInfixOf` format)
             ("creation" `L.isInfixOf` format)

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

data BMList = BMList Int Text
              deriving (Show)

instance FromJSON BMList where
  parseJSON (Object v)  =
    BMList <$> (convert <$> v .: "id")
           <*> v .: "name"
  parseJSON _ = mzero

ppBMList :: BMList -> Text
ppBMList (BMList _ n) = n `append` "\n"

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
                     IO (Either String [Bookmark])
retrieveBookmarks token group from to limit = do
  let stream = savedIO query
  d <- (eitherDecode <$> stream) :: IO (Either String [Bookmark])
  case d of
    Left err    -> fmap Left (handleDecodeError stream err)
    Right marks -> return $ Right marks
  where query = foldl (>&&<)
                      ("bookmarks/" +?+ group)
                      [ tokenStr token
                      , fromStr from
                      , toStr to
                      , limitStr limit
                      ]

retrieveLists :: Token -> IO (Either String [BMList])
retrieveLists token = do
  let stream = savedIO query
  d <- (eitherDecode <$> stream) :: IO (Either String [BMList])
  case d of
    Left err    -> fmap Left (handleDecodeError stream err)
    Right l     -> return $ Right l
  where query = "lists" >&&< tokenStr token

-- | Redecode the stream as an SavedIOError to see if there was an API error.
-- This will either return the API error, if it can be obtained, or
-- the generic aeson parse error.
handleDecodeError :: IO B.ByteString -> String -> IO String
handleDecodeError stream errors = do
  d <- (eitherDecode <$> stream) :: IO (Either String SavedIOError)
  case d of
    Left  unknown   -> return $ "Unknown errors occured: " ++ errors ++ " " ++ unknown
    Right helpful   -> return $ unpack $ ppSavedIOError helpful
