-- | Bindings for saved.io, a cloud-based bookmark service.

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
, searchBookmarks
, extractShowy
, extractSearchKey
) where

import            SavedIO.Types

import            Data.Aeson                      (eitherDecode)
import qualified  Data.ByteString.Lazy    as      B
import qualified  Data.List               as      L
import            Data.Text               hiding  (foldr, foldl, group)
import            Data.Time                       (Day, defaultTimeLocale,
                                                   formatTime)
import            Network.HTTP.Conduit            (simpleHttp)

-- | Base URL for saved io API.
savedIOURL :: String
savedIOURL = "http://devapi.saved.io/v1/"

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

searchBookmarks :: SearchKey -> [Bookmark] -> [Bookmark]
searchBookmarks (BID x) marks
  = L.filter (\(Bookmark y _ _ _ _ _) -> x == y) marks

searchBookmarks (Url x) marks
  = L.filter (\(Bookmark _ y _ _ _ _) -> pack x `isInfixOf` y) marks

searchBookmarks (Title x) marks
  = L.filter (\(Bookmark _ _ y _ _ _) -> pack x `isInfixOf` y) marks

searchBookmarks (ListID x) marks
  = L.filter (\(Bookmark _ _ _ y _ _) -> x == y) marks

searchBookmarks (ListName x) marks
  = L.filter (\(Bookmark _ _ _ _ y _) -> pack x `isInfixOf` y) marks

searchBookmarks (Creation x) marks
  = L.filter (\(Bookmark _ _ _ _ _ y) -> x == y) marks

-- | Redecode the stream as an SavedIOError to see if there was an API error.
-- This will either return the API error, if it can be obtained, or
-- the generic aeson parse error.
handleDecodeError :: IO B.ByteString -> String -> IO String
handleDecodeError stream errors = do
  d <- (eitherDecode <$> stream) :: IO (Either String SavedIOError)
  case d of
    Left  unknown   -> return $ "Unknown errors occured: " ++ errors ++ " " ++ unknown
    Right helpful   -> return $ unpack $ ppSavedIOError helpful
