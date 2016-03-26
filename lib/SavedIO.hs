-- | Bindings for saved.io, a cloud-based bookmark service.
{-# LANGUAGE OverloadedStrings #-}

module SavedIO
( Token
, BMGroup
, BMFormat
, BMId
, BMTitle
, BMUrl
, Query
, Bookmark(..)
, SavedIOResponse(..)
, ppSavedIOError
, ppBookmark
, ppBMList
, retrieveBookmarks
, retrieveLists
, searchBookmarks
, createBookmark
, deleteBookmark
, extractShowy
, extractSearchKey
) where

import            SavedIO.Types
import            SavedIO.Util

import            Data.Aeson                      (eitherDecode)
import qualified  Data.ByteString.Lazy    as      B
import qualified  Data.ByteString.Lazy.Char8 as   BP
import qualified  Data.List               as      L
import            Data.Optional                   (Optional(..))
import            Data.Text               hiding  (foldr, foldl, group)
import            Data.Time                       (Day, defaultTimeLocale,
                                                   formatTime, UTCTime(..))
import            Network.HTTP.Conduit            (simpleHttp, newManager,
                                                   parseUrl, httpLbs, method,
                                                   requestBody, RequestBody(..),
                                                   responseBody, requestHeaders)
import            Network.HTTP.Client             (defaultManagerSettings)

-- | Base URL for saved io API.
savedIOURL :: String
savedIOURL = "http://devapi.saved.io/v1/"

-- | Fetch a URL from saved.io.
savedIO :: String -> IO B.ByteString
savedIO = simpleHttp . (++) savedIOURL

savedIOPOST :: String -> String -> IO B.ByteString
savedIOPOST url body = do
  manager <- newManager defaultManagerSettings
  initReq <- parseUrl $ savedIOURL ++ url
  let req = initReq { method = "POST"
                    , requestHeaders = [("Content-Type"
                                       , "application/x-www-form-urlencoded")
                                       ]
                    , requestBody = RequestBodyLBS $ BP.pack body
                    }
  result <- httpLbs req manager
  pure $ responseBody result

epochTime :: Day -> String
epochTime = formatTime defaultTimeLocale "%s" . flip UTCTime 0

-- | Format a POST/GET parameter, allowing for
-- optional parameters (in which case Nothing will yield an empty string.)
-- Examples: formatParam "limit:" (Just "2")  ->  "limit:2"
--           formatParam "limit:" Nothint     ->  ""
formatParam :: String -> Optional String -> String
formatParam _ Default      = ""
formatParam s (Specific x) = s ++ x

tokenStr :: Token -> String
tokenStr = formatParam "&token=" . Specific

(>&&<) :: String -> String -> String
left >&&< right = left ++ "&" ++ right

(+?+) :: String -> Optional String -> String
s +?+ Default       = s
s +?+ (Specific s2) = s ++ s2

retrieveBookmarks :: Token            -- ^ API Token
                  -> Optional BMGroup -- ^ Bookmark Group
                  -> Optional Day     -- ^ From timestamp
                  -> Optional Day     -- ^ To timestamp
                  -> Optional Int     -- ^ Limit
                  -> IO (Either String [Bookmark])
retrieveBookmarks token group from to limit = do
  let stream = savedIO query
  print query
  d <- (eitherDecode <$> stream) :: IO (Either String [Bookmark])
  case d of
    Left err    -> fmap Left (handleDecodeError stream err)
    Right marks -> pure $ Right marks
  where query = foldl (>&&<)
                      ("bookmarks/" +?+ group)
                      [ tokenStr token
                      , fromStr from
                      , toStr to
                      , limitStr limit
                      ]
        toStr :: Optional Day -> String
        toStr = formatParam "to=" . (epochTime <$>)
        fromStr :: Optional Day -> String
        fromStr = formatParam "from=" . (epochTime <$>)
        limitStr :: Optional Int -> String
        limitStr = formatParam "limit=" . (show <$>)

retrieveLists :: Token -> IO (Either String [BMList])
retrieveLists token = do
  let stream = savedIO query
  d <- (eitherDecode <$> stream) :: IO (Either String [BMList])
  case d of
    Left err    -> fmap Left (handleDecodeError stream err)
    Right l     -> pure $ Right l
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

postAction :: String -> String -> IO (Either String Bool)
postAction urlSuffix qString = do
  let stream = savedIOPOST urlSuffix qString
  d <- (eitherDecode <$> stream) :: IO (Either String SavedIOResponse)
  case d of
    Left str                    -> pure $ Left str
    Right (SavedIOResponse e m) -> e ? pure (Left $ unpack m)
                                     $ pure (Right True)

createBookmark :: Token -> BMTitle -> BMUrl -> Optional BMGroup -> IO (Either String Bool)
createBookmark token title url group = postAction urlSuffix query
    where urlSuffix = "create"
          query     = foldl (>&&<) (formatParam "token=" $ pure token)
                                   [ formatParam "title=" $ pure title
                                   , formatParam "url=" $ pure url
                                   , formatParam "list=" group
                                   ]

deleteBookmark :: Token -> BMId -> IO (Either String Bool)
deleteBookmark token bkid = postAction urlSuffix query
    where urlSuffix = "delete"
          query     = formatParam "token=" (Specific token)
                 >&&< formatParam "bk_id=" (Specific $ show bkid)

-- | Redecode the stream as an SavedIOResponse to see if there was an API error.
-- This will either return the API error, if it can be obtained, or
-- the generic aeson parse error.
handleDecodeError :: IO B.ByteString -> String -> IO String
handleDecodeError stream errors = do
  d <- (eitherDecode <$> stream) :: IO (Either String SavedIOResponse)
  case d of
    Left  unknown   -> pure $ "Unknown errors occured: " ++ errors ++ " " ++ unknown
    Right helpful   -> pure $ unpack $ ppSavedIOError helpful
