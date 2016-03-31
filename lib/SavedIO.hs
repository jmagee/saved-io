-- | Bindings for saved.io, a cloud-based bookmark service.
--
-- This library provides a Haskell interface for thes API described here:
-- http://saved.io/api.php
{-# LANGUAGE OverloadedStrings #-}

module SavedIO (
  -- * saved.io API
  retrieveBookmarks
, retrieveGroups
, searchBookmarks
, createBookmark
, deleteBookmark

  -- * Exported types
, Token
, BMGroup
, BMFormat
, BMId
, BMTitle
, BMUrl
, Query
, Bookmark(..)
, SavedIOResponse(..)

  -- * Pretty Printing Utilities
, ppSavedIOError
, ppBookmark
, ppBMList

  -- * Utilities
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

-- | Send a POST request to saved.io.
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

-- | Convert a Day into epoch time.
epochTime :: Day -> String
epochTime = formatTime defaultTimeLocale "%s" . flip UTCTime 0

-- | Format a POST/GET parameter, allowing for
-- optional parameters (in which case Nothing will yield an empty string.)
-- Examples: formatParam "limit:" (Just "2")  ->  "limit:2"
--           formatParam "limit:" Nothint     ->  ""
formatParam :: String -> Optional String -> String
formatParam _ Default      = ""
formatParam s (Specific x) = s ++ x

-- | Format a token into a URL parameter.
tokenStr :: Token -> String
tokenStr = formatParam "&token=" . Specific

-- | Join URL parameters with an ampersand.
(>&&<) :: String -> String -> String
left >&&< right = left ++ "&" ++ right

-- | Append an Optional string.
-- >>> "foo" +?+ Default
-- "foo"
-- >>> "foo" +?+ (Specific "bar")
-- "foobar"
(+?+) :: String -> Optional String -> String
s +?+ Default       = s
s +?+ (Specific s2) = s ++ s2

-- | Retrieve a list of bookmarks.
retrieveBookmarks :: Token            -- ^ API Token
                  -> Optional BMGroup -- ^ Bookmark Group
                  -> Optional Day     -- ^ From timestamp
                  -> Optional Day     -- ^ To timestamp
                  -> Optional Int     -- ^ Limit
                  -> IO (Either String [Bookmark])
retrieveBookmarks token group from to limit = do
  let stream = savedIO query
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

-- | Retrieve the list of bookmark groups.
retrieveGroups :: Token -> IO (Either String [Group])
retrieveGroups token = do
  let stream = savedIO query
  d <- (eitherDecode <$> stream) :: IO (Either String [Group])
  case d of
    Left err    -> fmap Left (handleDecodeError stream err)
    Right l     -> pure $ Right l
  where query = "lists" >&&< tokenStr token

-- | Search bookmarks.
-- 
-- This call does retrieves all bookmarks then does a search.  The saved.io
-- API does not provide a server side search call.
searchBookmarks :: SearchKey -> [Bookmark] -> [Bookmark]
searchBookmarks (BID x) marks
  = L.filter (\y -> x == _id y) marks

searchBookmarks (Url x) marks
  = L.filter (\y -> pack x `isInfixOf` _url y) marks

searchBookmarks (Title x) marks
  = L.filter (\y -> pack x `isInfixOf` _title y) marks

searchBookmarks (ListID x) marks
  = L.filter (\y -> x == _list y) marks

searchBookmarks (ListName x) marks
  = L.filter (\y -> pack x `isInfixOf` _listName y) marks

searchBookmarks (Creation x) marks
  = L.filter (\y -> x == _creation y) marks

-- | Create a bookmark entry.
createBookmark :: Token             -- ^ API Token
               -> BMTitle           -- ^ Bookmark title
               -> BMUrl             -- ^ Bookmark URL
               -> Optional BMGroup  -- ^ Optional Bookmark group
               -> IO (Either String Bool) -- ^ Either API error message or Success flag
createBookmark token title url group = postAction urlSuffix query
    where urlSuffix = "create"
          query     = foldl (>&&<) (formatParam "token=" $ pure token)
                                   [ formatParam "title=" $ pure title
                                   , formatParam "url=" $ pure url
                                   , formatParam "list=" group
                                   ]

-- | Delete a bookmark.
deleteBookmark :: Token   -- ^ API token
               -> BMId    -- ^ Bookmark ID
               -- | Either API error message or Success flag
               -- Note that this call returns succes even if it
               -- did not actually delete anything.
               -> IO (Either String Bool)
deleteBookmark token bkid = postAction urlSuffix query
    where urlSuffix = "delete"
          query     = formatParam "token=" (Specific token)
                    >&&< formatParam "bk_id=" (Specific $ show bkid)

-- | Perform a url POST action, and check for API failure.
postAction :: String -> String -> IO (Either String Bool)
postAction urlSuffix qString = do
  let stream = savedIOPOST urlSuffix qString
  d <- (eitherDecode <$> stream) :: IO (Either String SavedIOResponse)
  case d of
    Left str                    -> pure $ Left str
    Right (SavedIOResponse e m) -> e ? pure (Left $ unpack m)
                                     $ pure (Right True)

-- | Redecode the stream as an SavedIOResponse to see if there was an API error.
-- This will either return the API error, if it can be obtained, or
-- the generic aeson parse error.
handleDecodeError :: IO B.ByteString -> String -> IO String
handleDecodeError stream errors = do
  d <- (eitherDecode <$> stream) :: IO (Either String SavedIOResponse)
  case d of
    Left  unknown   -> pure $ "Unknown errors occured: " ++ errors ++ " " ++ unknown
    Right helpful   -> pure $ unpack $ ppSavedIOError helpful
