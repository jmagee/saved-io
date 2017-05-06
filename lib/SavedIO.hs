-- | Bindings for saved.io, a cloud-based bookmark service.
--
-- This library provides a Haskell interface for thes API described here:
-- http://saved.io/api.php
--
-- = Usage Example
-- == Imports/setup used in the examples
-- >>> import SavedIO
-- >>> import Data.Optional (Optional(..))
-- >>> import Data.Time (Day, fromGregorian)
-- >>> let token = "your-api-token"
-- >>> let (<$$>) = fmap . fmap
-- >>> let (<$$$>) = fmap . fmap . fmap
--
-- == Retrieve all bookmarks for an account
-- >>> retrieveBookmarks token Default Default Default Default
--
-- == Retrieve bookmarks in group Haskell
-- >>> retrieveBookmarks token Haskell Default Default Default
--
-- == Retrieve bookmarks in group Haskell, created between a date range, limit to 10 results.
-- >>> retrieveBookmarks token
--                       (Specific "Haskell")
--                       (Specific (fromGregorian 2016 01 01))
--                       (Specific (fromGregorian (2016 03 01))
--                       (Specific 10)
--
-- == Search for bookmark by title
-- >>> searchBookmarks (Title "Hask") <$$> retrieveBookmarks token Default Default Default Default
--
-- == Search for bookmark by URL
-- >>> searchBookmarks (Url "haskell.org") <$$> retrieveBookmarks token Default Default Default Default
--
-- == Search for bookmark by ID
-- >>> searchBookmarks (BID 901210) <$$> retrieveBookmarks token Default Default Default Default
--
-- == Retrive groups
-- >>> retrieveGroups token
--
-- == Add a bookmark
-- >>> createBookmark token "My Page" "http://www.me.me" Default
--
-- == Add a bookmark within a named group
-- >>> createBookmark token "My Page" "http://www.me.me" (Specific "stuff")
--
-- == Delete a bookmark
-- >>> deleteBookmark token 90210
--
-- == Pretty print (format) a list of bookmarks
-- >>> ppBookmark (BookmarkConfig (pack "title, url") Nothing) <$$$>
--     retrieveBookmarks token Default Default Default Default
{-# LANGUAGE OverloadedStrings #-}

module SavedIO (
  -- * saved.io API
  retrieveBookmarks
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
, BookmarkConfig(..)
, SavedIOResponse(..)

  -- * Pretty Printing Utilities
, ppSavedIOError
, defBookColors
, defBookKeys
, defBookmarkConfig
, ppBookmark

  -- * Utilities
, extractSearchKey
) where

import           SavedIO.Internal
import           SavedIO.Types
import           SavedIO.Util

import           Debug.Trace                (trace)

import           Data.Aeson                 (eitherDecode)
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as BP
import qualified Data.List                  as L
import           Data.Optional              (Optional (..))
import           Data.Text                  hiding (foldl, foldr, group)
import           Data.Time                  (Day)
import           Network.HTTP.Client        (defaultManagerSettings)
import           Network.HTTP.Conduit       (RequestBody (..), httpLbs, method,
                                             newManager, parseUrl, requestBody,
                                             requestHeaders, responseBody,
                                             simpleHttp)
import           Network.HTTP.Types.Method  (Method, methodDelete,
                                             methodPost)

-- | Base URL for saved io API.
savedIOURL :: String
savedIOURL = "http://devapi.saved.io/bookmarks"

-- | Fetch a URL from saved.io.
savedIO :: String -> IO B.ByteString
savedIO = simpleHttp . (++) savedIOURL

-- | Send a POST request to saved.io.
savedIOHTTP :: Method -> String -> IO B.ByteString
savedIOHTTP htype body = do
  manager <- newManager defaultManagerSettings
  initReq <- parseUrl savedIOURL
  let req = initReq { method = htype
                    , requestHeaders = [("Content-Type"
                                       , "application/x-www-form-urlencoded")
                                       ]
                    , requestBody = RequestBodyLBS $ BP.pack body
                    }
  result <- httpLbs req manager
  pure $ responseBody result

-- | Retrieve a list of bookmarks.
retrieveBookmarks :: Token            -- ^ API Token
                  -> Optional BMGroup -- ^ Bokmark Group
                  -> Optional Int     -- ^ Limit
                  -> IO (Either String [Bookmark])
retrieveBookmarks token group limit = do
  trace (retrieveBookmarksQ token group limit) $ pure ()
  let stream = savedIO $ retrieveBookmarksQ token group limit
  d <- (eitherDecode <$> stream) :: IO (Either String [Bookmark])
  case d of
    Left err    -> fmap Left (handleDecodeError stream err)
    Right marks -> pure $ Right marks

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

searchBookmarks (Note x) marks
  = L.filter (\y -> pack x `isInfixOf` _note y) marks

--searchBookmarks (GroupID x) marks
--  = L.filter (\y -> x == _list y) marks

--searchBookmarks (GroupName x) marks
--  = L.filter (\y -> pack x `isInfixOf` _listName y) marks

searchBookmarks (Creation x) marks
  = L.filter (\y -> x == _creation y) marks

-- | Create a bookmark entry.
createBookmark :: Token             -- ^ API Token
               -> BMTitle           -- ^ Bookmark title
               -> BMUrl             -- ^ Bookmark URL
               -> Optional BMGroup  -- ^ Optional Bookmark group
               -> IO (Either String Bool) -- ^ Either API error message or Success flag
createBookmark token title url group =
  trace (createBookmarkQ token title url group) $
  postAction $ createBookmarkQ token title url group

-- | Delete a bookmark.
deleteBookmark :: Token   -- ^ API token
               -> BMId    -- ^ Bookmark ID
               -- | Either API error message or Success flag
               -- Note that this call returns succes even if it
               -- did not actually delete anything.
               -> IO String
deleteBookmark token bkid =
  trace (deleteBookmarkQ token bkid) $
  deleteAction $ deleteBookmarkQ token bkid

-- | Perform a url POST action, and check for API failure.
-- FIXME: Should we return the bm_id instead of Bool?
postAction :: String -> IO (Either String Bool)
postAction qString =
  boolify <$> ((eitherDecode <$> stream) :: IO (Either String Bookmark))
    where
      stream = savedIOHTTP methodPost qString
      boolify :: Either String Bookmark -> Either String Bool
      boolify (Left x)  = Left x
      boolify (Right _) = Right True

-- | Perform a url DELETE action, and check for API failure.
-- FIXME: Should we return the bm_id instead of Bool?
deleteAction :: String -> IO String
deleteAction b = BP.unpack <$> savedIOHTTP methodDelete b

-- | Redecode the stream as an SavedIOResponse to see if there was an API error.
-- This will either return the API error, if it can be obtained, or
-- the generic aeson parse error.
handleDecodeError :: IO B.ByteString -> String -> IO String
handleDecodeError stream errors = do
  d <- (eitherDecode <$> stream) :: IO (Either String SavedIOResponse)
  case d of
    Left  unknown   -> pure $ "Unknown errors occured: " ++ errors ++ " " ++ unknown
    Right helpful   -> pure $ unpack $ ppSavedIOError helpful
