-- | Bindings for saved.io, a cloud-based bookmark service.
--
-- This library provides a Haskell interface for thes API described here:
-- http://devapi.saved.io
--
-- = Usage Example
-- == Imports/setup used in the examples
-- >>> :set -XOverloadedStrings
-- >>> import SavedIO
-- >>> import Data.Optional (Optional(..))
-- >>> let token = mkToken "your-dev-key" "your-user-key"
-- >>> let (<$$>) = fmap . fmap
-- >>> let (<$$$>) = fmap . fmap . fmap
--
-- == Retrieve all bookmarks for an account
-- >>> retrieveBookmarks token Default Default
--
-- == Retrieve bookmarks in group Haskell
-- >>> retrieveBookmarks token "Haskell" Default
--
-- == Retrieve bookmarks in group Haskell, limit to 10 results.
-- >>> retrieveBookmarks token "Haskell" 10
--
-- == Search for bookmark by title
-- >>> searchBookmarks (Title "Hask") <$$> retrieveBookmarks token Default Default
--
-- == Search for bookmark by URL
-- >>> searchBookmarks (Url "haskell.org") <$$> retrieveBookmarks token Default Default
--
-- == Search for bookmark by ID
-- >>> searchBookmarks (BID "p6Nm8") <$$> retrieveBookmarks token Default Default
--
-- == Add a bookmark
-- >>> createBookmark token "My Page" "http://www.me.me" Default
--
-- == Add a bookmark within a named group
-- >>> createBookmark token "My Page" "http://www.me.me" "stuff"
--
-- == Delete a bookmark
-- >>> deleteBookmark token "p6Nm8"
--
-- == Pretty print (format) a list of bookmarks
-- >>> ppBookmark (BookmarkConfig "title,url" Nothing) <$$$>
--     retrieveBookmarks token Default Default
{-# LANGUAGE OverloadedStrings #-}

module SavedIO (
  -- * saved.io API
  retrieveBookmarks
, getBookmark
, searchBookmarks
, createBookmark
, createBookmark'
, deleteBookmark
, deleteBookmark'

  -- * Token creation
, mkToken

  -- * Exported types
, Key
, Token
, BMGroup
, BMFormat
, BMId
, BMTitle
, BMUrl
, Query
, Bookmark(..)
, BookmarkConfig(..)

  -- * Pretty Printing Utilities
, defBookColors
, defBookKeys
, defBookmarkConfig
, ppBookmark

  -- * Utilities
, extractSearchKey

 -- * FIXME: File in proper place
, SavedIOError (..)
, display
) where

import Debug.Trace

import           SavedIO.Display
import           SavedIO.Internal
import           SavedIO.Types

import           Control.Monad              (void)
import           Data.Aeson                 (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as BP
import qualified Data.List                  as L
import           Data.Optional              (Optional (..))
import           Data.Text                  hiding (group)
import           Network.HTTP.Client        (defaultManagerSettings)
import           Network.HTTP.Conduit       (RequestBody (..), httpLbs, method,
                                             newManager, parseRequest,
                                             requestBody, requestHeaders,
                                             responseBody, simpleHttp)
import           Network.HTTP.Types.Method  (Method, methodDelete, methodPost)

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
  initReq <- parseRequest savedIOURL
  let req = initReq { method = htype
                    , requestHeaders = [("Content-Type"
                                       , "application/x-www-form-urlencoded")
                                       ]
                    , requestBody = RequestBodyLBS $ BP.pack body
                    }
  result <- httpLbs req manager
  pure $ responseBody result

-- | Retrieve a list of bookmarks.
retrieveBookmarks :: Token            -- ^ API Token.
                  -> Optional BMGroup -- ^ Bokmark Group.
                  -> Optional Int     -- ^ Limit the number of results returned.
                  -> IO (Either SavedIOError [Bookmark]) -- ^ Either API error message
                                                   -- or a list of bookmarks.
retrieveBookmarks token group limit =
  getAction $ retrieveBookmarksQ token group limit

-- | Retrieve a single bookmark based on the bookmark id.
getBookmark :: Token -- ^ API Token.
            -> BMId  -- ^ Bookmark id.
            -> IO (Either SavedIOError Bookmark) -- ^ Either API error message or a
                                           -- Bookmark.
getBookmark token bid = getAction $ getBookmarkQ token bid

-- | Search bookmarks.
--
-- This call retrieves all bookmarks then does a search.  The saved.io
-- API does not provide a server side search call.
searchBookmarks :: SearchKey -> [Bookmark] -> [Bookmark]
searchBookmarks (BID x)      = L.filter $ (x ==) . _id
searchBookmarks (Url x)      = L.filter $ (pack x `isInfixOf`) . _url
searchBookmarks (Title x)    = L.filter $ (pack x `isInfixOf`) . _title
searchBookmarks (Note x)     = L.filter $ (pack x `isInfixOf`) . _note
searchBookmarks (Creation x) = L.filter $ (x ==) . _creation

-- | Create a bookmark entry.
-- This returns a 'BMId' instead of a full bookmark, primarily due to limitations
-- in the remote API.  Specifically, the API returns only a partial
-- bookmark entry on this call.
--
-- For a version that returns the complete bookmark, see 'createBookmark''.
createBookmark :: Token             -- ^ API Token.
               -> BMTitle           -- ^ Bookmark title.
               -> BMUrl             -- ^ Bookmark URL.
               -> Optional BMGroup  -- ^ Optional Bookmark group.
               -> IO (Either SavedIOError BMId) -- ^ Either API error message or new BMId.
createBookmark token title url group =
  postAction $ createBookmarkQ token title url group

-- | Create a bookmark entry.
-- This returns a full bookmark entry.  Note that this call will incur two
-- remote API calls.  It is equivalent to calling 'createBookmark' followed by
-- 'getBookmark'.
createBookmark' :: Token             -- ^ API Token.
                -> BMTitle           -- ^ Bookmark title.
                -> BMUrl             -- ^ Bookmark URL.
                -> Optional BMGroup  -- ^ Optional Bookmark group.
                -> IO (Either SavedIOError Bookmark) -- ^ Either API error message or new Bookmark.
createBookmark' token title url group = do
  b <- postAction $ createBookmarkQ token title url group
  either propagateLeft (getBookmark token) b
  where
    propagateLeft = pure . Left

-- | Delete a bookmark.
-- This call does not provide any indication if the delete was succesfull or not.
-- For a version that does, see 'deleteBookmark''.
deleteBookmark :: Token   -- ^ API token.
               -> BMId    -- ^ Bookmark ID.
               -> IO ()
deleteBookmark token bkid =
  void $ deleteAction $ deleteBookmarkQ token bkid

-- | Delete a bookmark.
-- This version will return a Left value upon error.
--
-- Note that this function will result in up to three remote API calls:
--
--  (1) Check if the mark exists.
--  (2) Delete the mark.
--  (3) Confirm that that mark is deleted.
deleteBookmark' :: Token   -- ^ API token.
                -> BMId    -- ^ Bookmark ID.
                -> IO (Either SavedIOError ())
deleteBookmark' token bkid = do
  existsBefore <- markExists token bkid
  if not existsBefore
    then pLeft $ DoesNotExistError bkid
    else do
      deleteBookmark token bkid
      existsAfter <- markExists token bkid
      if existsAfter
        then pLeft $ NotDeletedError bkid
        else pRight ()
  where
    pLeft = pure . Left
    pRight = pure . Right
    markExists t b = either (const False) (const True) <$> getBookmark t b

-- | Perform a url GET action, and check for API failure.
getAction :: FromJSON b => String -> IO (Either SavedIOError b)
getAction query = process <$> savedIO query
  where
    process stream = either (Left . handleDecodeError stream)
                     Right
                     (eitherDecode stream)

-- | Perform a url POST action, and check for API failure.
postAction :: String -> IO (Either SavedIOError BMId)
postAction qString =
  either (Left . exceptionFromString)
         (Right . _id)
         <$> ((eitherDecode <$> stream) :: IO (Either String Bookmark))
  where
    stream = savedIOHTTP methodPost qString

-- | Perform a url DELETE action, and check for API failure.
deleteAction :: String -> IO String
deleteAction b = BP.unpack <$> savedIOHTTP methodDelete b

-- | Construct a decode error with the content message from the remote.
-- The decoding of the server response failed.  v1 of the API would return
-- a well structured JSON response, but v2 returns a combination of HTML texts
-- wrapping warnings and some potentially unpredictable JSON.
--
-- Rather than try to make heads or tails of this, just raised a decoding error
-- with the content of the raw remote stream.  We could also return the
-- error from the failed JSON decode, but its likely to be useless so we
-- just ignore it for now.
handleDecodeError :: B.ByteString -> String -> SavedIOError
handleDecodeError = const . DecodeError . BP.unpack
