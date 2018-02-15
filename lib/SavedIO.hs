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
, getBookmark'
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
, SavedIOException (..)
, display
, catchSavedIOException
) where

import Debug.Trace

import           SavedIO.Display
import           SavedIO.Exception
import           SavedIO.Internal
import           SavedIO.Types

import           Control.Exception          (evaluate, throw, throwIO, tryJust)
import           Control.Monad              (void)
import           Data.Aeson                 (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as BP
import qualified Data.List                  as L
import           Data.Maybe                 (isJust)
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
savedIO = rethrowHttpExceptionAsSavedIO . simpleHttp . (++) savedIOURL

-- | Send a POST request to saved.io.
savedIOHTTP :: Method -> String -> IO B.ByteString
savedIOHTTP a b = rethrowHttpExceptionAsSavedIO $ go a b
  where
    go htype body = do
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
                  -> IO [Bookmark]    -- ^ A list of bookmarks
retrieveBookmarks token group limit =
  getAction $ retrieveBookmarksQ token group limit

-- | Retrieve a single bookmark based on the bookmark id.
getBookmark :: Token       -- ^ API Token.
            -> BMId        -- ^ Bookmark id.
            -> IO Bookmark -- ^ The bookmark
getBookmark token bid = getAction $ getBookmarkQ token bid

-- | Retrieve a single bookmark based on the bookmark id.
-- Equvialant to getBookmark, except it checks for exceptions that may
-- indicate that the bookmark does not exist and returns a Maybe.
getBookmark' :: Token
             -> BMId
             -> IO (Maybe Bookmark)
getBookmark' token bid = either (const Nothing) Just
                                <$> tryJust justDecodeError mark
  where
    justDecodeError x@(DecodeError _) = Just x
    justDecodeError _ = Nothing
    mark = getBookmark token bid

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
               -> IO BMId           -- ^ The new BMId.
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
                -> IO Bookmark       -- ^ The new Bookmark.
createBookmark' token title url group = do
  bid <- postAction $ createBookmarkQ token title url group
  getBookmark token bid

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
                -> IO ()
deleteBookmark' token bkid = do
  existsBefore <- markExists token bkid
  if not existsBefore
    then throwIO $ DoesNotExistError bkid
    else do
      deleteBookmark token bkid
      existsAfter <- markExists token bkid
      if existsAfter
        then throwIO $ NotDeletedError bkid
        else pure ()
  where
    markExists t b = isJust <$> getBookmark' t b

-- | Perform a url GET action, and check for API failure.
getAction :: FromJSON a => String -> IO a
getAction query = savedIO query >>= evaluate . decodeAction

-- | Perform a url POST action, and check for API failure.
postAction :: String -> IO BMId
postAction qString = savedIOHTTP methodPost qString
                   >>= evaluate . _id . decodeAction

-- | Decode a JSON stream.
decodeAction :: FromJSON a => B.ByteString -> a
decodeAction stream = case eitherDecode stream of
  Left err -> throwDecodeError $ stream `BP.append` " -> " `BP.append` BP.pack err
  Right r  -> r

-- | Throw a decode error.
throwDecodeError :: B.ByteString -> a
throwDecodeError = throw . DecodeError . BP.unpack

-- | Perform a url DELETE action, and check for API failure.
deleteAction :: String -> IO String
deleteAction b = BP.unpack <$> savedIOHTTP methodDelete b
