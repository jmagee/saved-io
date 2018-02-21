-- | Bindings for saved.io, a cloud-based bookmark service.
--
-- This library provides a Haskell interface for thes API described here:
-- http://devapi.saved.io
--
-- The only module that is necessary to import is SavedIO:
--
-- > import SavedIO
--
-- All other modules are exposed, but should be considered internal.
--
-- = Usage Example
-- == Simple example
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Data.Foldable (toList)
-- > import qualified Data.Text     as T
-- > import qualified Data.Text.IO  as T
-- > import SavedIO
-- >
-- > -- Print all bookmarks
-- > printMarks :: Key -> Key -> IO ()
-- > printMarks devKey userKey = do
-- >   let token = mkToken devKey userKey
-- >
-- >   marks <- retrieveBookmarks token Default Default
-- >   printTextSeq (ppBookmark defBookmarkConfig <$> marks)
-- >
-- >  where
-- >    printTextSeq  = T.putStrLn . T.intercalate "\n" . toList
--
-- == Handling errors
-- The SavedIO library reports errors via exceptions.  It will throw
-- exceptions of type 'SavedIOException'.  Further, it may throw
-- exceptions of type 'HttpException'.  Other IO exceptions are possible,
-- but not expected.  (In other words, handling SavedIOException and
-- 'HttpException' ought to be the first order of business.)
--
-- In some cases the library will catch and rethrow specific exceptions.
-- These are:
--
-- * status403 -> BadToken
-- * status404 -> BadURL
-- * ConnectionFailure -> BadURL
--
-- The function 'catchSavedIOException' is provided to catch
-- exceptions of type 'SavedIOException'  Building on this previous example,
-- here is a sample that handles common errors by printing the exception message.
--
-- > saferPrintMarks :: Key -> Key -> IO ()
-- > saferPrintMarks devKey userKey =
-- >   catchSavedIOException (printMarks devKey userKey)
-- >                         (T.putStrLn . display)
--
-- Note that 'catchSavedIOException' does not sure you program is completely
-- exception safe as it only catches a subset of possible exceptions thrown.
--
-- == Imports/setup used in the examples
-- >>> :set -XOverloadedStrings
-- >>> import SavedIO
-- >>> import Data.Optional (Optional(..))
-- >>> let token = mkToken "your-dev-key" "your-user-key"
-- >>> let (<$$>) = fmap . fmap
--
-- == Pretty print (format) a list of bookmarks
-- >>> ppBookmark (BookmarkConfig "title,url" Nothing) <$$>
--     retrieveBookmarks token Default Default
{-# LANGUAGE OverloadedStrings #-}

module SavedIO (
  -- * saved.io API
  retrieveBookmarks
, getBookmark
, getBookmarkMaybe
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

 -- * Exception Handling
, SavedIOException (..)
, display
, catchSavedIOException

 -- * Re-exports
, Optional (..)
) where

import           SavedIO.Display
import           SavedIO.Exception
import           SavedIO.Query
import           SavedIO.Types

import           Control.Exception         (evaluate, throw, throwIO, tryJust)
import           Control.Monad             (void)
import           Data.Aeson                (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy      as B (ByteString, append)
import           Data.Maybe                (isJust)
import           Data.Optional             (Optional (..))
import           Data.Sequence             (Seq)
import qualified Data.Sequence             as S (filter)
import           Data.String.Conversions   (cs)
import           Data.Text                 (Text, append, isInfixOf)
import           Network.HTTP.Conduit      (RequestBody (..), method,
                                            parseUrlThrow, requestBody,
                                            requestHeaders)
import           Network.HTTP.Simple       (getResponseBody, httpLBS)
import           Network.HTTP.Types.Method (Method, methodDelete, methodPost)

-- | Base URL for saved io API.
savedIOURL :: Text
savedIOURL = "http://devapi.saved.io/bookmarks"

-- | Fetch a URL from saved.io.
savedIO :: Text -> IO B.ByteString
savedIO query =
  let url = cs $ append savedIOURL query
  in getResponseBody <$>
    (parseUrlThrow url >>= rethrowHttpExceptionAsSavedIO . httpLBS)

-- | Send a POST request to saved.io.
savedIOHTTP :: Method -> Text -> IO B.ByteString
savedIOHTTP htype body =
  let url = cs savedIOURL
  in do
    initReq <- parseUrlThrow url
    let req = initReq { method = htype
                      , requestHeaders = [("Content-Type"
                                         , "application/x-www-form-urlencoded")
                                         ]
                      , requestBody = RequestBodyLBS $ cs body
                      }
    result <- rethrowHttpExceptionAsSavedIO $ httpLBS req
    pure $ getResponseBody result

-- | Retrieve a sequence of bookmarks.
--
-- Examples:
-- Retrieve all bookmarks for an account
--
-- >>> retrieveBookmarks token Default Default
--
-- Retrieve bookmarks in group Haskell
--
-- >>> retrieveBookmarks token "Haskell" Default
--
-- Retrieve bookmarks in group Haskell, limit to 10 results.
--
-- >>> retrieveBookmarks token "Haskell" 10
retrieveBookmarks :: Token            -- ^ API Token.
                  -> Optional BMGroup -- ^ Bokmark Group.
                  -> Optional Int     -- ^ Limit the number of results returned.
                  -> IO (Seq Bookmark)  -- ^ A sequence of bookmarks
retrieveBookmarks token group limit =
  getAction $ retrieveBookmarksQ token group limit

-- | Retrieve a single bookmark based on the bookmark id.
getBookmark :: Token       -- ^ API Token.
            -> BMId        -- ^ Bookmark id.
            -> IO Bookmark -- ^ The bookmark
getBookmark token bid = getAction $ getBookmarkQ token bid

-- | Retrieve a single bookmark based on the bookmark id.
-- Equvialant to 'getBookmark', except it checks for exceptions that may
-- indicate that the bookmark does not exist and returns a Maybe.
getBookmarkMaybe :: Token
                 -> BMId
                 -> IO (Maybe Bookmark)
getBookmarkMaybe token bid = either (const Nothing) Just
                                    <$> tryJust justDecodeError mark
  where
    justDecodeError x@(DecodeError _) = Just x
    justDecodeError _ = Nothing
    mark = getBookmark token bid

-- | Search bookmarks.
--
-- This call retrieves all bookmarks then does a search.  The saved.io
-- API does not provide a server side search call.
--
-- Examples:
-- Search for bookmark by title
--
-- >>> searchBookmarks (Title "Hask") <$> retrieveBookmarks token Default Default
--
-- Search for bookmark by URL
--
-- >>> searchBookmarks (Url "haskell.org") <$> retrieveBookmarks token Default Default
--
--  Search for bookmark by ID
--
-- >>> searchBookmarks (BID "p6Nm8") <$> retrieveBookmarks token Default Default
searchBookmarks :: SearchKey -> Seq Bookmark -> Seq Bookmark
searchBookmarks (BID x)      = S.filter $ (x ==) . _id
searchBookmarks (Url x)      = S.filter $ (cs x `isInfixOf`) . _url
searchBookmarks (Title x)    = S.filter $ (cs x `isInfixOf`) . _title
searchBookmarks (Note x)     = S.filter $ (cs x `isInfixOf`) . _note
searchBookmarks (Creation x) = S.filter $ (x ==) . _creation

-- | Create a bookmark entry.
-- This returns a 'BMId' instead of a full bookmark, primarily due to limitations
-- in the remote API.  Specifically, the API returns only a partial
-- bookmark entry on this call.
--
-- For a version that returns the complete bookmark, see 'createBookmark'.
createBookmark' :: Token             -- ^ API Token.
                -> BMTitle           -- ^ Bookmark title.
                -> BMUrl             -- ^ Bookmark URL .
                -> Optional BMGroup  -- ^ Optional Bookmark group.
                -> IO BMId           -- ^ The new BMId.
createBookmark' token title url group =
  postAction $ createBookmarkQ token title url group

-- | Create a bookmark entry.
-- This returns a full bookmark entry.  Note that this call will incur two
-- remote API calls.  It is equivalent to calling 'createBookmark' followed by
-- 'getBookmark'.
--
-- Examples:
-- Add a bookmark
--
-- >>> createBookmark token "My Page" "http://www.me.me" Default
--
-- Add a bookmark within a named group
--
-- >>> createBookmark token "My Page" "http://www.me.me" "stuff"
createBookmark :: Token             -- ^ API Token.
               -> BMTitle           -- ^ Bookmark title.
               -> BMUrl             -- ^ Bookmark URL.
               -> Optional BMGroup  -- ^ Optional Bookmark group.
               -> IO Bookmark       -- ^ The new Bookmark.
createBookmark token title url group = do
  bid <- postAction $ createBookmarkQ token title url group
  getBookmark token bid

-- | Delete a bookmark.
-- This call does not provide any indication if the delete was succesfull or not.
-- For a version that does, see 'deleteBookmark'.
--
-- Example:
-- Delete a bookmark
--
-- >>> deleteBookmark token "p6Nm8"
deleteBookmark' :: Token   -- ^ API token.
               -> BMId    -- ^ Bookmark ID.
               -> IO ()
deleteBookmark' token bkid =
  void $ deleteAction $ deleteBookmarkQ token bkid

-- | Delete a bookmark.
-- This version will return a Left value upon error.
--
-- Note that this function will result in up to three remote API calls:
--
--  (1) Check if the mark exists.
--  (2) Delete the mark.
--  (3) Confirm that that mark is deleted.
deleteBookmark :: Token   -- ^ API token.
                -> BMId    -- ^ Bookmark ID.
                -> IO ()
deleteBookmark token bkid = do
  existsBefore <- markExists token bkid
  if not existsBefore
    then throwIO $ DoesNotExistError (cs bkid)
    else do
      deleteBookmark' token bkid
      existsAfter <- markExists token bkid
      if existsAfter
        then throwIO $ NotDeletedError (cs bkid)
        else pure ()
  where
    markExists t b = isJust <$> getBookmarkMaybe t b

-- | Perform a url GET action, and check for API failure.
getAction :: FromJSON a => Text -> IO a
getAction query = savedIO query >>= evaluate . decodeAction

-- | Perform a url POST action, and check for API failure.
postAction :: Text -> IO BMId
postAction qString = savedIOHTTP methodPost qString
                   >>= evaluate . _id . decodeAction

-- | Decode a JSON stream.
decodeAction :: FromJSON a => B.ByteString -> a
decodeAction stream = case eitherDecode stream of
  Left err -> throwDecodeError $ stream `B.append` " -> " `B.append` cs err
  Right r  -> r

-- | Throw a decode error.
throwDecodeError :: B.ByteString -> a
throwDecodeError = throw . DecodeError . cs

-- | Perform a url DELETE action, and check for API failure.
deleteAction :: Text -> IO String
deleteAction b = cs <$> savedIOHTTP methodDelete b
