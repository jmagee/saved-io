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
, deleteBookmark

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

import           Data.Aeson                 (eitherDecode)
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as BP
import qualified Data.List                  as L
import           Data.Optional              (Optional (..))
import           Data.Text                  hiding (foldl, foldr, group)
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
                  -> IO (Either String [Bookmark]) -- ^ Either API error message
                                                   -- or a list of bookmarks.
retrieveBookmarks token group limit = do
  let stream = savedIO $ retrieveBookmarksQ token group limit
  d <- (eitherDecode <$> stream) :: IO (Either String [Bookmark])
  case d of
    Left err    -> fmap Left (handleDecodeError stream err)
    Right marks -> pure $ Right marks

-- | Retrieve a single bookmark based on the bookmark id.
getBookmark :: Token -- ^ API Token.
            -> BMId  -- ^ Bookmark id.
            -> IO (Either String Bookmark) -- ^ Either API error message or a
                                           -- Bookmark.
getBookmark token bid = do
  let stream = savedIO $ getBookmarkQ token bid
  d <- (eitherDecode <$> stream) :: IO (Either String Bookmark)
  case d of
    Left err    -> fmap Left (handleDecodeError stream err)
    Right marks -> pure $ Right marks

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
createBookmark :: Token             -- ^ API Token.
               -> BMTitle           -- ^ Bookmark title.
               -> BMUrl             -- ^ Bookmark URL.
               -> Optional BMGroup  -- ^ Optional Bookmark group.
               -> IO (Either String BMId) -- ^ Either API error message or new BMId.
createBookmark token title url group =
  postAction $ createBookmarkQ token title url group

-- | Delete a bookmark.
deleteBookmark :: Token   -- ^ API token.
               -> BMId    -- ^ Bookmark ID.
               -> IO String  -- ^ Either API error message or Success flag.
                             -- Note that this call returns success even if it
                             -- did not actually delete anything.
deleteBookmark token bkid =
  deleteAction $ deleteBookmarkQ token bkid

-- | Perform a url POST action, and check for API failure.
postAction :: String -> IO (Either String BMId)
postAction qString =
  boolify <$> ((eitherDecode <$> stream) :: IO (Either String Bookmark))
    where
      stream = savedIOHTTP methodPost qString
      boolify :: Either String Bookmark -> Either String BMId
      boolify (Left x)  = Left x
      boolify (Right (Bookmark x _ _ _ _)) = Right x

-- | Perform a url DELETE action, and check for API failure.
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
