-- | Internal SavedIO functions.
{-# LANGUAGE OverloadedStrings #-}

module SavedIO.Internal
( retrieveBookmarksQ
, getBookmarkQ
, createBookmarkQ
, deleteBookmarkQ
, (>&&<)
, (+?+)
, epochTime
, formatParam
, tokenStr
) where

import           SavedIO.Types

import           Data.Optional           (Optional (..))
import           Data.String.Conversions (cs)
import           Data.Text               (Text, append)
import qualified Data.Text               as T (null)
import           Data.Time               (Day, UTCTime (..), defaultTimeLocale,
                                          formatTime)

-- | Prepare the query string for retrieveBookmark
retrieveBookmarksQ :: Token
                   -> Optional BMGroup
                   -> Optional Int
                   -> Text
retrieveBookmarksQ token group limit =
  "?" `append` foldl (>&&<) "&"
                           [ tokenStr token
                           , pageStr Default -- See NB1.
                           , limitStr limit
                           , groupStr group
                           ]
    where
      limitStr = formatParam "limit=" . ((cs . show) <$>)
      groupStr = formatParam "list=" . (id <$>)
      -- NB1: We may consider exposing the page option in our API
      -- later, but for now we just fall back to the upstream default (1).
      -- My testing could not find any noticeable effect of this option.
      pageStr :: Optional Int -> Text
      pageStr  = formatParam "page=" . ((cs . show) <$>)

getBookmarkQ :: Token -> BMId -> Text
getBookmarkQ token bid = "/" `append` bid `append` "?" `append` tokenStr token

-- | Prepare the query string for createBookmark
createBookmarkQ :: Token
                -> BMTitle
                -> BMUrl
                -> Optional BMGroup
                -> Text
createBookmarkQ token title url group =
  foldl (>&&<) (tokenStr token)
               [ formatParam "title=" $ pure title
               , formatParam "url=" $ pure url
               , formatParam "list=" group
               ]

-- | Prepare the query string for deleteBookmark
deleteBookmarkQ :: Token -> BMId -> Text
deleteBookmarkQ token bkid =
  tokenStr token >&&<
  formatParam "id=" (Specific bkid)

-- | Join URL parameters with an ampersand.
(>&&<) :: Text -> Text -> Text
left >&&< right | T.null right = left
                | T.null left  = right
                | otherwise  = left `append` "&" `append` right
{-left >&&< []    = left-}
{-[]   >&&< right = right-}
{-left >&&< right = left `append` "&" `append` right-}

-- | Append an Optional string.
-- >>> "foo" +?+ Default
-- "foo"
-- >>> "foo" +?+ (Specific "bar")
-- "foobar"
(+?+) :: Text -> Optional Text -> Text
s +?+ Default       = s
s +?+ (Specific s2) = s `append` s2

-- | Convert a Day into epoch time.
epochTime :: Day -> Text
epochTime = cs . formatTime defaultTimeLocale "%s" . flip UTCTime 0

-- | Format a POST/GET parameter, allowing for
-- optional parameters (in which case Nothing will yield an empty string.)
-- Examples: formatParam "limit:" (Just "2")  ->  "limit:2"
--           formatParam "limit:" Nothint     ->  ""
formatParam :: Text -> Optional Text -> Text
formatParam _ Default      = ""
formatParam s (Specific x) = s `append` x

-- | Format a token into a URL parameter.
tokenStr :: Token -> Text
tokenStr (Token dev user) =
  formatParam "devkey=" (Specific dev) >&&< formatParam "key=" (Specific user)
