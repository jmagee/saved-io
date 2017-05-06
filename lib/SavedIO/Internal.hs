-- | Internal SavedIO functions.

module SavedIO.Internal
( retrieveBookmarksQ
, createBookmarkQ
, deleteBookmarkQ
, (>&&<)
, (+?+)
, epochTime
, formatParam
, tokenStr
) where

import           SavedIO.Types

import           Data.Optional (Optional (..))
import           Data.Time     (Day, UTCTime (..), defaultTimeLocale,
                                formatTime)

-- | The developer API key
devKey :: Token
devKey = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

-- | Prepare the query string for retrieveBookmark
retrieveBookmarksQ :: Token
                   -> Optional BMGroup
                   -> Optional Int
                   -> String
retrieveBookmarksQ token group limit =
  "?" ++ foldl (>&&<) "&"
                      [ tokenStr token
                      , pageStr Default -- See NB1.
                      , limitStr limit
                      , groupStr group
                      ]
    where
      limitStr = formatParam "limit=" . (show <$>)
      groupStr = formatParam "list=" . (id <$>)
      -- NB1: We may consider exposing the page option in our API
      -- later, but for now we just fall back to the upstream default (1).
      -- My testing could not find any noticeable effect of this option.
      pageStr :: Optional Int -> String
      pageStr  = formatParam "page=" . (show <$>)

-- | Prepare the query string for createBookmark
createBookmarkQ :: Token
                -> BMTitle
                -> BMUrl
                -> Optional BMGroup
                -> String
createBookmarkQ token title url group =
  foldl (>&&<) (tokenStr token)
               [ formatParam "title=" $ pure title
               , formatParam "url=" $ pure url
               , formatParam "list=" group
               ]

-- | Prepare the query string for deleteBookmark
deleteBookmarkQ :: Token -> BMId -> String
deleteBookmarkQ token bkid =
  tokenStr token >&&<
  formatParam "id=" (Specific bkid)

-- | Join URL parameters with an ampersand.
(>&&<) :: String -> String -> String
left >&&< []    = left
[]   >&&< right = right
left >&&< right = left ++ "&" ++ right

-- | Append an Optional string.
-- >>> "foo" +?+ Default
-- "foo"
-- >>> "foo" +?+ (Specific "bar")
-- "foobar"
(+?+) :: String -> Optional String -> String
s +?+ Default       = s
s +?+ (Specific s2) = s ++ s2

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
tokenStr t = formatParam "devkey=" (Specific devKey)
           >&&< formatParam "key=" (Specific t)
