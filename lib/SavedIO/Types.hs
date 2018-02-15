-- | Types for SavedIO.
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DeriveDataTypeable #-}

module SavedIO.Types (
  -- * Exported Types
  Key
, Token (..)
, BMGroup
, BMFormat
, Query
, Bookmark (..)
, BookmarkConfig (..)
, SearchKey (..)
, BMTitle
, BMUrl
, BMId

  -- * Token creation
, mkToken

  -- * Pretty Printing Utilities
, defBookColors
, defBookKeys
, defBookmarkConfig
, ppBookmark

  -- * Search Utility
, extractSearchKey

  -- * Misc Utility
, dateFromString

 -- * FIXME: File in proper place
, SavedIOException (..)
, exceptionFromString
) where

import           SavedIO.Display

import           Control.Exception   (Exception)
import           Control.Monad       (mzero)
import           Data.Aeson
import qualified Data.List           as L
import           Data.Maybe          (fromMaybe)
import           Data.Optional       (Optional (..))
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time           (Day, defaultTimeLocale, parseTimeOrError)
import qualified System.Console.ANSI as CS

-- | Key type for API keys.  Alias of string
type Key = String

-- | The saved.io API Token.
-- User keys can be generated here: http://saved.io/key
-- Dev keys can be generated here: http://devapi.saved.io/key
data Token =
  Token { _devKey  :: Key
        , _userKey :: Key
        } deriving (Show, Eq)

-- | Create a token from the devkey and userkey.
mkToken :: Key -> Key -> Token
mkToken = Token

-- | The bookmark group.  saved.io commonly refers to this as a "list", but
-- that terminology is a bit overloaded.
type BMGroup        = String

-- | The bookmark format.  This is simply a string that will be matched
-- for the keywords:
--
--   * id
--   * url
--   * title
--   * note
--   * creation
type BMFormat       = String

-- | Search query; i.e. search string.
type Query          = String

-- | Bookmark URL.
type BMUrl          = String

-- | Bookmark title.
type BMTitle        = String

-- | Bookmark ID. (saved.io's internal identiciation numbers.)
type BMId           = String

-- | saved.io Bookmark.
data Bookmark =
  Bookmark { _id       :: BMId
           , _url      :: Text
           , _title    :: Text
           , _note     :: Text
           , _creation :: Day
           } deriving (Show)

instance FromJSON Bookmark where
  parseJSON (Object v) =
    Bookmark <$> v .: "bk_id"
             <*> v .: "bk_url"
             <*> v .: "bk_title"
             <*> (fromMaybe "" <$> v .:? "bk_note")
             <*> (dateFromString . fromMaybe "" <$> v .:? "bk_date")
  parseJSON _ = mzero

-- | Color scheme pair to match a key to a color.
type ColorScheme = [(String, CS.Color)]

-- | Config for bookmark pretty printing.
data BookmarkConfig =
  BookmarkConfig { _keys        :: Text
                 , _colorScheme :: Maybe ColorScheme
                 } deriving (Show, Eq)

-- | Default color scheme for bookmark.
defBookColors :: ColorScheme
defBookColors =
  [ ("id", CS.Cyan)
  , ("title", CS.Green)
  , ("url", CS.Blue)
  , ("note", CS.Yellow)
  , ("creation", CS.Red)
  ]

-- | Default bookmark key string
defBookKeys :: Text
defBookKeys = "title,url"

-- | Default bookmark config
defBookmarkConfig :: BookmarkConfig
defBookmarkConfig = BookmarkConfig defBookKeys Nothing

-- | Pretty print a saved.io bookmark.
ppBookmark :: BookmarkConfig  -- ^ Pretty printer configuration for bookmarks.
           -> Bookmark        -- ^ The bookmark to print.
           -> Text            -- ^ The formatted result.
ppBookmark (BookmarkConfig k scheme)
           (Bookmark theID theURL theTitle theNote theCreation)
  = T.concat . newlineate $ prettyField <$> T.splitOn "," k
    where
      newlineate    = fmap $ flip T.append "\n"
      colorize'     = colorize scheme
      prettyField s = case s of
        "id"        -> T.append "ID: "       $ colorize' "id" $ T.pack theID
        "title"     -> T.append "Bookmark: " $ colorize' "title" theTitle
        "url"       -> T.append "URL: "      $ colorize' "url" theURL
        "note"      -> T.append "Note: "     $ colorize' "note" theNote
        "creation"  -> T.append "Created: "  $ colorize' "creation" $ tshow theCreation
        e@_         -> T.append "unrecognized format " e

-- Show for Text
tshow :: Show a => a -> Text
tshow = T.pack . show

-- Colorize it!
colorize :: Maybe ColorScheme -> String -> Text -> Text
colorize Nothing       key text = colorize (Just []) key text
colorize (Just scheme) key text =
  case lookup key scheme of
    Nothing       -> text
    (Just color)  -> foldl T.append (startColor color) [text, endColor]
  where startColor c = T.pack $ CS.setSGRCode [CS.SetColor CS.Foreground CS.Vivid c]
        endColor     = T.pack $ CS.setSGRCode [CS.Reset]

-- | A concrete type for errors returned by the saved.io API.
data SavedIOException
  = UnknownError String
  | DoesNotExistError String -- ^ The bookmark does not (appear to) exist.
  | NotDeletedError String   -- ^ The bookmark was not deleted.
  | DecodeError String       -- ^ Could not decode the response from the server.
  | BadToken String          -- ^ Bad token (user key and developer key)
  | BadURL String            -- ^ The saved.io URl is down or incorrect
  deriving (Eq, Show)

instance Exception SavedIOException

instance Display SavedIOException where
  display (UnknownError s)      = "Unknown error: " ++ s
  display (DoesNotExistError s) = "Bookmark does exist: " ++ s
  display (NotDeletedError s)   = "Bookmark was not deleted: " ++ s
  display (DecodeError s)       = "Could not decode remote response: " ++ s
  display (BadToken s)          = "Bad user key or developer.  HTTP request was: \n" ++ s
  display (BadURL s)            = "Bad or unreachable URL.  HTTP request was: \n" ++ s

exceptionFromString :: String -> SavedIOException
exceptionFromString = UnknownError

type SearchString = String
type SearchDay    = Day

-- | SearchKey encodes "what to search for" and "where to search for it."
data SearchKey    = BID SearchString       -- ^ Search by ID.
                  | Url SearchString       -- ^ Search by URL.
                  | Title SearchString     -- ^ Search by Title.
                  | Note SearchString      -- ^ Search by note.
                  | Creation SearchDay     -- ^ Search by Creation date.
                  deriving (Show)

-- | Extract a SearchKey from a BMFormat string.
extractSearchKey:: Optional BMFormat -> Query -> SearchKey
extractSearchKey Default q = Title q
extractSearchKey (Specific format) q
  | "id" `L.isInfixOf` format        = BID q
  | "url" `L.isInfixOf` format       = Url q
  | "title" `L.isInfixOf` format     = Title q
  | "note" `L.isInfixOf` format      = Note q
  | "creation" `L.isInfixOf` format  = Creation $ convert q
  | otherwise                        = extractSearchKey Default q

-- | Convert a string to a type with a slightly more elegant error
-- than plain read.
convert :: Read a => String -> a
convert s =
  case reads s of
    [(x, "")]  -> x
    _          -> error $ "Could not convert " ++ s

-- | Convert a string into a Day.
-- We define this instead of using read, because has a friendlier parse.
-- For example, read :: Day, would choke on "2016-1-2", but the solution
-- below parses it as "2016-01-02".
dateFromString :: String -> Day
dateFromString = parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d %H:%M:%S"
