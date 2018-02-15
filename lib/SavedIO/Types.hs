-- | Types for SavedIO.
{-# LANGUAGE OverloadedStrings #-}

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

) where

import           Data.String.Conversions    (cs)
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
type Key = Text

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
type BMGroup        = Text

-- | The bookmark format.  This is simply a string that will be matched
-- for the keywords:
--
--   * id
--   * url
--   * title
--   * note
--   * creation
type BMFormat       = Text

-- | Search query; i.e. search string.
type Query          = Text

-- | Bookmark URL.
type BMUrl          = Text

-- | Bookmark title.
type BMTitle        = Text

-- | Bookmark ID. (saved.io's internal identiciation numbers.)
type BMId           = Text

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
        "id"        -> T.append "ID: "       $ colorize' "id" theID
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

type SearchString = Text
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
  | "id" `T.isInfixOf` format        = BID q
  | "url" `T.isInfixOf` format       = Url q
  | "title" `T.isInfixOf` format     = Title q
  | "note" `T.isInfixOf` format      = Note q
  | "creation" `T.isInfixOf` format  = Creation $ convert q
  | otherwise                        = extractSearchKey Default q

-- | Convert a string to a type with a slightly more elegant error
-- than plain read.
convert :: Read a => Text -> a
convert s =
  case (reads . cs) s of
    [(x, "")]  -> x
    _          -> error $ "Could not convert " ++ cs s

-- | Convert a string into a Day.
-- We define this instead of using read, because has a friendlier parse.
-- For example, read :: Day, would choke on "2016-1-2", but the solution
-- below parses it as "2016-01-02".
dateFromString :: String -> Day
dateFromString = parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d %H:%M:%S"
