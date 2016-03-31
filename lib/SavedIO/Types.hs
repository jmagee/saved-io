-- | Types for SavedIO.
{-# LANGUAGE OverloadedStrings #-}

module SavedIO.Types (
  -- * Exported Types
  Token
, BMGroup
, BMFormat
, Query
, ShowyField(..)
, Bookmark(..)
, SavedIOResponse(..)
, Group(..)
, SearchKey(..)
, BMTitle
, BMUrl
, BMId

  -- * Pretty Printing Utilities
, extractShowy
, ppBookmark
, ppBMList
, ppSavedIOError

  -- * Search Utility
, extractSearchKey
) where

import            SavedIO.Util

import            Control.Monad                   (mzero)
import            Data.Aeson
import qualified  Data.List               as      L
import            Data.Optional                   (Optional(..))
import            Data.Text               hiding  (foldr, foldl, group)
import            Data.Time                       (Day, defaultTimeLocale,
                                                   parseTimeOrError)
import qualified  System.Console.ANSI     as      CS

-- | The saved.io API Token.  It can be generated here: linkme.
type Token          = String

-- | The bookmark group.  saved.io commonly refers to this as a "list", but 
-- that terminology is a bit overloaded.
type BMGroup        = String

-- | The bookmark format.  This is simply a string that will be matchd
-- for the keywords:
--
--   * bid
--   * url
--   * title
--   * listid
--   * listname
--   * creation
type BMFormat       = String

-- | Search query; i.e. search string.
type Query          = String

-- | Bookmark URL.
type BMUrl          = String

-- | Bookmark title.
type BMTitle        = String

-- | Bookmark ID. (saved.io's internal identiciation numbers.)
type BMId           = Int

-- | Flags to indicate which fields of a Bookmark record to print.
data ShowyField =
  ShowyField { _showId       :: Bool
             , _showUrl      :: Bool
             , _showTitle    :: Bool
             , _showList     :: Bool
             , _showListName :: Bool
             , _showCreation :: Bool
             } deriving (Show)

-- | Extract a ShowyField from a BMFormat string.'
-- This matches the string for any text string matching the bookmark
-- fields - each match indicates that the field should be shown.
extractShowy :: Optional BMFormat -> ShowyField
extractShowy Default = ShowyField False True True False True False
extractShowy (Specific format)
  | "all" `L.isInfixOf` format  = ShowyField True True True True True True
  | notAny format               = extractShowy Default -- See Note: notAny
  | otherwise                   = fromList $ (`L.isInfixOf` format) <$> needles
      where needles = ["bid", "url", "title", "listid", "listname", "creation"]
            fromList [a, b, c, d, e, f] = ShowyField a b c d e f
            fromList _ = undefined -- unreachable
            -- Note: notAny
            -- If BMFormat contains no valid field identifiers, then silently
            -- fall back to the default.
            notAny :: String -> Bool
            notAny f = not . or $ fmap (`L.isInfixOf` f) needles

-- | saved.io Bookmark.
data Bookmark =
  Bookmark { _id       :: BMId
           , _url      :: Text
           , _title    :: Text
           , _list     :: Int
           , _listName :: Text
           , _creation :: Day
           } deriving (Show)

instance FromJSON Bookmark where
  parseJSON (Object v) =
    Bookmark <$> (convert <$> v .: "bk_id")
             <*> v .: "url"
             <*> v .: "title"
             <*> (convert <$> v .: "list")
             <*> v .:? "list_name" .!= "none"
             <*> (dateFromString <$> v .: "creation_date")
  parseJSON _ = mzero

-- | Pretty print a saved.io bookmark.
ppBookmark :: ShowyField -- ^ A ShowyField indicating which fields to display.
           -> Bool       -- ^ Whether to use color.
           -> Bookmark   -- ^ The bookmark to print.
           -> Text       -- ^ The formatted result.
ppBookmark (ShowyField sID sURL sTitle sList sListName sCreation)
           color
           (Bookmark theID theURL theTitle theList theListName theCreation)
  = Prelude.foldr append "\n" [ppID, ppTitle, ppUrl, ppBlist, ppLname, ppCreation]
      where
        colorize' c t  = color ? colorize c t $ t
        ppID       = sID       ? append "\nID: "       (tshow theID)       $ ""
        ppTitle    = sTitle    ? append "\nBookmark: " (colorize' CS.Green theTitle) $ ""
        ppUrl      = sURL      ? append "\nURL: "      (colorize' CS.Blue theURL) $ ""
        ppBlist    = sList     ? append "\nList ID: "  (tshow theList)     $ ""
        ppLname    = sListName ? append "\nList: "     theListName         $ ""
        ppCreation = sCreation ? append "\nCreated: "  (tshow theCreation) $ ""

-- Show for Text
tshow :: Show a => a -> Text
tshow = pack . show

-- Colorize it!
colorize :: CS.Color -> Text -> Text
colorize c t = Prelude.foldl append c' [t, c'']
  where c'  = pack $ CS.setSGRCode [CS.SetColor CS.Foreground CS.Vivid c]
        c'' = pack $ CS.setSGRCode [CS.Reset]

-- | A response object from saved.io.  This is returned on error
-- and as a response to POST requests.  The response may have an optional
-- data payload as well, but we ignore it.
-- 
-- The ignored data payload is usually either empty or contains a copy
-- of the content just POSTed.
data SavedIOResponse=
  SavedIOResponse { isError  :: Bool
                  , message  :: Text
                  } deriving (Show)

instance FromJSON SavedIOResponse where
  parseJSON (Object v) =
    SavedIOResponse <$> v .: "is_error"
                    <*> v .: "message"
  parseJSON _ = mzero

-- | Pretty print a SavedIOResponse.
ppSavedIOError :: SavedIOResponse -> Text
ppSavedIOError (SavedIOResponse _ msg) = append "Saved.io error: " msg

-- | A "group" of bookmarks.
data Group = Group Int Text
              deriving (Show)

instance FromJSON Group where
  parseJSON (Object v)  =
    Group <$> (convert <$> v .: "id")
            <*> v .: "name"
  parseJSON _ = mzero

-- | Pretty print a bookmark group
ppBMList :: Group -> Text
ppBMList (Group _ n) = n `append` "\n"

type SearchString = String
type SearchInt    = Int
type SearchDay    = Day

-- | SearchKey encodes "what to search for" and "where to search for it."
data SearchKey    = BID SearchInt         -- ^ Search by ID.
                  | Url SearchString      -- ^ Search by URL.
                  | Title SearchString    -- ^ Search by Title.
                  | ListID SearchInt      -- ^ Search by Group ID.
                  | ListName SearchString -- ^ Search by Group Name.
                  | Creation SearchDay    -- ^ Search by Creation date.
                  deriving (Show)

-- | Extract a SearchKey from a BMFormat string.
extractSearchKey:: Optional BMFormat -> Query -> SearchKey
extractSearchKey Default q = Title q
extractSearchKey (Specific format) q
  | "bid" `L.isInfixOf` format       = BID $ convert q
  | "url" `L.isInfixOf` format       = Url q
  | "title" `L.isInfixOf` format     = Title q
  | "listid" `L.isInfixOf` format    = ListID $ convert q
  | "listname" `L.isInfixOf` format  = ListName q
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

