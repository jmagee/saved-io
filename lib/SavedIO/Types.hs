-- | Types for SavedIO.
{-# LANGUAGE OverloadedStrings #-}

module SavedIO.Types
( Token
, BMGroup
, BMFormat
, Query
, ShowyField(..)
, Bookmark(..)
, SavedIOResponse(..)
, BMList(..)
, SearchKey(..)
, BMTitle
, BMUrl
, extractShowy
, ppBookmark
, ppBMList
, ppSavedIOError
, extractSearchKey
) where

import            SavedIO.Util

import            Control.Monad                   (mzero)
import            Data.Aeson
import qualified  Data.List               as      L
import            Data.Text               hiding  (foldr, foldl, group)
import            Data.Time                       (Day, defaultTimeLocale,
                                                   parseTimeOrError)
import qualified  System.Console.ANSI     as      CS

type Token          = String
type BMGroup        = Maybe String
type BMFormat       = Maybe String
type Query          = String
type BMUrl          = String
type BMTitle        = String

data ShowyField =
  ShowyField { _showId       :: Bool
             , _showUrl      :: Bool
             , _showTitle    :: Bool
             , _showList     :: Bool
             , _showListName :: Bool
             , _showCreation :: Bool
             } deriving (Show)

extractShowy :: BMFormat -> ShowyField
extractShowy Nothing = ShowyField False True True False True False
extractShowy (Just format)
  | "all" `L.isInfixOf` format  = ShowyField True True True True True True
  | notAny format               = extractShowy Nothing -- See Note: notAny
  | otherwise                   = fromList $ (`L.isInfixOf` format) <$> needles
      where needles = ["bid", "url", "title", "listid", "listname", "creation"]
            fromList [a, b, c, d, e, f] = ShowyField a b c d e f
            fromList _ = undefined -- unreachable
            -- Note: notAny
            -- If BMFormat contains no valid field identifiers, then silently
            -- fall back to the default.
            notAny :: String -> Bool
            notAny f = not . or $ fmap (`L.isInfixOf` f) needles

data Bookmark =
  Bookmark { _id       :: Int
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

ppBookmark :: ShowyField -> Bool -> Bookmark -> Text
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

-- |Show for Text
tshow :: Show a => a -> Text
tshow = pack . show

colorize :: CS.Color -> Text -> Text
colorize c t = Prelude.foldl append c' [t, c'']
  where c'  = pack $ CS.setSGRCode [CS.SetColor CS.Foreground CS.Vivid c]
        c'' = pack $ CS.setSGRCode [CS.Reset]

data SavedIOResponse=
  SavedIOResponse { isError  :: Bool
                  , message  :: Text
                  } deriving (Show)

instance FromJSON SavedIOResponse where
  parseJSON (Object v) =
    SavedIOResponse <$> v .: "is_error"
                    <*> v .: "message"
  parseJSON _ = mzero

ppSavedIOError :: SavedIOResponse -> Text
ppSavedIOError (SavedIOResponse _ msg) = append "Saved.io error: " msg

data BMList = BMList Int Text
              deriving (Show)

instance FromJSON BMList where
  parseJSON (Object v)  =
    BMList <$> (convert <$> v .: "id")
           <*> v .: "name"
  parseJSON _ = mzero

ppBMList :: BMList -> Text
ppBMList (BMList _ n) = n `append` "\n"

type SearchString = String
type SearchInt    = Int
type SearchDay    = Day
data SearchKey    = BID SearchInt
                  | Url SearchString
                  | Title SearchString
                  | ListID SearchInt
                  | ListName SearchString
                  | Creation SearchDay
                  deriving (Show)

extractSearchKey:: BMFormat -> Query -> SearchKey
extractSearchKey Nothing q = Title q
extractSearchKey (Just format) q
  | "bid" `L.isInfixOf` format       = BID $ convert q
  | "url" `L.isInfixOf` format       = Url q
  | "title" `L.isInfixOf` format     = Title q
  | "listid" `L.isInfixOf` format    = ListID $ convert q
  | "listname" `L.isInfixOf` format  = ListName q
  | "creation" `L.isInfixOf` format  = Creation $ convert q
  | otherwise                        = extractSearchKey Nothing q

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

