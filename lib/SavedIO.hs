-- | Bindings for saved.io, a cloud-based bookmark service.
{-# LANGUAGE OverloadedStrings #-}

module SavedIO
( Token
, BMGroup
, BMFormat
, Query
, Bookmark(..)
, SavedIOError(..)
, ppSavedIOError
, ppBookmark
, ppBMList
, retrieveBookmarks
, retrieveLists
, searchBookmarks
, extractShowy
, extractSearchKey
) where

import            Control.Monad                   (mzero)
import            Data.Aeson
import qualified  Data.ByteString.Lazy    as      B
import qualified  Data.List               as      L
import            Data.Text               hiding  (foldr, foldl, group)
import            Data.Time                       (Day, defaultTimeLocale,
                                                   formatTime,
                                                   parseTimeOrError)
import            Network.HTTP.Conduit            (simpleHttp)
import qualified  System.Console.ANSI     as      CS

-- FIXME: Move me
-- |Functional alternative to if-then-else.
-- See https://wiki.haskell.org/If-then-else
if' :: Bool -> a -> a -> a
if' True x _  = x
if' False _ y = y

-- |C-like ternary operator.
--  Usage: cond ? exp1 $ exp2
infixr 1 ?
(?) :: Bool -> a -> a -> a
(?) = if'

type Token          = String
type BMGroup        = Maybe String
type BMFormat       = Maybe String
type Query          = String

-- | Base URL for saved io API.
savedIOURL :: String
savedIOURL = "http://devapi.saved.io/v1/"

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

tshow :: Show a => a -> Text
tshow = pack . show

colorize :: CS.Color -> Text -> Text
colorize c t = Prelude.foldl append c' [t, c'']
  where c'  = pack $ CS.setSGRCode [CS.SetColor CS.Foreground CS.Vivid c]
        c'' = pack $ CS.setSGRCode [CS.Reset]

data SavedIOError =
  SavedIOError { isError  :: Bool
               , message   :: Text
               } deriving (Show)

instance FromJSON SavedIOError where
  parseJSON (Object v) =
    SavedIOError <$> v .: "is_error"
                 <*> v .: "message"
  parseJSON _ = mzero

ppSavedIOError :: SavedIOError -> Text
ppSavedIOError (SavedIOError _ msg) = append "Saved.io error: " msg

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

-- | Fetch a URL from saved.io.
savedIO :: String -> IO B.ByteString
savedIO = simpleHttp . (++) savedIOURL

tokenStr :: Token -> String
tokenStr = (++) "&token="

epochTime :: Day -> String
epochTime = formatTime defaultTimeLocale "%s"

toStr :: Maybe Day -> String
toStr Nothing    = ""
toStr (Just day) = "to:" ++ epochTime day

fromStr :: Maybe Day -> String
fromStr Nothing    = ""
fromStr (Just day) = "from:" ++ epochTime day

limitStr :: Maybe Int -> String
limitStr Nothing    = ""
limitStr (Just x)   = "limit: " ++ show x

(>&&<) :: String -> String -> String
left >&&< right = left ++ "&" ++ right

(+?+) :: String -> Maybe String -> String
s +?+ Nothing   = s
s +?+ (Just s2) = s ++ s2

retrieveBookmarks :: Token     -> -- ^ API Token
                     BMGroup   -> -- ^ Bookmark Group
                     Maybe Day -> -- ^ From timestamp
                     Maybe Day -> -- ^ To timestamp
                     Maybe Int -> -- ^ Limit
                     IO (Either String [Bookmark])
retrieveBookmarks token group from to limit = do
  let stream = savedIO query
  d <- (eitherDecode <$> stream) :: IO (Either String [Bookmark])
  case d of
    Left err    -> fmap Left (handleDecodeError stream err)
    Right marks -> return $ Right marks
  where query = foldl (>&&<)
                      ("bookmarks/" +?+ group)
                      [ tokenStr token
                      , fromStr from
                      , toStr to
                      , limitStr limit
                      ]

retrieveLists :: Token -> IO (Either String [BMList])
retrieveLists token = do
  let stream = savedIO query
  d <- (eitherDecode <$> stream) :: IO (Either String [BMList])
  case d of
    Left err    -> fmap Left (handleDecodeError stream err)
    Right l     -> return $ Right l
  where query = "lists" >&&< tokenStr token

searchBookmarks :: SearchKey -> [Bookmark] -> [Bookmark]
searchBookmarks (BID x)      marks =
  L.filter (\(Bookmark y _ _ _ _ _) -> x == y) marks
searchBookmarks (Url x)    marks =
  L.filter (\(Bookmark _ y _ _ _ _) -> pack x `isInfixOf` y) marks
searchBookmarks (Title x)      marks =
  L.filter (\(Bookmark _ _ y _ _ _) -> pack x `isInfixOf` y) marks
searchBookmarks (ListID x)   marks =
  L.filter (\(Bookmark _ _ _ y _ _) -> x == y) marks
searchBookmarks (ListName x) marks =
  L.filter (\(Bookmark _ _ _ _ y _) -> pack x `isInfixOf` y) marks
searchBookmarks (Creation x) marks = undefined

-- | Redecode the stream as an SavedIOError to see if there was an API error.
-- This will either return the API error, if it can be obtained, or
-- the generic aeson parse error.
handleDecodeError :: IO B.ByteString -> String -> IO String
handleDecodeError stream errors = do
  d <- (eitherDecode <$> stream) :: IO (Either String SavedIOError)
  case d of
    Left  unknown   -> return $ "Unknown errors occured: " ++ errors ++ " " ++ unknown
    Right helpful   -> return $ unpack $ ppSavedIOError helpful
