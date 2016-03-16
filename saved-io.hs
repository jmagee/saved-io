-- | Command line client for saved-io
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified  Data.ByteString.Lazy    as      B
import            Control.Monad                   (mzero)
import            Data.Aeson
import            Data.Time                       (Day, defaultTimeLocale,
                                                   parseTimeOrError)
import            Data.Text               hiding  (foldr)
import            Network.HTTP.Conduit            (simpleHttp)
import            Options.Applicative

-- | Base URL for saved io API.
savedIOURL :: String
savedIOURL = "http://devapi.saved.io/v1/"

data Bookmark =
  Bookmark { id       :: Int
           , url      :: Text
           , title    :: Text
           , list     :: Int
           , listName :: Text
           , creation :: Day
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

ppBookmark :: Bookmark -> Bool -> Text
ppBookmark (Bookmark _ theUrl theTitle _ theList _) color =
  Prelude.foldr append "\n"
    [ "\nBookmark: ", theTitle
    , "\nURL: ", theUrl
    , "\nList: ", theList
    ]

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

type Token = String
type BMGroup = Maybe String
type Query = String

data Command = List BMGroup
             | Search Query

data Options = Options Token Command

parseOptions :: Parser Options
parseOptions = Options <$> parseToken <*> parseCommand

parseToken :: Parser Token
parseToken = strOption
  $  short 't'
  <> long "token"
  <> metavar "TOKEN"
  <> help "Saved.io token;fixme"

parseList :: Parser Command
parseList = List <$> optional (argument str (metavar "BMGROUP"))

parseSearch :: Parser Command
parseSearch = Search <$> argument str (metavar "SEARCH-STR")

parseCommand :: Parser Command
parseCommand = subparser
  $  command "list"    (parseList `withInfo` "List bookmark groups")
  <> command "search"  (parseSearch `withInfo` "Search for bookmark")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

appendMaybe :: String -> Maybe String -> String
appendMaybe s (Just s2) = s ++ s2
appendMaybe s Nothing = s

run :: Options -> IO ()
run (Options token cmd) = do
  d <- (eitherDecode <$> savedIO query) :: IO (Either String [Bookmark])
  case d of
    Left err    -> putStrLn $ "Error parsing " ++ err
    Right marks -> putStrLn . unpack . Data.Text.concat $ (`ppBookmark` False) <$> marks
    where
      query    = queryStr ++ tokenStr
      tokenStr = "&token=" ++ token
      queryStr = case cmd of
        List group    -> "bookmarks/" `appendMaybe` group
        Search query  -> undefined

main :: IO ()
main = run =<< execParser (parseOptions `withInfo` "Command Line Interface to saved.io")
