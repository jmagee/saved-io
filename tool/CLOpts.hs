-- | Command line option  handling
{-# LANGUAGE OverloadedStrings #-}

module CLOpts
( parseOptions
, withInfo
, Options(..)
, Common(..)
, Command(..)
, Direction(..)
, SortMethod(..)
, execParser -- from Options.Applicative
) where

import           SavedIO
import           SavedIO.Util

import           Control.Monad       (mzero)
import           Data.Aeson          (FromJSON, Object, ToJSON, Value (..),
                                      object, (.:?), (.=))
import qualified Data.Aeson.Types    as A
import           Data.Optional       (Optional (..))
import           Data.Semigroup      ((<>))
import           Data.Text           (Text)
import           Options.Applicative hiding (optional)

-- | Color flag.
type Color  = Bool

-- | Limit on the number of results displayed.
type Limit  = Int

-- | Sort flag.
type Sort   = Bool

-- | Sorting direction.
data Direction = Ascending
               | Descending
               deriving (Read, Show)

-- | Sub-command line argument.
data Command = Listing (Optional BMGroup)
             | Search Query (Optional BMFormat)
             | AddMark BMTitle BMUrl (Optional BMGroup)
             | DelMark BMId
             | GetMark BMId
             | MakeRC
             deriving (Show)

-- | Sorting method.
-- Currently only sorting by title is supported.
data SortMethod = SortByTitle Direction deriving (Read, Show)

-- | Common options.  These are options that apply to at least two
-- different Commands.
data Common = Common
              (Optional Key)
              (Optional Key)
              (Optional BMFormat)
              (Optional Color)
              (Optional Limit)
              (Optional Sort)
              (Optional SortMethod)
              deriving (Show)

instance FromJSON Common where
  parseJSON (Object v) =
    Common <$> v .:¿ "devkey"
           <*> v .:¿ "userkey"
           <*> v .:¿ "format"
           <*> v .:¿ "color"
           <*> v .:¿ "limit"
           <*> v .:¿ "sort"
           <*> pure Default
  parseJSON _ = mzero

instance ToJSON Common where
  toJSON (Common dev user format color limit sort _) =
    object $  "devkey" `encodeOpt` dev
           ++ "userkey" `encodeOpt` user
           ++ "format" `encodeOpt` format
           ++ "color" `encodeOpt` color
           ++ "limit" `encodeOpt` limit
           ++ "sort" `encodeOpt` sort
      where
        encodeOpt _ Default = []
        encodeOpt t (Specific x) = [t .= x]

-- | Retrieve the value associated with the given key of an Object.
-- The result is 'Default' if the key is not present.
--
-- This is equivalent to the .:? from the Aeson library, but
-- converts the 'Maybe' result into an 'Optional' result.
(.:¿) :: FromJSON a => Object -> Text -> A.Parser (Optional a)
x .:¿ y = go =<< (x .:? y)
  where go Nothing  = pure Default
        go (Just g) = pure $ Specific g

-- | The last item wins!  Returns the last item in the list, or
-- the default item in the first parameter.
lastWins :: a -> [a] -> a
lastWins first [] = first
lastWins _ lst = last lst

-- | A toggle - a pair of on/off switches with a default value; we accept
-- zero or more of them and the last one wins.
toggle :: Alternative f => Optional Bool -> f Bool -> f Bool -> f (Optional Bool)
toggle def a b = pick def (a <|> b)

-- | Pick the last instance of an option if it is specified, otherwise use the
-- provided default.
pick :: Alternative f => Optional a -> f a -> f (Optional a)
pick def x = perhaps checkOptsNoDef checkOptsWithDef def
  where
    checkOptsNoDef     = Specific <$> (last <$> some x) <|> pure Default
    checkOptsWithDef d = Specific <$> lastWins d <$> many x

-- | Full command line option format.
data Options = Options Common Command deriving (Show)

-- | Parse the full command line options.
parseOptions :: Common -> Parser Options
parseOptions common_def =
  Options <$> parseCommon common_def <*> parseCommand

-- | Parse the optional sort method.
parseSortMethod :: Parser SortMethod
parseSortMethod = SortByTitle
  <$> option auto ( long "sort-method"
                  <> metavar "SORT-DIRECTION"
                  <> help "Ascending|Descending")

-- | Parse the optional common options and flags.
parseCommon :: Common -> Parser Common
parseCommon (Common dev user format color limit sort _) = Common
  <$> pick dev (strOption
               $  short 'd'
               <> long "devkey"
               <> metavar "DEVKEY"
               <> help "Saved.io developer key. See http://devapi.saved.io/key")
  <*> pick user (strOption
                $  short 'u'
                <> long "userkey"
                <> metavar "USERKEY"
                <> help "Saved.io user key. See http://saved.io/key")
  <*> pick format (strOption
                  $  short 'f'
                  <> long "format"
                  <> metavar "BMFORMAT"
                  <> help "id,title,url,note,creation")
  <*> toggle color
                       (flag' True
                              $  short 'c'
                              <> long "color"
                              <> help "Enable color output")
                       (flag' False
                              $  short 'b'
                              <> long "no-color"
                              <> help "Disable color output")
  <*> pick limit (option auto
                 $  long "limit"
                 <> metavar "N"
                 <> help "Limit to N results")
  <*> toggle sort
                       (flag' True $  short 's'
                                   <> long "sort"
                                   <> help "Sort output")
                       (flag' False $  short 'n'
                                    <> long "no-sort"
                                    <> help "Do not sort output")
  <*> pick Default parseSortMethod

-- | Parse the listing command.
parseListing :: Parser Command
parseListing = Listing <$> optional (argument str (metavar "BMGROUP"))

-- | Parse the search command.
parseSearch :: Parser Command
parseSearch = Search
  <$> argument str (metavar "SEARCH-STR")
  <*> pick Default (strOption $  short '/'
                              <> long "type"
                              <> metavar "SEARCH-TYPE"
                              <> help "bid,url,groupid,groupname,creation")

-- |  Parse the add bookmark command.
parseAddMark :: Parser Command
parseAddMark = AddMark
           <$> strOption (  long "title"
                         <> metavar "BMTITLE"
                         <> help "Bookmark title")
           <*> strOption (  long "url"
                         <> metavar "BMURL"
                         <> help "Bookmark URL")
           <*> optional (strOption $  long "group"
                                   <> metavar "BMGROUP"
                                   <> help "Bookmark group")

-- | Parse the delete bookmark command.
parseDelMark :: Parser Command
parseDelMark = DelMark <$> argument str (metavar "BMID" <> help "Bookmark ID")

-- | Parse the get bookmark command.
parseGetMark :: Parser Command
parseGetMark = GetMark <$> argument str (metavar "BMID" <> help "Bookmark id")

-- | Parse the MakeRC command
parseMakeRC :: Parser Command
parseMakeRC = pure MakeRC

-- | Parse subcommands.
parseCommand :: Parser Command
parseCommand = subparser
  $  command "list"       (parseListing `withInfo` "List bookmark groups")
  <> command "search"     (parseSearch `withInfo` "Search for bookmark")
  <> command "addmark"    (parseAddMark `withInfo` "Add bookmark")
  <> command "delmark"    (parseDelMark `withInfo` "Delete bookmark")
  <> command "getmark"    (parseGetMark `withInfo` "Get bookmark")
  <> command "mkrc"       (parseMakeRC `withInfo` "Make RC File")

-- | Display help for a parser.
withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
