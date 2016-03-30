-- | Command line option  handling

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

import            SavedIO
import            SavedIO.Util

import            Data.Optional                   (Optional(..))
import            Data.Time                       (Day)
import            Options.Applicative     hiding  (optional)

type Color  = Bool
type Limit  = Int

data Direction = Ascending
               | Descending
               deriving (Read, Show)

data Command = Listing (Optional BMGroup)
             | Search Query (Optional BMFormat)
             | ShowLists
             | AddMark BMTitle BMUrl (Optional BMGroup)
             | DelMark BMId

data SortMethod = SortByTitle Direction deriving (Read, Show)

data Common = Common
              (Optional BMFormat)
              (Optional Color)
              (Optional Day)
              (Optional Day)
              (Optional Limit)
              (Optional SortMethod)

data Options = Options Token Common Command

parseOptions :: Parser Options
parseOptions = Options <$> parseToken <*> parseCommon <*> parseCommand

parseToken :: Parser Token
parseToken = strOption
  $ short 't'
 <> long "token"
 <> metavar "TOKEN"
 <> help "Saved.io token;fixme"

parseSort :: Parser SortMethod
parseSort = SortByTitle <$> option auto ( short 's'
                                       <> long "sort"
                                       <> metavar "SORT-DIRECTION"
                                       <> help "Ascending|Descending"
                                       )

parseCommon :: Parser Common
parseCommon = Common <$> optional (strOption $ short 'f'
                                            <> long "format"
                                            <> metavar "BMFORMAT"
                                            <> help "bid,url,listid,listname,creation,all")
                     <*> optional (switch $ short 'c'
                                         <> long "color"
                                         <> help "Use color")
                     <*> optional (option auto $ long "from"
                                             <> metavar "START-DATE"
                                             <> help "Specify oldest date to consider")
                     <*> optional (option auto $ long "until"
                                              <> metavar "END-DATE"
                                              <> help "Specify newest date to consider")
                     <*> optional (option auto $ long "limit"
                                              <> metavar "N"
                                              <> help "Limit to N results")
                     <*> optional parseSort

parseListing :: Parser Command
parseListing = Listing <$> optional (argument str (metavar "BMGROUP"))

parseSearch :: Parser Command
parseSearch = Search <$> argument str (metavar "SEARCH-STR")
                     <*> optional (strOption $ short '/'
                                            <> long "type"
                                            <> metavar "SEARCH-TYPE"
                                            <> help "bid,url,listid,listname,creation")

parseShowLists :: Parser Command
parseShowLists = pure ShowLists

parseAddMark :: Parser Command
parseAddMark = AddMark <$> strOption ( long "title"
                                    <> metavar "BMTITLE"
                                    <> help "Bookmark title")
                       <*> strOption ( long "url"
                                    <> metavar "BMURL"
                                    <> help "Bookmark URL")
                       <*> optional (strOption $ long "group"
                                              <> metavar "BMGROUP"
                                              <> help "Bookmark group")

parseDelMark :: Parser Command
parseDelMark = DelMark <$> argument auto (metavar "BMID" <> help "Bookmark ID")

parseCommand :: Parser Command
parseCommand = subparser
  $ command "list"       (parseListing `withInfo` "List bookmark groups")
 <> command "search"     (parseSearch `withInfo` "Search for bookmark")
 <> command "showlists"  (parseShowLists `withInfo` "Show groups")
 <> command "addmark"    (parseAddMark `withInfo` "Add bookmark")
 <> command "delmark"    (parseDelMark `withInfo` "Delete bookmark")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
