-- | Command line option  handling

module CLOpts
( parseOptions
, withInfo
, Options(..)
, Common(..)
, Command(..)
, execParser -- from Options.Applicative
) where

import            SavedIO
import            SavedIO.Util

import            Data.Optional                   (Optional(..))
import            Options.Applicative     hiding  (optional)

data Command = Listing (Optional BMGroup)
             | Search Query (Optional BMFormat)
             | ShowLists
             | AddMark BMTitle BMUrl (Optional BMGroup)
             | DelMark BMId

type Color  = Bool
data Common = Common (Optional BMFormat) (Optional Color)

data Options = Options Token Common Command

parseOptions :: Parser Options
parseOptions = Options <$> parseToken <*> parseCommon <*> parseCommand

parseToken :: Parser Token
parseToken = strOption
  $ short 't'
 <> long "token"
 <> metavar "TOKEN"
 <> help "Saved.io token;fixme"

parseCommon :: Parser Common
parseCommon = Common <$> optional (strOption $ short 'f'
                                            <> long "format"
                                            <> metavar "BMFORMAT"
                                            <> help "bid,url,listid,listname,creation,all")
                     <*> optional (switch $ short 'c'
                                         <> long "color"
                                         <> help "Use color")

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
