-- | Command line option  handling

module CLOpts
( parseOptions
, withInfo
, Options(..)
, Command(..)
, execParser -- from Options.Applicative
) where

import            SavedIO
import            Options.Applicative

data Command = Listing BMGroup BMFormat
             | Search Query BMFormat Command
             | ShowLists

data Options = Options Token Command

parseOptions :: Parser Options
parseOptions = Options <$> parseToken <*> parseCommand

parseToken :: Parser Token
parseToken = strOption
  $  short 't'
  <> long "token"
  <> metavar "TOKEN"
  <> help "Saved.io token;fixme"

parseListing :: Parser Command
parseListing = Listing <$> optional (argument str (metavar "BMGROUP"))
                       <*> optional (strOption $ short 'f'
                                               <> long "format"
                                               <> metavar "BMFORMAT"
                                               <> help "bid,url,listid,listname,creation,all")

parseSearch :: Parser Command
parseSearch = Search <$> argument str (metavar "SEARCH-STR")
                     <*> optional (strOption $ short '/'
                                             <> long "type"
                                             <> metavar "SEARCH-TYPE"
                                             <> help "bid,url,listid,listname,creation")
                     <*> parseCommand -- FIXME: factor out common options

parseShowLists :: Parser Command
parseShowLists = pure ShowLists

parseCommand :: Parser Command
parseCommand = subparser
  $  command "list"       (parseListing `withInfo` "List bookmark groups")
  <> command "search"     (parseSearch `withInfo` "Search for bookmark")
  <> command "showlists"  (parseShowLists `withInfo` "Show groups")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
