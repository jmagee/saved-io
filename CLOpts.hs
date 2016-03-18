-- | Command line option  handling

module CLOpts 
( parseOptions
, withInfo
, Options(..)
, Command(..)
, execParser -- from Options.Applicative
) where

import            Options.Applicative

type Token    = String
type BMGroup  = Maybe String
type Query    = String

data Command = Listing BMGroup
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

parseListing :: Parser Command
parseListing = Listing <$> optional (argument str (metavar "BMGROUP"))

parseSearch :: Parser Command
parseSearch = Search <$> argument str (metavar "SEARCH-STR")

parseCommand :: Parser Command
parseCommand = subparser
  $  command "list"    (parseListing `withInfo` "List bookmark groups")
  <> command "search"  (parseSearch `withInfo` "Search for bookmark")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
