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
             | ShowLists
             | AddMark BMTitle BMUrl (Optional BMGroup)
             | DelMark BMId

-- | Sorting method.
-- Currently only sorting by title is supported.
data SortMethod = SortByTitle Direction deriving (Read, Show)

-- | Common options.  These are options that apply to at least two
-- different Commands.
data Common = Common
              (Optional BMFormat)
              (Optional Color)
              (Optional Day)
              (Optional Day)
              (Optional Limit)
              (Optional Sort)
              (Optional SortMethod)

-- | Full command line option format.
data Options = Options Token Common Command

-- | Parse the full command line options.
parseOptions :: Parser Options
parseOptions = Options <$> parseToken <*> parseCommon <*> parseCommand

-- | Parse the required token option.
parseToken :: Parser Token
parseToken = strOption
  $  short 't'
  <> long "token"
  <> metavar "TOKEN"
  <> help "Saved.io token;fixme"

-- | Parse the optional sort method.
parseSortMethod :: Parser SortMethod
parseSortMethod = SortByTitle
  <$> option auto ( long "sort-method"
                  <> metavar "SORT-DIRECTION"
                  <> help "Ascending|Descending"
                  )

-- | Parse the optional common options and flags.
parseCommon :: Parser Common
parseCommon = Common
  <$> optional (strOption
               $  short 'f'
               <> long "format"
               <> metavar "BMFORMAT"
               <> help "bid,url,listid,listname,creation,all")
  <*> optional (switch
               $  short 'c'
               <> long "color"
               <> help "Use color")
  <*> optional (option auto
               $  long "from"
               <> metavar "START-DATE"
               <> help "Specify oldest date to consider")
  <*> optional (option auto
               $  long "until"
               <> metavar "END-DATE"
               <> help "Specify newest date to consider")
  <*> optional (option auto
               $  long "limit"
               <> metavar "N"
               <> help "Limit to N results")
  <*> optional (switch
               $  short 's'
               <> long "sort"
               <> help "Sort output")
  <*> optional parseSortMethod

-- | Parse the listing command.
parseListing :: Parser Command
parseListing = Listing <$> optional (argument str (metavar "BMGROUP"))

-- | Parse the search command.
parseSearch :: Parser Command
parseSearch = Search
  <$> argument str (metavar "SEARCH-STR")
  <*> optional (strOption $  short '/'
                          <> long "type"
                          <> metavar "SEARCH-TYPE"
                          <> help "bid,url,listid,listname,creation")

-- | Parse the show groups command.
parseShowGroups :: Parser Command
parseShowGroups = pure ShowLists

-- |  Parse the add bookmark command.
parseAddMark :: Parser Command
parseAddMark = AddMark
           <$> strOption ( long "title"
                         <> metavar "BMTITLE"
                         <> help "Bookmark title")
           <*> strOption ( long "url"
                         <> metavar "BMURL"
                         <> help "Bookmark URL")
           <*> optional (strOption $  long "group"
                                   <> metavar "BMGROUP"
                                   <> help "Bookmark group")

-- | Parse the delete bookmark command.
parseDelMark :: Parser Command
parseDelMark = DelMark <$> argument auto (metavar "BMID" <> help "Bookmark ID")

-- | Parse subcommands.
parseCommand :: Parser Command
parseCommand = subparser
  $  command "list"       (parseListing `withInfo` "List bookmark groups")
  <> command "search"     (parseSearch `withInfo` "Search for bookmark")
  <> command "groups"     (parseShowGroups `withInfo` "Show groups")
  <> command "addmark"    (parseAddMark `withInfo` "Add bookmark")
  <> command "delmark"    (parseDelMark `withInfo` "Delete bookmark")

-- | Display help for a parser.
withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
