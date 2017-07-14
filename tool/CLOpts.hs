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

import            SavedIO
import            SavedIO.Util

import            Control.Monad                   (mzero)
import            Data.Aeson
import qualified  Data.Aeson.Types        as      A
import            Data.Optional                   (Optional(..))
import            Data.Semigroup                  ((<>))
import            Data.Text                       (Text)
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
              (Optional Token)
              (Optional BMFormat)
              (Optional Color)
              (Optional Limit)
              (Optional Sort)
              (Optional SortMethod)
              deriving (Show)

instance FromJSON Common where
  parseJSON (Object v) =
    Common <$>  v .:¿ "token"
           <*>  v .:¿ "format"
           <*>  v .:¿ "color"
           <*> v .:¿ "limit"
           <*> v .:¿ "sort"
           <*> pure Default
  parseJSON _ = mzero

instance ToJSON Common where
  toJSON (Common token format color limit sort _) =
    object $  "token" `encodeOpt` token
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
-- This is equivelant to the .:? from the Aeson library, but
-- converts the 'Maybe' result into an 'Optional' result.
(.:¿) :: FromJSON a => Object -> Text -> A.Parser (Optional a)
x .:¿ y = go =<< (x .:? y)
  where go Nothing  = pure Default
        go (Just g) = pure $ Specific g

-- | Given two Optionals, pick the one that is the most specific.
-- If both are specific, then favor the second (RHS).
pickSpecific :: Optional a -> Optional a -> Optional a
pickSpecific (Specific x) Default = pure x
pickSpecific _ (Specific x)       = pure x
pickSpecific _ _                  = Default

-- | Given two Optional Bools, yeild True if either are True.
optionTrue :: Optional Bool -> Optional Bool -> Optional Bool
optionTrue (Specific True)   _                = Specific True
optionTrue _                 (Specific False) = Specific False
optionTrue Default           Default          = Default
optionTrue _                 _                = Specific True

-- | Merge two sets of Common options, where Specific overrides default.
mergeCommon :: Common -> Common -> Common
mergeCommon (Common aToken aFormat aColor aLimit aSort aMethod)
            (Common bToken bFormat bColor bLimit bSort bMethod)
  = Common (pickSpecific aToken bToken)
           (pickSpecific aFormat bFormat)
           (optionTrue aColor bColor)
           (pickSpecific aLimit bLimit)
           (optionTrue aSort bSort)
           (pickSpecific aMethod bMethod)

-- | Full command line option format.
data Options = Options Common Command deriving (Show)

-- | Parse the full command line options.
parseOptions :: Common -> Parser Options
parseOptions common_def =
  Options <$> ((common_def `mergeCommon`) <$> parseCommon)
          <*> parseCommand

-- | Parse the token option.
parseToken :: Parser (Optional Token)
parseToken = optional $ strOption
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
  <$> parseToken
  <*> optional (strOption
               $  short 'f'
               <> long "format"
               <> metavar "BMFORMAT"
               <> help "id,title,url,note,creation")
  <*> optional (switch
               $  short 'c'
               <> long "color"
               <> help "Use color")
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
