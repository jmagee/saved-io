-- | Command line client for saved-io
{-# LANGUAGE OverloadedStrings #-}

module Main where

import            CLOpts
import            SavedIO

import qualified  Data.ByteString.Lazy    as      B
import            Control.Monad                   (mzero)
import            Data.Aeson
import            Data.Aeson.Types        as      AT
import            Data.Time                       (Day, defaultTimeLocale,
                                                   parseTimeOrError)
import            Data.Text               hiding  (foldr)
import            Network.HTTP.Conduit            (simpleHttp)

{--- | Base URL for saved io API.-}
savedIOURL :: String
savedIOURL = "http://devapi.saved.io/v1/"

{-data Bookmark =-}
  {-Bookmark { id       :: Int-}
           {-, url      :: Text-}
           {-, title    :: Text-}
           {-, list     :: Int-}
           {-, listName :: Text-}
           {-, creation :: Day-}
           {-} deriving (Show)-}

{-instance FromJSON Bookmark where-}
  {-parseJSON (Object v) =-}
    {-Bookmark <$> (convert <$> v .: "bk_id")-}
             {-<*> v .: "url"-}
             {-<*> v .: "title"-}
             {-<*> (convert <$> v .: "list")-}
             {-<*> v .:? "list_name" .!= "none"-}
             {-<*> (dateFromString <$> v .: "creation_date")-}
  {-parseJSON _ = mzero-}

{-ppBookmark :: Bookmark -> Bool -> Text-}
{-ppBookmark (Bookmark _ theUrl theTitle _ theList _) color =-}
  {-Prelude.foldr append "\n"-}
    {-[ "\nBookmark: ", theTitle-}
    {-, "\nURL: ", theUrl-}
    {-, "\nList: ", theList-}
    {-]-}

{-data SavedIOError =-}
  {-SavedIOError { isError  :: Bool-}
               {-, message   :: Text-}
               {-} deriving (Show)-}

{-instance FromJSON SavedIOError where-}
  {-parseJSON (Object v) =-}
    {-SavedIOError <$> v .: "is_error"-}
                 {-<*> v .: "message"-}
  {-parseJSON _ = mzero-}

{-ppSavedIOError :: SavedIOError -> Text-}
{-ppSavedIOError (SavedIOError _ msg) = append "Saved.io error: " msg-}

{--- | Convert a string to a type with a slightly more elegant error-}
{--- than plain read.-}
{-convert :: Read a => String -> a-}
{-convert s =-}
  {-case reads s of-}
    {-[(x, "")]  -> x-}
    {-_          -> error $ "Could not convert " ++ s-}

{--- | Convert a string into a Day.-}
{--- We define this instead of using read, because has a friendlier parse.-}
{--- For example, read :: Day, would choke on "2016-1-2", but the solution-}
{--- below parses it as "2016-01-02".-}
{-dateFromString :: String -> Day-}
{-dateFromString = parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d %H:%M:%S"-}

{--- | Fetch a URL from saved.io.-}
savedIO :: String -> IO B.ByteString
savedIO = simpleHttp . (++) savedIOURL

appendMaybe :: String -> Maybe String -> String
appendMaybe s (Just s2) = s ++ s2
appendMaybe s Nothing = s

{-decodeSavedIO :: FromJSON a => IO B.ByteString -> IO (Either String [a])-}
{-decodeSavedIO stream = do-}
  {-result <- (eitherDecode stream) :: IO (Either String [Bookmark])-}
  {-case result of-}
    {-Right success   -> return $ Right success-}
    {-Left failure    -> do-}
      {-errorResult <- (eitherDecode stream) :: IO (Either String SavedIOError)-}
      {-case errorResult of-}
        {-Right e   -> return $ Left $ "Saved.io error: " ++ e-}
        {-Left f    -> return $ Left $ "Unknown error: " ++ f-}


--decodeSavedIO :: FromJSON a => B.ByteString -> Either String [a]
{-decodeSavedIO stream = eitherDecode stream >>= \result ->-}
  {-case result of-}
    {-Left decodeFail   -> return $ Left $ "Decoding failed: " ++ decodeFail-}
    {-Right decoded     -> (AT.parseEither parseJSON :: IO (Either String [Bookmark])) >>= \parsed ->-}
      {-case parsed of-}
        {-Right success   -> return $ Right success-}
        {-Left parseFail  -> (AT.parseEither parseJSON :: IO (Either String SavedIOError)) >>= \ioerror ->-}
          {-case ioerror of-}
            {-Left die      -> return $ Left $ "Cannot make heads or tails of output: " ++ die-}
            {-Right rerror  -> return $ Left $ "Saved.io error: " ++ rerror-}
{-decodeSavedIO stream = do-}
  {-result <- eitherDecode stream-}
  {-case result of-}
    {-Left decodeFail   -> Left $ "Decoding failed: " ++ decodeFail-}
    {-Right decoded     -> do-}
      {-parsed <- (AT.parseEither parseJSON decoded) :: Either String [Bookmark]-}
      {-case parsed of-}
        {-Right success   -> Right success-}
        {-Left _ -> do-}
          {-ioerror <- (AT.parseEither parseJSON decoded) :: Either String SavedIOError-}
          {-case ioerror of-}
            {-Left die      -> Left $ "Cannot make heads or tails of output: " ++ die-}
            {-Right rerror  -> Left $ "Saved.io error: " ++ (show rerror)-}
-- | Redecode the stream as an SavedIOError to see if there was an API error.
-- This will either print the API error, if it can be obtained, or
-- the generic aeson parse error.
handleDecodeError :: IO B.ByteString -> String -> IO ()
handleDecodeError stream errors = do
  d <- (eitherDecode <$> stream) :: IO (Either String SavedIOError)
  case d of
    Left  unknown   -> putStrLn $ "Unknown errors occured: " ++ errors ++ " " ++ unknown
    Right helpful   -> putStrLn . unpack $ ppSavedIOError helpful

run :: CLOpts.Options -> IO ()
run (CLOpts.Options token cmd) = do
  let stream = savedIO query
  d <- (eitherDecode <$> stream) :: IO (Either String [Bookmark])
  case d of
    Left err    -> handleDecodeError stream err
    Right marks -> putStrLn . unpack . Data.Text.concat $ (`ppBookmark` False) <$> marks
    where
      query    = queryStr ++ tokenStr
      tokenStr = "&token=" ++ token
      queryStr = case cmd of
        Listing group    -> retrieveBookmarks token group Nothing Nothing Nothing
          --"bookmarks/" `appendMaybe` group
        Search query  -> undefined

main :: IO ()
main = run =<< execParser (parseOptions `withInfo` "Command Line Interface to saved.io")
