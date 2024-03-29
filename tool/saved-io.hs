-- | Command line client for saved-io
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           CLOpts                     as CL
import           SavedIO
import           SavedIO.Util
import           Version

import           Data.Foldable              (toList)
import           Data.Sequence              (Seq)
import qualified Data.Sequence              as S (sortBy, reverse)
import           Data.Aeson                 (eitherDecode', toJSON)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as B (putStrLn, readFile)
import           Data.Function              (on)
import           Data.Text                  (Text)
import qualified Data.Text                  as T (intercalate)
import qualified Data.Text.IO               as T (putStrLn)
import           System.Directory           (doesFileExist, getHomeDirectory)
import           System.Exit                (die)
import           System.FilePath.Posix      (pathSeparator)
import           System.IO                  (hPutStrLn, hSetEncoding, stderr,
                                             stdout, utf8)

-- | RC File path
rcFile :: IO FilePath
rcFile = fmap (++ pathSeparator : ".saved-io.rc") getHomeDirectory

main :: IO ()
main = hSetEncoding stdout utf8 -- Hack for Windows to avoid "commitBuffer: invalid argument"
     >> getRCDefaults
     >>= execParser . (`withInfo` infoStr) . parseOptions
     >>= \options -> catchSavedIOException (run options) (T.putStrLn . display)
  where
    infoStr = "Command Line Interface to saved.io " ++ version

-- | Read default settings for Common options from RC file.
-- If there is no RC file or it cannot be decoded then this returns
-- Common of all Default.
getRCDefaults :: IO Common
getRCDefaults = do
  rc <- rcFile
  exists <- doesFileExist rc
  exists ? decode rc $ pure comDef
    where
      decode :: FilePath -> IO Common
      decode prefs = do
        d <- (eitherDecode' <$> B.readFile prefs) :: IO (Either String Common)
        warn d
        pure $ defaults d
      defaults (Left _)  = comDef
      defaults (Right x) = x
      comDef = Common Default Default Default Default
                      Default Default Default

run :: CL.Options -> IO ()
run (CL.Options c@(Common dev user format color limit sort sortMethod) cmd) =
  let token = checkToken (mkToken <$> dev <*> user)
  in case cmd of
    Listing group             -> token
      >>= \t -> retrieveBookmarks t group limit
      >>= \x -> printTextSeq $ ppMarkDef <$> sortIf sort sortMethod x

    Search query searchFormat -> token
      >>= \t -> retrieveBookmarks t Default limit
      >>= \x -> printTextSeq $ ppMarkDef <$>
                sortIf sort
                       sortMethod
                       (searchBookmarks (extractSearchKey searchFormat
                                                          query)
                                        x)

    AddMark title url group   -> token
      >>= \t -> createBookmark t title url group
      >>= \x -> putStrLn "Success!  Created: " >> (T.putStrLn . ppMarkFull) x

    DelMark bkid              -> token
      >>= \t -> deleteBookmark t bkid

    GetMark bkid              -> token
      >>= \t -> getBookmark t bkid >>= T.putStrLn . ppMarkDef

    MakeRC                    -> do
      rc <- rcFile
      hPutStrLn stderr $ "#Redirect the contents below to " ++ rc
      B.putStrLn $ encodePretty $ toJSON c

    where
      formatText = perhaps defBookKeys id
      useColor = perhaps Nothing (\x -> x ? Just defBookColors $ Nothing) color
      ppMarkDef = ppBookmark $ BookmarkConfig (formatText format) useColor
      ppMarkFull = ppBookmark $ BookmarkConfig "id,title,url,note,creation" useColor
      sortIf (Specific True) m x = sortMarks m x
      sortIf _ _ x               = x
      checkToken Default         = die $ "Missing keys; -d|--devkey" ++
                                       " and -u|--userkey options required."
      checkToken (Specific x)    = pure x

-- | Print a sequence of Text.
printTextSeq :: Seq Text -> IO ()
printTextSeq  = T.putStrLn . T.intercalate "\n" . toList

-- | Print a warning on failure.
warn :: Either String a -> IO ()
warn (Left e)  = putStrLn $ "Warning: " ++ e
warn _         = pure ()

-- | Sort bookmarks.
-- If no SortMethod is provided then default to an ascending sort by
-- title.
sortMarks :: Optional SortMethod -> Seq Bookmark -> Seq Bookmark
sortMarks Default           = S.sortBy (compare `on` _title)
sortMarks (Specific method) = case method of
  SortByTitle d -> case d of
    Ascending   -> sortMarks Default
    Descending  -> S.reverse . sortMarks Default
