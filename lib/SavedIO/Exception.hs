-- | Exception related types and functions for SavedIO.
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module SavedIO.Exception
( SavedIOException (..)
, rethrowHttpExceptionAsSavedIO
, httpExceptionToSavedIO
, catchSavedIOException
) where

import           SavedIO.Display

import           Control.Exception         (Exception, catch, throwIO, tryJust)
import           Data.String.Conversions   (ConvertibleStrings, cs)
import           Data.Text                 (Text, append)
import           Network.HTTP.Conduit      (HttpException (..),
                                            HttpExceptionContent (..),
                                            responseStatus)
import           Network.HTTP.Types.Status (status403, status404)

-- | A concrete type for errors returned by the saved.io API.
data SavedIOException
  = DoesNotExistError Text   -- ^ The bookmark does not (appear to) exist.
  | NotDeletedError Text     -- ^ The bookmark was not deleted.
  | DecodeError Text         -- ^ Could not decode the response from the server.
  | BadToken Text            -- ^ Bad token (user key and developer key).
  | BadURL Text              -- ^ The saved.io URl is down or incorrect.
  deriving (Eq, Show)

instance Exception SavedIOException

instance Display SavedIOException where
  display (DoesNotExistError s) = "Bookmark does exist: " `append` s
  display (NotDeletedError s)   = "Bookmark was not deleted: " `append` s
  display (DecodeError s)       = "Could not decode remote response: " `append` s
  display (BadToken s)          = "Bad user key or developer.  HTTP request was: \n" `append` s
  display (BadURL s)            = "Bad or unreachable URL.  HTTP request was: \n" `append` s

-- | Catch specific 'HttpException's and re-throw them as 'SavedIOException's.
rethrowHttpExceptionAsSavedIO :: IO a -> IO a
rethrowHttpExceptionAsSavedIO act = do
  result <- tryJust justHttpException act
  case result of
    Left e  -> 
      case httpExceptionToSavedIO e of
        Just x  -> throwIO x
        Nothing -> throwIO e
    Right x -> pure x
  where
    justHttpException x@(HttpExceptionRequest _ _) = Just x
    justHttpException _ = Nothing

-- | Convert specific 'HttpException's to 'SavedIOException's.
httpExceptionToSavedIO :: HttpException -> Maybe SavedIOException
httpExceptionToSavedIO (HttpExceptionRequest req (StatusCodeException response _)) =
  responseTo $ responseStatus response
  where
    responseTo r | r == status403 = Just $ BadToken (cshow req)
                 | r == status404 = Just $ BadURL (cshow req)
                 | otherwise      = Nothing
httpExceptionToSavedIO (HttpExceptionRequest req (ConnectionFailure _)) =
  Just $ BadURL (cshow req)
httpExceptionToSavedIO _ = Nothing

-- | Show a type and convert the string.
cshow :: (Show a, ConvertibleStrings String b) => a -> b
cshow = cs . show

-- | Catch only 'SavedIOException's.
-- Equivalent to 'Control.Exception.Catch', except the handler can only handle
-- 'SavedIOException'.
catchSavedIOException :: IO a                   -- ^ The computation to run
                      -> (SavedIOException -> IO a) -- ^ The handler
                      -> IO a
catchSavedIOException = catch
