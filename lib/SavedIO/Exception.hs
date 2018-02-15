-- | Exception related types and functions for SavedIO.

module SavedIO.Exception
( SavedIOException (..)
, rethrowHttpExceptionAsSavedIO
, httpExceptionToSavedIO
, catchSavedIOException
) where

import           SavedIO.Display

import           Control.Exception         (Exception, catch, throwIO, tryJust)
import           Network.HTTP.Conduit      (HttpException (..),
                                            HttpExceptionContent (..),
                                            responseStatus)
import           Network.HTTP.Types.Status (status403, status404)

-- | A concrete type for errors returned by the saved.io API.
data SavedIOException
  = DoesNotExistError String -- ^ The bookmark does not (appear to) exist.
  | NotDeletedError String   -- ^ The bookmark was not deleted.
  | DecodeError String       -- ^ Could not decode the response from the server.
  | BadToken String          -- ^ Bad token (user key and developer key).
  | BadURL String            -- ^ The saved.io URl is down or incorrect.
  deriving (Eq, Show)

instance Exception SavedIOException

instance Display SavedIOException where
  display (DoesNotExistError s) = "Bookmark does exist: " ++ s
  display (NotDeletedError s)   = "Bookmark was not deleted: " ++ s
  display (DecodeError s)       = "Could not decode remote response: " ++ s
  display (BadToken s)          = "Bad user key or developer.  HTTP request was: \n" ++ s
  display (BadURL s)            = "Bad or unreachable URL.  HTTP request was: \n" ++ s

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
    responseTo r | r == status403 = Just $ BadToken (show req)
                 | r == status404 = Just $ BadURL (show req)
                 | otherwise      = Nothing
httpExceptionToSavedIO (HttpExceptionRequest req (ConnectionFailure _)) =
  Just $ BadURL (show req)
httpExceptionToSavedIO _ = Nothing

-- | Catch only 'SavedIOException's.
-- Equivalent to 'Control.Exception.Catch', except the handler can only handle
-- 'SavedIOException'.
catchSavedIOException :: IO a                   -- ^ The computation to run
                      -> (SavedIOException -> IO a) -- ^ The handler
                      -> IO a
catchSavedIOException = catch
