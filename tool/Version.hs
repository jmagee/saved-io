-- | Version information, including git info.

{-# LANGUAGE TemplateHaskell #-}
module Version ( version ) where

-- Get our version number from the cabal Paths module.
-- To change the version number, update the cabal file.
import qualified Paths_SavedIO      as V (version)

import           Data.Version       (showVersion)
import           Development.GitRev (gitBranch, gitDirtyTracked, gitHash)

-- | Return the git revision information in the form of:
--     branch @ #[short-hash] [dirty-flag]
revisionInfo :: String
revisionInfo = $(gitBranch) ++ " @ #" ++ take 8 $(gitHash) ++ dirty
  where
    dirty | $(gitDirtyTracked) = " [dirty]"
          | otherwise          = ""

-- | Return the package version info from the cabal file.
versionInfo :: String
versionInfo = showVersion V.version

--  | Return a formatted string containing the cabal version and git info.
--  The format is: vD.D.D.D (branch @ #[shot-hash] [dirty-flag])
version :: String
version = "v" ++ versionInfo ++ " (" ++ revisionInfo ++ ")"
