{-

Copyright (C) 2013 Iustin Pop

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Types ( Config(..)
             , Regex
             , reRegex
             , reString
             , FolderClass(..)
             , ImageStatus(..)
             , JSDiffTime(..)
             ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Time.Clock
import qualified Data.Text as T
import qualified Text.Regex.PCRE as PCRE
import Yesod
import Prelude

data Regex = Regex
    { reString :: String
    , reRegex  :: PCRE.Regex
    }

instance Show Regex where
  show = show . reString

instance FromJSON Regex where
  parseJSON (String txt) =
    let str = T.unpack txt
    in case PCRE.makeRegexM str of
         Nothing -> mzero
         Just r -> return $ Regex str r
  parseJSON _ = mzero

-- | Wrapper over NominalDiffTime so that we can add our FromJSON
-- instance without orphan instances warning (sigh).
newtype JSDiffTime = JSDiffTime NominalDiffTime
  deriving (Show)

instance FromJSON JSDiffTime where
  parseJSON (Number num) =
    return . JSDiffTime . fromRational . toRational $ num
  parseJSON _ = mzero

data Config = Config
    { cfgSourceDirs      :: [FilePath]
    , cfgOutputDirs      :: [FilePath]
    , cfgBlacklistedDirs :: [FilePath]
    , cfgRawExts         :: [FilePath]
    , cfgJpegExts        :: [FilePath]
    , cfgSidecarExts     :: [FilePath]
    , cfgOtherImgExts    :: [FilePath]
    , cfgDirRegex        :: Regex
    , cfgRangeRegex      :: Regex
    , cfgCopyRegex       :: Regex
    , cfgOutdatedError   :: JSDiffTime
    } deriving (Show)

instance FromJSON Config where
  parseJSON (Object v) =
    Config <$>
         v .: "sourcedirs"      <*>
         v .: "outputdirs"      <*>
         v .: "blacklisteddirs" <*>
         v .: "rawexts"         <*>
         v .: "jpegexts"        <*>
         v .: "sidecarexts"     <*>
         v .: "otherexts"       <*>
         v .: "dirregex"        <*>
         v .: "rangeregex"      <*>
         v .: "copyregex"       <*>
         v .: "outdatederror"

  parseJSON _ = mzero

data ImageStatus = ImageOrphaned
                 | ImageStandalone
                 | ImageRaw
                 | ImageOutdated
                 | ImageProcessed
                   deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | Custom yesod instance for ImageStatus.
instance PathPiece ImageStatus where
  toPathPiece ImageOrphaned   = "orphaned"
  toPathPiece ImageStandalone = "standalone"
  toPathPiece ImageRaw        = "raw"
  toPathPiece ImageOutdated   = "outdated"
  toPathPiece ImageProcessed  = "processed"
  fromPathPiece "orphaned"    = Just ImageOrphaned
  fromPathPiece "raw"         = Just ImageRaw
  fromPathPiece "standalone"  = Just ImageStandalone
  fromPathPiece "processed"   = Just ImageProcessed
  fromPathPiece "outdated"    = Just ImageOutdated
  fromPathPiece _             = Nothing

-- | Custom Path piece instance for [ImageStatus].
instance PathPiece [ImageStatus] where
  toPathPiece = T.intercalate "," . map toPathPiece
  fromPathPiece "all" = Just [minBound..maxBound]
  fromPathPiece v     = mapM fromPathPiece $ T.split (==',') v

data FolderClass = FolderEmpty
                 | FolderRaw
                 | FolderStandalone
                 | FolderUnprocessed
                 | FolderProcessed
                 | FolderOutdated
                 | FolderMixed
                   deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | Custom yesod instance for FolderClass. This really could use some TH.
instance PathPiece FolderClass where
  toPathPiece FolderEmpty       = "empty"
  toPathPiece FolderRaw         = "raw"
  toPathPiece FolderStandalone  = "standalone"
  toPathPiece FolderUnprocessed = "unprocessed"
  toPathPiece FolderProcessed   = "processed"
  toPathPiece FolderOutdated    = "outdated"
  toPathPiece FolderMixed       = "mixed"
  fromPathPiece "empty"       = Just FolderEmpty
  fromPathPiece "raw"         = Just FolderRaw
  fromPathPiece "standalone"  = Just FolderStandalone
  fromPathPiece "unprocessed" = Just FolderUnprocessed
  fromPathPiece "processed"   = Just FolderProcessed
  fromPathPiece "outdated"    = Just FolderOutdated
  fromPathPiece "mixed"       = Just FolderMixed
  fromPathPiece _             = Nothing

-- | Custom Path piece instance for [FolderClass].
instance PathPiece [FolderClass] where
  toPathPiece = T.intercalate "," . map toPathPiece
  fromPathPiece "all" = Just [minBound..maxBound]
  fromPathPiece v     = mapM fromPathPiece $ T.split (==',') v
