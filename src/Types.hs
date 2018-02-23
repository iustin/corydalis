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
             , UrlParams
             ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.Aeson
import           Data.List           (nub, sort)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Time.Clock
import           Prelude
import qualified Text.Regex.TDFA     as TDFA
import           Yesod

data Regex = Regex
    { reString :: String
    , reRegex  :: TDFA.Regex
    }

instance Show Regex where
  show = show . reString

instance FromJSON Regex where
  parseJSON (String txt) =
    let str = Text.unpack txt
    in case TDFA.makeRegexM str of
         Nothing -> mzero
         Just r  -> return $ Regex str r
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
    , cfgCacheDir        :: FilePath
    , cfgThumbnailSize   :: Int
    , cfgAutoImageSizes  :: [Int]
    , cfgAllImageSizes   :: [Int]
    , cfgRawExts         :: [FilePath]
    , cfgJpegExts        :: [FilePath]
    , cfgSidecarExts     :: [FilePath]
    , cfgOtherImgExts    :: [FilePath]
    , cfgDirRegex        :: Regex
    , cfgRangeRegex      :: Regex
    , cfgCopyRegex       :: Regex
    , cfgPeoplePrefix    :: Text
    , cfgIgnorePrefix    :: Text
    } deriving (Show)

instance FromJSON Config where
  parseJSON (Object v) =
    Config <$>
         v .: "sourcedirs"      <*>
         v .: "outputdirs"      <*>
         v .: "blacklisteddirs" <*>
         v .: "cachedir"        <*>
         thumbsize              <*>
         autosizes              <*>
         allsizes'              <*>
         v .: "rawexts"         <*>
         v .: "jpegexts"        <*>
         v .: "sidecarexts"     <*>
         v .: "otherexts"       <*>
         v .: "dirregex"        <*>
         v .: "rangeregex"      <*>
         v .: "copyregex"       <*>
         v .: "peopleprefix"    <*>
         v .: "ignoreprefix"
    where autosizes = sort . nub <$> ((:) <$> thumbsize <*> v .: "autoimgsizes")
          thumbsize = v .: "thumbnailsize"
          demandsizes = v .: "demandimgsizes"
          allsizes = (++) <$> autosizes <*> demandsizes
          allsizes' = sort . nub <$> allsizes

  parseJSON _ = mzero

data ImageStatus = ImageOrphaned
                 | ImageStandalone
                 | ImageRaw
                 | ImageProcessed
                   deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | Custom yesod instance for ImageStatus.
instance PathPiece ImageStatus where
  toPathPiece ImageOrphaned   = "orphaned"
  toPathPiece ImageStandalone = "standalone"
  toPathPiece ImageRaw        = "raw"
  toPathPiece ImageProcessed  = "processed"
  fromPathPiece "orphaned"   = Just ImageOrphaned
  fromPathPiece "raw"        = Just ImageRaw
  fromPathPiece "standalone" = Just ImageStandalone
  fromPathPiece "processed"  = Just ImageProcessed
  fromPathPiece _            = Nothing

-- | Custom Path piece instance for [ImageStatus].
instance PathPiece [ImageStatus] where
  toPathPiece = Text.intercalate "," . map toPathPiece
  fromPathPiece "all" = Just [minBound..maxBound]
  fromPathPiece v     = mapM fromPathPiece $ Text.split (==',') v

data FolderClass = FolderEmpty
                 | FolderRaw
                 | FolderStandalone
                 | FolderUnprocessed
                 | FolderProcessed
                 | FolderMixed
                   deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance NFData FolderClass where
  rnf _ = ()

-- | Custom yesod instance for FolderClass. This really could use some TH.
instance PathPiece FolderClass where
  toPathPiece FolderEmpty       = "empty"
  toPathPiece FolderRaw         = "raw"
  toPathPiece FolderStandalone  = "standalone"
  toPathPiece FolderUnprocessed = "unprocessed"
  toPathPiece FolderProcessed   = "processed"
  toPathPiece FolderMixed       = "mixed"
  fromPathPiece "empty"       = Just FolderEmpty
  fromPathPiece "raw"         = Just FolderRaw
  fromPathPiece "standalone"  = Just FolderStandalone
  fromPathPiece "unprocessed" = Just FolderUnprocessed
  fromPathPiece "processed"   = Just FolderProcessed
  fromPathPiece "mixed"       = Just FolderMixed
  fromPathPiece _             = Nothing

-- | Custom Path piece instance for [FolderClass].
instance PathPiece [FolderClass] where
  toPathPiece = Text.intercalate "," . map toPathPiece
  fromPathPiece "all" = Just [minBound..maxBound]
  fromPathPiece v     = mapM fromPathPiece $ Text.split (==',') v

-- | Type alias for query (URL) params.
type UrlParams = [(Text, Text)]
