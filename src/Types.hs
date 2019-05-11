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
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types ( Config(..)
             , Regex
             , reRegex
             , reString
             , FolderClass(..)
             , ImageStatus(..)
             , JSDiffTime(..)
             , UrlParams
             , pathSep
             , LazyText
             , LogFn
             , Progress(..)
             , pgTotal
             , incErrors
             , incNoop
             , incDone
             , incProgress
             , WorkStart(..)
             , WorkResults(..)
             ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.Aeson
import           Data.Default          (Default, def)
import           Data.Function         (on)
import           Data.List             (nub, sort)
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Data.Store
import           Data.Store.TH         (makeStore)
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.Lazy        as TextL
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Prelude
import           System.FilePath       (pathSeparator)
import           System.Log.FastLogger (LogStr)
import qualified Text.Regex.TDFA       as TDFA
import           Yesod

import           Compat.Orphans        ()

data Regex = Regex
    { reString :: Text
    , reRegex  :: TDFA.Regex
    }

instance Show Regex where
  show = show . reString

instance Eq Regex where
  (==) = (==) `on` reString

instance FromJSON Regex where
  parseJSON (String txt) =
    let str = Text.unpack txt
    in case TDFA.makeRegexM str of
         Nothing -> mzero
         Just r  -> return $ Regex txt r
  parseJSON _ = mzero

instance Store Regex where
  size = case (size::Size Text) of
           ConstSize n -> ConstSize n -- Unlikely :)
           VarSize f   -> VarSize (f . reString)
  poke = poke . reString
  peek = do
    s <- peek
    case TDFA.makeRegexM (Text.unpack s) of
      Nothing -> fail "Can't build regex even though it was serialized"
      Just r  -> return $ Regex s r

-- | Wrapper over NominalDiffTime so that we can add our FromJSON
-- instance without orphan instances warning (sigh).
newtype JSDiffTime = JSDiffTime NominalDiffTime
  deriving (Show)

instance FromJSON JSDiffTime where
  parseJSON (Number num) =
    return . JSDiffTime . fromRational . toRational $ num
  parseJSON _ = mzero

-- | Helper for text version of 'pathSeparator'.
pathSep :: Text
pathSep = Text.singleton pathSeparator

-- | Type alias for readabiliy.
type LazyText = TextL.Text

data Config = Config
    { cfgSourceDirs      :: [FilePath]
    , cfgOutputDirs      :: [FilePath]
    , cfgBlacklistedDirs :: [FilePath]
    , cfgCacheDir        :: FilePath
    , cfgThumbnailSize   :: Int
    , cfgBrowsingSize    :: Int
    , cfgAutoImageSizes  :: [Int]
    , cfgAllImageSizes   :: [Int]
    , cfgPageSize        :: Int
    , cfgRawExts         :: [FilePath]
    , cfgRawExtsSet      :: Set Text
    , cfgJpegExts        :: Set Text
    , cfgSidecarExts     :: Set Text
    , cfgOtherImgExts    :: [FilePath]
    , cfgMovieExts       :: Set Text
    , cfgDirRegex        :: Regex
    , cfgRangeRegex      :: Regex
    , cfgCopyRegex       :: Regex
    , cfgPeoplePrefix    :: Text
    , cfgIgnorePrefix    :: Text
    } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON (Object v) =
    Config <$>
         v .: "sourcedirs"      <*>
         v .: "outputdirs"      <*>
         v .: "blacklisteddirs" <*>
         v .: "cachedir"        <*>
         thumbsize              <*>
         browsingsize           <*>
         autosizes              <*>
         allsizes'              <*>
         v .: "pagesize"        <*>
         rawexts                <*>
         rawextsset             <*>
         v .: "jpegexts"        <*>
         v .: "sidecarexts"     <*>
         v .: "otherexts"       <*>
         v .: "movieexts"       <*>
         v .: "dirregex"        <*>
         v .: "rangeregex"      <*>
         v .: "copyregex"       <*>
         v .: "peopleprefix"    <*>
         v .: "ignoreprefix"
    where autosizes = sort . nub <$> ((:) <$> thumbsize <*> ((:) <$> browsingsize <*> v .: "autoimgsizes"))
          thumbsize = v .: "thumbnailsize"
          browsingsize = v .: "browsingsize"
          demandsizes = v .: "demandimgsizes"
          allsizes = (++) <$> autosizes <*> demandsizes
          allsizes' = sort . nub <$> allsizes
          rawexts = v .: "rawexts"
          rawextsset = Set.fromList . map Text.pack <$> rawexts

  parseJSON _ = mzero

data ImageStatus = ImageOrphaned
                 | ImageStandalone
                 | ImageRaw
                 | ImageProcessed
                   deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance NFData ImageStatus where
  rnf = rwhnf

$(makeStore ''ImageStatus)

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
  rnf = rwhnf

$(makeStore ''FolderClass)

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

$(makeStore ''Config)

type LogFn = LogStr -> IO ()

-- | Progress of a work item.
data Progress = Progress
  { pgErrors :: !Int  -- ^ Work that was attempted but failed.
  , pgNoop   :: !Int  -- ^ Work that was skipped.
  , pgDone   :: !Int  -- ^ Work that was attempted and succeeded.
  }

instance Default Progress where
  def = Progress 0 0 0

pgTotal :: Progress -> Int
pgTotal p = pgErrors p + pgNoop p + pgDone p

incErrors :: Progress -> Progress
incErrors p@Progress { pgErrors = old } =
  p { pgErrors = old + 1 }

incNoop :: Progress -> Progress
incNoop p@Progress { pgNoop = old } =
  p { pgNoop = old + 1 }

incDone :: Progress -> Progress
incDone p@Progress { pgDone = old } =
  p { pgDone = old + 1 }

incProgress :: Int -> Int -> Int -> Progress -> Progress
incProgress e n d Progress{..} =
  Progress { pgErrors = pgErrors + e
           , pgNoop = pgNoop + n
           , pgDone = pgDone + d
           }

data WorkStart = WorkStart
  { wsStart :: !ZonedTime
  , wsGoal  :: !Int
  }
  deriving (Show)

instance NFData WorkStart where
  rnf (WorkStart s g) = rnf s `seq` rnf g

data WorkResults = WorkResults
  { wrStart  :: !ZonedTime
  , wrEnd    :: !ZonedTime
  , wrGoal   :: !Int
  , wrDone   :: !Int
  , wrErrors :: !Int
  }
  deriving (Show)

instance NFData WorkResults where
  rnf (WorkResults a b c d e) =
    rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e

$(makeStore ''WorkStart)
$(makeStore ''WorkResults)
