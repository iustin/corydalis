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
             , showFolderClass
             , parseFolderClass
             , ImageStatus(..)
             , showImageStatus
             , parseImageStatus
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
             , Context
             , ctxConfig
             , ctxRepo
             , ctxScanner
             , ctxScanProgress
             , ctxRenderProgress
             , ctxSearchCache
             , ctxLogger
             , newContext
             ) where

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.DeepSeq
import           Control.Monad
import           Data.Aeson
import           Data.Default             (Default, def)
import           Data.Function            (on)
import           Data.List                (nub, sort)
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Store
import           Data.Store.TH            (makeStore)
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.Lazy           as TextL
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Prelude
import           System.FilePath          (pathSeparator)
import           System.Log.FastLogger    (LogStr)
import qualified Text.Regex.TDFA          as TDFA
import           Yesod

import           Compat.Orphans           ()

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
                 | ImageUnprocessed
                 | ImageProcessed
                   deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance NFData ImageStatus where
  rnf = rwhnf

$(makeStore ''ImageStatus)

showImageStatus :: ImageStatus -> Text
showImageStatus ImageOrphaned    = "orphaned"
showImageStatus ImageStandalone  = "standalone"
showImageStatus ImageUnprocessed = "unprocessed"
showImageStatus ImageProcessed   = "processed"

parseImageStatus :: Text -> Maybe ImageStatus
parseImageStatus "orphaned"    = Just ImageOrphaned
parseImageStatus "unprocessed" = Just ImageUnprocessed
parseImageStatus "standalone"  = Just ImageStandalone
parseImageStatus "processed"   = Just ImageProcessed
parseImageStatus _             = Nothing

-- | Custom yesod instance for ImageStatus.
instance PathPiece ImageStatus where
  toPathPiece = showImageStatus
  fromPathPiece = parseImageStatus

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

showFolderClass :: FolderClass -> Text
showFolderClass FolderEmpty       = "empty"
showFolderClass FolderRaw         = "raw"
showFolderClass FolderStandalone  = "standalone"
showFolderClass FolderUnprocessed = "unprocessed"
showFolderClass FolderProcessed   = "processed"
showFolderClass FolderMixed       = "mixed"

parseFolderClass :: Text -> Maybe FolderClass
parseFolderClass "empty"       = Just FolderEmpty
parseFolderClass "raw"         = Just FolderRaw
parseFolderClass "standalone"  = Just FolderStandalone
parseFolderClass "unprocessed" = Just FolderUnprocessed
parseFolderClass "processed"   = Just FolderProcessed
parseFolderClass "mixed"       = Just FolderMixed
parseFolderClass _             = Nothing

-- | Custom yesod instance for FolderClass. This really could use some TH.
instance PathPiece FolderClass where
  toPathPiece = showFolderClass
  fromPathPiece = parseFolderClass

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
  deriving (Show)

instance Default Progress where
  def = Progress 0 0 0

instance NFData Progress where
  rnf (Progress a b c) = rnf a `seq` rnf b `seq` rnf c

$(makeStore ''Progress)

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
  { wrStart :: !ZonedTime
  , wrEnd   :: !ZonedTime
  , wrGoal  :: !Int
  , wrDone  :: !Progress
  }
  deriving (Show)

instance NFData WorkResults where
  rnf (WorkResults a b c d) =
    rnf a `seq` rnf b `seq` rnf c `seq` rnf d


-- | Data representing the entirety of the application state.
--
-- Sadly this is parametrized only to work around the big monolitic
-- Pics module, not because it needs to be parametrized.
data Context a b = Context
  { ctxConfig         :: !Config
  , ctxRepo           :: !(TVar a)
  , ctxScanner        :: !(TVar (Maybe (Async a)))
  , ctxScanProgress   :: !(TVar Progress)
  , ctxRenderProgress :: !(TVar Progress)
  -- TODO: this really shouldn't be parametrised…
  , ctxSearchCache    :: !(TVar b)
  , ctxLogger         :: !LogFn
  }

newContext :: Config -> LogFn -> a -> b -> STM (Context a b)
newContext config logfn a b = do
  ta <- newTVar a
  tb <- newTVar b
  scanner <- newTVar Nothing
  scanProg <- newTVar def
  renderProg <- newTVar def
  return $ Context { ctxConfig         = config
                   , ctxRepo           = ta
                   , ctxScanner        = scanner
                   , ctxScanProgress   = scanProg
                   , ctxRenderProgress = renderProg
                   , ctxSearchCache    = tb
                   , ctxLogger         = logfn
                   }

$(makeStore ''WorkStart)
$(makeStore ''WorkResults)
