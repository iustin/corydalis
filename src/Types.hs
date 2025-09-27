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

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module Types ( Config(..)
             , Regex
             , reRegex
             , reString
             , mkRegex
             , FolderClass(..)
             , showFolderClass
             , parseFolderClass
             , ImageStatus(..)
             , showImageStatus
             , parseImageStatus
             , ImageName(..)
             , JSDiffTime(..)
             , UrlParams
             , pathSep
             , LazyText
             , LogFn
             , ProgressError(..)
             , Progress(..)
             , pgTotal
             , pgNumErrors
             , pgProgress
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
             , ctxCleanProgress
             , ctxSearchCache
             , ctxLogger
             , newContext
             , ViewPresentation(..)
             , ViewMode(..)
             , formatViewMode
             , parseViewMode
             ) where

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.DeepSeq
import           Control.Monad
import           Data.Aeson
import           Data.Default             (Default, def)
import qualified Data.Set                 as Set
import           Data.Store
import           Data.Store.TH            (makeStore)
import qualified Data.Text                as Text
import qualified Data.Text.Lazy           as TextL
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Database.Persist.Sql     (PersistFieldSql)
import           System.FilePath          (pathSeparator)
import           System.Log.FastLogger    (LogStr)
import           Text.Blaze               (ToMarkup)
import qualified Text.Regex.TDFA          as TDFA
import           Yesod

-- Note: Can't import Import, cycle. So directly import ClassyPrelude.
import           ClassyPrelude
import           Compat.Orphans           ()

data Regex = Regex
    { reString :: Text
    , reRegex  :: TDFA.Regex
    }

mkRegex :: MonadFail m => Text -> m Regex
mkRegex txt =
  case TDFA.makeRegexM (Text.unpack txt) of
    Nothing -> fail $ "Invalid regex pattern: " <> Text.unpack txt
    Just r  -> return $ Regex txt r

instance Show Regex where
  show = show . reString

instance Eq Regex where
  (==) = (==) `on` reString

instance FromJSON Regex where
  parseJSON = withText "Regex" mkRegex

instance Store Regex where
  size = case (size::Size Text) of
           ConstSize n -> ConstSize n -- Unlikely :)
           VarSize f   -> VarSize (f . reString)
  poke = poke . reString
  peek = peek >>= mkRegex

-- | Wrapper over NominalDiffTime so that we can add our FromJSON
-- instance without orphan instances warning (sigh).
newtype JSDiffTime = JSDiffTime NominalDiffTime
  deriving (Show)

instance FromJSON JSDiffTime where
  parseJSON = withScientific "JSDiffTime" $
    return . JSDiffTime . fromRational . toRational

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
    , cfgAutoImageSizes  :: Set Int
    , cfgOnDemandSizes   :: Set Int
    , cfgAllImageSizes   :: Set Int
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
    , cfgViewableImages  :: [Text]        -- ^ Images directly viewable in browser.
    } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v ->
    let autosizes = Set.fromList <$> ((:) <$> thumbsize <*> ((:) <$> browsingsize <*> v .: "autoimgsizes"))
        thumbsize = v .: "thumbnailsize"
        browsingsize = v .: "browsingsize"
        demandsizes = Set.fromList <$> v .: "demandimgsizes"
        demandsizes' = Set.difference <$> demandsizes <*> autosizes
        allsizes = Set.union <$> autosizes <*> demandsizes
        rawexts = v .: "rawexts"
        rawextsset = Set.fromList . map Text.pack <$> rawexts
        viewableimages = v .: "viewableimages"
        viewableimageslist = map (Text.pack . ('.':)) <$> viewableimages
    in
    Config <$>
         v .: "sourcedirs"      <*>
         v .: "outputdirs"      <*>
         v .: "blacklisteddirs" <*>
         v .: "cachedir"        <*>
         thumbsize              <*>
         browsingsize           <*>
         autosizes              <*>
         demandsizes'           <*>
         allsizes               <*>
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
         v .: "ignoreprefix"    <*>
         viewableimageslist

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

-- | Newtype wrapper for image names.
newtype ImageName = ImageName { unImageName :: Text }
  deriving (Show, Read, Eq, Ord, IsString, NFData,
            ToMarkup, ToJSON, PersistField, PersistFieldSql)

instance PathMultiPiece ImageName where
  fromPathMultiPiece = Just . ImageName . Text.intercalate "/"
  toPathMultiPiece = Text.splitOn "/" . unImageName

$(makeStore ''ImageName)

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

type LogFn = LogLevel -> LogStr -> IO ()

-- | Represents an error during processing.
data ProgressError = ProgressError
  { peItem  :: !Text
  , peError :: !Text
  } deriving (Show)

instance NFData ProgressError where
  rnf (ProgressError a b) = rnf a `seq` rnf b

$(makeStore ''ProgressError)

-- | Progress of a work item.
data Progress = Progress
  { pgErrors :: ![ProgressError] -- ^ Work that was attempted but failed.
  , pgNoop   :: !Int             -- ^ Work that was skipped.
  , pgDone   :: !Int             -- ^ Work that was attempted and succeeded.
  , pgGoal   :: !Int             -- ^ Target work goal.
  }
  deriving (Show)

instance Default Progress where
  def = Progress [] 0 0 0

instance NFData Progress where
  rnf (Progress a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d

$(makeStore ''Progress)

pgNumErrors :: Progress -> Int
pgNumErrors = length . pgErrors

pgTotal :: Progress -> Int
pgTotal p = pgNumErrors p + pgNoop p + pgDone p

-- | Returns progress as a ratio, or Nothing if no goal.
pgProgress :: Progress -> Maybe Double
pgProgress p@Progress{..} =
  if pgGoal == 0
  then Nothing
  else Just $ fromIntegral (pgTotal p) / fromIntegral pgGoal

incErrors :: Text -> Text -> Progress -> Progress
incErrors item err p@Progress { pgErrors = old } =
  p { pgErrors = ProgressError item err:old  }

incNoop :: Progress -> Progress
incNoop p@Progress { pgNoop = old } =
  p { pgNoop = old + 1 }

incDone :: Progress -> Progress
incDone p@Progress { pgDone = old } =
  p { pgDone = old + 1 }

incProgress :: [ProgressError] -> Int -> Int -> Progress -> Progress
incProgress e n d p@Progress{..} =
  p { pgErrors = e ++ pgErrors
    , pgNoop = pgNoop + n
    , pgDone = pgDone + d
    }

newtype WorkStart = WorkStart { wsStart :: ZonedTime }
  deriving (Show)

instance NFData WorkStart where
  rnf (WorkStart s) = rnf s

data WorkResults = WorkResults
  { wrStart :: !ZonedTime
  , wrEnd   :: !ZonedTime
  , wrDone  :: !Progress
  }
  deriving (Show)

instance NFData WorkResults where
  rnf (WorkResults a b c) =
    rnf a `seq` rnf b `seq` rnf c


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
  , ctxCleanProgress  :: !(TVar Progress)
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
  cleanProg <- newTVar def
  return $ Context { ctxConfig         = config
                   , ctxRepo           = ta
                   , ctxScanner        = scanner
                   , ctxScanProgress   = scanProg
                   , ctxRenderProgress = renderProg
                   , ctxCleanProgress  = cleanProg
                   , ctxSearchCache    = tb
                   , ctxLogger         = logfn
                   }

$(makeStore ''WorkStart)
$(makeStore ''WorkResults)

-- | View presentation.
data ViewPresentation = PresentationGrid | PresentationList
  deriving (Eq, Show)

-- | View mode type.
data ViewMode = ViewSingleImage
              | ViewImagesGrid
              | ViewImagesList
              | ViewFoldersList
  deriving (Eq, Show)

-- | Poor man's view mode encoding.
formatViewMode :: ViewMode -> ByteString
formatViewMode ViewSingleImage = "images-view"
formatViewMode ViewImagesGrid  = "images-grid"
formatViewMode ViewImagesList  = "images-list"
formatViewMode ViewFoldersList = "folders-list"

-- | Poor man's view mode decoding.
parseViewMode :: Text -> Maybe ViewMode
parseViewMode "images-view"  = Just ViewSingleImage
parseViewMode "images-grid"  = Just ViewImagesGrid
parseViewMode "images-list"  = Just ViewImagesList
parseViewMode "folders-list" = Just ViewFoldersList
parseViewMode _              = Nothing
