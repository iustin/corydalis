{-# LANGUAGE FlexibleInstances #-}

module Types ( Config(..)
             , Regex
             , reRegex
             , reString
             , FolderClass(..)
             ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.Text as T
import qualified Text.Regex.PCRE as PCRE
import Yesod

data Regex = Regex
    { reString :: String
    , reRegex  :: PCRE.Regex
    }

instance Show Regex where
  show (Regex str _) = show str

instance FromJSON Regex where
  parseJSON (String txt) =
    let str = T.unpack txt
    in case PCRE.makeRegexM str of
         Nothing -> mzero
         Just r -> return $ Regex str r
  parseJSON _ = mzero

data Config = Config
    { cfgDirs            :: [FilePath]
    , cfgBlacklistedDirs :: [FilePath]
    , cfgRawExts         :: [FilePath]
    , cfgJpegExts        :: [FilePath]
    , cfgSidecarExts     :: [FilePath]
    , cfgOtherImgExts    :: [FilePath]
    , cfgDirRegex        :: Regex
    } deriving (Show)

instance FromJSON Config where
  parseJSON (Object v) =
    Config <$>
         v .: "dirs" <*>
         v .: "blacklisteddirs" <*>
         v .: "rawexts" <*>
         v .: "jpegexts" <*>
         v .: "sidecarexts" <*>
         v .: "otherexts" <*>
         v .: "dirregex"

  parseJSON _ = mzero

data FolderClass = FolderEmpty
                 | FolderRaw
                 | FolderStandalone
                 | FolderUnprocessed
                 | FolderProcessed
                 | FolderMixed
                   deriving (Show, Read, Eq, Ord, Enum, Bounded)

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
  toPathPiece = T.intercalate "," . map toPathPiece
  fromPathPiece "all" = Just [minBound..maxBound]
  fromPathPiece v     = mapM fromPathPiece $ T.split (==',') v
