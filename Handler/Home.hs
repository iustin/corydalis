{-# LANGUAGE TupleSections, OverloadedStrings, NoCPP #-}
module Handler.Home where

import Import
import Pics
import Types

import Data.List
import qualified Data.Map as Map
import qualified Data.Text as T
import Text.Printf

-- | Formats a double as a percent value. NaN values are transformed
-- into a Nothing.
formatPercent :: Double -> Maybe String
formatPercent v | isNaN v = Nothing
                | otherwise = Just $ printf "%.02f" v

fcName :: FolderClass -> Text
fcName FolderRaw         = "raw"
fcName FolderStandalone  = "standalone"
fcName FolderUnprocessed = "not fully processed"
fcName FolderProcessed   = "fully processed"
fcName FolderEmpty       = "empty"
fcName FolderMixed       = "mixed"
fcName FolderOutdated    = "outdated"

fcDescription :: FolderClass -> Text
fcDescription FolderRaw         = "contains only RAW files"
fcDescription FolderStandalone  = "contains only files without a RAW format"
fcDescription FolderUnprocessed = "contains RAW files and some processed files"
fcDescription FolderProcessed   = "contains RAW files, all processed"
fcDescription FolderEmpty       = "contains no image files"
fcDescription FolderMixed       = "contains both RAW files (either processed or \
                                  \not) and files without RAW storage"
fcDescription FolderOutdated    = "contains RAW files, all processed, but some of \
                                  \the processed files are outdated (corresponding \
                                  \RAW file has been retouched more recently)"

getHomeR :: Handler Html
getHomeR = do
  config <- extraConfig `fmap` getExtra
  pics <- liftIO $ scanAll config
  let ((Stats unprocessed standalone processed outdated orphaned), fcm) =
          computeRepoStats pics
      allpics = unprocessed + standalone + processed + outdated
      fstats = Map.toAscList fcm
      numfolders = Map.size pics
      all_fc = [minBound..maxBound]
  defaultLayout $ do
    setTitle "PicMan: home"
    $(widgetFile "homepage")

getFolderR :: Text -> Handler Html
getFolderR name = do
  config <- extraConfig `fmap` getExtra
  pics <- liftIO $ scanAll config
  case Map.lookup name pics of
    Nothing -> notFound
    Just dir -> defaultLayout $ do
      setTitle . toHtml $ "PicMan: folder " `T.append` name
      $(widgetFile "folder")

getBrowseFoldersR :: [FolderClass] -> Handler Html
getBrowseFoldersR kinds = do
  let kinds_string = T.intercalate ", " . map fcName $ kinds
  config <- extraConfig `fmap` getExtra
  pics <- liftIO $ scanAll config
  let folders = filterDirsByClass kinds pics
      allpics = sum . map numPics $ folders
      allraws = sum . map numRawPics $ folders
      allunproc = sum . map numUnprocessedPics $ folders
      allprocessed = sum . map numProcessedPics $ folders
      allstandalone = sum . map numStandalonePics $ folders
      alloutdated = sum . map numOutdatedPics $ folders
      tp = formatPercent $
           (fromIntegral allunproc) * 100 / fromIntegral allpics
      npairs = map (\n -> let unproc = fromIntegral (numUnprocessedPics n)
                              numraw = fromIntegral (numRawPics n)
                          in (n, formatPercent $ unproc * 100 / numraw))
               folders
  defaultLayout $ do
    setTitle . toHtml $
      "PicMan: browsing folders of type " `T.append` kinds_string
    $(widgetFile "browsefolders")

getReloadR :: Handler Html
getReloadR = do
  config <- extraConfig `fmap` getExtra
  _ <- liftIO $ forceScanAll config
  setMessage "Cache reloaded"
  redirect HomeR
