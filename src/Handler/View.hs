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

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Handler.View ( getViewR
                    , getImageBytesR
                    , getMovieBytesR
                    , getImageInfoR
                    , getRandomImageInfoR
#ifdef TEST
                    , basicSvg
                    , imageError
                    , imageErrorText
                    , ImageInfo(..)
#endif
                    ) where

import           Data.Aeson.Text               (encodeToLazyText)
import qualified Data.Map                      as Map
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text (encodeUtf8)
import qualified Data.Text.Short               as TS
import           System.Log.FastLogger         (toLogStr)
import           Text.Blaze                    (ToMarkup, toMarkup)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Svg                as Svg
import           Text.Blaze.Svg11              (Svg, docTypeSvg, rect, text_,
                                                tspan, (!))
import qualified Text.Blaze.Svg11.Attributes   as SA
import qualified Text.Hamlet                   as Hamlet (Render)


import qualified ClassyPrelude                 as Data.Text.Lazy
import           Exif
import           Handler.Utils
import           Import
import           Pics

data ImageInfo = ImageInfo
  { iiInfoUrl   :: Text
  , iiBytesUrl  :: Text
  , iiMovieUrl  :: Maybe Text
  , iiViewUrl   :: Text
  , iiFlagUrl   :: Text
  , iiListUrl   :: Text
  , iiBrowseUrl :: Text
  , iiName      :: ImageName
  , iiTransform :: (Int, Bool, Bool)
  , iiMatrix    :: (Double, Double, Double, Double)
  , iiExifHtml  :: Text
  }

instance ToJSON ImageInfo where
  toJSON ImageInfo {..} =
    object [ "info"      .= iiInfoUrl
           , "bytes"     .= iiBytesUrl
           , "movie"     .= iiMovieUrl
           , "view"      .= iiViewUrl
           , "flag"      .= iiFlagUrl
           , "list"      .= iiListUrl
           , "browse"    .= iiBrowseUrl
           , "name"      .= iiName
           , "transform" .= iiTransform
           , "matrix"    .= iiMatrix
           , "exifhtml"  .= iiExifHtml
           ]

data MaybeMarkup a = RealMarkup a | NoMarkup

wrapMarkup :: Maybe a -> MaybeMarkup a
wrapMarkup Nothing  = NoMarkup
wrapMarkup (Just a) = RealMarkup a

instance ToMarkup a => ToMarkup (MaybeMarkup a) where
  toMarkup NoMarkup       = toMarkup ("(missing)"::Text)
  toMarkup (RealMarkup a) = toMarkup a

renderExifHTML :: Image -> Text
renderExifHTML img =
  let exif = imgExif img
      dimensions = case (exifWidth exif, exifHeight exif) of
                    (Just w, Just h) -> Just (w, h)
                    _                -> Nothing
  in Data.Text.Lazy.toStrict $ renderHtml [shamlet|
        <b>Date:
        #{wrapMarkup $ exifCreateDate exif}
        <br>
      $maybe title <- exifTitle exif
        <b>Title:
        #{title}
        <br>
      $maybe caption <- exifCaption exif
        <b>Caption:
        #{caption}
        <br>
      <b>Camera:
      #{wrapMarkup $ maybeDesymbolizeItem $ exifCamera exif}
      <br>
      <b>Lens:
      #{lensDisplayName (exifLens exif)}
      <br>
      <b>Focal length:
      #{wrapMarkup (exifFL35mm exif)}mm
      <br>
      <b>Exposure:
      #{wrapMarkup $ exifSSpeedDesc exif}s @ &fnof;/#{wrapMarkup (exifAperture exif)}, ISO #{wrapMarkup (exifISO exif)}
      <br>
      <b>Dimensions:
      $maybe (dimw, dimh) <- dimensions
        #{dimw}×#{dimh}
      $nothing
        unknown
      <br>
      <b>Size:
      $maybe mp <- exifMegapixels exif
        #{sformat (fixed 2) mp}MP
      $nothing
        unknown
    |]

mkImageInfo :: Image -> Hamlet.Render (Route App) -> UrlParams -> ImageInfo
mkImageInfo img render params =
  ImageInfo (render (ImageInfoR folder iname) params)
            (render (ImageBytesR folder iname) [])
            (if movie then Just (render (MovieBytesR folder iname) []) else Nothing)
            (render (ViewR folder iname) params)
            (render (ImageFlagR folder iname) [])
            (render ListImagesR params)
            (render (BrowseImagesR 0) params)
            iname (transformParams t) (transformMatrix t)
            (renderExifHTML img)
    where folder = imgParent img
          iname = imgName img
          movie = isJust $ bestMovie img
          t = transformForImage img

data ViewInfo = ViewInfo
  { viYear       :: Text
  , viYearUrl    :: Text
  , viFolder     :: ShortText
  , viFldUrl     :: Text
  , viFldList    :: Text
  , viImage      :: ImageName
  , viImgUrl     :: Text
  , viFirst      :: ImageInfo
  , viPrevFolder :: Maybe ImageInfo
  , viPrev       :: Maybe ImageInfo
  , viCurrent    :: ImageInfo
  , viNext       :: Maybe ImageInfo
  , viNextFolder :: Maybe ImageInfo
  , viLast       :: ImageInfo
  }

instance ToJSON ViewInfo where
  toJSON ViewInfo {..} =
    object [ "year"         .= viYear
           , "yearurl"      .= viYearUrl
           , "folder"       .= viFolder
           , "folderurl"    .= viFldUrl
           , "folderlist"   .= viFldList
           , "image"        .= viImage
           , "imageurl"     .= viImgUrl
           , "first"        .= viFirst
           , "prevfolder"   .= viPrevFolder
           , "prev"         .= viPrev
           , "current"      .= viCurrent
           , "next"         .= viNext
           , "nextfolder"   .= viNextFolder
           , "last"         .= viLast
           ]

-- | Ensure that requested image is present in the (filtered) map.
locateCurrentImage :: ShortText -> ImageName -> SearchResultsPics -> Handler Image
locateCurrentImage fname iname images = do
  unfiltered <- getImage fname iname
  maybe notFound return $ Map.lookup (fname, imageTimeKey unfiltered) images

getViewR :: ShortText -> ImageName -> Handler Html
getViewR folder iname = do
  (params, _, images) <- getAtomAndSearch
  vi <- viewInfoForImage params images folder iname
  let viewInfo = encodeToLazyText (toJSON vi)
      isMovie = isJust (iiMovieUrl $ viCurrent vi)
      displayPicElem = if isMovie then "none" else "block"::Text
      displayMovElem = if isMovie then "block" else "none"::Text
  debug <- encodeToLazyText . appShouldLogAll . appSettings <$> getYesod
  defaultLayout $ do
    setHtmlTitle $ "image " <> TS.toText folder <> "/" <> (TS.toText . unImageName $ iname)
    $(widgetFile "view")

basicSvg :: Text -> Svg
basicSvg msg =
  docTypeSvg ! SA.version "1.1" ! SA.width "600" ! SA.height "800"
    ! SA.viewbox "0 0 600 800" $ do
  rect ! SA.x "0" ! SA.y "0" ! SA.height "100%" ! SA.width "100%"
    ! SA.fill "#b0b0b0"
  text_ (breakText msg)
    ! SA.fontSize "14px"
    ! SA.x "50" ! SA.y "50"
    -- ! SA.textlength "100%" ! SA.lengthadjust "spacingAndGlyphs"

  where breakText msgx =
          let w = words msgx
              lns = go w []
          in Svg.toSvg $ tspan "":map (\l -> tspan (Svg.toSvg l) ! SA.dy "14" ! SA.x "0") lns
        go [] lns = reverse lns
        go ws lns = let (tw, lw) = splitAt 7 ws
                    in go lw (unwords tw:lns)

-- | Returns an image saying the given image is not viewable.
imageNotViewable :: TypedContent
imageNotViewable =
  imageError "Image has no viewable version ☹"


-- | Returns an error image (an image containting an error message).
imageError :: Text -> TypedContent
imageError msg =
  TypedContent typeSvg . toContent $ basicSvg ("Error: " <> msg)

-- | The text of an image error, as shown to a user with (or without)
-- access to storage paths (see 'hasAccessToStoragePaths').
--
-- The details come from the external tools or from exceptions, and
-- can include storage paths (of the picture, or of the cache), so
-- others only get a generic message.
imageErrorText :: Bool -> Text -> Text
imageErrorText True  details = details
imageErrorText False _       = "The image could not be read or converted."

-- | Responds with an error image for an error building or reading an
-- image (see 'imageErrorText').
--
-- The error is always logged (as a warning, being a problem with the
-- files rather than with the application), with its details, whoever
-- requested the image (and whether they see the details), so that log
-- watchers see it without depending on users reporting it.
sendImageError :: LogFn -> ShortText -> ImageName -> Text -> Handler a
sendImageError logfn folder iname details = do
  liftIO . logfn LevelWarn $
    "Error for image " <> toLogStr (TS.toText folder) <> "/" <>
    toLogStr (TS.toText (unImageName iname)) <> ": " <> toLogStr details
  access <- hasAccessToStoragePaths
  sendResponse . imageError $ imageErrorText access details

-- | Restricts what browsers can do with raw file contents opened directly.
--
-- The contents come from the repository, and might be crafted documents
-- rather than media; in a sandbox, they cannot run scripts, nor act as
-- part of the site. This doesn't affect their use as images or movies in
-- our own pages.
sandboxRawContents :: Handler ()
sandboxRawContents = addHeader "Content-Security-Policy" "sandbox"

-- | Returns the image bytes for a given image.
--
-- This always returns the image bytes, even for a movie (in which case
-- the poster image is returned), potentially at the requested resolution.
getImageBytesR :: ShortText -> ImageName -> Handler ()
getImageBytesR folder iname = do
  sandboxRawContents
  config <- getConfig
  img <- getImage folder iname
  res <- getResParam
  ctx <- getContext
  let logfn = ctxLogger ctx
  imgbytes <- liftIO $ imageAtRes (ctxRenderToolLimits ctx) logfn config img res
  case imgbytes of
    Left ImageNotViewable ->
      sendResponse imageNotViewable
    Left (ImageError err) ->
      sendImageError logfn folder iname err
    Right (_, ctype, rpath) ->
      sendFile (Text.encodeUtf8 ctype) rpath

-- | Returns the movie bytes for a given image.
--
-- Note this always hardcodes the mime type to video/mp4, as we don't
-- handle file types well.
getMovieBytesR :: ShortText -> ImageName -> Handler ()
getMovieBytesR folder iname = do
  sandboxRawContents
  img <- getImage folder iname
  config <- getConfig
  logfn <- ctxLogger <$> getContext
  movie <- liftIO $ movieToServe logfn config img
  case movie of
    Left (ImageError err) -> sendImageError logfn folder iname err
    Left ImageNotViewable -> sendResponse imageNotViewable
    Right f -> sendFile (Text.encodeUtf8 $ fileMimeType "video/mp4" f)
                        (fileFullPath f)

-- | Builds the complete view information for a given image.
--
-- This builds the full view information for a given image, including
-- previous/next images, previous/next folders, etc. so that the view page
-- can quickly navigate between images.
viewInfoForImage :: UrlParams -> SearchResults -> ShortText -> ImageName -> Handler ViewInfo
viewInfoForImage params (images, folders) folder iname = do
  picdir <- getFolder folder
  render <- getUrlRenderParams
  img <- locateCurrentImage folder iname images
  let ImageNeighbours {..} = imageNeighbours (images, folders) img
      mk i = mkImageInfo i render params
      y = maybe "?" (Text.pack . show) $ pdYear picdir
      yurl = maybe SearchFoldersNoYearR (SearchFoldersByYearR . sformat int) $ pdYear picdir
  return $
    ViewInfo y (render yurl [])
      folder (render (FolderR folder) params)
      (render ListFoldersR params)
      (imgName img) (render (ImageR folder (imgName img)) params)
      (mk nbFirst) (mk <$> nbPrevFolder) (mk <$> nbPrev) (mk img)
      (mk <$> nbNext) (mk <$> nbNextFolder) (mk nbLast)

-- | Get the image information for a given image.
getImageInfoR :: ShortText -> ImageName -> Handler Value
getImageInfoR folder iname = do
  (params, _, images) <- getAtomAndSearch
  vi <- viewInfoForImage params images folder iname
  return $ toJSON vi

-- | Get the image information for a random image.
getRandomImageInfoR :: Handler Value
getRandomImageInfoR = do
  (_, _, (images, _)) <- getAtomAndSearch
  mimage <- liftIO $ randomPick images
  case mimage of
    Nothing  -> notFound
    Just img -> getImageInfoR (imgParent img) (imgName img)
