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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
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
                    ) where

import           Data.Aeson.Text             (encodeToLazyText)
import qualified Data.Map                    as Map
import qualified Data.Text                   as Text
import qualified Data.Text.Encoding          as Text (encodeUtf8)
import qualified Data.Text.Lazy              as TextL
import qualified Text.Blaze.Svg              as Svg
import           Text.Blaze.Svg11            (Svg, docTypeSvg, rect, text_,
                                              tspan, (!))
import qualified Text.Blaze.Svg11.Attributes as SA
import qualified Text.Hamlet                 as Hamlet (Render)

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
           ]

mkImageInfo :: Text -> ImageName -> Bool -> Hamlet.Render (Route App)
            -> UrlParams -> Transform -> ImageInfo
mkImageInfo folder iname movie render params t =
  ImageInfo (render (ImageInfoR folder iname) params)
            (render (ImageBytesR folder iname) [])
            (if movie then Just (render (MovieBytesR folder iname) []) else Nothing)
            (render (ViewR folder iname) params)
            (render (ImageFlagR folder iname) [])
            (render ListImagesR params)
            (render (BrowseImagesR 0) params)
            iname (transformParams t) (transformMatrix t)

data ViewInfo = ViewInfo
  { viYear       :: Text
  , viYearUrl    :: Text
  , viFolder     :: Text
  , viFldUrl     :: Text
  , viFldList    :: Text
  , viFldBrowse  :: Text
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
           , "folderbrowse" .= viFldBrowse
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
locateCurrentImage :: Text -> ImageName -> SearchResultsPics -> Handler Image
locateCurrentImage fname iname images = do
  unfiltered <- getImage fname iname
  maybe notFound return $ Map.lookup (fname, imageTimeKey unfiltered) images

getViewR :: Text -> ImageName -> Handler Html
getViewR folder iname = do
  (params, _, images) <- getAtomAndSearch
  vi <- viewInfoForImage params images folder iname
  let viewInfo = encodeToLazyText (toJSON vi)
      isMovie = isJust (iiMovieUrl $ viCurrent vi)
      displayPicElem = if isMovie then "none" else "block"::Text
      displayMovElem = if isMovie then "block" else "none"::Text
  debug <- encodeToLazyText . appShouldLogAll . appSettings <$> getYesod
  defaultLayout $ do
    setHtmlTitle $ "image " <> folder <> "/" <> unImageName iname
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

-- | Returns the image bytes for a given image.
--
-- This always returns the image bytes, even for a movie (in which case
-- the poster image is returned), potentially at the requested resolution.
getImageBytesR :: Text -> ImageName -> Handler ()
getImageBytesR folder iname = do
  config <- getConfig
  img <- getImage folder iname
  res <- getResParam
  imgbytes <- liftIO $ imageAtRes config img res
  case imgbytes of
    Left ImageNotViewable ->
      sendResponse imageNotViewable
    Left (ImageError err) ->
      sendResponse $ imageError err
    Right (_, ctype, rpath) ->
      sendFile (Text.encodeUtf8 ctype) rpath

-- | Returns the movie bytes for a given image.
--
-- Note this always hardcodes the mime type to video/mp4, as we don't
-- handle file types well.
getMovieBytesR :: Text -> ImageName -> Handler ()
getMovieBytesR folder iname = do
  img <- getImage folder iname
  case bestMovie img of
    Just f -> sendFile
                (Text.encodeUtf8 $ fileMimeType "video/mp4" f)
                (TextL.unpack $ filePath f)
    _      -> sendResponse imageNotViewable

-- | Builds the complete view information for a given image.
--
-- This builds the full view information for a given image, including
-- previous/next images, previous/next folders, etc. so that the view page
-- can quickly navigate between images.
viewInfoForImage :: UrlParams -> SearchResults -> Text -> ImageName -> Handler ViewInfo
viewInfoForImage params (images, folders) folder iname = do
  picdir <- getFolder folder
  render <- getUrlRenderParams
  img <- locateCurrentImage folder iname images
  let -- since we have an image, it follows that min/max must exist
      -- (they might be the same), hence we can use the non-total
      -- functions findMin/findMax until newer containers package
      -- reaches LTS
      tkey = imageTimeKey img
      imgFirst = snd  $  Map.findMin images
      imgPrev  = snd <$> Map.lookupLT (folder, tkey) images
      imgNext  = snd <$> Map.lookupGT (folder, tkey) images
      imgLast  = snd  $  Map.findMax images
      fldPrev  = snd <$> Map.lookupLT folder folders
      fldNext  = snd <$> Map.lookupGT folder folders
      mk i = mkImageInfo (imgParent i) (imgName i) (isJust $ bestMovie i)
               render params (transformForImage i)
      y = maybe "?" (Text.pack . show) $ pdYear picdir
      yurl = maybe SearchFoldersNoYearR (SearchFoldersByYearR . sformat int) $ pdYear picdir
  return $
    ViewInfo y (render yurl [])
      folder (render (FolderR folder) params)
      (render ListFoldersR params) (render (BrowseFoldersR 0) params)
      (imgName img) (render (ImageR folder (imgName img)) params)
      (mk imgFirst) (mk <$> fldPrev) (mk <$> imgPrev) (mk img)
      (mk <$> imgNext) (mk <$> fldNext)(mk imgLast)

-- | Get the image information for a given image.
getImageInfoR :: Text -> ImageName -> Handler Value
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
