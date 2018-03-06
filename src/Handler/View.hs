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
                    , getImageInfoR
                    , getRandomImageInfoR
                    ) where

import           Data.Aeson.Text             (encodeToLazyText)
import qualified Data.Map                    as Map
import qualified Data.Text                   as Text
import qualified Data.Text.Encoding          as Text (encodeUtf8)
import           System.Random               (getStdRandom, randomR)
import qualified Text.Blaze.Svg              as Svg
import           Text.Blaze.Svg11            (Svg, docTypeSvg, text_, (!))
import qualified Text.Blaze.Svg11.Attributes as SA
import qualified Text.Hamlet                 as Hamlet (Render)
import           Text.Read                   (readMaybe)

import           Exif
import           Handler.Utils
import           Import
import           Pics
import           Types

data ImageInfo = ImageInfo
  { iiInfoUrl   :: Text
  , iiBytesUrl  :: Text
  , iiViewUrl   :: Text
  , iiName      :: Text
  , iiTransform :: (Int, Bool, Bool)
  }

instance ToJSON ImageInfo where
  toJSON ImageInfo {..} =
    object [ "info"      .= iiInfoUrl
           , "bytes"     .= iiBytesUrl
           , "view"      .= iiViewUrl
           , "name"      .= iiName
           , "transform" .= iiTransform
           ]

rotateToJSON :: Rotate -> Int
rotateToJSON RCenter =  0
rotateToJSON RLeft   = -1
rotateToJSON RRight  =  1

mkImageInfo :: Text -> Text -> Hamlet.Render (Route App)
            -> UrlParams -> Transform -> ImageInfo
mkImageInfo folder iname render params (Transform r fx fy) =
  ImageInfo (render (ImageInfoR folder iname) params)
            (render (ImageBytesR folder iname) [])
            (render (ViewR folder iname) params)
            iname (rotateToJSON r, fx, fy)

data ViewInfo = ViewInfo
  { viFolder  :: Text
  , viFldUrl  :: Text
  , viImage   :: Text
  , viImgUrl  :: Text
  , viFirst   :: ImageInfo
  , viPrev    :: Maybe ImageInfo
  , viCurrent :: ImageInfo
  , viNext    :: Maybe ImageInfo
  , viLast    :: ImageInfo
  }

instance ToJSON ViewInfo where
  toJSON ViewInfo {..} =
    object [ "folder"      .= viFolder
           , "folderurl"   .= viFldUrl
           , "image"       .= viImage
           , "imageurl"    .= viImgUrl
           , "first"       .= viFirst
           , "prev"        .= viPrev
           , "current"     .= viCurrent
           , "next"        .= viNext
           , "last"        .= viLast
           ]

-- | Ensure that requested image is present in the (filtered) map.
locateCurrentImage :: Text -> Text -> SearchResults -> Handler Image
locateCurrentImage fname iname images =
  case Map.lookup (fname, iname) images of
    Just img -> return img
    Nothing  -> notFound

getViewR :: Text -> Text -> Handler Html
getViewR folder iname = do
  (params, _, images) <- getAtomAndSearch
  img <- locateCurrentImage folder iname images
  let Transform r fx fy = transformForImage img
      initialTransform = encodeToLazyText (rotateToJSON r, fx, fy)
  debug <- appShouldLogAll . appSettings <$> getYesod
  defaultLayout $ do
    addScript $ StaticR corydalis_js_viewer_js
    addScript $ StaticR hammer_js_hammer_js
    setHtmlTitle $ "image " <> folder <> "/" <> imgName img
    $(widgetFile "view")

basicSvg :: Text -> Svg
basicSvg msg =
  docTypeSvg ! SA.version "1.1" ! SA.width "600" ! SA.height "600"
    ! SA.viewbox "0 0 600 600" $
  text_ (Svg.toSvg msg)
    ! SA.fontSize "14px"
    ! SA.x "50" ! SA.y "50"
    ! SA.textlength "500" ! SA.lengthadjust "spacingAndGlyphs"

imageNotViewable :: TypedContent
imageNotViewable =
  TypedContent typeSvg . toContent $ basicSvg "Image has no viewable version ☹"

imageError :: Text -> TypedContent
imageError msg =
  TypedContent typeSvg . toContent $ basicSvg ("Error: " <> msg)

getImageBytesR :: Text -> Text -> Handler ()
getImageBytesR folder iname = do
  config <- getConfig
  img <- getImage folder iname
  -- TODO: make this 'res' string and the javascript string derive from the same constant
  res <- lookupGetParam "res"
  let res' = fmap Text.unpack res >>= readMaybe
  imgbytes <- liftIO $ imageAtRes config img (ImageSize <$> res')
  case imgbytes of
    Left ImageNotViewable ->
      sendResponse imageNotViewable
    Left (ImageError err) ->
      sendResponse $ imageError err
    Right (ctype, rpath) ->
      -- TODO: don't use hardcoded jpeg type!
      sendFile (Text.encodeUtf8 ctype) (Text.unpack rpath)

fileToView :: Image -> Maybe File
fileToView img =
  case imgJpegPath img of
    f:_ -> Just f
    []  -> imgRawPath img

transformForFile :: File -> Maybe Transform
transformForFile  =
  either (const Nothing) (Just . affineTransform . exifOrientation) . fileExif

transformForImage :: Image -> Transform
transformForImage img =
  fromMaybe def (fileToView img >>= transformForFile)

randomPick :: SearchResults -> IO Image
randomPick images = do
  idx <- getStdRandom $ randomR (0, Map.size images - 1)
  -- This _should_ be safe, since idx in the right range. If not,
  -- well...
  return . snd . Map.elemAt idx $ images

getImageInfoR :: Text -> Text -> Handler Value
getImageInfoR folder iname = do
  (params, _, images) <- getAtomAndSearch
  img <- locateCurrentImage folder iname images
  render <- getUrlRenderParams
  let -- since we have an image, it follows that min/max must exist
      -- (they might be the same), hence we can use the non-total
      -- functions findMin/findMax until newer containers package
      -- reaches LTS
      imgFirst = snd  $  Map.findMin images
      imgPrev  = snd <$> Map.lookupLT (folder, iname) images
      imgNext  = snd <$> Map.lookupGT (folder, iname) images
      imgLast  = snd  $  Map.findMax images
      mk i = mkImageInfo (imgParent i) (imgName i) render params
               (transformForImage i)
  return . toJSON $
    ViewInfo
      folder (render (FolderR folder) params)
      (imgName img) (render (ImageR folder iname) params)
      (mk imgFirst) (mk <$> imgPrev) (mk img) (mk <$> imgNext) (mk imgLast)

getRandomImageInfoR :: Handler Value
getRandomImageInfoR = do
  (_, _, images) <- getAtomAndSearch
  when (null images) notFound
  image <- lift $ randomPick images
  getImageInfoR (imgParent image) (imgName image)
