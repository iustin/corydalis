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

import           Exif
import           Handler.Utils
import           Import
import           Pics

import           Data.Aeson.Text             (encodeToLazyText)
import qualified Data.Map                    as Map
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T (encodeUtf8)
import           System.Random               (getStdRandom, randomR)
import qualified Text.Blaze.Svg              as Svg
import           Text.Blaze.Svg11            (Svg, docTypeSvg, text_, (!))
import qualified Text.Blaze.Svg11.Attributes as SA
import           Text.Read                   (readMaybe)

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

mkImage :: Text -> Text -> (Route App -> Text) -> Transform -> ImageInfo
mkImage folder iname render (Transform r fx fy) =
  ImageInfo (render $ ImageInfoR folder iname)
            (render $ ImageBytesR folder iname)
            (render $ ViewR folder iname)
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

-- | Helper until newer containers reach LTS.
lookupMin :: Map a b -> Maybe (a, b)
lookupMin m = if Map.null m then Nothing else Just $ Map.findMin m

-- | Helper until newer containers reach LTS.
lookupMax :: Map a b -> Maybe (a, b)
lookupMax m = if Map.null m then Nothing else Just $ Map.findMax m

getNextImageAnyFolder :: Repository -> Text -> Text -> Bool -> Maybe Image
getNextImageAnyFolder (repoDirs -> pics) folder iname forward = do
  let nextElem :: (Ord a) => a -> Map a b -> Maybe (a, b)
      nextElem  = if forward then Map.lookupGT else Map.lookupLT
      nextFolder = flip nextElem pics
      firstImage = if forward then lookupMin else lookupMax
      hasImages = isJust . firstImage . pdImages
  curFolder <- pdImages <$> folder `Map.lookup` pics
  -- The result of any lookup is (k, v), hence the many fst and snd
  -- calls below.
  case iname `nextElem` curFolder of
    Nothing -> do
      nf <- until
                    (maybe True (hasImages . snd))
                    (>>= nextFolder . fst)
                    (nextFolder folder)
      fmap snd . firstImage . pdImages . snd $ nf
    img -> snd <$> img

getViewR :: Text -> Text -> Handler Html
getViewR folder iname = do
  img <- getImage folder iname
  let Transform r fx fy = transformForImage img
      initialTransform = encodeToLazyText (rotateToJSON r, fx, fy)
  debug <- appShouldLogAll . appSettings <$> getYesod
  defaultLayout $ do
    addScript $ StaticR js_viewer_js
    addScript $ StaticR js_hammer_js
    setTitle . toHtml $ "Corydalis: Image " <> folder
               <> "/" <> imgName img
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
  let res' = fmap T.unpack res >>= readMaybe
  imgbytes <- liftIO $ imageAtRes config img (ImageSize <$> res')
  case imgbytes of
    Left ImageNotViewable ->
      sendResponse imageNotViewable
    Left (ImageError err) ->
      sendResponse $ imageError err
    Right (ctype, rpath) ->
      -- TODO: don't use hardcoded jpeg type!
      sendFile (T.encodeUtf8 ctype) (T.unpack rpath)

fileToView :: Image -> Maybe File
fileToView img =
  case imgJpegPath img of
    f:_ -> Just f
    []  -> imgRawPath img

transformForFile :: File -> Maybe Transform
transformForFile  =
  fmap (affineTransform . exifOrientation) . fileExif

transformForImage :: Image -> Transform
transformForImage img =
  fromMaybe def (fileToView img >>= transformForFile)

getImageInfoR :: Text -> Text -> Handler Value
getImageInfoR folder iname = do
  (pics, dir) <- getPicsAndFolder folder
  let images = pdImages dir
  img <- getFolderImage dir iname
  render <- getUrlRender
  let -- since we have an image, it follows that min/max must exist
      -- (they might be the same), hence we can use the non-total
      -- functions findMin/findMax until newer containers package
      -- reaches LTS
      imgFirst = snd  $ Map.findMin images
      imgPrev  = getNextImageAnyFolder pics folder iname False
      imgNext  = getNextImageAnyFolder pics folder iname True
      imgLast  = snd  $ Map.findMax images
      mk i = mkImage (imgParent i) (imgName i) render
               (transformForImage i)
  return . toJSON $
    ViewInfo
      folder (render $ FolderR folder)
      (imgName img) (render $ ImageR folder iname)
      (mk imgFirst) (mk <$> imgPrev) (mk img) (mk <$> imgNext) (mk imgLast)

getRandomImageInfoR :: Handler Value
getRandomImageInfoR = do
  pics <- getPics
  let nonEmptyFolders = Map.filter hasViewablePics (repoDirs pics)
  when (Map.null nonEmptyFolders) notFound
  fidx <- liftIO $ getStdRandom $ randomR (0, Map.size nonEmptyFolders - 1)
  let (fname, folder) = Map.elemAt fidx nonEmptyFolders
  let images = Map.filter (not . null . imgJpegPath) $ pdImages folder
  iidx <- liftIO $ getStdRandom $ randomR (0, Map.size images - 1)
  let (iname, _) = Map.elemAt iidx images
  getImageInfoR fname iname
