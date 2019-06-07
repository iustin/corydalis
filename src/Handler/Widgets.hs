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
{-# LANGUAGE NoCPP             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module Handler.Widgets where

import qualified Data.Map      as Map
import           Data.Ord
import qualified Data.Set      as Set
import qualified Data.Text     as Text
import           Formatting

import           Exif
import           Handler.Utils
import           Import
import           Indexer
import           Pics

showFile :: Pics.File -> Widget
showFile f =
  $(widgetFile "showfile")

folderCover :: Int -> Bool -> UrlParams -> Atom -> PicDir -> Widget
folderCover size browsing params atom folder = do
  let name = pdName folder
      all_images = Map.elems (pdImages folder)
      images = imageSearchFunction atom `filter` all_images ++ all_images
      images' = filter (not . null . allViewableImageFiles) images ++ images
      bytes_fn = if browsing
        then imageBytesForFolder
        else imageBytes
  case images' of
    []    -> if browsing
             then toWidget [hamlet|<div .card .mx-1 .my-1>
                                    <div .card-header>
                                      #{name}
                                    <div .card-body>
                                      Folder '#{name}' contains no viewable images
                                      |]
             else toWidget [hamlet|<span .disabled>N/A|]
    img:_ -> bytes_fn size params name (imgName img)

imageBytes :: Int -> UrlParams -> Text -> Text -> Widget
imageBytes thumbsize params folder image =
  toWidget [hamlet|<a href=@?{(ViewR folder image, params)}>
                     <img
                       src="@{ImageBytesR folder image thumbsize}"
                       width=#{thumbsize} height=#{thumbsize}
                       >|]

imageBytesForFolder :: Int -> UrlParams -> Text -> Text -> Widget
imageBytesForFolder size params folder image =
  toWidget [hamlet|<a href=@?{(ViewR folder image, params)}>
                     <img
                       .grid-item-image
                       src="@{ImageBytesR folder image size}"
                       >|]

-- | Generates srcset for an image based on all auto-built versions.
imageSrcSet :: Config -> (Route App -> Text) -> Text -> Text -> Int -> Text
imageSrcSet config renderer folder image minsize =
  -- FIXME: this should filter out all sizes greater than actual image
  -- size; this needs knowing the image size in Image. [performance]
  let allSizes = sortOn Down . filter (>= minsize) . cfgAutoImageSizes $ config
      sizes = map (\size -> sformat (stext % " " % int % "w")
                            (renderer (ImageBytesR folder image size))
                            size
                  ) allSizes
      sizes' = renderer (ImageBytesR folder image 0):sizes
  in Text.intercalate ", " $ reverse sizes'

imageBytesNoStyle :: Config -> Int -> UrlParams -> Text -> Image -> Widget
imageBytesNoStyle config imagesize params folder img = do
  renderP <- getUrlRenderParams
  render <- getUrlRender
  let image = imgName img
      viewurl = renderP (ViewR folder image) params
      infourl = renderP (ImageR folder image) params
  case bestMovie img of
    Just f ->
      toWidget [hamlet|<a href=@?{(MovieBytesR folder image, params)}
                         class="fbox-item"
                         data-type="video"
                         data-video-format=#{fileMimeType "video/mp4" f}
                         data-viewurl="#{viewurl}"
                         data-infourl="#{infourl}"
                         >
                         <img
                           .grid-item-image
                           src="@{ImageBytesR folder image imagesize}"
                           >
                           <span class="fas fa-file-video fa-2x icon-overlay"></span>
                           |]
    Nothing -> do
      let srcset = imageSrcSet config render folder image imagesize
      toWidget [hamlet|<a href=@{ImageBytesR folder image 0}
                         class="fbox-item"
                         data-type="image"
                         data-viewurl="#{viewurl}"
                         data-infourl="#{infourl}"
                         data-srcset="#{srcset}"
                         data-sizes="100vw"
                         >
                         <img
                           .grid-item-image
                           src="@{ImageBytesR folder image imagesize}"
                           >
                           |]

generatePrevNext :: (Ord k) => k -> Set k -> (k -> Route App) -> Widget
generatePrevNext k m r = do
  let prevRoute = r <$> Set.lookupLT k m
      nextRoute = r <$> Set.lookupGT k m
  $(widgetFile "prevnext")

imageList :: Int -> UrlParams -> Bool -> Bool -> [Image] -> Widget
imageList thumbsize params showParent hideStatus images = do
  let exifs = map imgExif images
      cameras = countItems . map exifCamera $ exifs
      numCameras = length cameras
      lenses = countItems . map exifLens $ exifs
      numLenses = length lenses
      sortColumns = toJSON $
                    if showParent
                      then [[1, 0], [3, 0], [2, 0]]
                      else [[2, 0], [1, 0]]::[[Int]]
  $(widgetFile "imagelist")

imageGrid :: Config -> Int -> UrlParams -> [Image] -> Widget
imageGrid config imagesize params images =
  [whamlet|
     <div .grid>
       <div .grid-sizer .col-12 .col-lg-6 .col-xl-4 .p-0>
       $forall img@Image{imgParent=p} <- images
         <div .grid-item .col-12 .col-lg-6 .col-xl-4 .p-0>
           ^{imageBytesNoStyle config imagesize params p img}
      |]

folderGrid :: Int -> UrlParams -> Atom -> [PicDir] -> Widget
folderGrid imagesize params atom dirs =
  [whamlet|
    <div .grid>
      <div .grid-sizer .col-12 .col-lg-6 .col-xl-4 .p-0>
      $forall d <- dirs
        <div .grid-item .col-12 .col-lg-6 .col-xl-4 .p-0>
          ^{folderCover imagesize True params atom d}
          |]

showExif :: Exif -> Widget
showExif Exif {..} = do
  let create_date = sformat shown <$> exifCreateDate
      f1 = fixed 1
      focal_length =
        case (exifFocalLength, exifFL35mm) of
          (Nothing, Nothing) -> "unknown focal length"
          (Just fn, Nothing) -> sformat (f1 % "mm (unknown equiv.)") fn
          (Nothing, Just ff) -> sformat (f1 % "mm (FF)") ff
          (Just fn, Just ff) ->
            if fn == ff
              then sformat (f1 % "mm (FF)") fn
              else sformat (f1 % "mm (" % f1 % "mm equiv.)") fn ff
      aperture = maybe "f/?" (sformat ("f/" % f1)) exifAperture
      sspeed = fromMaybe "?" exifSSpeedDesc <> "s"
      shutterCount = sformat int <$> exifShutterCount
      iso = "ISO " ++ maybe "unknown" show exifISO
      rating = maybe "unrated" (\v -> if v == 0
                                 then "unrated"
                                 else sformat ("rated " % int % " stars") v
                               ) exifRating
  -- TODO: serial field, links to camera/lens?, move capture time earlier.
  $(widgetFile "exif")

showMaybeField :: Text -> Maybe Text -> Widget
showMaybeField r Nothing   = toWidget [hamlet|<i>#{r}|]
showMaybeField _ (Just "") = toWidget [hamlet|<i>empty|]
showMaybeField _ (Just v)  = toWidget [hamlet|#{v}|]

showSetField :: Set Text -> Widget
showSetField (Set.null -> True) =
  toWidget [hamlet|<i>empty|]
showSetField v =
  toWidget [hamlet|#{Text.intercalate ", " (Set.toList v)}|]

showListWithHeader :: Text -> [Text] -> Widget
showListWithHeader txt [] =
  toWidget [hamlet|
                  <tr>
                    <td>#{txt}
                    <td><i>none
           |]
showListWithHeader txt (x:xs) =
  toWidget [hamlet|
                  <tr>
                    <td colspan=#{length xs}>#{txt}
                    <td>#{x}
                  $forall w <- xs
                    <tr>
                      <td>#{w}
            |]

noImageSearchPossible :: Text -> Widget
noImageSearchPossible search_string =
  toWidget [hamlet|
       <div .card .border-danger>
         <div .card-header>
           No image search possible
         <div .card-body>
           The filter <i>#{search_string}</i>
           is not able to match files. How did you end up on this page?
           |]

noItemsFound :: Text -> Bool -> Widget
noItemsFound filter_desc images =
  let what = if images then "images" else "folders"::Text
  in toWidget [hamlet|
       <div .card .border-warning>
         <div .card-header>
           Nothing found
         <div .card-body>
           The filter <i>#{filter_desc}</i>
           doesn't match any #{what}.
           |]
