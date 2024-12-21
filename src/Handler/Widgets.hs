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

import           Data.Aeson.Text     (encodeToLazyText)
import qualified Data.Map            as Map
import           Data.Ord
import qualified Data.Set            as Set
import qualified Data.Text           as Text
import           Data.Time.Calendar  (addDays, diffGregorianDurationClip)
import           Formatting

import           Data.Time.LocalTime (LocalTime (localDay))
import           Exif
import           Handler.Utils
import           Import
import           Indexer
import           Pics
import           Stats               (DateRange)

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
      link_wrapper = if atomFindsFiles atom
        then imageLinkWrapper
        else folderLinkWrapper
  case images' of
    []    -> if browsing
             then toWidget [hamlet|<div .card .mx-1 .my-1>
                                    <div .card-header>
                                      #{name}
                                    <div .card-body>
                                      Folder '#{name}' contains no viewable images
                                      |]
             else toWidget [hamlet|<span .disabled>N/A|]
    img:_ -> link_wrapper img params $ bytes_fn size img

-- | Wraps a widget in a link to an image.
imageLinkWrapper :: Image -> [(Text, Text)] -> Widget -> Widget
imageLinkWrapper Image{imgParent = folder, imgName = image} params w =
  [whamlet|<a href=@?{(ViewR folder image, params)}>^{w}|]

-- | Wraps a widget in a link to a folder.
folderLinkWrapper :: Image -> [(Text, Text)] -> Widget -> Widget
folderLinkWrapper Image{imgParent = folder} params w =
  [whamlet|<a href=@?{(FolderR folder, params)}>^{w}|]

imageBytes :: Int -> Image -> Widget
imageBytes thumbsize Image{imgParent = folder, imgName = image} =
  toWidget [hamlet|<img
                      src="@?{imageBytesAtRes folder image thumbsize}"
                      width=#{thumbsize} height=#{thumbsize}
                      loading="lazy"
                      >|]

imageBytesForFolder :: Int -> Image -> Widget
imageBytesForFolder size Image{imgParent = folder, imgName = image} =
  toWidget [hamlet|<img
                      .grid-item-image
                       src="@?{imageBytesAtRes folder image size}"
                       >|]

-- | Generates srcset for an image based on all auto-built versions.
imageSrcSet :: Config -> (Route App -> [(Text, Text)] -> Text) -> Text -> ImageName -> Int -> Text
imageSrcSet config renderer folder image minsize =
  -- FIXME: this should filter out all sizes greater than actual image
  -- size; this needs knowing the image size in Image. [performance]
  let allSizes = sortOn Down . filter (>= minsize) . Set.toList . cfgAutoImageSizes $ config
      sizes = map (\size -> sformat (stext % " " % int % "w")
                            (renderer (ImageBytesR folder image) (makeResParam size))
                            size
                  ) allSizes
      sizes' = renderer (ImageBytesR folder image) []:sizes
  in Text.intercalate ", " $ reverse sizes'

imageBytesNoStyle :: Config -> Int -> UrlParams -> Text -> Image -> Widget
imageBytesNoStyle config imagesize params folder img = do
  renderP <- getUrlRenderParams
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
                           src="@?{imageBytesAtRes folder image imagesize}"
                           >
                           <span class="fa-solid fa-file-video fa-2x icon-overlay"></span>
                           |]
    Nothing -> do
      let srcset = imageSrcSet config renderP folder image imagesize
      toWidget [hamlet|<a href=@{ImageBytesR folder image}
                         class="fbox-item"
                         data-type="image"
                         data-viewurl="#{viewurl}"
                         data-infourl="#{infourl}"
                         data-srcset="#{srcset}"
                         data-sizes="100vw"
                         >
                         <img
                           .grid-item-image
                           src="@?{imageBytesAtRes folder image imagesize}"
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
      sortColumnsEnc = encodeToLazyText sortColumns
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
  let create_date = showExifTime <$> exifCreateDate
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
      flash_source = formatFlashSource (fiSource exifFlashInfo)
      flash_mode   = fromMaybe "unknown" (fiMode exifFlashInfo)
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

imageFlagActions :: Text -> ImageName -> Bool -> Widget
imageFlagActions folder image flag =
  toWidget [hamlet|
            <form action=@{ImageFlagR folder image} method=post style="display:inline-block">
              $if flag
                <button .btn .btn-light type="submit" name="_method" value="PUT">Flag
              <button .btn .btn-light type="submit" name="_method" value="DELETE">Un-flag
              |]

imageViewActions :: Text -> ImageName -> UrlParams -> Widget
imageViewActions folder image params =
  toWidget [hamlet|
            <a href=@?{(ViewR folder image, params)} title="View image in Corydalis">
              <span .far .fa-image .fa-lg>#
            <a href=@{ImageBytesR folder image} target=_blank title="View image in the browser">
              <span .fa-solid .fa-eye .fa-lg>#
            <a href=@{ImageBytesR folder image} download=#{image} title="Download image">
              <span .fa-solid .fa-download .fa-lg>#
           |]

movieViewActions :: Image -> UrlParams -> Widget
movieViewActions img params = do
  let folder = imgParent img
      image = imgName img
      download = maybe "" Pics.fileName (bestMovie img)
  toWidget [hamlet|
            <a href=@?{(ViewR folder image, params)} title="View movie in Corydalis">
              <span .far .fa-image .fa-lg>#
            <a href=@{MovieBytesR folder image} target=_blank title="Play movie in the browser">
              <span .fa-solid .fa-eye .fa-lg>#
            <a href=@{MovieBytesR folder image} download=#{download} title="Download the movie">
              <span .fa-solid .fa-download .fa-lg>#
           |]

actionsWidget :: UrlParams -> Image -> Widget
actionsWidget params img =
  case imgType img of
    MediaImage   -> imageViewActions (imgParent img) (imgName img) params
    MediaMovie   -> movieViewActions img params
    MediaUnknown -> toWidget [hamlet|<p>No actions for files of unknown type 😞|]

showCameraLink :: Maybe Text -> Widget
showCameraLink (Just camera) =
  toWidget [hamlet|<a href=@{CameraInfoR camera}>#{camera}|]
showCameraLink Nothing =
  toWidget [hamlet|#{unknown}|]

showDateRange :: Text -> Maybe DateRange -> Widget
showDateRange _ Nothing =
  toWidget [hamlet|No date information.|]
showDateRange kind (Just (begin, end)) =
  let formatter = showLocalDateWithFormat "%A, %B %d %Y"
      begin' = localDay begin
      end' = localDay end
  in toWidget
   [hamlet|
            $if begin' == end'
              This #{kind} contains pictures taken on
              #{formatter begin'}.
            $else
              This #{kind} contains pictures taken between
              #{formatter begin'} and
              #{formatter end'} (#{showDaysDuration (diffGregorianDurationClip (addDays 1 end') begin')}).
           |]
