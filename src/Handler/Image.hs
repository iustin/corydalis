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
{-# LANGUAGE NoCPP                 #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Image
  ( getImageR
  ) where

import qualified Data.Text.Short    as TS
import           Text.Pretty.Simple


import           Exif
import           Handler.Utils
import           Handler.Widgets
import           Import
import           Repository
import           Repository.Names    (rangeNames)

-- | How many names the image page lists, at most, for the names of a
-- composite image's range or the composite images covering a
-- component.
linkListCap :: Int
linkListCap = 10

-- | The names of a list shown on the image page: the first
-- 'linkListCap' ones, each with whether it is the last one shown (to
-- separate them), and how many more there are.
cappedList :: [a] -> ([(a, Bool)], Int)
cappedList xs =
  let (firstNames, rest) = splitAt linkListCap xs
  in (zip firstNames [i == length firstNames | i <- [1..]], length rest)

getImageR :: ShortText -> ImageName -> Handler Html
getImageR folder iname = do
  dir <- getFolder folder
  img <- getFolderImage dir iname
  params <- getParams
  -- The record holds the full paths of the files.
  showRecord <- hasAccessToStoragePaths
  let images = pdTimeSort dir
      tkey = imageTimeKey img
      flags = if flagsSoftMaster (imgFlags img)
                 then "soft master"::Text
                 else "(none)"
      lens = exifLens exif
      exif = imgExif img
      -- The names of a composite image's range, each with whether it
      -- exists, i.e. is a component.
      (rangeShown, rangeMore) =
        case imgRole img of
          CompositeOf range components ->
            cappedList [ (name, name `elem` components)
                       | name <- rangeNames range ]
          _ -> ([], 0)
      -- The composite images covering a component.
      (coveringShown, coveringMore) =
        case imgRole img of
          ComponentOf composites -> cappedList (toList composites)
          _                      -> ([], 0)
  defaultLayout $ do
    setHtmlTitle $ "image " <> TS.toText folder <> "/" <> (TS.toText . unImageName . imgName $ img)
    $(widgetFile "image")
