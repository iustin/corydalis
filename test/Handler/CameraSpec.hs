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

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.CameraSpec (spec) where

import           Exif      (Exif (..), LensInfo (..), unknownLens)
import           TestImport

-- | A lens with the given name.
lensNamed :: Text -> LensInfo
lensNamed name = unknownLens { liName = mkSym name }

-- | Adds pictures taken with camera @TestCam@ (and one without a
-- camera).
addCameraImages :: YesodExample App ()
addCameraImages = do
  config <- ctxConfig . appContext <$> getTestYesod
  let shot folder name lens date shutter =
        exifImage config folder name $ \e ->
          e { exifCamera = Just (mkSym "TestCam")
            , exifLens = lensNamed lens
            , exifCreateDate = date
            , exifShutterCount = shutter
            }
  injectImages
    [ shot "cam" "a" "LensA" (Just (exifDate 2020 1 2)) (Just 100)
    , shot "cam" "b" "LensA" (Just (exifDate 2021 3 4)) (Just 103)
    , shot "cam" "c" "LensB" (Just (exifDate 2019 5 6)) Nothing
    , shot "cam" "d" "LensB" Nothing Nothing
    , exifImage config "cam" "e" id
    ]

-- | Requests the given route's JSON (the charts' data).
getJson :: Route App -> YesodExample App ()
getJson route = request $ do
  setUrl route
  addRequestHeader ("Accept", "application/json")

spec :: Spec
spec = parallel $ withApp $ do
  it "loads the camera stats page and checks it looks right" $ do
    login
    checkRoute CameraStatsR
    htmlAllContain "h1" "Camera statistics"
    htmlAnyContain "p" "No camera information found."
  it "load a wrong camera details page" $ do
    login
    checkNotFound $ CameraInfoR "foo"
  it "load a camera details page" $ do
    addCameraImages
    login
    checkRoute $ CameraInfoR "TestCam"
    htmlAllContain "h1" "Camera statistics"
    -- Shutter count range and keeper rate: 4 TestCam images over the
    -- shutter counts 100-103 (only two of them have one).
    htmlAnyContain "td" "100 - 103"
    htmlAnyContain "td" "100.00%"
    htmlAnyContain "th" "Lens (2 in total)"
    htmlAnyContain "td" "LensA"
    htmlAnyContain "td" "LensB"
    -- Two lenses, so the dates show the lens too.
    htmlAnyContain "td" "(LensB)"
    htmlAnyContain "td" "(LensA)"
    bodyContains "2019-05-06"
    bodyContains "2021-03-04"
  it "loads the camera details page for unknown cameras" $ do
    addCameraImages
    login
    checkRoute $ CameraInfoR "unknown"
    htmlAnyContain "th" "Lens (1 in total)"
    htmlAnyContain "tr" "No available image date information"
  it "returns the camera details charts' data" $ do
    addCameraImages
    login
    getJson $ CameraInfoR "TestCam"
    statusIs 200
    bodyContains "\"lensapfl\""
    bodyContains "\"trends\""
