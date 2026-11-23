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

module Handler.LensSpec (spec) where

import           Exif      (Exif (..), LensInfo (..), unknownLens)
import           TestImport

-- | Adds pictures taken with lens @TestLens@ (on two cameras, and once
-- without a camera).
addLensImages :: YesodExample App ()
addLensImages = do
  config <- ctxConfig . appContext <$> getTestYesod
  let shot folder name camera date =
        exifImage config folder name $ \e ->
          e { exifCamera = mkSym <$> camera
            , exifLens = unknownLens { liName = mkSym "TestLens"
                                     , liSpec = mkSym "TestSpec" }
            , exifCreateDate = date
            }
  injectImages
    [ shot "lens" "a" (Just "CamA") (Just (exifDate 2020 1 2))
    , shot "lens" "b" (Just "CamA") Nothing
    , shot "lens" "c" (Just "CamB") (Just (exifDate 2018 7 8))
    , shot "lens" "d" Nothing (Just (exifDate 2022 9 10))
    ]

spec :: Spec
spec = parallel $ withApp $ do
  it "loads the lens page and checks it looks right" $ do
    login
    checkRoute LensStatsR
    htmlAllContain "h1" "Lens statistics"
  it "loads a wrong lens info page and returns not found" $ do
    login
    checkNotFound $ LensInfoR "foo"
  it "load a lens info page and checks it looks right" $ do
    addLensImages
    login
    checkRoute $ LensInfoR "TestLens"
    htmlAllContain "h1" "Lens statistics"
    htmlAnyContain "td" "TestSpec"
    -- Images without a camera are not counted as a camera.
    htmlAnyContain "th" "Camera (2 in total)"
    htmlAnyContain "td" "CamA"
    htmlAnyContain "td" "CamB"
    -- But their dates are used, with an unknown camera.
    htmlAnyContain "td" "2018-07-08"
    htmlAnyContain "td" "(CamB)"
    htmlAnyContain "td" "2022-09-10"
    htmlAnyContain "td" "(unknown)"
  it "returns the lens info charts' data" $ do
    addLensImages
    login
    request $ do
      setUrl $ LensInfoR "TestLens"
      addRequestHeader ("Accept", "application/json")
    statusIs 200
    bodyContains "\"lensapfl\""
    bodyContains "\"trends\""
