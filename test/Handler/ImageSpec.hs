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

module Handler.ImageSpec (spec) where

import           TestImport

import           Handler.Image (getImageR)

spec :: Spec
spec = parallel $ withApp $ do
  it "loads an image page and checks it looks right" $ do
    login
    checkNotFound $ ImageR "foo" "bar"
  it "shows the full paths and the record to logged in users" $ do
    dir <- addScannedFolder "2020-01-01-trip" ["a.jpg"]
    login
    get $ ImageR "2020-01-01-trip" "a"
    statusIs 200
    bodyContains $ dir </> "a.jpg"
    bodyContains "imgName"
  it "shows the exif warnings to logged in users" $ do
    injectProblemImage
    login
    get $ ImageR "test" "a"
    statusIs 200
    bodyContains problemWarning
  it "doesn't show the exif warnings to anonymous users" $ do
    injectProblemImage
    page <- anonymousPage [] (getImageR "test" "a")
    liftIO $ do
      page `shouldNotSatisfy` (problemWarning `isInfixOf`)
      page `shouldSatisfy` ("MIME type" `isInfixOf`)
  it "links a composite image and its components both ways" $ do
    _ <- addScannedFolder "2020-01-02-pano" ["d_01.jpg", "d_02.jpg", "d_01-03.jpg"]
    login
    get $ ImageR "2020-01-02-pano" "d_01-03"
    statusIs 200
    bodyContains "Composite image of"
    htmlAnyContain "a" "d_01"
    htmlAnyContain "a" "d_02"
    -- d_03 doesn't exist: named, not linked.
    htmlNoneContain "a" "d_03"
    htmlAnyContain "span.text-muted" "d_03"
    get $ ImageR "2020-01-02-pano" "d_02"
    statusIs 200
    bodyContains "Component of"
    htmlAnyContain "a" "d_01-03"
  it "lists only the first names of a long range" $ do
    _ <- addScannedFolder "2020-01-03-stack" ["s_01-15.jpg"]
    login
    get $ ImageR "2020-01-03-stack" "s_01-15"
    statusIs 200
    htmlAnyContain "span.text-muted" "s_10"
    htmlNoneContain "span.text-muted" "s_11"
    bodyContains "and 5 more"
