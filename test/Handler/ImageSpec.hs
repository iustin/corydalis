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
    htmlAnyContain "#composite-of" "d_01 to d_03 (3 names)"
    htmlCount "#components a" 2
    htmlAnyContain "#components a" "images/d_01"
    htmlAnyContain "#components a" "images/d_02"
    -- d_03 doesn't exist: only in the range.
    htmlNoneContain "#components" "d_03"
    htmlCount "#component-of" 0
    get $ ImageR "2020-01-02-pano" "d_02"
    statusIs 200
    htmlCount "#component-of a" 1
    htmlAnyContain "#component-of a" "images/d_01-03"
    htmlCount "#composite-of" 0
    htmlCount "#components" 0
  it "shows a composite image without components" $ do
    _ <- addScannedFolder "2020-01-03-stack" ["s_01-15.jpg"]
    login
    get $ ImageR "2020-01-03-stack" "s_01-15"
    statusIs 200
    htmlAnyContain "#composite-of" "s_01 to s_15 (15 names)"
    htmlCount "#components a" 0
    htmlAnyContain "#components" "none"
  it "lists only the first components of a composite image" $ do
    let components = ["s_" <> pad (n :: Int) <> ".jpg" | n <- [1..15]]
        pad n = (if n < 10 then "0" else "") <> show n
    _ <- addScannedFolder "2020-01-05-stack" ("s_01-15.jpg" : components)
    login
    get $ ImageR "2020-01-05-stack" "s_01-15"
    statusIs 200
    htmlCount "#components a" 10
    htmlAnyContain "#components a" "images/s_10"
    htmlNoneContain "#components a" "s_11"
    htmlAnyContain "#components" "and 5 more"
  it "lists only the first composite images covering a component" $ do
    let composites = ["a_01-" <> show (n :: Int) <> ".jpg" | n <- [10..20]]
    _ <- addScannedFolder "2020-01-04-many" ("a_01.jpg" : composites)
    login
    get $ ImageR "2020-01-04-many" "a_01"
    statusIs 200
    htmlCount "#component-of a" 10
    htmlAnyContain "#component-of" "and 1 more"
