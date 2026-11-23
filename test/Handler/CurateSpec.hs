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

module Handler.CurateSpec (spec) where

import           TestImport

import           Exif       (Exif (..))
import           Handler.Curate (getCurateR)
import           Pics       (Image (..))

-- | A picture taken with a camera whose name contains markup.
cameraWithMarkup :: Config -> Image
cameraWithMarkup config =
  let img = simpleRawImage config
  in img { imgExif = (imgExif img) { exifCamera = Just (mkSym "<b>Nikon</b>") } }

-- | Requests the given route's JSON (the charts' data).
getJson :: Route App -> YesodExample App ()
getJson route = request $ do
  setUrl route
  addRequestHeader ("Accept", "application/json")

spec :: Spec
spec = parallel $ withApp $ do
  it "escapes the names shown on the charts" $ do
    ctx <- appContext <$> getTestYesod
    injectImages [cameraWithMarkup (ctxConfig ctx)]
    login
    getJson CurateR
    statusIs 200
    bodyContains "&lt;b&gt;Nikon&lt;/b&gt;"
    bodyNotContains "<b>Nikon"
  it "escapes the names shown on the camera statistics' charts" $ do
    ctx <- appContext <$> getTestYesod
    injectImages [cameraWithMarkup (ctxConfig ctx)]
    login
    getJson CameraStatsR
    statusIs 200
    bodyContains "&lt;b&gt;Nikon&lt;/b&gt;"
    bodyNotContains "<b>Nikon"
  it "groups the cameras past the top ten as others" $ do
    config <- ctxConfig . appContext <$> getTestYesod
    injectImagesAfterScan
      [ exifImage config "curate" (tshow n) $ \e ->
          e { exifCamera = Just (mkSym ("Camera" <> tshow n)) }
      | n <- [1 .. 12 :: Int] ]
    login
    getJson CurateR
    statusIs 200
    bodyContains "\"Others\""
    getJson CameraStatsR
    statusIs 200
    bodyContains "\"others\""
  it "loads the empty curate page and checks it looks right" $ do
    login
    checkRoute CurateR
    htmlAllContain "h1" "Corydalis"
    htmlCount "div#images" 0
    htmlNoneContain "div#images" "contains 0 images"
  it "shows the problems to logged in users" $ do
    injectProblemImage
    login
    checkRoute CurateR
    bodyContains problemWarning
  it "doesn't show the problems to anonymous users" $ do
    injectProblemImage
    page <- anonymousPage [] getCurateR
    liftIO $ do
      page `shouldNotSatisfy` (problemWarning `isInfixOf`)
      page `shouldNotSatisfy` ("/list/problems" `isInfixOf`)
      page `shouldNotSatisfy` ("No pictures with problems" `isInfixOf`)
      page `shouldSatisfy` ("Flash mode" `isInfixOf`)
