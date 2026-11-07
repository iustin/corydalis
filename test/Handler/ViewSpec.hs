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

module Handler.ViewSpec (spec) where

import           Handler.View
import           Pics
import           TestImport

import           Data.Aeson
import qualified Data.ByteString                as BS
import qualified Data.Text.Short                as TS
import           Network.HTTP.Types.Header      (HeaderName)
import           Network.Wai.Test               (simpleBody, simpleHeaders)
import           System.Directory               (createDirectoryIfMissing)

import           Text.Blaze.Svg.Renderer.String

-- | A 1x1 pixel TIFF; TIFF is both a raw format and directly viewable,
-- so it is served as is.
--
-- Generated with ImageMagick 7.1.1-43:
--
-- > convert -size 1x1 xc:white -strip -compress none tiff:- | od -An -v -tx1
minimalTiff :: ByteString
minimalTiff = BS.pack
  [ 0x49, 0x49, 0x2a, 0x00, 0x0a, 0x00, 0x00, 0x00, 0xff, 0xff, 0x0f, 0x00
  , 0x00, 0x01, 0x03, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00
  , 0x01, 0x01, 0x03, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00
  , 0x02, 0x01, 0x03, 0x00, 0x01, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00
  , 0x03, 0x01, 0x03, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00
  , 0x06, 0x01, 0x03, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00
  , 0x0a, 0x01, 0x03, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00
  , 0x11, 0x01, 0x04, 0x00, 0x01, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00
  , 0x12, 0x01, 0x03, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00
  , 0x15, 0x01, 0x03, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00
  , 0x16, 0x01, 0x03, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00
  , 0x17, 0x01, 0x04, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00
  , 0x1c, 0x01, 0x03, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00
  , 0x29, 0x01, 0x03, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00
  , 0x3e, 0x01, 0x05, 0x00, 0x02, 0x00, 0x00, 0x00, 0xf4, 0x00, 0x00, 0x00
  , 0x3f, 0x01, 0x05, 0x00, 0x06, 0x00, 0x00, 0x00, 0xc4, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x85, 0xeb, 0x51, 0x00, 0x00, 0x00, 0x80, 0x00
  , 0xc3, 0xf5, 0xa8, 0x00, 0x00, 0x00, 0x00, 0x02, 0xcd, 0xcc, 0x4c, 0x00
  , 0x00, 0x00, 0x00, 0x01, 0xcd, 0xcc, 0x4c, 0x00, 0x00, 0x00, 0x80, 0x00
  , 0xcd, 0xcc, 0x4c, 0x00, 0x00, 0x00, 0x00, 0x02, 0x8f, 0xc2, 0xf5, 0x00
  , 0x00, 0x00, 0x00, 0x10, 0x37, 0x1a, 0xa0, 0x00, 0x00, 0x00, 0x00, 0x02
  , 0x2b, 0x87, 0x0a, 0x00, 0x00, 0x00, 0x20, 0x00
  ]

-- | The folder holding the untrusted files.
untrustedFolder :: ShortText
untrustedFolder = "2020-01-01-untrusted"

-- | Creates a folder with files whose contents don't match their names,
-- as a malicious user might, and rescans the repository.
addUntrustedFolder :: YesodExample App ()
addUntrustedFolder = do
  liftIO requireExifTool
  ctx <- appContext <$> getTestYesod
  -- Folders are two levels below the base directory; the name of the
  -- intermediate level is not used.
  let dir = fromMaybe (error "No output directory")
              (headMay (cfgOutputDirs (ctxConfig ctx))) </> "2020" </>
              TS.toString untrustedFolder
  liftIO $ do
    -- Wait for the initial scan, so that it doesn't miss the new folder.
    _ <- waitForScan ctx
    createDirectoryIfMissing True dir
    BS.writeFile (dir </> "photo.jpg")
      "<svg xmlns=\"http://www.w3.org/2000/svg\"><script>alert(1)</script></svg>"
    BS.writeFile (dir </> "clip.mp4")
      "<!DOCTYPE html><html><body><script>alert(1)</script></body></html>"
    BS.writeFile (dir </> "scan.tif") minimalTiff
    _ <- launchScanFileSystem ctx
    void $ waitForScan ctx

-- | Returns the values of a header in the last response.
responseHeader :: HeaderName -> YesodExample App [ByteString]
responseHeader name = withResponse $ \response ->
  return [ value | (header, value) <- simpleHeaders response
                 , header == name ]

-- | Checks the type and the security headers of a raw contents response.
checkRawContents :: ByteString -> YesodExample App ()
checkRawContents ctype = do
  statusIs 200
  responseHeader "Content-Type" >>= liftIO . (`shouldBe` [ctype])
  responseHeader "X-Content-Type-Options" >>= liftIO . (`shouldBe` ["nosniff"])
  policies <- responseHeader "Content-Security-Policy"
  liftIO $ policies `shouldSatisfy` elem "sandbox"

spec :: Spec
spec = parallel $ do
  describe "Internal functionality" $ do
    describe "SVG tests" $ do
      it "generates valid SVG with error message" $ do
        let svg = basicSvg "Test error message"
            rendered = renderSvg svg
        rendered `shouldContain` "Test error message"
        rendered `shouldContain` "<svg"
        rendered `shouldContain` "viewBox=\"0 0 600 800\""

    describe "ImageInfo JSON serialization" $ do
      it "serializes ImageInfo correctly" $ do
        let imgInfo = ImageInfo
              { iiInfoUrl = "/info"
              , iiBytesUrl = "/bytes"
              , iiMovieUrl = Nothing
              , iiViewUrl = "/view"
              , iiFlagUrl = "/flag"
              , iiListUrl = "/list"
              , iiBrowseUrl = "/browse"
              , iiName = ImageName "test.jpg"
              , iiTransform = (0, False, False)
              , iiMatrix = (1.0, 0.0, 0.0, 1.0)
              , iiExifHtml = "<p>Exif data</p>"
              }
        toJSON imgInfo `shouldNotBe` Null

  describe "handler tests" $ withApp $ do
    describe "checks 404 results" $
      forM_ [ ViewR "foo" "bar"
            , ImageInfoR "foo" "bar"
            , ImageBytesR "foo" "bar"
            , MovieBytesR "foo" "bar"
            , RandomImageInfoR
            ] $ \ route ->
       it ("checks route " ++ show route) $ do
         login
         checkNotFound route
    describe "checks the view page" $ do
      it "loads the view page and checks it looks right" $
        const pending
    describe "returns correct error on non-viewable items" $
      forM_ [("ImageBytesR", ImageBytesR), ("MovieBytesR", MovieBytesR)] $ \(routeName, route) ->
        it ("checks route" ++ routeName) $ do
          app <- getTestYesod
          let ctx = appContext app
          -- first make sure that scan is finished, so that it won't override our artificial image.
          _ <- liftIO $ launchScanFileSystem ctx
          _ <- liftIO $ waitForScan ctx
          let img = simpleUntrackedImage (ctxConfig ctx) "test" "a"
          injectImages [img]
          login
          get $ route "test" (ImageName "a")
          statusIs 200
          bodyContains "Error:"
          bodyContains "<svg"
          bodyContains "Image has no viewable version"
    describe "serves untrusted file contents safely" $ do
      it "does not serve an SVG image as SVG" $ do
        addUntrustedFolder
        login
        get $ ImageBytesR untrustedFolder "photo"
        checkRawContents "image/jpeg"
      it "does not serve an SVG image as SVG for a too small size" $ do
        addUntrustedFolder
        login
        -- Smaller than all configured sizes, so the original is returned.
        request $ do
          setUrl $ ImageBytesR untrustedFolder "photo"
          addGetParam "res" "1"
        checkRawContents "image/jpeg"
      it "does not serve an HTML movie as HTML" $ do
        addUntrustedFolder
        login
        get $ MovieBytesR untrustedFolder "clip"
        checkRawContents "video/mp4"
      it "still serves a real image with its own type" $ do
        addUntrustedFolder
        login
        get $ ImageBytesR untrustedFolder "scan"
        checkRawContents "image/tiff"
        withResponse $ \response ->
          liftIO $ simpleBody response `shouldBe` fromStrict minimalTiff
      it "sandboxes error responses too" $ do
        login
        get $ ImageBytesR "foo" "bar"
        statusIs 404
        policies <- responseHeader "Content-Security-Policy"
        liftIO $ policies `shouldSatisfy` elem "sandbox"
