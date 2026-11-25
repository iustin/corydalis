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
import           Repository
import           Repository.Scan
import           Repository.State
import           TestImport

import           Data.Aeson
import qualified Data.Aeson                     as Aeson (Key)
import           Data.Aeson.Types               (parseMaybe)
import qualified Data.ByteString                as BS
import qualified Data.Text.Short                as TS
import           Network.HTTP.Types.Header      (HeaderName)
import           Network.Wai.Test               (simpleBody, simpleHeaders)
import           System.Directory               (createDirectoryIfMissing,
                                                 removeFile)
import           System.Posix.Files             (createSymbolicLink)

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

-- | The directory of the folder holding the untrusted files.
untrustedDir :: Ctx -> FilePath
untrustedDir ctx =
  -- Folders are two levels below the base directory; the name of the
  -- intermediate level is not used.
  fromMaybe (error "No output directory")
    (headMay (cfgOutputDirs (ctxConfig ctx))) </> "2020" </>
    TS.toString untrustedFolder

-- | Replaces the given untrusted files with symbolic links to a file
-- outside the picture folders, as if done after the scan.
replaceWithSymlinks :: [FilePath] -> YesodExample App ()
replaceWithSymlinks names = do
  ctx <- appContext <$> getTestYesod
  let dir = untrustedDir ctx
      secret = cfgCacheDir (ctxConfig ctx) </> "secret.txt"
  liftIO $ do
    BS.writeFile secret "top secret"
    forM_ names $ \name -> do
      removeFile (dir </> name)
      createSymbolicLink secret (dir </> name)

-- | Creates a folder with files whose contents don't match their names,
-- as a malicious user might, and rescans the repository.
addUntrustedFolder :: YesodExample App ()
addUntrustedFolder = do
  liftIO requireExifTool
  ctx <- appContext <$> getTestYesod
  let dir = untrustedDir ctx
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
    repo <- waitForScan ctx
    -- A failed scan leaves an empty repository, which would otherwise only
    -- show up as puzzling 404s in the tests.
    case repoStatus repo of
      RepoError err -> expectationFailure $ "The scan failed: " ++ unpack err
      _             -> return ()

-- | Returns the values of a header in the last response.
responseHeader :: HeaderName -> YesodExample App [ByteString]
responseHeader name = withResponse $ \response ->
  return [ value | (header, value) <- simpleHeaders response
                 , header == name ]

-- | Checks that the response carries @Cache-Control: private, no-cache@.
checkPrivateNoCache :: YesodExample App ()
checkPrivateNoCache =
  responseHeader "Cache-Control" >>= liftIO . (`shouldBe` ["private, no-cache"])

-- | Checks the type and the security headers of a raw contents response.
checkRawContents :: ByteString -> YesodExample App ()
checkRawContents ctype = do
  statusIs 200
  responseHeader "Content-Type" >>= liftIO . (`shouldBe` [ctype])
  responseHeader "X-Content-Type-Options" >>= liftIO . (`shouldBe` ["nosniff"])
  policies <- responseHeader "Content-Security-Policy"
  liftIO $ policies `shouldSatisfy` elem "sandbox"
  checkPrivateNoCache

-- | Looks up a path of keys in a JSON value.
jsonAt :: [Aeson.Key] -> Value -> Maybe Value
jsonAt []     v = Just v
jsonAt (k:ks) v = parseMaybe (withObject "object" (.: k)) v >>= jsonAt ks

-- | Decodes the body of the last response as JSON.
responseJson :: YesodExample App Value
responseJson = withResponse $ \response ->
  case eitherDecode (simpleBody response) of
    Left err -> liftIO (expectationFailure ("Invalid JSON: " ++ err)) >> return Null
    Right v  -> return v

-- | Checks the value at the given path in a JSON value.
jsonShouldBe :: Value -> [Aeson.Key] -> Value -> YesodExample App ()
jsonShouldBe v path expected = liftIO $ jsonAt path v `shouldBe` Just expected

-- | Adds the images used by the navigation tests: folder @nav-a@ with
-- image @1@, and folder @nav-b@ with images @1@ and @2@.
addNavigationImages :: YesodExample App ()
addNavigationImages = do
  config <- ctxConfig . appContext <$> getTestYesod
  injectImages
               [ exifImage config "nav-a" "1" id
               , exifImage config "nav-b" "1" id
               , exifImage config "nav-b" "2" id
               ]

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

  describe "imageErrorText" $ do
    let details = "convert: unable to open image `/srv/pics/a.jpg'"
    it "shows the details to users with access to storage paths" $
      imageErrorText True details `shouldBe` details
    it "hides the details, and so the paths, from other users" $ do
      let shown' = imageErrorText False details
      shown' `shouldNotSatisfy` ("/srv/pics" `isInfixOf`)
      shown' `shouldNotBe` ""

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
      it "loads the view page and checks it looks right" $ do
        addNavigationImages
        login
        get $ ViewR "nav-b" "1"
        statusIs 200
        htmlAnyContain "title" "image nav-b/1"
        -- The navigation information is embedded in the page.
        bodyContains "/image/info/nav-b/1"
        bodyContains "/image/info/nav-b/2"
      it "returns 404 for an image filtered out by the search" $ do
        addNavigationImages
        login
        request $ do
          setUrl $ ViewR "nav-b" "1"
          addGetParam "folder" "nav-a"
        statusIs 404
    describe "image information" $ do
      it "returns the navigation information for an image" $ do
        addNavigationImages
        login
        get $ ImageInfoR "nav-b" "1"
        statusIs 200
        v <- responseJson
        jsonShouldBe v ["folder"] "nav-b"
        jsonShouldBe v ["image"] "1"
        jsonShouldBe v ["year"] "?"
        jsonShouldBe v ["current", "name"] "1"
        jsonShouldBe v ["current", "info"] "http://localhost:3000/image/info/nav-b/1?all=0"
        jsonShouldBe v ["current", "view"] "http://localhost:3000/view/nav-b/1?all=0"
        jsonShouldBe v ["current", "bytes"] "http://localhost:3000/image/bytes/nav-b/1"
        jsonShouldBe v ["current", "movie"] Null
        jsonShouldBe v ["first", "info"] "http://localhost:3000/image/info/nav-a/1?all=0"
        jsonShouldBe v ["prev", "info"] "http://localhost:3000/image/info/nav-a/1?all=0"
        jsonShouldBe v ["next", "info"] "http://localhost:3000/image/info/nav-b/2?all=0"
        jsonShouldBe v ["last", "info"] "http://localhost:3000/image/info/nav-b/2?all=0"
        jsonShouldBe v ["prevfolder", "info"] "http://localhost:3000/image/info/nav-a/1?all=0"
        jsonShouldBe v ["nextfolder"] Null
      it "renders the image's exif information" $ do
        addNavigationImages
        login
        get $ ImageInfoR "nav-a" "1"
        statusIs 200
        v <- responseJson
        case jsonAt ["current", "exifhtml"] v of
          Just (String html) -> liftIO $ do
            html `shouldSatisfy` ("<b>Camera:</b>" `isInfixOf`)
            html `shouldSatisfy` ("unknown" `isInfixOf`)
          other -> liftIO $ expectationFailure $ "Unexpected exifhtml: " ++ show other
        jsonShouldBe v ["prev"] Null
        jsonShouldBe v ["prevfolder"] Null
        jsonShouldBe v ["nextfolder", "info"] "http://localhost:3000/image/info/nav-b/1?all=0"
      it "keeps the search parameters in the navigation links" $ do
        addNavigationImages
        login
        request $ do
          setUrl $ ImageInfoR "nav-b" "2"
          addGetParam "folder" "nav-b"
        statusIs 200
        v <- responseJson
        -- The search limits the images to the folder.
        jsonShouldBe v ["first", "info"]
          "http://localhost:3000/image/info/nav-b/1?folder=nav-b"
        jsonShouldBe v ["prev", "info"]
          "http://localhost:3000/image/info/nav-b/1?folder=nav-b"
        jsonShouldBe v ["prevfolder"] Null
        jsonShouldBe v ["next"] Null
        jsonShouldBe v ["nextfolder"] Null
        jsonShouldBe v ["current", "info"]
          "http://localhost:3000/image/info/nav-b/2?folder=nav-b"
        -- Raw contents don't depend on the search.
        jsonShouldBe v ["current", "bytes"] "http://localhost:3000/image/bytes/nav-b/2"
      it "returns the information of a random image" $ do
        config <- ctxConfig . appContext <$> getTestYesod
        injectImages [exifImage config "nav-a" "1" id]
        login
        get RandomImageInfoR
        statusIs 200
        v <- responseJson
        -- The random image is linked within all images.
        jsonShouldBe v ["current", "info"]
          "http://localhost:3000/image/info/nav-a/1?all=0"
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
          checkPrivateNoCache
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
      it "does not serve files replaced by symbolic links after the scan" $ do
        addUntrustedFolder
        replaceWithSymlinks ["photo.jpg", "clip.mp4", "scan.tif"]
        login
        forM_ [ImageBytesR untrustedFolder "scan",
               ImageBytesR untrustedFolder "photo",
               MovieBytesR untrustedFolder "clip"] $ \route -> do
          get route
          statusIs 200
          checkPrivateNoCache
          bodyContains "<svg"
          -- The message is wrapped in the image, so only check its start.
          bodyContains "Error: The file has changed since"
          bodyNotContains "top secret"
      it "sandboxes error responses too" $ do
        login
        get $ ImageBytesR "foo" "bar"
        statusIs 404
        checkPrivateNoCache
        policies <- responseHeader "Content-Security-Policy"
        liftIO $ policies `shouldSatisfy` elem "sandbox"
