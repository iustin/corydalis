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
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ExifSpec (spec) where

import           Exif
import           TestImport

import           ClassyPrelude.Yesod
import           Data.Aeson
import           Data.Aeson.Types    (parseMaybe)
import qualified Data.ByteString     as BS
import           Data.Either         (isLeft, isRight)
import qualified Data.Map            as Map
import           Data.Text.Arbitrary ()
import           System.Directory    (findExecutable, listDirectory)
import           Test.QuickCheck

-- | A 1x1 pixel JPEG, which exiftool is able to rewrite.
--
-- Generated with ImageMagick 7.1.1-43:
--
-- > convert -size 1x1 xc:white -strip jpeg:- | od -An -v -tx1
minimalJpeg :: ByteString
minimalJpeg = BS.pack
  [ 0xff, 0xd8, 0xff, 0xe0, 0x00, 0x10, 0x4a, 0x46, 0x49, 0x46, 0x00, 0x01
  , 0x01, 0x00, 0x00, 0x01, 0x00, 0x01, 0x00, 0x00, 0xff, 0xdb, 0x00, 0x43
  , 0x00, 0x03, 0x02, 0x02, 0x02, 0x02, 0x02, 0x03, 0x02, 0x02, 0x02, 0x03
  , 0x03, 0x03, 0x03, 0x04, 0x06, 0x04, 0x04, 0x04, 0x04, 0x04, 0x08, 0x06
  , 0x06, 0x05, 0x06, 0x09, 0x08, 0x0a, 0x0a, 0x09, 0x08, 0x09, 0x09, 0x0a
  , 0x0c, 0x0f, 0x0c, 0x0a, 0x0b, 0x0e, 0x0b, 0x09, 0x09, 0x0d, 0x11, 0x0d
  , 0x0e, 0x0f, 0x10, 0x10, 0x11, 0x10, 0x0a, 0x0c, 0x12, 0x13, 0x12, 0x10
  , 0x13, 0x0f, 0x10, 0x10, 0x10, 0xff, 0xc0, 0x00, 0x0b, 0x08, 0x00, 0x01
  , 0x00, 0x01, 0x01, 0x01, 0x11, 0x00, 0xff, 0xc4, 0x00, 0x14, 0x00, 0x01
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x09, 0xff, 0xc4, 0x00, 0x14, 0x10, 0x01, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0xff, 0xda, 0x00, 0x08, 0x01, 0x01, 0x00, 0x00, 0x3f, 0x00
  , 0x54, 0xdf, 0xff, 0xd9
  ]

-- | Marks the test as pending if exiftool is not installed.
requireExifTool :: IO ()
requireExifTool =
  findExecutable "exiftool" >>=
    maybe (pendingWith "exiftool is not available") (const (return ()))

-- | Creates files in the source directory, and returns the directory.
populateSourceDir :: Config -> [(FilePath, ByteString)] -> IO FilePath
populateSourceDir config files = do
  let dir = fromMaybe (error "No source directory") $
              headMay (cfgSourceDirs config)
  forM_ files $ \(name, contents) -> BS.writeFile (dir </> name) contents
  return dir

noLog :: LogFn
noLog _ _ = return ()

-- Arbitrary instances for testing

instance Arbitrary Orientation where
  arbitrary = chooseEnum (minBound, maxBound)

instance Arbitrary FlashSource where
  arbitrary = chooseEnum (minBound, maxBound)

spec :: Spec
spec = parallel $ do
  describe "Orientation JSON roundtrip" $ do
    -- Orientation has both ToJSON and FromJSON
    prop "toJSON/fromJSON roundtrip" $ \orientation ->
      decode (encode orientation) === Just (orientation :: Orientation)

    it "checks specific values" $ do
      decode (encode OrientationTopLeft) `shouldBe` Just OrientationTopLeft
      decode (encode OrientationRightBot) `shouldBe` Just OrientationRightBot

  describe "FlashSource JSON roundtrip" $ do
    prop "toJSON/fromJSON conversion matches parseFlashSource" $ \flashSource ->
      let encoded = encode flashSource
          asNumber = decode encoded :: Maybe Int
      in case asNumber of
           Just n  -> parseFlashSource n === Just (flashSource :: FlashSource)
           Nothing -> counterexample "Failed to decode as number" False

    it "checks specific mappings" $ do
      decode (encode FlashSourceNone) `shouldBe` Just (0 :: Int)
      decode (encode FlashSourceExternal) `shouldBe` Just (1 :: Int)
      decode (encode FlashSourceInternal) `shouldBe` Just (2 :: Int)

      parseFlashSource (0 :: Int) `shouldBe` Just FlashSourceNone
      parseFlashSource (1 :: Int) `shouldBe` Just FlashSourceExternal
      parseFlashSource (2 :: Int) `shouldBe` Just FlashSourceInternal

    it "checks that Maybe fields become null in JSON" $ do
      let province = mkSymbolizedItem ("CA"::Text)
          exif = def { exifCountry = Nothing, exifProvince = Just province }
          encoded = encode exif
          decoded = decode encoded :: Maybe Value
      case decoded of
        Just val -> do
            parseMaybe (withObject "test" (.: "country")) val `shouldBe` Just Null
            parseMaybe (withObject "test" (.: "province")) val `shouldBe` Just (String "CA")
        _ -> expectationFailure "Failed to decode as Value"

  describe "Orientation helpers" $ do
    it "maps orientations to transforms" $ do
      transformParams (affineTransform OrientationTopLeft)  `shouldBe` (0, False, False)
      transformParams (affineTransform OrientationTopRight) `shouldBe` (0, True, False)
      transformParams (affineTransform OrientationBotRight) `shouldBe` (0, True, True)
      transformParams (affineTransform OrientationBotLeft)  `shouldBe` (0, False, True)
      transformParams (affineTransform OrientationLeftTop)  `shouldBe` (-1, False, True)
      transformParams (affineTransform OrientationRightTop) `shouldBe` (1, False, False)
      transformParams (affineTransform OrientationRightBot) `shouldBe` (1, False, True)
      transformParams (affineTransform OrientationLeftBot)  `shouldBe` (-1, False, False)

    it "formats transform parameters and matrices" $ do
      let approx (a, b, c, d) (x, y, z, w) =
            and [ abs (a - x) < 1e-9
                , abs (b - y) < 1e-9
                , abs (c - z) < 1e-9
                , abs (d - w) < 1e-9
                ]
      transformParams (Transform RLeft True False) `shouldBe` (-1, True, False)
      transformMatrix (Transform RCenter False False) `shouldBe` (1, 0, 0, 1)
      transformMatrix (Transform RLeft False False) `shouldSatisfy` approx (0, -1, 1, 0)
      transformMatrix (Transform RRight False False) `shouldSatisfy` approx (0, 1, -1, 0)

  describe "Lens helpers" $ do
    let unknownNamedLens = LensInfo
          (mkSym "Unknown (Canon)")
          (mkSym "Canon EF 50mm")
          (Just (Prime 50))
          (Just (FixedAperture 1.8))
          (Just (mkSym "123"))
        zoomLens = LensInfo
          (mkSym "Canon EF 70-200mm")
          (mkSym "Canon EF 70-200mm")
          (Just (Zoom 70 200))
          (Just (VariableAperture 2.8 4.0))
          Nothing
        showLensType :: LensType -> String
        showLensType lt = case lt of
          LensPrime -> "prime"
          LensConstantApertureZoom -> "constant-zoom"
          LensVariableApertureZoom -> "variable-zoom"
          LensUnknown -> "unknown"
    it "chooses the most useful display name" $ do
      lensDisplayName unknownNamedLens `shouldBe` mkSym "Canon EF 50mm"
      lensDisplayName zoomLens `shouldBe` mkSym "Canon EF 70-200mm"

    it "formats short names and serials" $ do
      lensShortName unknownNamedLens `shouldBe` "Canon EF 50mm (#123)"
      lensShortName zoomLens `shouldBe` "Canon EF 70-200mm"

    it "classifies lens types" $ do
      showLensType (lensType (LensInfo (mkSym "prime") (mkSym "prime") (Just (Prime 50)) (Just (FixedAperture 1.8)) Nothing))
        `shouldBe` "prime"
      showLensType (lensType zoomLens) `shouldBe` "variable-zoom"
      case lensType (LensInfo (mkSym "fixed") (mkSym "fixed") (Just (Zoom 24 70)) (Just (FixedAperture 4.0)) Nothing) of
        LensConstantApertureZoom -> pure ()
        _ -> expectationFailure "Expected constant-zoom"
      showLensType (lensType (LensInfo (mkSym "unknown") (mkSym "unknown") Nothing Nothing Nothing))
        `shouldBe` "unknown"

  describe "Person formatting" $ do
    it "formats slash and space-separated names" $ do
      formatPerson False "Doe/John" `shouldBe` "John Doe"
      formatPerson True "Doe/John" `shouldBe` "John D."
      formatPerson False "John Doe" `shouldBe` "John Doe"
      formatPerson False "SingleName" `shouldBe` "SingleName"

  describe "exiftool batching" $ do
    it "keeps a positive batch size" $
      exifToolBatchSize `shouldSatisfy` (> 0)
    it "does not split a batch-sized list" $
      chunkPaths [1..exifToolBatchSize] `shouldBe` [[1..exifToolBatchSize :: Int]]
    it "splits just over one batch into two" $ do
      let xs = [1..exifToolBatchSize + 1] :: [Int]
      chunkPaths xs `shouldBe` [[1..exifToolBatchSize], [exifToolBatchSize + 1]]
    it "returns no chunks for an empty list" $
      chunkPaths ([] :: [Int]) `shouldBe` []

  describe "exiftool arguments" $ do
    it "passes file names unchanged after the end of options" $
      exifToolArgs ["-Comment=x.jpg", "-Directory=/tmp/x.jpg", "sub/a.jpg"]
        `shouldBe` ["-json", "-struct", "-l", "--",
                    "-Comment=x.jpg", "-Directory=/tmp/x.jpg", "sub/a.jpg"]

  withConfig $ describe "exiftool file name handling" $ do
    it "does not interpret file names as options" $ \config -> do
      requireExifTool
      let names = ["-Comment=x.jpg", "-Directory=moved.jpg", "normal.jpg"]
      dir <- populateSourceDir config $ map (, minimalJpeg) names
      (readCount, exifs) <- getExif noLog config dir names
      -- Nothing was renamed, moved or backed-up by a write mode run.
      (sort <$> listDirectory dir) `shouldReturn` sort names
      forM_ names $ \name ->
        BS.readFile (dir </> name) `shouldReturn` minimalJpeg
      readCount `shouldBe` length names
      Map.keys exifs `shouldBe` map pack (sort names)
      Map.filter isLeft exifs `shouldSatisfy` null

    it "keys per-file errors by the real file name" $ \config -> do
      requireExifTool
      let names = ["-empty.jpg", "normal.jpg"]
      dir <- populateSourceDir config
               [("-empty.jpg", BS.empty), ("normal.jpg", minimalJpeg)]
      (_, exifs) <- getExif noLog config dir names
      Map.keys exifs `shouldBe` map pack (sort names)
      (isLeft <$> lookup "-empty.jpg" exifs) `shouldBe` Just True
      (isRight <$> lookup "normal.jpg" exifs) `shouldBe` Just True

    it "stores the real file name in the raw exif cache" $ \config -> do
      requireExifTool
      dir <- populateSourceDir config [("normal.jpg", minimalJpeg)]
      _ <- getExif noLog config dir ["normal.jpg"]
      rawCache <- BS.readFile (exifPath config (dir </> "normal.jpg"))
      let source = decodeStrict' rawCache >>=
                     parseMaybe (withObject "cached exif" (.: "SourceFile"))
      source `shouldBe` Just ("normal.jpg" :: Text)

  describe "NFData" $ do
    it "forces previously skipped exif fields" $ do
      evaluate (rnf (def :: Exif)) `shouldReturn` ()
      evaluate (rnf (def { exifMake = Just (error "make") } :: Exif))
        `shouldThrow` anyErrorCall
      evaluate (rnf (def { exifLensMake = Just (error "lensmake") } :: Exif))
        `shouldThrow` anyErrorCall

  describe "FlashSource parseFlashSource function" $ do
    it "handles valid values" $ do
      parseFlashSource (0 :: Int) `shouldBe` Just FlashSourceNone
      parseFlashSource (1 :: Int) `shouldBe` Just FlashSourceExternal
      parseFlashSource (2 :: Int) `shouldBe` Just FlashSourceInternal

    it "handles invalid values" $ do
      parseFlashSource (3 :: Int) `shouldBe` Nothing
      parseFlashSource (-1 :: Int) `shouldBe` Nothing
      parseFlashSource (999 :: Int) `shouldBe` Nothing
