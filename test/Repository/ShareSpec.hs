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

{-# OPTIONS_GHC -fno-hpc #-}

{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repository.ShareSpec (spec) where

import           Data.Default
import qualified Data.Map              as Map
import qualified Data.Set              as Set
import           Data.Store            (Store, decodeEx, encode)
import           Data.Time             (LocalTime (..), TimeOfDay (..),
                                        TimeZone (..), ZonedTime (..),
                                        hoursToTimeZone, utc)
import           System.Mem.StableName (makeStableName)
import           Test.QuickCheck       (Gen, counterexample, elements, forAll,
                                        ioProperty, vectorOf, (.&&.), (===))

import           Exif
import           Repository
import           Repository.Build
import           Repository.Share
import           TestImport

-- | Whether two values are the same object in memory (once evaluated).
sameObject :: a -> a -> IO Bool
sameObject a b = do
  a' <- evaluate a
  b' <- evaluate b
  (==) <$> makeStableName a' <*> makeStableName b'

-- | A copy of a value, as reading it from a cache gives: an equal
-- value, made of new objects. The index makes each copy's input
-- different, so that the compiler can't make them one.
copyOf :: forall a. Store a => Int -> a -> a
copyOf i x = fst (decodeEx (encode (x, i)) :: (a, Int))

-- | An exif with a value in each field that is shared, all different
-- (but for the type), so that a field given another's value shows.
richExif :: Exif
richExif =
  Exif { exifPeople       = Set.fromList [mkSym "Alice"]
       , exifKeywords     = Set.fromList [mkSym "Mountains", mkSym "Switzerland"]
       , exifCountry      = Just (mkSym "Switzerland (country)")
       , exifProvince     = Just (mkSym "Zurich (province)")
       , exifCity         = Just (mkSym "Zurich")
       , exifLocation     = Just (mkSym "Old town")
       , exifCamera       = Just (mkSym "NIKON D850 (#3001234)")
       , exifModel        = Just (mkSym "D850")
       , exifSerial       = Just (mkSym "3001234")
       , exifLens         = (exifLens def) { liName = mkSym "AF-S 24-70mm f/2.8E"
                                           , liFL = Just (Zoom 24 70)
                                           , liAp = Just (FixedAperture 2.8)
                                           , liSerial = Just (mkSym "201234") }
       , exifOrientation  = OrientationTopLeft
       , exifCreateDate   = Just (ExifTime (ZonedTime (LocalTime (fromGregorian 2024 6 2)
                                                                 (TimeOfDay 10 0 0)) utc))
       , exifTitle        = Just (mkSym "A title")
       , exifCaption      = Just (mkSym "A caption")
       , exifAperture     = Just 2.8
       , exifFocalLength  = Just 35
       , exifFL35mm       = Just 36
       , exifISO          = Just 400
       , exifSSpeedDesc   = Just (mkSym "1/250")
       , exifSSpeedVal    = Just 0.004
       , exifShutterCount = Just 12345
       , exifMimeType     = Just (mkSym "image/x-nikon-nef")
       , exifRating       = Just 4
       , exifFlashInfo    = FlashInfo (Just FlashSourceNone) (Just (mkSym "Off"))
       , exifWidth        = Just 8256
       , exifHeight       = Just 5504
       , exifMegapixels   = Just 45.4
       , exifMake         = Just (mkSym "NIKON CORPORATION")
       , exifLensMake     = Just (mkSym "Nikon")
       , exifWarning      = Set.empty
       }

-- | The finished folder "f", with a raw and a JPEG for each of the
-- images "a" and "b": the files a.nef, a.jpg, b.nef and b.jpg get the
-- exif the function gives for 1, 2, 3 and 4.
folderWith :: Config -> (Int -> Exif) -> PicDir
folderWith config exifOf =
  let names = ["a.nef", "a.jpg", "b.nef", "b.jpg"]
      cache = Map.fromList [ (pack n, Right (exifOf i)) | (i, n) <- zip [1..] names ]
  in compositeFolderWith cache config
       [[("a.nef", 10), ("b.nef", 10)]] [[("a.jpg", 5), ("b.jpg", 5)]]

-- | The exif of each image file of a folder, by relative path.
filesExif :: PicDir -> Map Text Exif
filesExif dir =
  Map.fromList [ (pack (fileRelPath f), fileExif f)
               | img <- Map.elems (pdImages dir), f <- allImageFiles img ]

-- | Each field that is shared, as a check of two exifs.
sharedFields :: [(String, Exif -> Exif -> IO Bool)]
sharedFields =
  [ ("people",       same exifPeople)
  , ("keywords",     same exifKeywords)
  , ("country",      same exifCountry)
  , ("province",     same exifProvince)
  , ("city",         same exifCity)
  , ("location",     same exifLocation)
  , ("camera",       same exifCamera)
  , ("model",        same exifModel)
  , ("serial",       same exifSerial)
  , ("lens",         same exifLens)
  , ("date",         same exifCreateDate)
  , ("title",        same exifTitle)
  , ("caption",      same exifCaption)
  , ("aperture",     same exifAperture)
  , ("focal length", same exifFocalLength)
  , ("FL35mm",       same exifFL35mm)
  , ("ISO",          same exifISO)
  , ("speed desc",   same exifSSpeedDesc)
  , ("speed",        same exifSSpeedVal)
  , ("shutters",     same exifShutterCount)
  , ("MIME type",    same exifMimeType)
  , ("rating",       same exifRating)
  , ("flash",        same exifFlashInfo)
  , ("width",        same exifWidth)
  , ("height",       same exifHeight)
  , ("megapixels",   same exifMegapixels)
  , ("make",         same exifMake)
  , ("lens make",    same exifLensMake)
  ]
  where same field x y = sameObject (field x) (field y)

-- | Checks that each shared field of two exifs is (or isn't) the same
-- object.
fieldsShared :: Bool -> Exif -> Exif -> Expectation
fieldsShared = fieldsSharedOf sharedFields

-- | 'fieldsShared' for an image's exif and a file's: an image's exif
-- has no MIME type.
imageFieldsShared :: Exif -> Exif -> Expectation
imageFieldsShared = fieldsSharedOf (filter ((/= "MIME type") . fst) sharedFields) True

fieldsSharedOf :: [(String, Exif -> Exif -> IO Bool)] -> Bool -> Exif -> Exif -> Expectation
fieldsSharedOf fields expected x y = do
  results <- forM fields $ \(name, check) -> (,) name <$> check x y
  results `shouldBe` map (\(name, _) -> (name, expected)) fields

-- | A date, from a few that are equal or close: the same local time
-- in different zones, the same instant in different zones, zones that
-- differ only by their name or their summer-only flag, and fractions
-- of a second.
genDate :: Gen ExifTime
genDate = do
  day <- elements [fromGregorian 2024 6 2, fromGregorian 2024 6 3]
  time <- elements [TimeOfDay 10 0 0, TimeOfDay 12 0 0, TimeOfDay 10 0 0.5]
  zone <- elements [ utc, hoursToTimeZone 0, hoursToTimeZone 2
                   , TimeZone 120 False "CEST", TimeZone 120 True "CEST"
                   , TimeZone 60 False "CET", TimeZone (-330) False "" ]
  return $ ExifTime (ZonedTime (LocalTime day time) zone)

-- | All of a date, field by field: 'show' leaves out a zone's offset
-- when it has a name, and its summer-only flag. Matched by position,
-- so that a new field breaks the build.
exactDate :: ExifTime -> (LocalTime, Int, Bool, String)
exactDate (ExifTime (ZonedTime local (TimeZone minutes summerOnly name))) =
  (local, minutes, summerOnly, name)

spec :: Spec
spec = withConfig $ do
  describe "shareFolder" $ do
    it "gives back an equal folder" $ \config -> do
      let dir = mixedExifFolder config
      show (shareFolder dir) `shouldBe` show dir
    it "gives back an equal unfinished folder" $ \config -> do
      let dir = buildFolderFromInodes config "f" "/src/f" True
                  [mkInode "a.nef", mkInode "b.nef", mkInode "notes.txt"]
                  (Map.fromList [ ("a.nef", Right (copyOf 1 richExif))
                                , ("b.nef", Right (copyOf 2 richExif)) ])
                  Nothing
      show (shareFolder dir) `shouldBe` show dir
    it "can tell the same object (so the sharing checks can pass)" $ \_ ->
      fieldsShared True richExif richExif
    it "starts from separate copies (so the sharing checks can fail)" $ \_ ->
      fieldsShared False (copyOf 1 richExif) (copyOf 2 richExif)
    it "shares the equal values of the files of a finished folder" $ \config -> do
      let exifs = filesExif (folderWith config (`copyOf` richExif))
          fileNamed n = fromMaybe (error $ "no file " ++ unpack n) (Map.lookup n exifs)
      Map.keys exifs `shouldBe` ["a.jpg", "a.nef", "b.jpg", "b.nef"]
      fieldsShared True (fileNamed "a.nef") (fileNamed "a.jpg")
      fieldsShared True (fileNamed "a.nef") (fileNamed "b.nef")
      fieldsShared True (fileNamed "a.nef") (fileNamed "b.jpg")
    it "shares them with the images' exif" $ \config -> do
      let dir = folderWith config (`copyOf` richExif)
      forM_ (pdImages dir) $ \img ->
        forM_ (allImageFiles img) $ imageFieldsShared (imgExif img) . fileExif
    it "builds the time index from the dates kept" $ \config -> do
      -- All the images have equal dates, so the time index holds one
      -- date for all, if it is built from the dates kept.
      let dir = folderWith config (`copyOf` richExif)
      case mapMaybe fst (Set.toList (pdTimeSort dir)) of
        dates@(date0:_:_) -> forM_ dates (sameObject date0 >=> (`shouldBe` True))
        other -> expectationFailure $ "expected two dates, got " ++ show other
    it "keeps dates of the same instant in different time zones apart" $ \config -> do
      let at tz = def { exifCreateDate = Just (ExifTime (ZonedTime (LocalTime (fromGregorian 2024 6 2)
                                                                     (TimeOfDay (10 + tz) 0 0))
                                                                   (hoursToTimeZone tz))) }
          dir = folderWith config (\i -> if even i then at 2 else at 0)
          exifs = filesExif dir
          shown n = show (exifCreateDate <$> Map.lookup n exifs)
      -- 10:00 UTC, as 10:00 and as 12:00+02:00
      shown "a.nef" `shouldBe` show (Just (exifCreateDate (at 0)))
      shown "a.jpg" `shouldBe` show (Just (exifCreateDate (at 2)))
    it "keeps zero and negative zero apart" $ \config -> do
      let dir = folderWith config (\i -> def { exifAperture = Just (if even i then -0.0 else 0.0) })
          exifs = filesExif dir
          isNegZero n = maybe False isNegativeZero (Map.lookup n exifs >>= exifAperture)
      isNegZero "a.nef" `shouldBe` False
      isNegZero "a.jpg" `shouldBe` True
    it "keeps lenses that differ only by the sign of a zero apart" $ \config -> do
      let lens z = (exifLens def) { liName = mkSym "Adapted", liFL = Just (Prime z) }
          dir = folderWith config (\i -> def { exifLens = lens (if even i then -0.0 else 0.0) })
          exifs = filesExif dir
          isNegZero n = case liFL . exifLens <$> Map.lookup n exifs of
                          Just (Just (Prime f)) -> isNegativeZero f
                          _                     -> False
      isNegZero "a.nef" `shouldBe` False
      isNegZero "a.jpg" `shouldBe` True
    it "keeps each date as it is, and shares the equal ones" $ \config ->
      forAll (vectorOf 4 genDate) $ \dates -> ioProperty $ do
        let given = Map.fromList (zip [1..] dates)
            exifOf i = def { exifCreateDate = copyOf i <$> Map.lookup i given }
            exifs = filesExif (folderWith config exifOf)
            names = ["a.nef", "a.jpg", "b.nef", "b.jpg"]
            kept = [ (n, exifCreateDate =<< Map.lookup n exifs) | n <- names ]
        shared <- forM [ (x, y) | (x:ys) <- tails kept, y <- ys
                                , (exactDate <$> snd x) == (exactDate <$> snd y) ] $
          \((nx, dx), (ny, dy)) -> (,) (nx, ny) <$> sameObject dx dy
        return $ map (fmap exactDate . snd) kept === map (Just . exactDate) dates
            .&&. counterexample "equal dates not shared" (all snd shared)
  describe "restoreSharing" $
    it "shares the equal values of a folder read from the cache" $ \config -> do
      let dir = restoreSharing (copyOf 1 (folderWith config (`copyOf` richExif)))
          exifs = filesExif dir
          fileNamed n = fromMaybe (error $ "no file " ++ unpack n) (Map.lookup n exifs)
      Map.keys exifs `shouldBe` ["a.jpg", "a.nef", "b.jpg", "b.nef"]
      forM_ (Map.elems exifs) $ fieldsShared True (fileNamed "a.nef")
      forM_ (pdImages dir) $ imageFieldsShared (fileNamed "a.nef") . imgExif
