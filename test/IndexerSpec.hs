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
{-# OPTIONS_GHC -Wno-orphans #-}

module IndexerSpec (spec) where

import           Data.Default
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as Text
import qualified Data.Text.Short     as TS
import           Data.Time.LocalTime
import qualified Types

import           Yesod.Core          (fromPathPiece, toPathPiece)

import           GHC.Stack           ()
import           Test.QuickCheck     (Arbitrary (..), chooseEnum)

import           AtomTypes
import           Exif
import           Indexer
import           Pics
import           TestImport

instance Arbitrary FolderClass where
  arbitrary = chooseEnum (minBound, maxBound)

atomDescContains :: HasCallStack => Atom -> Text -> Expectation
atomDescContains atom str =
  str `shouldSatisfy` (`Text.isInfixOf` atomDescription atom)

missingAtomSymbols :: [Symbol]
missingAtomSymbols =
  filter (`notElem` [TFolder, TFileName, TStatus, TFClass, TPplCnt, TKwdCnt])
         [minBound .. maxBound]

roundTripAtom :: HasCallStack => Atom -> Expectation
roundTripAtom atom =
  parseAtomParams (atomToParams atom) `shouldBe` Right atom

withDate :: Integer -> Int -> Int -> Exif -> Exif
withDate y m d exif =
  let day = LocalTime (fromGregorian y m d) (TimeOfDay 12 0 0)
  in exif { exifCreateDate = Just (ExifTime (ZonedTime day utc)) }

mkImg :: Text -> ShortText -> Exif -> Image
mkImg name parent exif =
  Image { imgName = ImageName (TS.fromText name)
        , imgParent = parent
        , imgStatus = ImageStandalone
        , imgType = MediaImage
        , imgExif = exif
        , imgRawPath = Nothing
        , imgSidecarPath = Nothing
        , imgJpegPath = [simpleFile name]
        , imgMasterMov = Nothing
        , imgMovs = []
        , imgUntracked = []
        , imgRange = Nothing
        , imgFlags = def
        }

picDirWith :: ShortText -> [Image] -> PicDir
picDirWith name images =
  let imgMap = Map.fromList [(imgName i, i) | i <- images]
  in (createTestPicDir name)
       { pdImages = imgMap
       , pdTimeSort = buildTimeSort imgMap
       , pdStats = computeImagesStats imgMap
       , pdExif = buildGroupExif imgMap
       }

testRepo :: [PicDir] -> Repository
testRepo dirs = mkRepository $ Map.fromList [(pdName d, d) | d <- dirs]

explicitEvent :: ShortText -> [ShortText] -> Types.Event
explicitEvent name people =
  Types.GenericEvent { eventName = name
                     , eventPeople = people
                     , eventSource = EventExplicit Nothing
                     }

richExif :: Exif
richExif = withDate 2020 7 15 $ def
  { exifCountry = Just (mkSym "Spain")
  , exifProvince = Just (mkSym "Andalusia")
  , exifCity = Just (mkSym "Seville")
  , exifLocation = Just (mkSym "Alcazar")
  , exifPeople = Set.fromList [mkSym "John", mkSym "Jane"]
  , exifKeywords = Set.fromList [mkSym "travel", mkSym "castle"]
  , exifTitle = Just (mkSym "Summer trip")
  , exifCaption = Just (mkSym "A hot day")
  , exifCamera = Just (mkSym "Nikon Z6")
  , exifLens = def { liName = mkSym "Summicron", liSpec = mkSym "50mm f/2" }
  , exifAperture = Just 2.8
  , exifFocalLength = Just 50
  , exifISO = Just 200
  , exifSSpeedVal = Just 0.004
  , exifRating = Just 4
  , exifFlashInfo = FlashInfo (Just FlashSourceInternal) (Just (mkSym "On"))
  , exifMegapixels = Just 24.2
  , exifWarning = Set.singleton "truncated"
  }

richImg :: Image
richImg = mkImg "rich.jpg" "spain" richExif

blankImg :: Image
blankImg = mkImg "blank.jpg" "blank" def

onePersonImg :: Image
onePersonImg = mkImg "one.jpg" "blank" $ def
  { exifPeople = Set.singleton (mkSym "Ada")
  , exifKeywords = Set.singleton (mkSym "portrait")
  , exifFlashInfo = FlashInfo (Just FlashSourceExternal) Nothing
  , exifMegapixels = Just 12
  }

noFlashImg :: Image
noFlashImg = mkImg "noflash.jpg" "blank" $ def
  { exifFlashInfo = FlashInfo (Just FlashSourceNone) Nothing
  }

spainDir :: PicDir
spainDir = (picDirWith "spain" [richImg])
  { pdYear = Just 2020
  , pdEvent = Just BirthdayEvent
      { eventName = "John"
      , eventPeople = ["John"]
      , eventSource = EventExplicit Nothing
      }
  }

blankDir :: PicDir
blankDir = picDirWith "blank" [blankImg, onePersonImg, noFlashImg]

getawayDir :: PicDir
getawayDir = (createTestPicDir "getaway")
  { pdEvent = Just GetawayEvent
      { eventName = "Paris"
      , eventPeople = ["Eve"]
      , eventSource = EventExplicit Nothing
      }
  }

grandDir :: PicDir
grandDir = (createTestPicDir "grand")
  { pdEvent = Just GrandVacationEvent
      { eventName = "Japan"
      , eventPeople = ["Osamu"]
      , eventSource = EventExplicit Nothing
      }
  }

workDir :: PicDir
workDir = (createTestPicDir "work")
  { pdEvent = Just WorkTripEvent
      { eventName = "Berlin"
      , eventPeople = ["Ada"]
      , eventSource = EventExplicit Nothing
      }
  }

genericDir :: PicDir
genericDir = (createTestPicDir "generic")
  { pdEvent = Just (explicitEvent "Meetup" ["Bob"])
  }

populatedRepo :: Repository
populatedRepo = testRepo
  [spainDir, blankDir, getawayDir, grandDir, workDir, genericDir]

spec :: Spec
spec = parallel $ do
  describe "Symbol" $ do
    it "converts to PathPiece and back" $ do
      let symbols = [minBound..maxBound] :: [Symbol]
      let pathPieces = map toPathPiece symbols
      map fromPathPiece pathPieces `shouldBe` map Just symbols

    it "converts to Text and back" $ do
      let symbols = [minBound..maxBound] :: [Symbol]
      let texts = map symbolName symbols
      map parseSymbol texts `shouldBe` map Just symbols

    it "symbolName returns the correct names" $ do
      symbolName TCountry `shouldBe` "country"
      symbolName TPerson `shouldBe` "person"
      symbolName TYear `shouldBe` "year"

    it "negSymbolName adds no- prefix" $ do
      negSymbolName TCountry `shouldBe` "no-country"
      negSymbolName TKeyword `shouldBe` "no-keyword"

  describe "parseAtom" $ do
    it "parses country atoms" $ do
      parseAtom "country" "Spain" `shouldBe` Just (Country (OpEqual "Spain"))
      parseAtom "country" "~Spain" `shouldBe` Just (Country (OpFuzzy (makeFuzzy "Spain")))
      parseAtom "no-country" "" `shouldBe` Just (Country OpMissing)

    it "parses numeric atoms" $ do
      parseAtom "year" "2020" `shouldBe` Just (Year (OpEq 2020))
      parseAtom "year" "<2020" `shouldBe` Just (Year (OpLt 2020))
      parseAtom "year" ">=2020" `shouldBe` Just (Year (OpGe 2020))

    it "parses f-stop atoms" $ do
      parseAtom "f-stop" "2.8" `shouldBe` Just (FStop (OpEq 2.8))
      parseAtom "f-stop" ">4" `shouldBe` Just (FStop (OpGt 4.0))

    it "parses season atoms" $ do
      parseAtom "season" "winter" `shouldBe` Just (Season Winter)
      parseAtom "season" "summer" `shouldBe` Just (Season Summer)

    it "parses month atoms" $ do
      parseAtom "month" "january" `shouldBe` Just (Month January)
      parseAtom "month" "12" `shouldBe` Just (Month December)

    it "parses day atoms" $ do
      parseAtom "day" "monday" `shouldBe` Just (Day Monday)
      parseAtom "day" "weekend" `shouldBe` Just (Day Weekend)
      parseAtom "day" "1st" `shouldBe` Just (Day (MonthDay 1))
      parseAtom "day" "31" `shouldBe` Just (Day (MonthDay 31))

    it "parses flash source atoms" $ do
      parseAtom "flash-source" "none" `shouldBe` Just (FlashSrc FlashNone)
      parseAtom "flash-source" "internal" `shouldBe` Just (FlashSrc FlashInternal)
      parseAtom "flash-source" "ext" `shouldBe` Just (FlashSrc FlashExternal)

    it "parses megapixels atoms" $ do
      parseAtom "megapixels" "24.2" `shouldBe` Just (Megapixels (OpEq 24.2))
      parseAtom "megapixels" ">20" `shouldBe` Just (Megapixels (OpGt 20.0))

  describe "atomToParams" $ do
    it "converts simple atoms back to params" $ do
      atomToParams (Country (OpEqual "Spain")) `shouldBe` [("country", "Spain")]
      atomToParams (Year (OpEq 2020)) `shouldBe` [("year", "2020")]
      atomToParams (FStop (OpGt 2.8)) `shouldBe` [("f-stop", ">2.8")]

    it "converts combined atoms correctly" $ do
      let atom = And (Country (OpEqual "Spain")) (Year (OpEq 2020))
      atomToParams atom `shouldBe` [("country", "Spain"), ("year", "2020"), ("and", "")]

      let atom2 = Or (Month January) (Month February)
      atomToParams atom2 `shouldBe` [("month", "January"), ("month", "February"), ("or", "")]

  describe "atomDescription" $ do
    it "describes country atoms" $ do
      atomDescContains (Country (OpEqual "Spain")) "country is Spain"
      atomDescContains (Country OpMissing) "has no country information"

    it "describes person atoms" $ do
      atomDescContains (Person (OpEqual "John")) "John is in the picture"
      atomDescContains (Person (OpFuzzy (makeFuzzy "John"))) "tagged with a person named like john"

    it "describes year atoms" $ do
      atomDescContains (Year (OpEq 2020)) "taken in the year 2020"
      atomDescContains (Year (OpLt 2020)) "taken before the year 2020"

    it "describes combined atoms" $ do
      let atom = And (Country (OpEqual "Spain")) (Year (OpEq 2020))
      atomDescContains atom "country is Spain"
      atomDescContains atom  "taken in the year 2020"

  describe "atomFindsFiles" $ do
    it "returns True for most atoms" $ do
      atomFindsFiles (Country (OpEqual "Spain")) `shouldBe` True
      atomFindsFiles (Year (OpEq 2020)) `shouldBe` True

    prop "returns False for folder class atoms" $ \fclass ->
      atomFindsFiles (FClass fclass) `shouldBe` False

    it "properly handles combined atoms" $ do
      atomFindsFiles (And (Country (OpEqual "Spain")) (Year (OpEq 2020))) `shouldBe` True
      atomFindsFiles (And (Country (OpEqual "Spain")) (FClass FolderStandalone)) `shouldBe` False
      atomFindsFiles (Or (FClass FolderMixed) (Year (OpEq 2020))) `shouldBe` True

  describe "parseAtomParams" $ do
    it "parses simple atom params" $ do
      parseAtomParams [("country", "Spain")] `shouldBe` Right (Country (OpEqual "Spain"))
      parseAtomParams [("year", "2020")] `shouldBe` Right (Year (OpEq 2020))

    it "parses multiple atom params" $ do
      let params = [("country", "Spain"), ("year", "2020"), ("and", "")]
      case parseAtomParams params of
        Right (And a b) -> do
          [a,b] `shouldMatchList` [Country (OpEqual "Spain"), Year (OpEq 2020)]
        _ -> expectationFailure "Failed to parse atom params correctly"

    it "returns an error for too many params" $ do
      let tooManyParams = replicate 51 ("country", "Spain")
      case parseAtomParams tooManyParams of
        Left err -> err `shouldSatisfy` ("Too many search parameters" `Text.isInfixOf`)
        Right _ -> expectationFailure "Should have returned an error for too many params"

  describe "picDay, picMonth and picSeason" $ do
    let testDate = LocalTime
                    { localDay = fromGregorian 2020 7 15
                    , localTimeOfDay = TimeOfDay 12 0 0
                    }
        testExifTime = ExifTime $ ZonedTime testDate utc
    let testExif = def { exifCreateDate = Just testExifTime }
    let testImage = Image
                      { imgName = ImageName "test.jpg"
                      , imgParent = "test"
                      , imgStatus = ImageStandalone
                      , imgType = MediaImage
                      , imgExif = testExif
                      , imgRawPath = Nothing
                      , imgSidecarPath = Nothing
                      , imgJpegPath = [simpleFile "test.jpg"]
                      , imgMasterMov = Nothing
                      , imgMovs = []
                      , imgUntracked = []
                      , imgRange = Nothing
                      , imgFlags = def
                      }

    it "extracts day correctly" $ do
      picDay testImage `shouldBe` Just Wednesday
      picMonthDay testImage `shouldBe` Just (MonthDay 15)

    it "extracts month correctly" $ do
      picMonth testImage `shouldBe` Just July

    it "extracts season correctly" $ do
      picSeason testImage `shouldBe` Just Summer

  describe "ShowText instances" $ do
    it "shows media type correctly" $ do
      showMedia MediaImage `shouldBe` "image"
      showMedia MediaMovie `shouldBe` "movie"

  describe "parseString, parseNumDecimal and parseNumReal" $ do
    it "parses fuzzy strings" $ do
      parseString "~test" `shouldBe` Just (OpFuzzy (makeFuzzy "test"))

    it "parses normal strings" $ do
      parseString "test" `shouldBe` Just (OpEqual "test")

    it "parses decimal numbers" $ do
      parseNumDecimal "123" `shouldBe` Just (OpEq 123 :: NumOp Int)
      parseNumDecimal "<123" `shouldBe` Just (OpLt 123 :: NumOp Int)
      parseNumDecimal ">=123" `shouldBe` Just (OpGe 123 :: NumOp Int)

    it "parses real numbers" $ do
      parseNumReal "2.8" `shouldBe` Just (OpEq 2.8)
      parseNumReal "<2.8" `shouldBe` Just (OpLt 2.8)
      parseNumReal ">=2.8" `shouldBe` Just (OpGe 2.8)

  describe "parseShutterSpeed" $ do
    it "parses fraction format" $ do
      parseShutterSpeed "1/250s" `shouldBe` Just (OpEq 0.004)
      parseShutterSpeed "1/1000" `shouldBe` Just (OpEq 0.001)

    it "parses decimal format" $ do
      parseShutterSpeed "0.5s" `shouldBe` Just (OpEq 0.5)
      parseShutterSpeed "2s" `shouldBe` Just (OpEq 2.0)

    it "parses with operators" $ do
      parseShutterSpeed ">1/250s" `shouldBe` Just (OpGt 0.004)
      parseShutterSpeed "<=2s" `shouldBe` Just (OpLe 2.0)

  describe "showShutterSpeed" $ do
    it "formats speeds less than 1 second as fractions" $ do
      showShutterSpeed 0.004 `shouldBe` "1/250s"
      showShutterSpeed 0.001 `shouldBe` "1/1000s"

    it "formats speeds 1 second or longer as decimals" $ do
      showShutterSpeed 1.0 `shouldBe` "1s"
      showShutterSpeed 2.5 `shouldBe` "2.5s"

  describe "parseAtom coverage" $ do
    it "parses every remaining atom kind" $ do
      parseAtom "province" "Andalusia" `shouldBe` Just (Province (OpEqual "Andalusia"))
      parseAtom "city" "Seville" `shouldBe` Just (City (OpEqual "Seville"))
      parseAtom "location" "Alcazar" `shouldBe` Just (Location (OpEqual "Alcazar"))
      parseAtom "person" "John" `shouldBe` Just (Person (OpEqual "John"))
      parseAtom "keyword" "travel" `shouldBe` Just (Keyword (OpEqual "travel"))
      parseAtom "title" "Summer" `shouldBe` Just (Title (OpEqual "Summer"))
      parseAtom "caption" "A day" `shouldBe` Just (Caption (OpEqual "A day"))
      parseAtom "camera" "Nikon" `shouldBe` Just (Camera (OpEqual "Nikon"))
      parseAtom "lens" "Summicron" `shouldBe` Just (Lens (OpEqual "Summicron"))
      parseAtom "shutter-speed" "1/250s" `shouldBe` Just (ShutterSpeed (OpEq 0.004))
      parseAtom "iso" "200" `shouldBe` Just (Iso (OpEq 200))
      parseAtom "focal-length" "50" `shouldBe` Just (FocalLength (OpEq 50))
      parseAtom "problem" "exif: truncated" `shouldBe` Just (Problem (OpEqual "exif: truncated"))
      parseAtom "type" "image" `shouldBe` Just (Type MediaImage)
      parseAtom "type" "movie" `shouldBe` Just (Type MediaMovie)
      parseAtom "type" "unknown" `shouldBe` Just (Type MediaUnknown)
      parseAtom "folder" "spain" `shouldBe` Just (Folder (OpEqual "spain"))
      parseAtom "filename" "rich.jpg" `shouldBe` Just (FileName (OpEqual "rich.jpg"))
      parseAtom "status" "standalone" `shouldBe` Just (Status ImageStandalone)
      parseAtom "status" "orphaned" `shouldBe` Just (Status ImageOrphaned)
      parseAtom "status" "unprocessed" `shouldBe` Just (Status ImageUnprocessed)
      parseAtom "status" "processed" `shouldBe` Just (Status ImageProcessed)
      parseAtom "folder-class" "empty" `shouldBe` Just (FClass FolderEmpty)
      parseAtom "folder-class" "mixed" `shouldBe` Just (FClass FolderMixed)
      parseAtom "rating" "4" `shouldBe` Just (Rating (OpEq 4))
      parseAtom "people-count" "2" `shouldBe` Just (PplCnt (OpEq 2))
      parseAtom "keyword-count" "2" `shouldBe` Just (KwdCnt (OpEq 2))
      parseAtom "flash-mode" "On" `shouldBe` Just (FlashMode (OpEqual "On"))
      parseAtom "event" "John" `shouldBe` Just (Event (OpEqual "John"))
      parseAtom "generic-event" "Meetup" `shouldBe` Just (Indexer.GenericEvent (OpEqual "Meetup"))
      parseAtom "birthday" "John" `shouldBe` Just (Birthday (OpEqual "John"))
      parseAtom "getaway" "Paris" `shouldBe` Just (Getaway (OpEqual "Paris"))
      parseAtom "grand-vacation" "Japan" `shouldBe` Just (GrandVacation (OpEqual "Japan"))
      parseAtom "vacation" "Italy" `shouldBe` Just (Vacation (OpEqual "Italy"))
      parseAtom "work-trip" "Berlin" `shouldBe` Just (WorkTrip (OpEqual "Berlin"))
      parseAtom "event-kind" "birthday" `shouldBe` Just (EventKind EKBirthday)
      parseAtom "event-kind" "generic" `shouldBe` Just (EventKind EKGeneric)
      parseAtom "flash-source" "any" `shouldBe` Just (FlashSrc FlashAny)
      parseAtom "flash-source" "int" `shouldBe` Just (FlashSrc FlashInternal)

    it "parses no- atoms for every supported symbol" $
      forM_ missingAtomSymbols $ \s ->
        parseAtom ("no-" <> symbolName s) "" `shouldBe` Just (buildMissingAtom s)

    it "rejects unknown symbols and invalid values" $ do
      parseAtom "nonesuch" "x" `shouldBe` Nothing
      parseAtom "year" "nope" `shouldBe` Nothing
      parseAtom "type" "photo" `shouldBe` Nothing
      parseSymbol "nonesuch" `shouldBe` Nothing
      fromPathPiece "nonesuch" `shouldBe` (Nothing :: Maybe Symbol)

    it "parses all numeric prefixes" $ do
      parseNumDecimal "=5" `shouldBe` Just (OpEq 5 :: NumOp Int)
      parseNumDecimal "/=5" `shouldBe` Just (OpNe 5 :: NumOp Int)
      parseNumDecimal "!=5" `shouldBe` Just (OpNe 5 :: NumOp Int)
      parseNumDecimal "≠5" `shouldBe` Just (OpNe 5 :: NumOp Int)
      parseNumDecimal "<5" `shouldBe` Just (OpLt 5 :: NumOp Int)
      parseNumDecimal "<=5" `shouldBe` Just (OpLe 5 :: NumOp Int)
      parseNumDecimal "≤5" `shouldBe` Just (OpLe 5 :: NumOp Int)
      parseNumDecimal ">=5" `shouldBe` Just (OpGe 5 :: NumOp Int)
      parseNumDecimal "≥5" `shouldBe` Just (OpGe 5 :: NumOp Int)
      parseNumDecimal ">5" `shouldBe` Just (OpGt 5 :: NumOp Int)
      parseNumDecimal "/5" `shouldBe` (Nothing :: Maybe (NumOp Int))
      parseNumDecimal "abc" `shouldBe` (Nothing :: Maybe (NumOp Int))
      parseNumReal "≠2.5" `shouldBe` Just (OpNe 2.5)
      parseNumReal "≤2.5" `shouldBe` Just (OpLe 2.5)
      parseNumReal "≥2.5" `shouldBe` Just (OpGe 2.5)
      parseShutterSpeed "/=1/250s" `shouldBe` Just (OpNe 0.004)
      parseShutterSpeed "!=2s" `shouldBe` Just (OpNe 2.0)
      parseShutterSpeed "≠1s" `shouldBe` Just (OpNe 1.0)
      parseShutterSpeed "≥1s" `shouldBe` Just (OpGe 1.0)
      parseShutterSpeed "≤1/2s" `shouldBe` Just (OpLe 0.5)

  describe "parseAtomParams combinators" $ do
    it "parses not, and, or" $ do
      parseAtomParams [("country", "Spain"), ("not", "")]
        `shouldBe` Right (Not (Country (OpEqual "Spain")))
      parseAtomParams [("country", "Spain"), ("not", ""), ("not", "")]
        `shouldBe` Right (Country (OpEqual "Spain"))
      parseAtomParams [("month", "January"), ("month", "February"), ("or", "")]
        `shouldBe` Right (Or (Month February) (Month January))

    it "parses all/any and ConstTrue" $ do
      parseAtomParams [] `shouldBe` Right ConstTrue
      parseAtomParams [("all", "0")] `shouldBe` Right ConstTrue
      parseAtomParams [("any", "0")] `shouldBe` Right (Any [])
      case parseAtomParams [("country", "A"), ("city", "B"), ("year", "2020"), ("all", "3")] of
        Right (All xs) -> xs `shouldMatchList`
          [Country (OpEqual "A"), City (OpEqual "B"), Year (OpEq 2020)]
        other -> expectationFailure ("expected All, got " <> show other)
      case parseAtomParams [("country", "A"), ("city", "B"), ("year", "2020"), ("any", "3")] of
        Right (Any xs) -> xs `shouldMatchList`
          [Country (OpEqual "A"), City (OpEqual "B"), Year (OpEq 2020)]
        other -> expectationFailure ("expected Any, got " <> show other)
      parseAtomParams [("country", "A"), ("any", "1")]
        `shouldBe` Right (Country (OpEqual "A"))
      parseAtomParams [("country", "A"), ("city", "B"), ("any", "2")]
        `shouldBe` Right (Or (City (OpEqual "B")) (Country (OpEqual "A")))

    it "returns errors for invalid RPN" $ do
      parseAtomParams [("bogus", "x")] `shouldBeLeftWithMessage` "Failed to parse the atom"
      parseAtomParams [("all", "2")] `shouldBeLeftWithMessage` "Failed to pop"
      parseAtomParams [("any", "3")] `shouldBeLeftWithMessage` "Failed to pop"
      parseAtomParams [("and", "")] `shouldBeLeftWithMessage` "Failed to parse the atom"
      parseAtomParams [("not", "")] `shouldBeLeftWithMessage` "Failed to parse the atom"
      parseAtomParams [("all", "x")] `shouldBeLeftWithMessage` "Failed"

    it "roundtrips atomToParams for simple and combined atoms" $ do
      mapM_ roundTripAtom
        [ Country (OpEqual "Spain")
        , Country (OpFuzzy (makeFuzzy "spa"))
        , Country OpMissing
        , Year (OpEq 2020)
        , Year (OpNe 2019)
        , Year (OpLt 2021)
        , Year (OpLe 2020)
        , Year (OpGe 2020)
        , Year (OpGt 2019)
        , Year OpNa
        , FStop (OpEq 2.8)
        , Type MediaMovie
        , Status ImageProcessed
        , FClass FolderMixed
        , Season Winter
        , Season SeasonUnknown
        , Month March
        , Month MonthUnknown
        , Day Monday
        , Day Weekend
        , Day (MonthDay 15)
        , Day DayUnknown
        , FlashSrc FlashInternal
        , FlashSrc FlashUnknown
        , EventKind EKGetaway
        , EventKind EKNoEvent
        , Indexer.GenericEvent (OpEqual "Meetup")
        , Birthday OpMissing
        , Not (City (OpEqual "Paris"))
        , ConstTrue
        ]
      let andAtom = And (Country (OpEqual "Spain")) (Year (OpEq 2020))
          orAtom = Or (Month January) (Month February)
      case parseAtomParams (atomToParams andAtom) of
        Right (And x y) -> [x, y] `shouldMatchList`
          [Country (OpEqual "Spain"), Year (OpEq 2020)]
        other -> expectationFailure ("expected And, got " <> show other)
      case parseAtomParams (atomToParams orAtom) of
        Right (Or x y) -> [x, y] `shouldMatchList`
          [Month January, Month February]
        other -> expectationFailure ("expected Or, got " <> show other)

  describe "atom metadata" $ do
    it "has names, descriptions and file-finding flags for every symbol" $ do
      let symbols = [minBound .. maxBound] :: [Symbol]
      symbolNames `shouldBe` map (\t -> (t, symbolName t)) symbols
      map atomTypeDescriptions symbols `shouldSatisfy` (not . any Text.null)
      symbolFindsFiles TCountry `shouldBe` True
      symbolFindsFiles TFClass `shouldBe` False
      symbolFindsFiles TEvent `shouldBe` False
      symbolFindsFiles TGenericEvent `shouldBe` False
      symbolFindsFiles TBirthday `shouldBe` False
      symbolFindsFiles TGetaway `shouldBe` False
      symbolFindsFiles TGrandVacation `shouldBe` False
      symbolFindsFiles TVacation `shouldBe` False
      symbolFindsFiles TWorkTrip `shouldBe` False
      symbolFindsFiles TEventKind `shouldBe` False

    it "reports atomFindsFiles for events and combinators" $ do
      atomFindsFiles (Event (OpEqual "x")) `shouldBe` False
      atomFindsFiles (Indexer.GenericEvent (OpEqual "x")) `shouldBe` True
      atomFindsFiles (Birthday (OpEqual "x")) `shouldBe` False
      atomFindsFiles (Getaway (OpEqual "x")) `shouldBe` False
      atomFindsFiles (GrandVacation (OpEqual "x")) `shouldBe` False
      atomFindsFiles (Vacation (OpEqual "x")) `shouldBe` False
      atomFindsFiles (WorkTrip (OpEqual "x")) `shouldBe` False
      atomFindsFiles (EventKind EKBirthday) `shouldBe` False
      atomFindsFiles (Not (FClass FolderEmpty)) `shouldBe` False
      atomFindsFiles (All [FClass FolderEmpty, Country (OpEqual "Spain")]) `shouldBe` False
      atomFindsFiles (Any [FClass FolderEmpty, Country (OpEqual "Spain")]) `shouldBe` True
      atomFindsFiles ConstTrue `shouldBe` True

  describe "atomDescription coverage" $ do
    it "describes string, numeric, date and media variants" $ do
      atomDescContains (Country (OpEqual "")) "country is empty"
      atomDescContains (Country (OpFuzzy (makeFuzzy ""))) "has any value"
      atomDescContains (Person (OpEqual "")) "empty person tag"
      atomDescContains (Person (OpFuzzy (makeFuzzy ""))) "any person"
      atomDescContains (Person OpMissing) "no person information"
      atomDescContains (Keyword (OpEqual "")) "empty keyword"
      atomDescContains (Keyword (OpFuzzy (makeFuzzy ""))) "any keyword"
      atomDescContains (Keyword (OpEqual "travel")) "keyword travel"
      atomDescContains (Keyword OpMissing) "not tagged with any keywords"
      atomDescContains (Year (OpNe 2020)) "not in the year 2020"
      atomDescContains (Year (OpLe 2020)) "or before"
      atomDescContains (Year (OpGe 2020)) "or after"
      atomDescContains (Year (OpGt 2020)) "after the year"
      atomDescContains (Year OpNa) "does not have date information"
      atomDescContains (Season SeasonUnknown) "unknown season"
      atomDescContains (Season Winter) "taken in winter"
      atomDescContains (Month MonthUnknown) "unknown month"
      atomDescContains (Month January) "taken in January"
      atomDescContains (Day Weekday) "weekday"
      atomDescContains (Day Weekend) "weekend"
      atomDescContains (Day (MonthDay 1)) "1st"
      atomDescContains (Day DayUnknown) "unknown day"
      atomDescContains (Day Monday) "Monday"
      atomDescContains (Camera OpMissing) "no camera information"
      atomDescContains (Camera (OpEqual "")) "empty camera"
      atomDescContains (Camera (OpEqual "Nikon")) "Nikon"
      atomDescContains (Camera (OpFuzzy (makeFuzzy ""))) "any) camera"
      atomDescContains (Camera (OpFuzzy (makeFuzzy "nik"))) "named like"
      atomDescContains (Lens OpMissing) "no lens information"
      atomDescContains (Lens (OpEqual "")) "empty lens"
      atomDescContains (Lens (OpEqual "Summicron")) "Summicron"
      atomDescContains (Lens (OpFuzzy (makeFuzzy ""))) "any) lens"
      atomDescContains (Lens (OpFuzzy (makeFuzzy "sum"))) "named like"
      atomDescContains (FStop (OpNe 2.8)) "different from f/"
      atomDescContains (FStop (OpLt 2.8)) "larger than"
      atomDescContains (FStop (OpLe 2.8)) "or faster"
      atomDescContains (FStop (OpGe 2.8)) "or slower"
      atomDescContains (FStop (OpGt 2.8)) "smaller than"
      atomDescContains (FStop OpNa) "without aperture"
      atomDescContains (ShutterSpeed (OpNe 0.004)) "different fro"
      atomDescContains (ShutterSpeed (OpLt 0.004)) "faster than"
      atomDescContains (ShutterSpeed (OpLe 0.004)) "or faster"
      atomDescContains (ShutterSpeed (OpGe 0.004)) "or slower"
      atomDescContains (ShutterSpeed (OpGt 0.004)) "slower than"
      atomDescContains (ShutterSpeed OpNa) "without shutter speed"
      atomDescContains (Iso (OpEq 200)) "ISO of 200"
      atomDescContains (Iso (OpNe 200)) "different than"
      atomDescContains (Iso (OpLt 200)) "lower than"
      atomDescContains (Iso (OpLe 200)) "or lower"
      atomDescContains (Iso (OpGe 200)) "or higher"
      atomDescContains (Iso (OpGt 200)) "greater than"
      atomDescContains (Iso OpNa) "without ISO"
      atomDescContains (FocalLength (OpEq 50)) "50.0mm"
      atomDescContains (FocalLength (OpNe 50)) "different from"
      atomDescContains (FocalLength (OpLt 50)) "shorter than"
      atomDescContains (FocalLength (OpLe 50)) "or shorter"
      atomDescContains (FocalLength (OpGe 50)) "or longer"
      atomDescContains (FocalLength (OpGt 50)) "greater than"
      atomDescContains (FocalLength OpNa) "without focal length"
      atomDescContains (Problem OpMissing) "has no problems"
      atomDescContains (Problem (OpEqual "")) "empty problem"
      atomDescContains (Problem (OpEqual "x")) "problem description of x"
      atomDescContains (Problem (OpFuzzy (makeFuzzy ""))) "any problem"
      atomDescContains (Problem (OpFuzzy (makeFuzzy "trunc"))) "matches"
      atomDescContains (Type MediaImage) "is an image"
      atomDescContains (Type MediaMovie) "is a movie"
      atomDescContains (Type MediaUnknown) "unknown type"
      atomDescContains (Status ImageProcessed) "processed"
      atomDescContains (FClass FolderMixed) "mixed"
      atomDescContains (Rating OpNa) "unrated"
      atomDescContains (Rating (OpEq 4)) "4 stars"
      atomDescContains (Rating (OpNe 4)) "not rated 4"
      atomDescContains (Rating (OpLt 4)) "less than 4"
      atomDescContains (Rating (OpLe 4)) "or lower"
      atomDescContains (Rating (OpGe 4)) "or more"
      atomDescContains (Rating (OpGt 4)) "more than 4"
      atomDescContains (PplCnt (OpEq 2)) "with 2 people"
      atomDescContains (PplCnt (OpNe 2)) "not with 2 people"
      atomDescContains (PplCnt (OpLt 2)) "less than 2 people"
      atomDescContains (PplCnt (OpLe 2)) "2 people or less"
      atomDescContains (PplCnt (OpGe 2)) "2 people or more"
      atomDescContains (PplCnt (OpGt 2)) "more than 2 people"
      atomDescContains (PplCnt OpNa) "unknown number of people"
      atomDescContains (FlashSrc FlashNone) "without flash"
      atomDescContains (Megapixels (OpEq 24.2)) "megapixel count of"
      atomDescContains (Megapixels (OpNe 24.2)) "different from"
      atomDescContains (Megapixels (OpLt 24.2)) "less than"
      atomDescContains (Megapixels (OpLe 24.2)) "at most"
      atomDescContains (Megapixels (OpGe 24.2)) "at least"
      atomDescContains (Megapixels (OpGt 24.2)) "more than"
      atomDescContains (Megapixels OpNa) "unknown megapixel"
      atomDescContains (And (Month March) (Day (MonthDay 15))) "March 15th"
      atomDescContains (And (Day (MonthDay 1)) (Month January)) "January 1st"
      atomDescContains (Or (City (OpEqual "A")) (City (OpEqual "B"))) " or "
      atomDescContains (Not (Type MediaMovie)) "not "
      atomDescContains (All [Year (OpEq 2020), Country (OpEqual "Spain")]) "all of"
      atomDescContains (Any [Year (OpEq 2020), Country (OpEqual "Spain")]) "any of"
      atomDescContains ConstTrue "any and all pictures"
      atomDescContains (Birthday (OpEqual "")) "all birthdays"
      atomDescContains (Birthday (OpEqual "John")) "celebrating"
      atomDescContains (Birthday (OpFuzzy (makeFuzzy ""))) "all birthdays"
      atomDescContains (Birthday (OpFuzzy (makeFuzzy "jo"))) "named like"
      atomDescContains (Birthday OpMissing) "not a birthday"
      atomDescContains (Getaway (OpEqual "Paris")) "getaway"
      atomDescContains (GrandVacation (OpEqual "Japan")) "grand vacation"
      atomDescContains (Vacation (OpEqual "Italy")) "vacation"
      atomDescContains (WorkTrip (OpEqual "Berlin")) "work trip"
      atomDescContains (Indexer.GenericEvent (OpEqual "Meetup")) "generic event"
      atomDescContains (EventKind EKGeneric) "generic event"
      atomDescContains (EventKind EKBirthday) "birthday event"
      atomDescContains (EventKind EKGetaway) "getaway event"
      atomDescContains (EventKind EKGrandVacation) "grand vacation event"
      atomDescContains (EventKind EKWorkTrip) "work trip event"
      atomDescContains (EventKind EKNoEvent) "is not an event"

    it "shows unknown media" $
      showMedia MediaUnknown `shouldBe` "unknown"

  describe "imageSearchFunction" $ do
    it "matches populated image metadata" $ do
      imageSearchFunction (Country (OpEqual "Spain")) richImg `shouldBe` True
      imageSearchFunction (Country (OpFuzzy (makeFuzzy "spa"))) richImg `shouldBe` True
      imageSearchFunction (Country OpMissing) richImg `shouldBe` False
      imageSearchFunction (Province (OpEqual "Andalusia")) richImg `shouldBe` True
      imageSearchFunction (City (OpEqual "Seville")) richImg `shouldBe` True
      imageSearchFunction (Location (OpEqual "Alcazar")) richImg `shouldBe` True
      imageSearchFunction (Person (OpEqual "John")) richImg `shouldBe` True
      imageSearchFunction (Person (OpFuzzy (makeFuzzy "jan"))) richImg `shouldBe` True
      imageSearchFunction (Person OpMissing) richImg `shouldBe` False
      imageSearchFunction (Keyword (OpEqual "travel")) richImg `shouldBe` True
      imageSearchFunction (Keyword OpMissing) richImg `shouldBe` False
      imageSearchFunction (Title (OpEqual "Summer trip")) richImg `shouldBe` True
      imageSearchFunction (Caption (OpFuzzy (makeFuzzy "hot"))) richImg `shouldBe` True
      imageSearchFunction (Year (OpEq 2020)) richImg `shouldBe` True
      imageSearchFunction (Year (OpNe 2019)) richImg `shouldBe` True
      imageSearchFunction (Year (OpLt 2021)) richImg `shouldBe` True
      imageSearchFunction (Year (OpLe 2020)) richImg `shouldBe` True
      imageSearchFunction (Year (OpGe 2020)) richImg `shouldBe` True
      imageSearchFunction (Year (OpGt 2019)) richImg `shouldBe` True
      imageSearchFunction (Year OpNa) richImg `shouldBe` False
      imageSearchFunction (Season Summer) richImg `shouldBe` True
      imageSearchFunction (Season SeasonUnknown) richImg `shouldBe` False
      imageSearchFunction (Month July) richImg `shouldBe` True
      imageSearchFunction (Month MonthUnknown) richImg `shouldBe` False
      imageSearchFunction (Day Wednesday) richImg `shouldBe` True
      imageSearchFunction (Day Weekday) richImg `shouldBe` True
      imageSearchFunction (Day Weekend) richImg `shouldBe` False
      imageSearchFunction (Day (MonthDay 15)) richImg `shouldBe` True
      imageSearchFunction (Day DayUnknown) richImg `shouldBe` False
      imageSearchFunction (Camera (OpEqual "Nikon Z6")) richImg `shouldBe` True
      imageSearchFunction (Lens (OpEqual "Summicron")) richImg `shouldBe` True
      imageSearchFunction (Lens (OpEqual "50mm f/2")) richImg `shouldBe` True
      imageSearchFunction (FStop (OpEq 2.8)) richImg `shouldBe` True
      imageSearchFunction (ShutterSpeed (OpEq 0.004)) richImg `shouldBe` True
      imageSearchFunction (Iso (OpEq 200)) richImg `shouldBe` True
      imageSearchFunction (FocalLength (OpEq 50)) richImg `shouldBe` True
      imageSearchFunction (Problem (OpEqual "exif: truncated")) richImg `shouldBe` True
      imageSearchFunction (Type MediaImage) richImg `shouldBe` True
      imageSearchFunction (Folder (OpEqual "spain")) richImg `shouldBe` True
      imageSearchFunction (FileName (OpEqual "rich.jpg")) richImg `shouldBe` True
      imageSearchFunction (Status ImageStandalone) richImg `shouldBe` True
      imageSearchFunction (FClass FolderStandalone) richImg `shouldBe` False
      imageSearchFunction (Rating (OpEq 4)) richImg `shouldBe` True
      imageSearchFunction (PplCnt (OpEq 2)) richImg `shouldBe` True
      imageSearchFunction (KwdCnt (OpEq 2)) richImg `shouldBe` True
      imageSearchFunction (FlashSrc FlashInternal) richImg `shouldBe` True
      imageSearchFunction (FlashSrc FlashAny) richImg `shouldBe` True
      imageSearchFunction (FlashMode (OpEqual "On")) richImg `shouldBe` True
      imageSearchFunction (Megapixels (OpEq 24.2)) richImg `shouldBe` True
      imageSearchFunction (Event (OpEqual "John")) richImg `shouldBe` False
      imageSearchFunction (Indexer.GenericEvent (OpEqual "Meetup")) richImg `shouldBe` False
      imageSearchFunction (Birthday (OpEqual "John")) richImg `shouldBe` False
      imageSearchFunction (Getaway (OpEqual "Paris")) richImg `shouldBe` False
      imageSearchFunction (GrandVacation (OpEqual "Japan")) richImg `shouldBe` False
      imageSearchFunction (Vacation (OpEqual "Paris")) richImg `shouldBe` False
      imageSearchFunction (WorkTrip (OpEqual "Berlin")) richImg `shouldBe` False
      imageSearchFunction (EventKind EKBirthday) richImg `shouldBe` False
      imageSearchFunction (And (Country (OpEqual "Spain")) (Year (OpEq 2020))) richImg `shouldBe` True
      imageSearchFunction (Or (City (OpEqual "Madrid")) (City (OpEqual "Seville"))) richImg `shouldBe` True
      imageSearchFunction (Not (Type MediaMovie)) richImg `shouldBe` True
      imageSearchFunction (All [Year (OpEq 2020), Month July]) richImg `shouldBe` True
      imageSearchFunction (Any [City (OpEqual "Madrid"), Country (OpEqual "Spain")]) richImg `shouldBe` True
      imageSearchFunction ConstTrue richImg `shouldBe` True

    it "matches missing metadata on a blank image" $ do
      imageSearchFunction (Country OpMissing) blankImg `shouldBe` True
      imageSearchFunction (Person OpMissing) blankImg `shouldBe` True
      imageSearchFunction (Keyword OpMissing) blankImg `shouldBe` True
      imageSearchFunction (Year OpNa) blankImg `shouldBe` True
      imageSearchFunction (Year (OpLt 2020)) blankImg `shouldBe` False
      imageSearchFunction (Season SeasonUnknown) blankImg `shouldBe` True
      imageSearchFunction (Month MonthUnknown) blankImg `shouldBe` True
      imageSearchFunction (Day DayUnknown) blankImg `shouldBe` True
      imageSearchFunction (FlashSrc FlashUnknown) blankImg `shouldBe` True
      imageSearchFunction (Megapixels OpNa) blankImg `shouldBe` True
      imageSearchFunction (Rating OpNa) blankImg `shouldBe` True
      imageSearchFunction (PplCnt (OpEq 0)) blankImg `shouldBe` True
      imageSearchFunction (Any []) blankImg `shouldBe` False
      imageSearchFunction (All []) blankImg `shouldBe` True

    it "matches flash source variants" $ do
      imageSearchFunction (FlashSrc FlashNone) noFlashImg `shouldBe` True
      imageSearchFunction (FlashSrc FlashAny) noFlashImg `shouldBe` False
      imageSearchFunction (FlashSrc FlashExternal) onePersonImg `shouldBe` True
      imageSearchFunction (FlashSrc FlashInternal) onePersonImg `shouldBe` False
      imageSearchFunction (FlashSrc FlashAny) onePersonImg `shouldBe` True
      imageSearchFunction (FlashSrc FlashUnknown) noFlashImg `shouldBe` False

    it "classifies weekend dates" $ do
      let sat = mkImg "sat.jpg" "w" (withDate 2020 7 18 def)
          sun = mkImg "sun.jpg" "w" (withDate 2020 7 19 def)
          winter = mkImg "win.jpg" "w" (withDate 2020 12 15 def)
      imageSearchFunction (Day Saturday) sat `shouldBe` True
      imageSearchFunction (Day Weekend) sat `shouldBe` True
      imageSearchFunction (Day Weekend) sun `shouldBe` True
      imageSearchFunction (Day Weekday) sat `shouldBe` False
      imageSearchFunction (Season Winter) winter `shouldBe` True

  describe "folderSearchFunction" $ do
    it "matches folder metadata and nested images" $ do
      folderSearchFunction (Country (OpEqual "Spain")) spainDir `shouldBe` True
      folderSearchFunction (Country (OpFuzzy (makeFuzzy "spa"))) spainDir `shouldBe` True
      folderSearchFunction (Country OpMissing) spainDir `shouldBe` False
      folderSearchFunction (Country OpMissing) blankDir `shouldBe` True
      folderSearchFunction (Province (OpEqual "Andalusia")) spainDir `shouldBe` True
      folderSearchFunction (City (OpEqual "Seville")) spainDir `shouldBe` True
      folderSearchFunction (Location (OpEqual "Alcazar")) spainDir `shouldBe` True
      folderSearchFunction (Person (OpEqual "John")) spainDir `shouldBe` True
      folderSearchFunction (Person (OpFuzzy (makeFuzzy "jan"))) spainDir `shouldBe` True
      folderSearchFunction (Keyword (OpEqual "travel")) spainDir `shouldBe` True
      folderSearchFunction (Title (OpEqual "Summer trip")) spainDir `shouldBe` True
      folderSearchFunction (Caption (OpFuzzy (makeFuzzy "hot"))) spainDir `shouldBe` True
      folderSearchFunction (Year (OpEq 2020)) spainDir `shouldBe` True
      folderSearchFunction (Year (OpEq 2020)) (spainDir { pdYear = Nothing }) `shouldBe` True
      folderSearchFunction (Season Summer) spainDir `shouldBe` True
      folderSearchFunction (Month July) spainDir `shouldBe` True
      folderSearchFunction (Day (MonthDay 15)) spainDir `shouldBe` True
      folderSearchFunction (Camera (OpEqual "Nikon Z6")) spainDir `shouldBe` True
      folderSearchFunction (Lens (OpEqual "Summicron")) spainDir `shouldBe` True
      folderSearchFunction (FStop (OpEq 2.8)) spainDir `shouldBe` True
      folderSearchFunction (ShutterSpeed (OpEq 0.004)) spainDir `shouldBe` True
      folderSearchFunction (Iso (OpEq 200)) spainDir `shouldBe` True
      folderSearchFunction (FocalLength (OpEq 50)) spainDir `shouldBe` True
      folderSearchFunction (Problem (OpEqual "exif: truncated")) spainDir `shouldBe` True
      folderSearchFunction (Type MediaImage) spainDir `shouldBe` True
      folderSearchFunction (Folder (OpEqual "spain")) spainDir `shouldBe` True
      folderSearchFunction (FileName (OpEqual "rich.jpg")) spainDir `shouldBe` True
      folderSearchFunction (Status ImageStandalone) spainDir `shouldBe` True
      folderSearchFunction (FClass FolderStandalone) spainDir `shouldBe` True
      folderSearchFunction (FClass FolderEmpty) (createTestPicDir "empty") `shouldBe` True
      folderSearchFunction (Rating (OpEq 4)) spainDir `shouldBe` True
      folderSearchFunction (PplCnt (OpEq 2)) spainDir `shouldBe` True
      folderSearchFunction (KwdCnt (OpEq 2)) spainDir `shouldBe` True
      folderSearchFunction (FlashSrc FlashInternal) spainDir `shouldBe` True
      folderSearchFunction (FlashMode (OpEqual "On")) spainDir `shouldBe` True
      folderSearchFunction (Megapixels (OpEq 24.2)) spainDir `shouldBe` True
      folderSearchFunction (Megapixels (OpNe 12)) spainDir `shouldBe` True
      folderSearchFunction (Megapixels (OpLt 30)) spainDir `shouldBe` True
      folderSearchFunction (Megapixels (OpLe 24.2)) spainDir `shouldBe` True
      folderSearchFunction (Megapixels (OpGe 24.2)) spainDir `shouldBe` True
      folderSearchFunction (Megapixels (OpGt 20)) spainDir `shouldBe` True
      folderSearchFunction (Megapixels OpNa) blankDir `shouldBe` True
      folderSearchFunction (And (Folder (OpEqual "spain")) (Year (OpEq 2020))) spainDir `shouldBe` True
      folderSearchFunction (Or (Folder (OpEqual "missing")) (FClass FolderStandalone)) spainDir `shouldBe` True
      folderSearchFunction (Not (FClass FolderEmpty)) spainDir `shouldBe` True
      folderSearchFunction (All [Year (OpEq 2020), Country (OpEqual "Spain")]) spainDir `shouldBe` True
      folderSearchFunction (Any [Folder (OpEqual "missing"), Country (OpEqual "Spain")]) spainDir `shouldBe` True
      folderSearchFunction ConstTrue spainDir `shouldBe` True

    it "matches events by kind, name and people" $ do
      folderSearchFunction (Event (OpEqual "John")) spainDir `shouldBe` True
      folderSearchFunction (Event OpMissing) (createTestPicDir "none") `shouldBe` True
      folderSearchFunction (Birthday (OpEqual "John")) spainDir `shouldBe` True
      folderSearchFunction (Birthday (OpFuzzy (makeFuzzy "jo"))) spainDir `shouldBe` True
      folderSearchFunction (Birthday OpMissing) spainDir `shouldBe` False
      folderSearchFunction (Indexer.GenericEvent (OpEqual "Meetup")) genericDir `shouldBe` True
      folderSearchFunction (Indexer.GenericEvent (OpEqual "Bob")) genericDir `shouldBe` True
      folderSearchFunction (Indexer.GenericEvent (OpEqual "Meetup")) spainDir `shouldBe` False
      folderSearchFunction (Getaway (OpEqual "Paris")) getawayDir `shouldBe` True
      folderSearchFunction (Getaway (OpEqual "Eve")) getawayDir `shouldBe` True
      folderSearchFunction (Getaway (OpEqual "Paris")) spainDir `shouldBe` False
      folderSearchFunction (GrandVacation (OpEqual "Japan")) grandDir `shouldBe` True
      folderSearchFunction (GrandVacation (OpEqual "Osamu")) grandDir `shouldBe` True
      folderSearchFunction (GrandVacation (OpEqual "Japan")) getawayDir `shouldBe` False
      folderSearchFunction (Vacation (OpEqual "Paris")) getawayDir `shouldBe` True
      folderSearchFunction (Vacation (OpEqual "Japan")) grandDir `shouldBe` True
      folderSearchFunction (Vacation (OpEqual "Berlin")) workDir `shouldBe` False
      folderSearchFunction (WorkTrip (OpEqual "Berlin")) workDir `shouldBe` True
      folderSearchFunction (WorkTrip (OpEqual "Ada")) workDir `shouldBe` True
      folderSearchFunction (WorkTrip (OpEqual "Berlin")) spainDir `shouldBe` False
      folderSearchFunction (EventKind EKBirthday) spainDir `shouldBe` True
      folderSearchFunction (EventKind EKGeneric) genericDir `shouldBe` True
      folderSearchFunction (EventKind EKGetaway) getawayDir `shouldBe` True
      folderSearchFunction (EventKind EKGrandVacation) grandDir `shouldBe` True
      folderSearchFunction (EventKind EKWorkTrip) workDir `shouldBe` True
      folderSearchFunction (EventKind EKNoEvent) (createTestPicDir "none") `shouldBe` True

  describe "getAtoms, maps and quick search" $ do
    it "builds stats for a populated repository" $ do
      getAtoms TCountry populatedRepo `shouldSatisfy` (not . null)
      [d | (_, Just d, _) <- getAtoms TPplCnt populatedRepo]
        `shouldSatisfy` (not . null)
      [d | (_, Just d, _) <- getAtoms TKwdCnt populatedRepo]
        `shouldSatisfy` (not . null)
      [d | (_, Just d, _) <- getAtoms TFlashSrc populatedRepo]
        `shouldSatisfy` any ("flash" `Text.isInfixOf`)
      getAtoms TType populatedRepo `shouldSatisfy` (not . null)
      getAtoms TStatus populatedRepo `shouldSatisfy` (not . null)
      getAtoms TFClass populatedRepo `shouldSatisfy` (not . null)
      getAtoms TFolder populatedRepo `shouldSatisfy` (not . null)
      getAtoms TFileName populatedRepo `shouldSatisfy` (not . null)
      getAtoms TEvent populatedRepo `shouldBe` []
      getAtoms TGenericEvent populatedRepo `shouldBe` []
      getAtoms TBirthday populatedRepo `shouldBe` []
      getAtoms TGetaway populatedRepo `shouldBe` []
      getAtoms TGrandVacation populatedRepo `shouldBe` []
      getAtoms TVacation populatedRepo `shouldBe` []
      getAtoms TWorkTrip populatedRepo `shouldBe` []
      getAtoms TEventKind populatedRepo `shouldSatisfy` (not . null)

    it "builds image and folder maps" $ do
      let early = mkImg "a.jpg" "f" (withDate 2020 1 1 def)
          late = mkImg "b.jpg" "f" (withDate 2020 6 1 def)
          repo = testRepo [picDirWith "f" [early, late], spainDir]
          (pics, folders) = buildImageMap ConstTrue repo
      Map.size pics `shouldBe` 3
      Map.lookup "f" folders `shouldBe` Just early
      Map.member "spain" (buildFolderMap (Country (OpEqual "Spain")) repo)
        `shouldBe` True
      Map.member "f" (buildFolderMap (Country (OpEqual "Spain")) repo)
        `shouldBe` False

    it "generates quick search params" $ do
      genQuickSearchParams populatedRepo ""
        `shouldBeLeftWithMessage` "Empty search parameter"
      case genQuickSearchParams populatedRepo "country:Spain" of
        Right (_, Just atom) -> imageSearchFunction atom richImg `shouldBe` True
        other -> expectationFailure ("expected a country search, got " <> show other)
      case genQuickSearchParams populatedRepo "year:2020" of
        Right (_, Just atom) -> imageSearchFunction atom richImg `shouldBe` True
        other -> expectationFailure ("expected a year search, got " <> show other)
      case genQuickSearchParams populatedRepo "Spain" of
        Right (_, Just atom) -> folderSearchFunction atom spainDir `shouldBe` True
        other -> expectationFailure ("expected a hit for Spain, got " <> show other)
      case genQuickSearchParams populatedRepo "zzznomatch" of
        Right (_, Just atom) -> atomFindsFiles atom `shouldBe` True
        other -> expectationFailure ("expected a fruitless atom, got " <> show other)
      case genQuickSearchParams populatedRepo "2020 Spain" of
        Right (_, Just atom) -> do
          folderSearchFunction atom spainDir `shouldBe` True
        other -> expectationFailure ("expected a combined search, got " <> show other)
      case genQuickSearchParams populatedRepo "andalusia" of
        Right (_, Just atom) -> do
          folderSearchFunction atom spainDir `shouldBe` True
          atom `shouldBe` Province (OpFuzzy (makeFuzzy "andalusia"))
        other -> expectationFailure ("expected a combined search, got " <> show other)
