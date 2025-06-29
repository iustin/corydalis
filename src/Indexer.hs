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

{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE ViewPatterns       #-}

module Indexer ( Symbol(..)
               , Atom(..)
               , NumOp(..)
               , MonthOp(..)
               , DayOp(..)
               , symbolNames
               , symbolName
               , negSymbolName
               , symbolFindsFiles
               , atomTypeDescriptions
               , atomDescription
               , parseAtom
               , atomToParams
               , buildMissingAtom
               , folderSearchFunction
               , imageSearchFunction
               , getAtoms
               , parseAtomParams
               , buildImageMap
               , buildFolderMap
               , searchImages
               , genQuickSearchParams
               , atomFindsFiles
               -- TODO: move to some more basic module, unify with pics
               , parseDecimalPlain
               , intToMonth
               , intToWeekDay
               , seasonStats
               ) where

import           Control.Monad               (foldM, when)
import           Data.List                   (foldl', nub, partition)
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromMaybe, isJust, isNothing,
                                              mapMaybe)
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.Read              as Text
import           Data.Time.Calendar          (toGregorian)
import           Data.Time.Calendar.WeekDate (toWeekDate)
import           Data.Time.LocalTime
import           Formatting
import           Yesod                       (PathPiece (..))

import           Exif
import           Pics

import           Types

data Symbol = TCountry
            | TProvince
            | TCity
            | TLocation
            | TPerson
            | TKeyword
            | TTitle
            | TCaption
            | TYear
            | TSeason
            | TMonth
            | TDay
            | TCamera
            | TLens
            | TFStop
            | TShutterSpeed
            | TIso
            | TFocalLength
            | TProblem
            | TType
            | TFolder
            | TFileName
            | TStatus
            | TFClass
            | TRating
            | TPplCnt
            | TKwdCnt
            | TFlashSrc
            | TFlashMode
            | TMegapixels
            deriving (Enum, Bounded, Show, Read, Eq)

instance PathPiece Symbol where
  toPathPiece TCountry      = "countries"
  toPathPiece TProvince     = "provinces"
  toPathPiece TCity         = "cities"
  toPathPiece TLocation     = "locations"
  toPathPiece TPerson       = "people"
  toPathPiece TKeyword      = "keywords"
  toPathPiece TTitle        = "title"
  toPathPiece TCaption      = "caption"
  toPathPiece TYear         = "years"
  toPathPiece TSeason       = "seasons"
  toPathPiece TMonth        = "months"
  toPathPiece TDay          = "days"
  toPathPiece TCamera       = "cameras"
  toPathPiece TLens         = "lenses"
  toPathPiece TFStop        = "f-stops"
  toPathPiece TShutterSpeed = "shutter-speed"
  toPathPiece TIso          = "iso"
  toPathPiece TFocalLength  = "focal-length"
  toPathPiece TProblem      = "problems"
  toPathPiece TType         = "types"
  toPathPiece TFolder       = "folders"
  toPathPiece TFileName     = "filenames"
  toPathPiece TStatus       = "image-status"
  toPathPiece TFClass       = "folder-class"
  toPathPiece TRating       = "rating"
  toPathPiece TPplCnt       = "people-count"
  toPathPiece TKwdCnt       = "keyword-count"
  toPathPiece TFlashSrc     = "flash-source"
  toPathPiece TFlashMode    = "flash-mode"
  toPathPiece TMegapixels   = "megapixels"
  fromPathPiece "countries"     = Just TCountry
  fromPathPiece "provinces"     = Just TProvince
  fromPathPiece "cities"        = Just TCity
  fromPathPiece "locations"     = Just TLocation
  fromPathPiece "people"        = Just TPerson
  fromPathPiece "keywords"      = Just TKeyword
  fromPathPiece "title"         = Just TTitle
  fromPathPiece "caption"       = Just TCaption
  fromPathPiece "years"         = Just TYear
  fromPathPiece "seasons"       = Just TSeason
  fromPathPiece "months"        = Just TMonth
  fromPathPiece "days"          = Just TDay
  fromPathPiece "cameras"       = Just TCamera
  fromPathPiece "lenses"        = Just TLens
  fromPathPiece "f-stops"       = Just TFStop
  fromPathPiece "shutter-speed" = Just TShutterSpeed
  fromPathPiece "iso"           = Just TIso
  fromPathPiece "focal-length"  = Just TFocalLength
  fromPathPiece "problems"      = Just TProblem
  fromPathPiece "types"         = Just TType
  fromPathPiece "folders"       = Just TFolder
  fromPathPiece "filenames"     = Just TFileName
  fromPathPiece "image-status"  = Just TStatus
  fromPathPiece "folder-class"  = Just TFClass
  fromPathPiece "rating"        = Just TRating
  fromPathPiece "people-count"  = Just TPplCnt
  fromPathPiece "keyword-count" = Just TKwdCnt
  fromPathPiece "flash-source"  = Just TFlashSrc
  fromPathPiece "flash-mode"    = Just TFlashMode
  fromPathPiece "megapixels"    = Just TMegapixels
  fromPathPiece _               = Nothing

symbolFindsFiles :: Symbol -> Bool
symbolFindsFiles TFClass = False
symbolFindsFiles _       = True

-- TODO: Replace this with Data.CaseInsensitive from case-insensitive
-- package, once the actual image metadata uses it too.
newtype FuzzyText = FuzzyText { unFuzzy :: Text }
  deriving (Show, Eq)

makeFuzzy :: Text -> FuzzyText
makeFuzzy = FuzzyText . Text.toCaseFold

data StrOp = OpEqual Text
           | OpFuzzy FuzzyText
           | OpMissing
           deriving (Show, Eq)

data NumOp a where
  OpEq :: (Eq a) => a -> NumOp a
  OpNe :: (Eq a) => a -> NumOp a
  OpNa :: NumOp a
  OpLt :: (Num a, Ord a) => a -> NumOp a
  OpLe :: (Num a, Ord a) => a -> NumOp a
  OpGe :: (Num a, Ord a) => a -> NumOp a
  OpGt :: (Num a, Ord a) => a -> NumOp a

deriving instance Show a => Show (NumOp a)
deriving instance Eq   a => Eq   (NumOp a)

data SeasonOp
  = Spring
  | Summer
  | Autumn
  | Winter
  | SeasonUnknown
    deriving (Show, Eq, Ord)

data MonthOp
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  | MonthUnknown
  deriving (Show, Eq, Ord)

data DayOp
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  | Weekday
  | Weekend
  | MonthDay Int
  | DayUnknown
  deriving (Show, Eq, Ord)


data FlashOp
 = FlashNone
 | FlashInternal
 | FlashExternal
 | FlashAny
 | FlashUnknown
 deriving (Show, Eq, Ord)

parseFlash :: Text -> Maybe FlashOp
parseFlash v
  | v == "none"                    = Just FlashNone
  | v == "internal" || v == "int"  = Just FlashInternal
  | v == "external" || v == "ext"  = Just FlashExternal
  | v == "yes" || v == "any"       = Just FlashAny
  | otherwise                      = Nothing

showFlash :: FlashOp -> Text
showFlash FlashNone     = "none"
showFlash FlashInternal = "internal"
showFlash FlashExternal = "external"
showFlash FlashAny      = "any"
showFlash FlashUnknown  = "unknown"

fuzzyMatch :: FuzzyText -> Text -> Bool
fuzzyMatch fz =
  (unFuzzy fz `Text.isInfixOf`) . Text.toCaseFold

evalStr :: StrOp -> Maybe Text -> Bool
evalStr (OpEqual a) = (Just a ==)
evalStr (OpFuzzy a) = maybe False (fuzzyMatch a)
evalStr  OpMissing  = isNothing

evalNum :: NumOp a -> Maybe a -> Bool
evalNum (OpEq a) = (== Just a)
evalNum (OpNe a) = maybe False (/= a)
evalNum (OpLt a) = maybe False (< a)
evalNum (OpLe a) = maybe False (<= a)
evalNum (OpGe a) = maybe False (>= a)
evalNum (OpGt a) = maybe False (> a)
evalNum OpNa     = isNothing

data Atom = Country  StrOp
          | Province StrOp
          | City     StrOp
          | Location StrOp
          | Person   StrOp
          | Keyword  StrOp
          | Title    StrOp
          | Caption  StrOp
          | Year     (NumOp Integer)
          | Season   SeasonOp
          | Month    MonthOp
          | Day      DayOp
          | Camera   StrOp
          | Lens     StrOp
          | FStop    (NumOp Double)
          | ShutterSpeed (NumOp Double)
          | Iso      (NumOp Integer)
          | FocalLength (NumOp Double)
          | Problem  StrOp
          | Type     MediaType
          | Folder   StrOp
          | FileName StrOp
          | Status   ImageStatus
          | FClass   FolderClass
          | Rating   (NumOp Int)
          | PplCnt   (NumOp Int)
          | KwdCnt   (NumOp Int)
          | FlashSrc FlashOp
          | FlashMode StrOp
          | Megapixels (NumOp Double)
          -- Meta atoms below
          | And Atom Atom
          | Or  Atom Atom
          | Not Atom
          | All [Atom]
          | Any [Atom]
          | ConstTrue
          deriving (Show, Eq)

symbolNames :: [(Symbol, Text)]
symbolNames = map (\t -> (t, symbolName t)) [minBound..maxBound]

symbolName :: Symbol -> Text
symbolName TCountry      = "country"
symbolName TProvince     = "province"
symbolName TCity         = "city"
symbolName TLocation     = "location"
symbolName TPerson       = "person"
symbolName TKeyword      = "keyword"
symbolName TTitle        = "title"
symbolName TCaption      = "caption"
symbolName TYear         = "year"
symbolName TSeason       = "season"
symbolName TMonth        = "month"
symbolName TDay          = "day"
symbolName TCamera       = "camera"
symbolName TLens         = "lens"
symbolName TFStop        = "f-stop"
symbolName TShutterSpeed = "shutter-speed"
symbolName TIso          = "iso"
symbolName TFocalLength  = "focal-length"
symbolName TProblem      = "problem"
symbolName TType         = "type"
symbolName TFolder       = "folder"
symbolName TFileName     = "filename"
symbolName TStatus       = "status"
symbolName TFClass       = "folder-class"
symbolName TRating       = "rating"
symbolName TPplCnt       = "people-count"
symbolName TKwdCnt       = "keyword-count"
symbolName TFlashSrc     = "flash-source"
symbolName TFlashMode    = "flash-mode"
symbolName TMegapixels   = "megapixels"

negSymbolName :: Symbol -> Text
negSymbolName atom = "no-" <> symbolName atom

parseSymbol :: Text -> Maybe Symbol
parseSymbol "country"       = Just TCountry
parseSymbol "province"      = Just TProvince
parseSymbol "city"          = Just TCity
parseSymbol "location"      = Just TLocation
parseSymbol "person"        = Just TPerson
parseSymbol "keyword"       = Just TKeyword
parseSymbol "title"         = Just TTitle
parseSymbol "caption"       = Just TCaption
parseSymbol "year"          = Just TYear
parseSymbol "season"        = Just TSeason
parseSymbol "month"         = Just TMonth
parseSymbol "day"           = Just TDay
parseSymbol "camera"        = Just TCamera
parseSymbol "lens"          = Just TLens
parseSymbol "f-stop"        = Just TFStop
parseSymbol "shutter-speed" = Just TShutterSpeed
parseSymbol "iso"           = Just TIso
parseSymbol "focal-length"  = Just TFocalLength
parseSymbol "problem"       = Just TProblem
parseSymbol "type"          = Just TType
parseSymbol "folder"        = Just TFolder
parseSymbol "filename"      = Just TFileName
parseSymbol "status"        = Just TStatus
parseSymbol "folder-class"  = Just TFClass
parseSymbol "rating"        = Just TRating
parseSymbol "people-count"  = Just TPplCnt
parseSymbol "keyword-count" = Just TKwdCnt
parseSymbol "flash-source"  = Just TFlashSrc
parseSymbol "flash-mode"    = Just TFlashMode
parseSymbol "megapixels"    = Just TMegapixels
parseSymbol _               = Nothing

buildMissingAtom :: Symbol -> Atom
buildMissingAtom s =
  case s of
    TCountry      -> Country   OpMissing
    TProvince     -> Province  OpMissing
    TCity         -> City      OpMissing
    TLocation     -> Location  OpMissing
    TPerson       -> Person    OpMissing
    TKeyword      -> Keyword   OpMissing
    TTitle        -> Title     OpMissing
    TCaption      -> Caption   OpMissing
    TYear         -> Year      OpNa
    TSeason       -> Season    SeasonUnknown
    TMonth        -> Month     MonthUnknown
    TDay          -> Day       DayUnknown
    TCamera       -> Camera    OpMissing
    TLens         -> Lens      OpMissing
    TFStop        -> FStop     OpNa
    TShutterSpeed -> ShutterSpeed OpNa
    TIso          -> Iso       OpNa
    TFocalLength  -> FocalLength OpNa
    TProblem      -> Problem   OpMissing
    TType         -> Type      MediaUnknown
    TRating       -> Rating    OpNa
    TFlashSrc     -> FlashSrc  FlashUnknown
    TFlashMode    -> FlashMode OpMissing
    TMegapixels   -> Megapixels OpNa
    -- FIXME: these should fail instead (using Maybe).
    TFolder       -> error "No missing atom for folder"
    TFileName     -> error "No missing atom for filename"
    TStatus       -> error "No missing atom for status"
    TFClass       -> error "No missing atom for folder class"
    TPplCnt       -> error "No missing atom for people count"
    TKwdCnt       -> error "No missing atom for keyword count"

parseAtom :: Text -> Text -> Maybe Atom
parseAtom (Text.splitAt 3 -> ("no-", v)) _ =
  buildMissingAtom <$> parseSymbol v

parseAtom a v = do
  s <- parseSymbol a
  let dec = parseDecimal v
      intDec = parseDecimal v
      double = parseReal v
      str = parseString v
      typ = parseType v
      sta = parseImageStatus v
  case s of
    TCountry      -> Country      <$> str
    TProvince     -> Province     <$> str
    TCity         -> City         <$> str
    TLocation     -> Location     <$> str
    TPerson       -> Person       <$> str
    TKeyword      -> Keyword      <$> str
    TTitle        -> Title        <$> str
    TCaption      -> Caption      <$> str
    TYear         -> Year         <$> dec
    TSeason       -> Season       <$> parseSeason v
    TMonth        -> Month        <$> parseMonth v
    TDay          -> Day          <$> parseDay v
    TCamera       -> Camera       <$> str
    TLens         -> Lens         <$> str
    TFStop        -> FStop        <$> double
    TShutterSpeed -> ShutterSpeed <$> parseShutterSpeed v
    TIso          -> Iso          <$> parseDecimal v
    TFocalLength  -> FocalLength  <$> double
    TProblem      -> Problem      <$> str
    TType         -> Type         <$> typ
    TFolder       -> Folder       <$> str
    TFileName     -> FileName     <$> str
    TStatus       -> Status       <$> sta
    TFClass       -> FClass       <$> parseFolderClass v
    TRating       -> Rating       <$> intDec
    TPplCnt       -> PplCnt       <$> intDec
    TKwdCnt       -> KwdCnt       <$> intDec
    TFlashSrc     -> FlashSrc     <$> parseFlash v
    TFlashMode    -> FlashMode    <$> str
    TMegapixels   -> Megapixels   <$> double

quickSearch :: Symbol -> Text -> Maybe Atom
quickSearch s v =
  case s of
    TCountry      -> fuzzer Country
    TProvince     -> fuzzer Province
    TCity         -> fuzzer City
    TLocation     -> fuzzer Location
    TPerson       -> fuzzer Person
    TKeyword      -> fuzzer Keyword
    TTitle        -> fuzzer Title
    TCaption      -> fuzzer Caption
    TCamera       -> fuzzer Camera
    TLens         -> fuzzer Lens
    TFStop        -> FStop  <$> real
    TShutterSpeed -> ShutterSpeed  <$> parseShutterSpeed v
    TIso          -> Iso    <$> parseDecimal v
    TFocalLength  -> FocalLength <$> real
    TProblem      -> fuzzer Problem
    TYear         -> Year   <$> parseDecimal v
    TSeason       -> Season <$> parseSeason v
    TMonth        -> Month  <$> parseMonth v
    TDay          -> Day    <$> parseDay v
    TType         -> Type   <$> parseType v
    TFolder       -> fuzzer Folder
    TFileName     -> fuzzer FileName
    TStatus       -> Status <$> parseImageStatus v
    TFClass       -> FClass <$> parseFolderClass v
    TRating       -> Rating <$> dec
    TPplCnt       -> PplCnt <$> dec
    TKwdCnt       -> KwdCnt <$> dec
    TFlashSrc     -> FlashSrc <$> parseFlash v
    TFlashMode    -> fuzzer FlashMode
    TMegapixels   -> Megapixels <$> real
  where f = makeFuzzy v
        fuzzer c = Just . c . OpFuzzy $ f
        dec = parseDecimal v
        real = parseReal v

atomTypeDescriptions :: Symbol -> Text
atomTypeDescriptions TCountry      = "countries"
atomTypeDescriptions TProvince     = "provinces"
atomTypeDescriptions TCity         = "cities"
atomTypeDescriptions TLocation     = "locations"
atomTypeDescriptions TPerson       = "people"
atomTypeDescriptions TKeyword      = "keywords"
atomTypeDescriptions TTitle        = "image titles"
atomTypeDescriptions TCaption      = "image captions"
atomTypeDescriptions TYear         = "years"
atomTypeDescriptions TSeason       = "seasons"
atomTypeDescriptions TMonth        = "months"
atomTypeDescriptions TDay          = "days"
atomTypeDescriptions TCamera       = "cameras"
atomTypeDescriptions TLens         = "lenses"
atomTypeDescriptions TFStop        = "f-stops"
atomTypeDescriptions TShutterSpeed = "shutter speeds"
atomTypeDescriptions TIso          = "ISO values"
atomTypeDescriptions TFocalLength  = "focal lengths"
atomTypeDescriptions TProblem      = "problems"
atomTypeDescriptions TType         = "types"
atomTypeDescriptions TFolder       = "folders"
atomTypeDescriptions TFileName     = "filenames"
atomTypeDescriptions TStatus       = "image statuses"
atomTypeDescriptions TFClass       = "folder classes"
atomTypeDescriptions TRating       = "ratings"
atomTypeDescriptions TPplCnt       = "people count"
atomTypeDescriptions TKwdCnt       = "keyword count"
atomTypeDescriptions TFlashSrc     = "flash source"
atomTypeDescriptions TFlashMode    = "flash mode"
atomTypeDescriptions TMegapixels   = "image megapixels"

class (Show a) => ToText a where
  toText :: a -> Text
  toText = sformat shown

instance ToText Text where
  toText = id

instance ToText Integer

instance ToText Int

instance ToText Double

instance ToText SeasonOp where
  toText = showSeason

instance ToText MonthOp where
  toText = showMonth

instance ToText DayOp where
  toText = showDay

instance ToText MediaType where
  toText = showMedia

describeEq :: (ToText a) => Text -> a -> Text
describeEq a v = a <> " is " <> toText v <> ""

-- | Describe a value.
--
-- 'describe' will show either lack of information about subject, or
-- tagged with empty value, or the actual value.
describeStr :: Text -> StrOp -> Text
describeStr a (OpEqual "") = a <> " is empty"
describeStr a (OpEqual v)  = describeEq a v
describeStr a (OpFuzzy v)  = a <> " contains " <> unFuzzy v
describeStr a  OpMissing   = "has no " <> a <> " information"

-- | Describe a numeric value
describeNumHaving :: Integral a => Text -> NumOp a -> Text
describeNumHaving t (OpEq v) = sformat ("with " % int % " " % stext) v t
describeNumHaving t (OpNe v) = sformat ("not with " % int % " " % stext) v t
describeNumHaving t (OpLt v) = sformat ("with less than " % int % " " % stext) v t
describeNumHaving t (OpLe v) = sformat ("with " % int % " " % stext % " or less") v t
describeNumHaving t (OpGe v) = sformat ("with " % int % " " % stext % " or more") v t
describeNumHaving t (OpGt v) = sformat ("with more than " % int % " " % stext) v t
describeNumHaving t OpNa     = sformat ("with unknown number of " % stext) t

-- | Formats a day of the month, e.g. "March 15th".
formatDayOfTheMonth :: Int -> MonthOp -> Text
formatDayOfTheMonth d m =   "taken on " <> toText m <> " " <> showOrdinal d

atomDescription :: Atom -> Text
atomDescription (Country  place) = describeStr "country"  place

atomDescription (Province place) = describeStr "province" place

atomDescription (City     place) = describeStr "city"     place

atomDescription (Location place) = describeStr "location" place

atomDescription (Person   (OpEqual who)) =
  case who of
    "" -> "has an empty person tag"
    p  -> formatPerson False p <> " is in the picture"
atomDescription (Person (OpFuzzy v)) =
  "tagged with a person named like " <> unFuzzy v
atomDescription (Person OpMissing)   = "has no person information"

atomDescription (Keyword (OpEqual keyword)) =
  case keyword of
    "" -> "tagged with an empty keyword"
    k  -> "tagged with keyword " <> k <> ""
atomDescription (Keyword (OpFuzzy v)) =
  "tagged with a keyword containing " <> unFuzzy v
atomDescription (Keyword OpMissing)   = "not tagged with any keywords"

atomDescription (Title t)              = describeStr "title" t

atomDescription (Caption t)            = describeStr "caption" t

atomDescription (Year (OpEq year))    = "taken in the year " <> toText year
atomDescription (Year (OpNe year))    = "taken not in the year " <> toText year
atomDescription (Year (OpLt year))    = "taken before the year " <> toText year
atomDescription (Year (OpLe year))    = "taken in, or before the year " <> toText year
atomDescription (Year (OpGe year))    = "taken in, or after the year " <> toText year
atomDescription (Year (OpGt year))    = "taken after the year " <> toText year
atomDescription (Year OpNa)           = "does not have date information"

atomDescription (Season SeasonUnknown) = "taken in an unknown season"
atomDescription (Season s)            = "taken in " <> toText s

atomDescription (Month MonthUnknown)  = "taken in an unknown month"
atomDescription (Month m)             = "taken in " <> toText m

atomDescription (Day Weekday)         = "taken on an weekday"
atomDescription (Day Weekend)         = "taken on an weekend"
atomDescription (Day (MonthDay d))    = "taken on a " <> showOrdinal d <> " day of the month"
atomDescription (Day DayUnknown)      = "taken on an unknown day"
atomDescription (Day d)               = "taken on a " <> toText d

atomDescription (Camera OpMissing)    = "has no camera information"
atomDescription (Camera (OpEqual "")) = "has defined but empty camera information"
atomDescription (Camera (OpEqual v))  = "shot with a " <> v <> " camera"
atomDescription (Camera (OpFuzzy v))  =
  "shot with a camera named like " <> unFuzzy v

atomDescription (Lens OpMissing)      = "has no lens information"
atomDescription (Lens (OpEqual ""))   = "has defined but empty lens information"
atomDescription (Lens (OpEqual v))    = "shot with a " <> v <> " lens"
atomDescription (Lens (OpFuzzy v))    =
  "shot with a lens named like " <> unFuzzy v

atomDescription (FStop (OpEq fstop))   = "shot at an aperture of f/" <> toText fstop
atomDescription (FStop (OpNe fstop))   = "shot at an aperture different from f/" <> toText fstop
atomDescription (FStop (OpLt fstop))   = "shot at an aperture larger than f/" <> toText fstop
atomDescription (FStop (OpLe fstop))   = "shot at the aperture of f/" <> toText fstop <> " or faster"
atomDescription (FStop (OpGe fstop))   = "shot at the aperture of f/" <> toText fstop <> " or slower"
atomDescription (FStop (OpGt fstop))   = "shot at an aperture smaller than f/" <> toText fstop
atomDescription (FStop OpNa)           = "without aperture information"

atomDescription (ShutterSpeed (OpEq speed))   = "shot with a shutter speed of " <> showShutterSpeed speed
atomDescription (ShutterSpeed (OpNe speed))   = "shot with shutter speed different fro " <> showShutterSpeed speed
atomDescription (ShutterSpeed (OpLt speed))   = "shot with a shutter speed faster than " <> showShutterSpeed speed
atomDescription (ShutterSpeed (OpLe speed))   = "shot with a shutter speed of " <> showShutterSpeed speed <> " or faster"
atomDescription (ShutterSpeed (OpGe speed))   = "shot with a shutter speed of " <> showShutterSpeed speed <> " or slower"
atomDescription (ShutterSpeed (OpGt speed))   = "shot with a shutter speed slower than " <> showShutterSpeed speed
atomDescription (ShutterSpeed OpNa)           = "without shutter speed information"

atomDescription (Iso (OpEq iso))   = "shot with an ISO of " <> toText iso
atomDescription (Iso (OpNe iso))   = "shot with an ISO different than " <> toText iso
atomDescription (Iso (OpLt iso))   = "shot with an ISO lower than " <> toText iso
atomDescription (Iso (OpLe iso))   = "shot with an ISO of " <> toText iso <> " or lower"
atomDescription (Iso (OpGe iso))   = "shot with an ISO of " <> toText iso <> " or higher"
atomDescription (Iso (OpGt iso))   = "shot with an ISO greater than " <> toText iso
atomDescription (Iso OpNa)           = "without ISO information"

atomDescription (FocalLength (OpEq fstop))   = "shot at a focal length of " <> toText fstop <> "mm"
atomDescription (FocalLength (OpNe fstop))   = "shot at a focal length different from " <> toText fstop <> "mm"
atomDescription (FocalLength (OpLt fstop))   = "shot at a focal length shorter than " <> toText fstop <> "mm"
atomDescription (FocalLength (OpLe fstop))   = "shot at a focal length of " <> toText fstop <> "mm or shorter"
atomDescription (FocalLength (OpGe fstop))   = "shot at a focal length of " <> toText fstop <> "mm or longer"
atomDescription (FocalLength (OpGt fstop))   = "shot at a focal length greater than " <> toText fstop <> "mm"
atomDescription (FocalLength OpNa)           = "without focal length information"

atomDescription (Problem OpMissing)    = "has no problems"
atomDescription (Problem (OpEqual "")) = "has an empty problem description"
atomDescription (Problem (OpEqual v))  = "has a problem description of " <> v
atomDescription (Problem (OpFuzzy v))  =
  "has a problem that matches " <> unFuzzy v

atomDescription (Type MediaImage)      = "is an image"
atomDescription (Type MediaMovie)      = "is a movie"
atomDescription (Type MediaUnknown)    = "is of unknown type"

atomDescription (Folder s)             = describeStr "folder" s

atomDescription (FileName s)           = describeStr "filename" s

atomDescription (Status v)             = "image status is " <> showImageStatus v

atomDescription (FClass v)             = "folder class is " <> showFolderClass v

atomDescription (Rating OpNa)          = "unrated"
atomDescription (Rating (OpEq v))      = sformat ("rated with " % int % " stars") v
atomDescription (Rating (OpNe v))      = sformat ("not rated " % int % " stars") v
atomDescription (Rating (OpLt v))      = sformat ("rated with less than " % int % " stars") v
atomDescription (Rating (OpLe v))      = sformat ("rated with " % int % " stars or lower") v
atomDescription (Rating (OpGe v))      = sformat ("rated with " % int % " stars or more") v
atomDescription (Rating (OpGt v))      = sformat ("rated with more than " % int % " stars") v

atomDescription (PplCnt v)             = describeNumHaving "people" v
atomDescription (KwdCnt v)             = describeNumHaving "keywords" v

atomDescription (FlashSrc v)           = formatFlashSource v

atomDescription (FlashMode mode)       = describeStr "flash mode" mode

atomDescription (Megapixels (OpEq megapixels))   = "with a megapixel count of " <> toText megapixels
atomDescription (Megapixels (OpNe megapixels))   = "with a megapixel count different from " <> toText megapixels
atomDescription (Megapixels (OpLt megapixels))   = "with a megapixel count less than " <> toText megapixels
atomDescription (Megapixels (OpLe megapixels))   = "with a megapixel count of at most " <> toText megapixels
atomDescription (Megapixels (OpGe megapixels))   = "with a megapixel count of at least " <> toText megapixels
atomDescription (Megapixels (OpGt megapixels))   = "with a megapixel count of more than" <> toText megapixels
atomDescription (Megapixels OpNa)                = "with an unknown megapixel count"

atomDescription (And (Month m) (Day (MonthDay d))) = formatDayOfTheMonth d m
atomDescription (And (Day (MonthDay d)) (Month m)) = formatDayOfTheMonth d m

atomDescription (And a b) =
  mconcat [ "("
          , atomDescription a
          , " and "
          , atomDescription b
          , ")"
          ]

atomDescription (Or a b) =
  mconcat [ "("
          , atomDescription a
          , " or "
          , atomDescription b
          , ")"
          ]
atomDescription (Not a)  = "(not " <> atomDescription a <> ")"
atomDescription (All as) = "(all of: " <> Text.intercalate ", " (map atomDescription as) <> ")"
atomDescription (Any as) = "(any of: " <> Text.intercalate ", " (map atomDescription as) <> ")"
atomDescription ConstTrue = "any and all pictures"

-- | Set search function for either membership or null set checking.
setSearch :: StrOp -> Set Text -> Bool
setSearch OpMissing = Set.null
setSearch (OpEqual v) = (v `Set.member`)
setSearch (OpFuzzy f)=
  Set.foldr' (\a v -> v || fuzzyMatch f a) False

-- | NameStats search function for either membership or null set checking.
nameStatsSearch :: StrOp -> NameStats Text -> Bool
nameStatsSearch OpMissing =
  maybe False (> 0) . (Nothing `Map.lookup`)
nameStatsSearch (OpEqual v) =
  maybe False (> 0) . (Just v `Map.lookup`)
nameStatsSearch (OpFuzzy f) =
  Map.foldrWithKey' (\k v a ->
                       let found = maybe False (fuzzyMatch f) k && v > 0
                       in found || a) False

-- | NameStats search function for numeric values.
numStatsSearch :: (Ord a) => NumOp a -> NameStats a -> Bool
numStatsSearch OpNa m =
  maybe False (> 0) (Nothing `Map.lookup` m)
numStatsSearch (OpEq v) m =
  maybe False (> 0) (Just v `Map.lookup` m)
numStatsSearch (OpNe v) m =
  Map.foldrWithKey' (\k count acc ->
                      acc || (isJust k && k /= Just v && count > 0))
                    False m
numStatsSearch (OpLt v) m =
  Map.foldrWithKey' (\k count acc ->
                      acc || (maybe False (< v) k && count > 0))
                    False m
numStatsSearch (OpLe v) m =
  Map.foldrWithKey' (\k count acc ->
                      acc || (maybe False (<= v) k && count > 0))
                    False m
numStatsSearch (OpGe v) m =
  Map.foldrWithKey' (\k count acc ->
                      acc || (maybe False (>= v) k && count > 0))
                    False m
numStatsSearch (OpGt v) m =
  Map.foldrWithKey' (\k count acc ->
                      acc || (maybe False (> v) k && count > 0))
                    False m

flashSearch :: FlashOp -> Maybe FlashSource -> Bool
flashSearch FlashUnknown  Nothing                     = True
flashSearch FlashUnknown  _                           = False
flashSearch FlashInternal (Just FlashSourceInternal)  = True
flashSearch FlashInternal _                           = False
flashSearch FlashExternal (Just FlashSourceExternal)  = True
flashSearch FlashExternal _                           = False
flashSearch FlashAny      (Just a)                    = a == FlashSourceInternal || a == FlashSourceExternal
flashSearch FlashAny      Nothing                     = False
flashSearch FlashNone     (Just FlashSourceNone)      = True
flashSearch FlashNone     _                           = False

-- TODO: implement searching type=unknown after untracked merging into image.
folderSearchFunction :: Atom -> PicDir -> Bool
folderSearchFunction (Country loc) =
  nameStatsSearch loc . gExifCountries . pdExif

folderSearchFunction (Province loc) =
  nameStatsSearch loc . gExifProvinces . pdExif

folderSearchFunction (City loc) =
  nameStatsSearch loc . gExifCities . pdExif

folderSearchFunction (Location loc) =
  nameStatsSearch loc . gExifLocations . pdExif

folderSearchFunction (Person who) =
  nameStatsSearch who . gExifPeople . pdExif

folderSearchFunction (Keyword k) =
  nameStatsSearch k . gExifKeywords . pdExif

folderSearchFunction (Title t) =
  nameStatsSearch t . gExifTitles . pdExif

folderSearchFunction (Caption c) =
  nameStatsSearch c . gExifCaptions . pdExif

-- Note: year is special because year is both property of an image and
-- (potentially different) property of a folder. So eithe the folder
-- year matches, or it contains images that match.
folderSearchFunction a@(Year y) =
  \p -> evalNum y (pdYear p) ||
        imagesMatchAtom a (pdImages p)

folderSearchFunction a@(Season _) =
  imagesMatchAtom a . pdImages

folderSearchFunction a@(Month _) =
  imagesMatchAtom a . pdImages

folderSearchFunction a@(Day _) =
  imagesMatchAtom a . pdImages

folderSearchFunction (Camera c) =
  nameStatsSearch c . gExifCameras . pdExif

folderSearchFunction (Lens l) =
  nameStatsSearch l . gExifLenses . pdExif

folderSearchFunction a@(FStop _) =
  imagesMatchAtom a . pdImages

folderSearchFunction a@(ShutterSpeed _) =
  imagesMatchAtom a . pdImages

folderSearchFunction a@(Iso _) =
  imagesMatchAtom a . pdImages

folderSearchFunction a@(FocalLength _) =
  imagesMatchAtom a . pdImages

-- TODO: cache folder problems?
folderSearchFunction a@(Problem _) =
  imagesMatchAtom a . pdImages

-- TODO: make something smarter here?
folderSearchFunction a@(Type _) =
  imagesMatchAtom a . pdImages

folderSearchFunction (Folder f) =
  evalStr f . Just . pdName

-- Note: unlikely, but do search on paths as recorded in keys of
-- pdImages?
folderSearchFunction a@(FileName _) =
  imagesMatchAtom a . pdImages

-- TODO: make status smarter based on folder class?
folderSearchFunction a@(Status _) =
  imagesMatchAtom a . pdImages

folderSearchFunction (FClass c) =
  (== c) . folderClass

folderSearchFunction a@(Rating _) =
  imagesMatchAtom a . pdImages

folderSearchFunction a@(PplCnt _) =
  imagesMatchAtom a . pdImages

folderSearchFunction a@(KwdCnt _) =
  imagesMatchAtom a . pdImages

folderSearchFunction a@(FlashSrc _) =
  imagesMatchAtom a . pdImages

folderSearchFunction (FlashMode m) =
  nameStatsSearch m . gExifFlashMode . pdExif

folderSearchFunction (Megapixels m) =
  numStatsSearch m . gExifMegapixels . pdExif

-- Generic ops below

folderSearchFunction (And a b) = \p ->
  folderSearchFunction a p &&
  folderSearchFunction b p

folderSearchFunction (Or a b) = \p ->
  folderSearchFunction a p ||
  folderSearchFunction b p

folderSearchFunction (Not a) =
  not . folderSearchFunction a

folderSearchFunction (All as) = \p ->
  all (`folderSearchFunction` p) as

folderSearchFunction (Any as) = \p ->
  any (`folderSearchFunction` p) as

folderSearchFunction ConstTrue = const True

imagesMatchAtom :: Atom -> Map.Map ImageName Image -> Bool
imagesMatchAtom a = any (imageSearchFunction a)

imageSearchFunction :: Atom -> (Image -> Bool)
imageSearchFunction (Country loc) =
  evalStr loc . exifCountry . imgExif

imageSearchFunction (Province loc) =
  evalStr loc . exifProvince . imgExif

imageSearchFunction (City loc) =
  evalStr loc . exifCity . imgExif

imageSearchFunction (Location loc) =
  evalStr loc . exifLocation . imgExif

imageSearchFunction (Person who) =
  setSearch who . exifPeople . imgExif

imageSearchFunction (Keyword keyword) =
  setSearch keyword . exifKeywords . imgExif

imageSearchFunction (Title t) =
  evalStr t . exifTitle . imgExif

imageSearchFunction (Caption c) =
  evalStr c . exifCaption . imgExif

imageSearchFunction (Year year) =
  evalNum year . imageYear

imageSearchFunction (Season SeasonUnknown) =
  isNothing . picSeason

imageSearchFunction (Season s) =
  (== Just s) . picSeason

imageSearchFunction (Month MonthUnknown) =
  isNothing . picMonth

imageSearchFunction (Month m) =
  (== Just m) . picMonth

imageSearchFunction (Day DayUnknown) =
  isNothing . picMonthDay

imageSearchFunction (Day md@(MonthDay _)) =
  (== Just md) . picMonthDay

imageSearchFunction (Day Weekday) = \img ->
  (== Just Weekday) $ weekdayToEnd <$> picDay img
imageSearchFunction (Day Weekend) = \img ->
  (== Just Weekend) $ weekdayToEnd <$> picDay img

imageSearchFunction (Day d) =
  (== Just d) . picDay

imageSearchFunction (Camera camera) =
  evalStr camera . exifCamera . imgExif

imageSearchFunction (Lens lens) =
  \img ->
    (evalStr lens . Just . liName . exifLens . imgExif) img ||
    (evalStr lens . Just . liSpec . exifLens . imgExif) img

imageSearchFunction (FStop f) =
  evalNum f . exifAperture . imgExif

imageSearchFunction (ShutterSpeed f) =
  evalNum f . exifSSpeedVal . imgExif

imageSearchFunction (Iso f) =
  evalNum f . exifISO . imgExif

imageSearchFunction (FocalLength f) =
  evalNum f . exifFocalLength . imgExif

imageSearchFunction (Problem who) =
  setSearch who . imgProblems

imageSearchFunction (Type t) =
  (== t) . imgType

imageSearchFunction (Folder p) =
  evalStr p . Just . imgParent

imageSearchFunction (FileName f) =
  evalStr f . Just . unImageName . imgName

imageSearchFunction (Status v) =
  (== v) . imgStatus

-- TODO: search based on parent folder status? Or something else?
imageSearchFunction (FClass _) = const False

imageSearchFunction (Rating r) =
  evalNum r . exifRating . imgExif

imageSearchFunction (PplCnt n) =
  evalNum n . Just . Set.size . exifPeople . imgExif

imageSearchFunction (KwdCnt n) =
  evalNum n . Just . Set.size . exifKeywords . imgExif

imageSearchFunction (FlashSrc s) =
  flashSearch s . fiSource . exifFlashInfo . imgExif

imageSearchFunction (FlashMode m) =
  evalStr m . fiMode . exifFlashInfo . imgExif

imageSearchFunction (Megapixels m) =
  evalNum m . exifMegapixels . imgExif

-- Generic ops below

imageSearchFunction (And a b) = \img ->
  imageSearchFunction a img &&
  imageSearchFunction b img

imageSearchFunction (Or a b) = \img ->
  imageSearchFunction a img ||
  imageSearchFunction b img

imageSearchFunction (Not a) =
  not . imageSearchFunction a

imageSearchFunction (All as) = \img ->
  all (`imageSearchFunction` img) as

imageSearchFunction (Any as) = \img ->
  any (`imageSearchFunction` img) as

imageSearchFunction ConstTrue = const True

-- | Computes whether a given atom can ever find files.
atomFindsFiles :: Atom -> Bool
atomFindsFiles (FClass _) = False
atomFindsFiles (And a b)  = atomFindsFiles a && atomFindsFiles b
atomFindsFiles (Or a b)   = atomFindsFiles a || atomFindsFiles b
atomFindsFiles (Not a)    = atomFindsFiles a
atomFindsFiles (All as)   = all atomFindsFiles as
atomFindsFiles (Any as)   = any atomFindsFiles as
atomFindsFiles ConstTrue  = True
atomFindsFiles _          = True

type AtomStats = [(Maybe Text, Maybe Text, Integer)]

gaBuilder :: (a -> Text) -> (a -> Text) -> NameStats a -> AtomStats
gaBuilder keyfn reprfn =
  map (\(k, v) -> (keyfn <$> k, reprfn <$> k, v)) . Map.toList

simpleBuilder :: (a -> Text) -> NameStats a -> AtomStats
simpleBuilder b = gaBuilder b b

toTextBuilder :: (ToText a) => NameStats a -> AtomStats
toTextBuilder = simpleBuilder toText

fancyTextBuilder :: (ToText a) => (a -> Text) -> NameStats a -> AtomStats
fancyTextBuilder = gaBuilder toText

idBuilder :: NameStats Text -> AtomStats
idBuilder = simpleBuilder id

formatZeroOneMore :: Text -> Text -> Int -> Text
formatZeroOneMore _ p 0 = sformat ("no " % stext) p
formatZeroOneMore s _ 1 = sformat ("1 " % stext) s
formatZeroOneMore _ p n = sformat (int % " " % stext) n p

-- TODO: remove duplication with Handler/Utils.hs
formatFlashSource :: FlashOp -> Text
formatFlashSource FlashNone     = "shot without flash"
formatFlashSource FlashInternal = "shot with internal flash"
formatFlashSource FlashExternal = "shot with an external flash"
formatFlashSource FlashAny      = "shot with an active flash (any type)"
formatFlashSource FlashUnknown  = "does not have flash information"

getAtoms :: Symbol -> Repository -> AtomStats
getAtoms TCountry      = idBuilder . gExifCountries . repoExif
getAtoms TProvince     = idBuilder . gExifProvinces . repoExif
getAtoms TCity         = idBuilder . gExifCities    . repoExif
getAtoms TLocation     = idBuilder . gExifLocations . repoExif
getAtoms TPerson       = idBuilder . gExifPeople    . repoExif
getAtoms TKeyword      = idBuilder . gExifKeywords  . repoExif
getAtoms TTitle        = idBuilder . gExifTitles    . repoExif
getAtoms TCaption      = idBuilder . gExifCaptions  . repoExif
getAtoms TCamera       = idBuilder . gExifCameras   . repoExif
getAtoms TLens         = idBuilder . gExifLenses    . repoExif
getAtoms TFStop        = gaBuilder (sformat shortest) (sformat ("f/" % shortest)) . apertureStats
getAtoms TShutterSpeed = gaBuilder toText showShutterSpeed . shutterSpeedStats
getAtoms TIso          = gaBuilder (sformat int) (sformat ("ISO " % int)) . isoStats
getAtoms TFocalLength  = fancyTextBuilder (sformat (shortest % "mm")) . focalLengthStats
getAtoms TYear         = toTextBuilder . yearStats
getAtoms TSeason       = toTextBuilder . seasonStats
getAtoms TDay          = toTextBuilder . dayStats
getAtoms TMonth        = toTextBuilder . monthStats
getAtoms TProblem      = idBuilder . repoProblems
getAtoms TType         = toTextBuilder . typeStats
getAtoms TFolder       = idBuilder .
  foldl' (\a p -> Map.insertWith (+) (Just $ pdName p) 1 a) Map.empty . repoDirs
-- TODO: this is expensive. Disable (const Map.empty)?
getAtoms TFileName     = idBuilder .
  foldl' (\a i -> Map.insertWith (+) (Just . unImageName $ imgName i) 1 a) Map.empty . filterImagesBy (const True)
getAtoms TStatus       = idBuilder . statusStats
getAtoms TFClass       = idBuilder . fClassStats
getAtoms TRating       = toTextBuilder . ratingStats
getAtoms TPplCnt       = gaBuilder (sformat int) (formatZeroOneMore "person" "people") . gExifPeopleCnt . repoExif
getAtoms TKwdCnt       = gaBuilder (sformat int) (formatZeroOneMore "keyword" "keywords") . gExifKwdCnt . repoExif
getAtoms TFlashSrc     = gaBuilder showFlash formatFlashSource . flashStats
getAtoms TFlashMode    = idBuilder . gExifFlashMode . repoExif
getAtoms TMegapixels   = fancyTextBuilder (sformat (shortest % " MP")) . gExifMegapixels . repoExif

-- | Computes type statistics.
typeStats :: Repository -> NameStats MediaType
typeStats = computePicStats $ \i -> [Just $ imgType i]

-- | Gets status stastics from repository statistics.
statusStats :: Repository -> NameStats Text
statusStats (rsPicStats . repoStats -> stats) =
  Map.fromList . map (\(s, f) -> (Just $ showImageStatus s,
                                  fromIntegral $ f stats)) $
  [ (ImageOrphaned,    sOrphaned)
  , (ImageStandalone,  sStandalone)
  , (ImageUnprocessed, sRaw)
  , (ImageProcessed,   sProcessed)
  ]

fClassStats :: Repository -> NameStats Text
fClassStats =
  Map.fromList . map (\(a, b) -> (Just $ showFolderClass a,
                                  fromIntegral b)) .
  Map.toList . rsFCStats . repoStats

-- | Helper to increase a count in a NameStats Text.
bumpCount :: (Ord a) => Maybe a -> NameStats a -> NameStats a
bumpCount a = Map.insertWith (+) a 1

-- | Computes namestats of a repository.
computePicStats :: (Ord a)
                => (Image -> [Maybe a]) -- ^ Computes what entries need to be bumped up.
                -> Repository           -- ^ Input repository
                -> NameStats a
computePicStats helper =
  foldl' (\stats ->
            Map.foldl' (\stats' -> foldr bumpCount stats' . helper)
            stats . pdImages
         ) Map.empty . Map.elems . repoDirs

-- | Computes year statistics.
yearStats :: Repository -> NameStats Integer
yearStats = computePicStats $ \i -> [imageYear i]

-- | Season statistics.
seasonStats :: Repository -> NameStats SeasonOp
seasonStats = computePicStats $ \i -> [picSeason i]

-- | Month statistics.
monthStats :: Repository -> NameStats MonthOp
monthStats = computePicStats $ \i -> [picMonth i]

apertureStats :: Repository -> NameStats Double
apertureStats = computePicStats $ \i ->
  [exifAperture (imgExif i)]

shutterSpeedStats :: Repository -> NameStats Double
shutterSpeedStats = computePicStats $ \i ->
  [exifSSpeedVal (imgExif i)]

isoStats :: Repository -> NameStats Integer
isoStats = computePicStats $ \i ->
  [exifISO (imgExif i)]

focalLengthStats :: Repository -> NameStats Double
focalLengthStats = computePicStats $ \i ->
  [exifFocalLength (imgExif i)]

-- | Day statistics.
dayStats :: Repository -> NameStats DayOp
dayStats =
  computePicStats (\pic -> let d = picDay pic
                               wd = weekdayToEnd <$> d
                               md = picMonthDay pic
                           in [ d , wd , md ]
                  )
-- | Rating statistics.
ratingStats :: Repository -> NameStats Int
ratingStats = computePicStats $ (:[]) . exifRating . imgExif

-- | Flash statistics.
flashStats :: Repository -> NameStats FlashOp
flashStats = computePicStats $ \i ->
  map Just $
  case fiSource . exifFlashInfo . imgExif $ i of
    Just FlashSourceNone     -> [FlashNone]
    Just FlashSourceInternal -> [FlashInternal, FlashAny]
    Just FlashSourceExternal -> [FlashExternal, FlashAny]
    Nothing                  -> [FlashUnknown]

-- | Computes the weekday of a picture.
picDay :: Image -> Maybe DayOp
picDay img = do
  d <- exifLocalCreateDate $ imgExif img
  let (_, _, wd) = toWeekDate $ localDay d
  intToWeekDay wd

-- | Computes the month-day of a picture.
picMonthDay :: Image -> Maybe DayOp
picMonthDay img = do
  d <- exifLocalCreateDate $ imgExif img
  let (_, _, md) = toGregorian $ localDay d
  return $ MonthDay md

-- | Converts a Day into another Day representing weekend or not.
--
-- Ordinal month days will be classified as weekday, sadly. This
-- points to some lack of soundness in the argument.
weekdayToEnd :: DayOp -> DayOp
weekdayToEnd Saturday = Weekend
weekdayToEnd Sunday   = Weekend
weekdayToEnd _        = Weekday

-- | Returns the month of a picture.
picMonth :: Image -> Maybe MonthOp
picMonth img = do
  d <- exifLocalCreateDate $ imgExif img
  let (_, m, _) = toGregorian $ localDay d
  intToMonth m

-- | Returns the season of a picture.
picSeason :: Image -> Maybe SeasonOp
picSeason img = picMonth img >>= monthToSeason

-- | Computes the season based on a month.
--
-- Note that the definition of season is currently hardcoded to
-- month-boundaries, not based on equinox, etc.
monthToSeason :: MonthOp -> Maybe SeasonOp
monthToSeason m
  | m == December || m == January || m == February = Just Winter
  | m == March || m == April || m == May = Just Spring
  | m == June || m == July || m == August = Just Summer
  | m == September || m == October || m == November = Just Autumn
  | otherwise = Nothing -- FIXME: is this needed?

-- | Builder for all atom.
--
-- Builds an 'All' atom, with special casing for small values:
-- * no sense in over-wrapping a single value.
-- * x and y is better (cleaner) than All [x, y].
-- * no child atoms means a ConstTrue atom.
allAtom :: [Atom] -> Atom
allAtom = go . nub
  where go []     = ConstTrue
        go [x]    = x
        go [x, y] = And x y -- 'x and y' is better than 'all of [x, y]'.
        go xs     = All xs  -- and normal wrapping.

-- | Builder for any atom.
--
-- Builds an 'Any' atom, with special casing for small values. See
-- 'allAtom' for explanations.
anyAtom :: [Atom] -> Atom
anyAtom = go . nub
  where go [x]    = x
        go [x, y] = Or x y
        go xs     = Any xs

-- | Simpler Text to decimal parsing with error handling.
parseDecimalPlain :: (Integral a) => Text -> Either Text a
parseDecimalPlain w =
  case Text.signed Text.decimal w of
    Right (w', "") -> Right w'
    Right (w', leftover) ->
      Left $ sformat ("Parsed " % int % " decimal but with leftover text '" %
                      stext % "'") w' leftover
    Left msg ->
      Left $ sformat ("Failed to parse integer from '" % stext % "': " %
                      string) w msg

-- | Simpler Text to real parsing with error handling.
parseRealPlain :: Text -> Either Text Double
parseRealPlain w =
  case Text.signed Text.double w of
    Right (w', "") -> Right w'
    Right (w', leftover) ->
      Left $ sformat ("Parsed " % shortest % " fractional but with leftover text '" %
                      stext % "'") w' leftover
    Left msg ->
      Left $ sformat ("Failed to parse fractional from '" % stext % "': " %
                      string) w msg

-- | Simpler Text to ordinal parsing with error handling.
--
-- It accepts usual prefixes such as 'th', 'st', 'nd', 'rd', as long as they're valid.
parseOrdinal :: (Integral a) => Text -> Either Text a
parseOrdinal w =
  case Text.decimal w of
    Right (w', "") -> Right w'
    Right (w', suff) | w == showOrdinal w' &&
                       (suff == "th" ||
                        suff == "st" ||
                        suff == "nd" ||
                        suff == "rd") -> Right w'
    Right (w', leftover) ->
      Left $ sformat ("Parsed " % int % " decimal but with leftover text '" %
                      stext % "'") w' leftover
    Left msg ->
      Left $ sformat ("Failed to parse integer from '" % stext % "': " %
                      string) w msg

-- | Helper that applies a "pop N from stack and build atom" function
-- to the stack.
--
-- Useful for any/all to correctly pop the required amount of items
-- from the stack, and not all.
anyAllParser :: ([Atom] -> Atom) -> [Atom] -> Int -> Either Text [Atom]
anyAllParser fn stack count =
  let (selected, remaining) = splitAt count stack
  in if length selected < count
     then Left $ sformat ("Failed to pop " % int % " items from the stack " % shown) count stack
     else Right $ fn selected:remaining

rpnParser :: [Atom] -> (Text, Text) -> Either Text [Atom]
rpnParser (x:y:ys) ("and",_) = Right $ And x y:ys
rpnParser (x:y:ys) ("or",_) = Right $ Or x y:ys
rpnParser (x:xs) ("not", _) =
  let a = case x of
            Not y -> y
            _     -> Not x
  in Right $ a:xs
rpnParser xs ("all", c) = parseDecimalPlain c >>= anyAllParser allAtom xs
rpnParser xs ("any", c) = parseDecimalPlain c >>= anyAllParser anyAtom xs
rpnParser xs (an, av) =
  let v = parseAtom an av
  in case v of
    Just v' -> Right $ v':xs
    Nothing -> Left $ "Failed to parse the atom " <>
               an <> "=" <> av <>
               " with stack " <> sformat shown (map atomDescription xs)

parseString :: Text -> Maybe StrOp
parseString (Text.uncons -> Just ('~', v)) = Just $ OpFuzzy (makeFuzzy v)
parseString v                              = Just $ OpEqual v

numPrefixes :: Set.Set Char
numPrefixes = Set.fromList ['=', '/', '!', '≠', '<', '>', '≤', '≥']

numParser :: (Num a, Ord a) => (Text -> Either Text a) -> Text -> Maybe (NumOp a)
numParser parser (Text.span (`Set.member` numPrefixes) -> (prefix, v)) =
  case prefix of
    "="  -> OpEq <$> v'
    "/=" -> OpNe <$> v'
    "!=" -> OpNe <$> v'
    "≠"  -> OpNe <$> v'
    ""   -> OpEq <$> v'
    "<"  -> OpLt <$> v'
    "<=" -> OpLe <$> v'
    "≤"  -> OpLe <$> v'
    ">=" -> OpGe <$> v'
    "≥"  -> OpGe <$> v'
    ">"  -> OpGt <$> v'
    _    -> Nothing
  where v' = either (const Nothing) Just . parser $ v

parseDecimal :: (Integral a) => Text -> Maybe (NumOp a)
parseDecimal = numParser parseDecimalPlain

parseReal :: Text -> Maybe (NumOp Double)
parseReal = numParser parseRealPlain

parseType :: Text -> Maybe MediaType
parseType v
  | v == "movie"    = Just MediaMovie
  | v == "image"    = Just MediaImage
  | v == "unknown"  = Just MediaUnknown
  | otherwise       = Nothing

showMedia :: MediaType -> Text
showMedia MediaMovie   = "movie"
showMedia MediaImage   = "image"
showMedia MediaUnknown = "unknown"

parseSeason :: Text -> Maybe SeasonOp
parseSeason (Text.toLower -> s)
  | s == "winter" = Just Winter
  | s == "spring" = Just Spring
  | s == "summer" = Just Summer
  | s == "autumn" = Just Autumn
  | otherwise     = Nothing

showSeason :: SeasonOp -> Text
showSeason Winter        = "winter"
showSeason Spring        = "spring"
showSeason Summer        = "summer"
showSeason Autumn        = "autumn"
showSeason SeasonUnknown = "unknown"

intToMonth :: Int -> Maybe MonthOp
intToMonth 1  = Just January
intToMonth 2  = Just February
intToMonth 3  = Just March
intToMonth 4  = Just April
intToMonth 5  = Just May
intToMonth 6  = Just June
intToMonth 7  = Just July
intToMonth 8  = Just August
intToMonth 9  = Just September
intToMonth 10 = Just October
intToMonth 11 = Just November
intToMonth 12 = Just December
intToMonth _  = Nothing

parseMonth :: Text -> Maybe MonthOp
parseMonth (Text.toLower -> m)
  | m == "january"   = Just January
  | m == "february"  = Just February
  | m == "march"     = Just March
  | m == "april"     = Just April
  | m == "may"       = Just May
  | m == "june"      = Just June
  | m == "july"      = Just July
  | m == "august"    = Just August
  | m == "september" = Just September
  | m == "october"   = Just October
  | m == "november"  = Just November
  | m == "december"  = Just December
  | otherwise =
      either (const Nothing) intToMonth $ parseDecimalPlain m

showMonth :: MonthOp -> Text
showMonth MonthUnknown = "unknown"
showMonth s            = sformat shown s

parseDay :: Text -> Maybe DayOp
parseDay (Text.toLower -> d)
  | d == "monday"    = Just Monday
  | d == "tuesday"   = Just Tuesday
  | d == "wednesday" = Just Wednesday
  | d == "thursday"  = Just Thursday
  | d == "friday"    = Just Friday
  | d == "saturday"  = Just Saturday
  | d == "sunday"    = Just Sunday
  | d == "weekday"   = Just Weekday
  | d == "weekend"   = Just Weekend
  | otherwise =
      case parseOrdinal d of
        Right v | v >= 1 && v <= 31 -> Just $ MonthDay v
        _                           -> Nothing

showDay :: DayOp -> Text
showDay Monday       = "Monday"
showDay Tuesday      = "Tuesday"
showDay Wednesday    = "Wednesday"
showDay Thursday     = "Thursday"
showDay Friday       = "Friday"
showDay Saturday     = "Saturday"
showDay Sunday       = "Sunday"
showDay Weekday      = "weekday"
showDay Weekend      = "weekend"
showDay (MonthDay d) = showOrdinal d
showDay DayUnknown   = "unknown"

-- FIXME: replace with ords when newer formatting library (no longer
-- .0 bug) [dependency].
showOrdinal :: (Integral a) => a -> Text
showOrdinal n
  | n < 0 = sformat int n
  | tens > 3 && tens < 21 = sformat int n <> "th"
  | otherwise =
      sformat int n <>
      case n `mod` 10 of
        1 -> "st"
        2 -> "nd"
        3 -> "rd"
        _ -> "th"
  where tens = n `mod` 100

intToWeekDay :: Int -> Maybe DayOp
intToWeekDay 1 = Just Monday
intToWeekDay 2 = Just Tuesday
intToWeekDay 3 = Just Wednesday
intToWeekDay 4 = Just Thursday
intToWeekDay 5 = Just Friday
intToWeekDay 6 = Just Saturday
intToWeekDay 7 = Just Sunday
intToWeekDay _ = Nothing

stripSuf :: Text -> Text -> Text
stripSuf v suf = fromMaybe v (Text.stripSuffix suf v)

parseShutterSpeedPlain :: Text -> Either Text Double
parseShutterSpeedPlain (Text.stripPrefix "1/" -> Just v) = (1/) <$> parseRealPlain (stripSuf v "s")
parseShutterSpeedPlain v                                 = parseRealPlain (stripSuf v "s")

parseShutterSpeed :: Text -> Maybe (NumOp Double)
parseShutterSpeed = numParser parseShutterSpeedPlain

showShutterSpeed :: Double -> Text
showShutterSpeed v
  | v >= 1 = sformat (shortest % "s") v
  | otherwise = sformat ("1/" % fixed 0 % "s") (1/v)

parseAtomParams :: [(Text, Text)] -> Either Text Atom
parseAtomParams params =
  if length params > 50
  then Left "Too many search parameters. Maximum allowed is 50."
  else allAtom <$> foldM rpnParser [] params

formatNo :: Text -> (Text, Text)
formatNo s = ("no-" <> s, "")

class OpParam a where
  opToParam :: Text -> a -> (Text, Text)

instance OpParam StrOp where
  opToParam s (OpEqual v) = (s, v)
  opToParam s (OpFuzzy v) = (s, '~' `Text.cons` unFuzzy v)
  opToParam s OpMissing   = formatNo s

instance (ToText a) => OpParam (NumOp a) where
  opToParam s (OpEq v) = (s, toText v)
  opToParam s (OpNe v) = (s, "/=" <> toText v)
  opToParam s (OpLt v) = (s, '<' `Text.cons` toText v)
  opToParam s (OpLe v) = (s, "<=" <> toText v)
  opToParam s (OpGe v) = (s, ">=" <> toText v)
  opToParam s (OpGt v) = (s, '>' `Text.cons` toText v)
  opToParam s  OpNa    = formatNo s

instance OpParam MediaType where
  opToParam s = (s, ) . showMedia

instance OpParam ImageStatus where
  opToParam s = (s, ) . showImageStatus

instance OpParam FolderClass where
  opToParam s = (s, ) . showFolderClass

instance OpParam SeasonOp where
  opToParam s SeasonUnknown = formatNo s
  opToParam s v             = (s, showSeason v)

instance OpParam MonthOp where
  opToParam s MonthUnknown = formatNo s
  opToParam s v            = (s, showMonth v)

instance OpParam DayOp where
  opToParam s DayUnknown = formatNo s
  opToParam s v          = (s, showDay v)

instance OpParam FlashOp where
  -- TODO: is formatNo here correct?
  opToParam s FlashUnknown = formatNo s
  opToParam s v            = (s, showFlash v)

formatParam :: (OpParam a) => Symbol -> a -> (Text, Text)
formatParam s = opToParam (symbolName s)

atomToParams :: Atom -> [(Text, Text)]
atomToParams (Country  v)     = [formatParam TCountry      v]
atomToParams (Province v)     = [formatParam TProvince     v]
atomToParams (City     v)     = [formatParam TCity         v]
atomToParams (Location v)     = [formatParam TLocation     v]
atomToParams (Person   v)     = [formatParam TPerson       v]
atomToParams (Keyword  v)     = [formatParam TKeyword      v]
atomToParams (Title    v)     = [formatParam TTitle        v]
atomToParams (Caption  v)     = [formatParam TCaption      v]
atomToParams (Year     n)     = [formatParam TYear         n]
atomToParams (Season   s)     = [formatParam TSeason       s]
atomToParams (Month    m)     = [formatParam TMonth        m]
atomToParams (Day      d)     = [formatParam TDay          d]
atomToParams (Camera   v)     = [formatParam TCamera       v]
atomToParams (Lens     v)     = [formatParam TLens         v]
atomToParams (FStop    v)     = [formatParam TFStop        v]
atomToParams (ShutterSpeed v) = [formatParam TShutterSpeed v]
atomToParams (Iso      v)     = [formatParam TIso          v]
atomToParams (FocalLength v)  = [formatParam TFocalLength  v]
atomToParams (Problem  v)     = [formatParam TProblem      v]
atomToParams (Type     v)     = [formatParam TType         v]
atomToParams (Folder   v)     = [formatParam TFolder       v]
atomToParams (FileName v)     = [formatParam TFileName     v]
atomToParams (Status   v)     = [formatParam TStatus       v]
atomToParams (FClass   v)     = [formatParam TFClass       v]
atomToParams (Rating   v)     = [formatParam TRating       v]
atomToParams (PplCnt   v)     = [formatParam TPplCnt       v]
atomToParams (KwdCnt   v)     = [formatParam TKwdCnt       v]
atomToParams (FlashSrc v)     = [formatParam TFlashSrc     v]
atomToParams (FlashMode v)    = [formatParam TFlashMode    v]
atomToParams (Megapixels v)   = [formatParam TMegapixels   v]
atomToParams (And a b)        =
  concat [atomToParams a, atomToParams b, [("and", "")]]
atomToParams (Or a b)         =
  concat [atomToParams a, atomToParams b, [("or", "")]]
atomToParams (Not a)          =
  atomToParams a ++ [("not", "")]
atomToParams (All xs)         =
  concatMap atomToParams xs ++ [("all", sformat int $ length xs)]
atomToParams (Any xs)         =
  concatMap atomToParams xs ++ [("any", sformat int $ length xs)]
atomToParams ConstTrue        = atomToParams (All [])

earliestImage :: Image -> Image -> Image
earliestImage a b =
  if imageTimeKey a < imageTimeKey b
  then a
  else b

-- | Build image map (with static sorting).
buildImageMap :: Atom -> Repository -> SearchResults
buildImageMap atom =
  foldl' (\(mimg, mfld) img ->
             (Map.insert (imgParent img, imageTimeKey img) img mimg,
              Map.insertWith earliestImage (imgParent img) img mfld)
         ) (Map.empty, Map.empty) .
  filterImagesBy (imageSearchFunction atom)

-- | Build image map (with static sorting).
buildFolderMap :: Atom -> Repository -> RepoDirs
buildFolderMap atom =
  filterFoldersBy (folderSearchFunction atom)

searchImages :: Ctx -> Atom -> Repository -> IO SearchResults
searchImages ctx atom pics = do
  -- Note: the call to buildImageMap must *not* be evaluated,
  -- otherwise we don't gain anything from caching it.
  -- TODO: remove repository argument, read from ctx? [cleanup]
  let lazyimages = buildImageMap atom pics
  getSearchResults ctx lazyimages (atomToParams atom)

-- | Computs the right atoms for an input quick search element.
--
-- Algorithm:
--
-- * tries to parse the input as symbol:keyword, and build an atom
--   based on these two (which might fail to)
-- * if the above is successful, return this single atom
-- * otherwise, return all atoms that are able to parse the input
--   string, unmodified.
getAtomsForQuickSearch :: Text -> [Atom]
getAtomsForQuickSearch word =
  let r = do
        let (h, t) = Text.break (== ':') word
        (p, t') <- Text.uncons t
        when (p /= ':') Nothing
        s <- parseSymbol h
        quickSearch s t'
  in case r of
    Just a -> [a]
    Nothing -> mapMaybe (`quickSearch` word)
               [minBound..maxBound]

-- | Generates a quick search atom.
genQuickSearchParams :: Repository -> Text ->
                        Either Text ([Atom], Maybe Atom)
genQuickSearchParams _ "" = Left "Empty search parameter"
genQuickSearchParams pics search =
  let swords = nub $ Text.words search
      -- TODO: this way of checking search is too slow, especially for
      -- negative searches, as it really needs to scan the entire
      -- repository. Improve by checking membership in sum of
      -- atom-values (e.g. a set with all defined cities, so on) where
      -- possible. [performance]
      findsAny a = (not . null . filterImagesBy (imageSearchFunction a) $ pics) ||
                   (not . null . filterFoldersBy (folderSearchFunction a) $ pics)
      -- Algorithm: for each word, get the right list of atoms (per
      -- getAtomsForQuickSearch). Combine the above list of atoms
      -- using the Any atom, and at the end, combine all word findings
      -- with the 'All' atom. This means that a search of type "2018
      -- Spain" finds correctly all pictures taken in Spain in the
      -- year 2018, but a search of type "Italy England" won't find
      -- anything, since no picture will have a location of both. It
      -- would be possible to understand that in this case the likely
      -- meaning is Italy *or* England, but with any complex filters
      -- it would become a headache.
      params = foldl' (\(pf, pm) w ->
                         let allA = getAtomsForQuickSearch w
                             -- allA is all atoms that were able to
                             -- parse from the input word.
                             (f, m) = partition findsAny allA
                             -- and if any are found, combine them
                             -- using any; in case none are found, we
                             -- can't simply ignore this word, so
                             -- explicitly add all atoms via Any, even
                             -- though we know this will fail.
                         in if null f
                            then (anyAtom allA:pf, pm)
                            else (anyAtom f:pf, m ++ pm)
                      ) ([], []) swords
      (found, notfound) = params
      p' = if null found
             then Nothing
             else Just . allAtom $ found
  in Right (notfound, p')
