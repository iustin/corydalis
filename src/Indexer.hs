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
               ) where

import           Control.Monad  (foldM, when)
import           Data.List      (foldl', nub, partition)
import qualified Data.Map       as Map
import           Data.Maybe     (isNothing, mapMaybe)
import           Data.Semigroup ((<>))
import           Data.Set       (Set)
import qualified Data.Set       as Set
import           Data.Text      (Text)
import qualified Data.Text      as Text
import qualified Data.Text.Read as Text
import           Formatting
import           Yesod          (PathPiece (..))

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
            | TCamera
            | TLens
            | TProblem
            | TType
            | TFolder
            | TFileName
            | TStatus
            | TFClass
            deriving (Enum, Bounded, Show, Read, Eq)

instance PathPiece Symbol where
  toPathPiece TCountry  = "countries"
  toPathPiece TProvince = "provinces"
  toPathPiece TCity     = "cities"
  toPathPiece TLocation = "locations"
  toPathPiece TPerson   = "people"
  toPathPiece TKeyword  = "keywords"
  toPathPiece TTitle    = "title"
  toPathPiece TCaption  = "caption"
  toPathPiece TYear     = "years"
  toPathPiece TCamera   = "cameras"
  toPathPiece TLens     = "lenses"
  toPathPiece TProblem  = "problems"
  toPathPiece TType     = "types"
  toPathPiece TFolder   = "folders"
  toPathPiece TFileName = "filenames"
  toPathPiece TStatus   = "image-status"
  toPathPiece TFClass   = "folder-class"
  fromPathPiece "countries"    = Just TCountry
  fromPathPiece "provinces"    = Just TProvince
  fromPathPiece "cities"       = Just TCity
  fromPathPiece "locations"    = Just TLocation
  fromPathPiece "people"       = Just TPerson
  fromPathPiece "keywords"     = Just TKeyword
  fromPathPiece "title"        = Just TTitle
  fromPathPiece "caption"      = Just TCaption
  fromPathPiece "years"        = Just TYear
  fromPathPiece "cameras"      = Just TCamera
  fromPathPiece "lenses"       = Just TLens
  fromPathPiece "problems"     = Just TProblem
  fromPathPiece "types"        = Just TType
  fromPathPiece "folders"      = Just TFolder
  fromPathPiece "filenames"    = Just TFileName
  fromPathPiece "image-status" = Just TStatus
  fromPathPiece "folder-class" = Just TFClass
  fromPathPiece _              = Nothing

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
  OpNa :: NumOp a
  OpLt :: (Num a, Ord a) => a -> NumOp a
  OpGt :: (Num a, Ord a) => a -> NumOp a

deriving instance Show a => Show (NumOp a)
deriving instance Eq   a => Eq   (NumOp a)

data TypeOp = TypeImage
            | TypeMovie
            | TypeUnknown
            deriving (Show, Eq)

fuzzyMatch :: FuzzyText -> Text -> Bool
fuzzyMatch fz =
  (unFuzzy fz `Text.isInfixOf`) . Text.toCaseFold

evalStr :: StrOp -> Maybe Text -> Bool
evalStr (OpEqual a) = (Just a ==)
evalStr (OpFuzzy a) = maybe False (fuzzyMatch a)
evalStr  OpMissing  = isNothing

evalNum :: NumOp a -> Maybe a -> Bool
evalNum (OpEq a) = (== Just a)
evalNum (OpLt a) = maybe False (< a)
evalNum (OpGt a) = maybe False (> a)
evalNum OpNa     = isNothing

evalType :: TypeOp -> Image -> Bool
evalType TypeMovie   = (== MediaMovie) . imgType
evalType TypeImage   = (== MediaImage) . imgType
evalType TypeUnknown = (== MediaUnknown) . imgType

data Atom = Country  StrOp
          | Province StrOp
          | City     StrOp
          | Location StrOp
          | Person   StrOp
          | Keyword  StrOp
          | Title    StrOp
          | Caption  StrOp
          | Year     (NumOp Integer)
          | Camera   StrOp
          | Lens     StrOp
          | Problem  StrOp
          | Type     TypeOp
          | Folder   StrOp
          | FileName StrOp
          | Status   ImageStatus
          | FClass   FolderClass
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
symbolName TCountry  = "country"
symbolName TProvince = "province"
symbolName TCity     = "city"
symbolName TLocation = "location"
symbolName TPerson   = "person"
symbolName TKeyword  = "keyword"
symbolName TTitle    = "title"
symbolName TCaption  = "caption"
symbolName TYear     = "year"
symbolName TCamera   = "camera"
symbolName TLens     = "lens"
symbolName TProblem  = "problem"
symbolName TType     = "type"
symbolName TFolder   = "folder"
symbolName TFileName = "filename"
symbolName TStatus   = "status"
symbolName TFClass   = "folder-class"

negSymbolName :: Symbol -> Text
negSymbolName atom = "no-" <> symbolName atom

parseSymbol :: Text -> Maybe Symbol
parseSymbol "country"      = Just TCountry
parseSymbol "province"     = Just TProvince
parseSymbol "city"         = Just TCity
parseSymbol "location"     = Just TLocation
parseSymbol "person"       = Just TPerson
parseSymbol "keyword"      = Just TKeyword
parseSymbol "title"        = Just TTitle
parseSymbol "caption"      = Just TCaption
parseSymbol "year"         = Just TYear
parseSymbol "camera"       = Just TCamera
parseSymbol "lens"         = Just TLens
parseSymbol "problem"      = Just TProblem
parseSymbol "type"         = Just TType
parseSymbol "folder"       = Just TFolder
parseSymbol "filename"     = Just TFileName
parseSymbol "status"       = Just TStatus
parseSymbol "folder-class" = Just TFClass
parseSymbol _              = Nothing

buildMissingAtom :: Symbol -> Atom
buildMissingAtom s =
  case s of
    TCountry  -> Country   OpMissing
    TProvince -> Province  OpMissing
    TCity     -> City      OpMissing
    TLocation -> Location  OpMissing
    TPerson   -> Person    OpMissing
    TKeyword  -> Keyword   OpMissing
    TTitle    -> Title     OpMissing
    TCaption  -> Caption   OpMissing
    TYear     -> Year      OpNa
    TCamera   -> Camera    OpMissing
    TLens     -> Lens      OpMissing
    TProblem  -> Problem   OpMissing
    TType     -> Type      TypeUnknown
    -- FIXME: these should fail instead (using Maybe).
    TFolder   -> error "No missing atom for folder"
    TFileName -> error "No missing atom for filename"
    TStatus   -> error "No missing atom for status"
    TFClass   -> error "No missing atom for folder class"

parseAtom :: Text -> Text -> Maybe Atom
parseAtom (Text.splitAt 3 -> ("no-", v)) _ =
  buildMissingAtom <$> parseSymbol v

parseAtom a v = do
  s <- parseSymbol a
  let dec = parseDecimal v
      str = parseString v
      typ = parseType v
      sta = parseImageStatus v
  case s of
    TCountry  -> Country  <$> str
    TProvince -> Province <$> str
    TCity     -> City     <$> str
    TLocation -> Location <$> str
    TPerson   -> Person   <$> str
    TKeyword  -> Keyword  <$> str
    TTitle    -> Title    <$> str
    TCaption  -> Caption  <$> str
    TYear     -> Year     <$> dec
    TCamera   -> Camera   <$> str
    TLens     -> Lens     <$> str
    TProblem  -> Problem  <$> str
    TType     -> Type     <$> typ
    TFolder   -> Folder   <$> str
    TFileName -> FileName <$> str
    TStatus   -> Status   <$> sta
    TFClass   -> FClass   <$> parseFolderClass v

quickSearch :: Symbol -> Text -> Maybe Atom
quickSearch s v =
  case s of
    TCountry  -> fuzzer Country
    TProvince -> fuzzer Province
    TCity     -> fuzzer City
    TLocation -> fuzzer Location
    TPerson   -> fuzzer Person
    TKeyword  -> fuzzer Keyword
    TTitle    -> fuzzer Title
    TCaption  -> fuzzer Caption
    TCamera   -> fuzzer Camera
    TLens     -> fuzzer Lens
    TProblem  -> fuzzer Problem
    TYear     ->
      case Text.decimal v of
        Right (w', "") -> Just . Year . OpEq $ w'
        _              -> Nothing
    TType     -> Type <$> parseType v
    TFolder   -> fuzzer Folder
    TFileName -> fuzzer FileName
    TStatus   -> Status <$> parseImageStatus v
    TFClass   -> FClass <$> parseFolderClass v
  where f = makeFuzzy v
        fuzzer c = Just . c . OpFuzzy $ f

atomTypeDescriptions :: Symbol -> Text
atomTypeDescriptions TCountry  = "countries"
atomTypeDescriptions TProvince = "provinces"
atomTypeDescriptions TCity     = "cities"
atomTypeDescriptions TLocation = "locations"
atomTypeDescriptions TPerson   = "people"
atomTypeDescriptions TKeyword  = "keywords"
atomTypeDescriptions TTitle    = "image titles"
atomTypeDescriptions TCaption  = "image captions"
atomTypeDescriptions TYear     = "years"
atomTypeDescriptions TCamera   = "cameras"
atomTypeDescriptions TLens     = "lenses"
atomTypeDescriptions TProblem  = "problems"
atomTypeDescriptions TType     = "types"
atomTypeDescriptions TFolder   = "folders"
atomTypeDescriptions TFileName = "filenames"
atomTypeDescriptions TStatus   = "image statuses"
atomTypeDescriptions TFClass   = "folder classes"

class (Show a) => ToText a where
  toText :: a -> Text
  toText = Text.pack . show

instance ToText Text where
  toText = id

instance ToText Integer

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
atomDescription (Year (OpLt year))    = "taken before the year " <> toText year
atomDescription (Year (OpGt year))    = "taken after the year " <> toText year
atomDescription (Year OpNa)           = "does not have date information"
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
atomDescription (Problem OpMissing)    = "has no problems"
atomDescription (Problem (OpEqual "")) = "has an empty problem description"
atomDescription (Problem (OpEqual v))  = "has a problem description of " <> v
atomDescription (Problem (OpFuzzy v))  =
  "has a problem that matches " <> unFuzzy v
atomDescription (Type TypeImage)       = "is an image"
atomDescription (Type TypeMovie)       = "is a movie"
atomDescription (Type TypeUnknown)     = "is of unknown type"
atomDescription (Folder s)             = describeStr "folder" s
atomDescription (FileName s)           = describeStr "filename" s
atomDescription (Status v)             = "image status is " <> showImageStatus v
atomDescription (FClass v)             = "folder class is " <> showFolderClass v
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
nameStatsSearch :: StrOp -> NameStats -> Bool
nameStatsSearch OpMissing =
  maybe False (> 0) . (Nothing `Map.lookup`)
nameStatsSearch (OpEqual v) =
  maybe False (> 0) . (Just v `Map.lookup`)
nameStatsSearch (OpFuzzy f) =
  Map.foldrWithKey' (\k v a ->
                       let found = maybe False (fuzzyMatch f) k && v > 0
                       in found || a) False

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

folderSearchFunction (Camera c) =
  nameStatsSearch c . gExifCameras . pdExif

folderSearchFunction (Lens l) =
  nameStatsSearch l . gExifLenses . pdExif

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

imagesMatchAtom :: Atom -> Map.Map Text Image -> Bool
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

imageSearchFunction (Camera camera) =
  evalStr camera . exifCamera . imgExif

imageSearchFunction (Lens lens) =
  \img ->
    (evalStr lens . Just . liName . exifLens . imgExif) img ||
    (evalStr lens . Just . liSpec . exifLens . imgExif) img

imageSearchFunction (Problem who) =
  setSearch who . imgProblems

imageSearchFunction (Type t) = evalType t

imageSearchFunction (Folder p) =
  evalStr p . Just . imgParent

imageSearchFunction (FileName f) =
  evalStr f . Just . imgName

imageSearchFunction (Status v) =
  (== v) . imgStatus

-- TODO: search based on parent folder status? Or something else?
imageSearchFunction (FClass _) = const False

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

getAtoms :: Symbol -> Repository -> NameStats
getAtoms TCountry  = gExifCountries . repoExif
getAtoms TProvince = gExifProvinces . repoExif
getAtoms TCity     = gExifCities    . repoExif
getAtoms TLocation = gExifLocations . repoExif
getAtoms TPerson   = gExifPeople    . repoExif
getAtoms TKeyword  = gExifKeywords  . repoExif
getAtoms TTitle    = gExifTitles    . repoExif
getAtoms TCaption  = gExifCaptions  . repoExif
getAtoms TCamera   = gExifCameras   . repoExif
getAtoms TLens     = gExifLenses    . repoExif
getAtoms TYear     = yearStats
getAtoms TProblem  = repoProblems
getAtoms TType     = typeStats
getAtoms TFolder   =
  foldl' (\a p -> Map.insertWith (+) (Just $ pdName p) 1 a) Map.empty . repoDirs
-- TODO: this is expensive. Disable (const Map.empty)?
getAtoms TFileName =
  foldl' (\a i -> Map.insertWith (+) (Just $ imgName i) 1 a) Map.empty . filterImagesBy (const True)
getAtoms TStatus   = statusStats
getAtoms TFClass   = fClassStats

-- | Media type to Type.
mediaToType :: MediaType -> TypeOp
mediaToType MediaImage   = TypeImage
mediaToType MediaMovie   = TypeMovie
mediaToType MediaUnknown = TypeUnknown

-- | Computes type statistics.
typeStats :: Repository -> NameStats
typeStats =
  foldl' (\stats ->
            Map.foldl' (\l img ->
                          l `incM` (showType . mediaToType . imgType $ img)
                       ) stats . pdImages
         ) Map.empty . Map.elems . repoDirs
    where incM m t = Map.insertWith (+) (Just t) 1 m

-- | Gets status stastics from repository statistics.
statusStats :: Repository -> NameStats
statusStats (rsPicStats . repoStats -> stats) =
  Map.fromList . map (\(s, f) -> (Just $ showImageStatus s,
                                  fromIntegral $ f stats)) $
  [ (ImageOrphaned,    sOrphaned)
  , (ImageStandalone,  sStandalone)
  , (ImageUnprocessed, sRaw)
  , (ImageProcessed,   sProcessed)
  ]

fClassStats :: Repository -> NameStats
fClassStats =
  Map.fromList . map (\(a, b) -> (Just $ showFolderClass a,
                                  fromIntegral b)) .
  Map.toList . rsFCStats . repoStats

-- | Computes year statistics.
yearStats :: Repository -> NameStats
yearStats =
  foldl' (\stats folder ->
            Map.insertWith (+)
              (Text.pack . show <$> pdYear folder)
              1 stats
         ) Map.empty . Map.elems . repoDirs

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
  case Text.decimal w of
    Right (w', "") -> Right w'
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
rpnParser xs ("all", c) = either Left (anyAllParser allAtom xs) $ parseDecimalPlain c
rpnParser xs ("any", c) = either Left (anyAllParser anyAtom xs) $ parseDecimalPlain c
rpnParser xs (an, av) =
  let v = parseAtom an av
  in case v of
    Just v' -> Right $ v':xs
    Nothing -> Left $ "Failed to parse the atom " <>
               an <> "=" <> av <>
               " with stack " <> Text.pack (show $ map atomDescription xs)

parseString :: Text -> Maybe StrOp
parseString (Text.uncons -> Just ('~', v)) = Just $ OpFuzzy (makeFuzzy v)
parseString v                              = Just $ OpEqual v

parseDecimal :: (Integral a, Ord a) => Text -> Maybe (NumOp a)
parseDecimal v =
  case Text.uncons v of
    Just ('<', v') -> OpLt <$> go v'
    Just ('>', v') -> OpGt <$> go v'
    _              -> OpEq <$> go v
  where go w =
          case Text.decimal w of
            Right (w',"") -> Just w'
            _             -> Nothing

parseType :: Text -> Maybe TypeOp
parseType v
  | v == "movie"    = Just TypeMovie
  | v == "image"    = Just TypeImage
  | v == "unknown"  = Just TypeUnknown
  | otherwise       = Nothing

showType :: TypeOp -> Text
showType TypeMovie   = "movie"
showType TypeImage   = "image"
showType TypeUnknown = "unknown"

parseAtomParams :: [(Text, Text)] -> Either Text Atom
parseAtomParams params =
  if length params > 50
  then Left "Too many search parameters. Maximum allowed is 50."
  else allAtom <$> foldM rpnParser [] params

class OpParam a where
  opToParam :: Text -> a -> (Text, Text)

instance OpParam StrOp where
  opToParam s (OpEqual v) = (s, v)
  opToParam s (OpFuzzy v) = (s, '~' `Text.cons` unFuzzy v)
  opToParam s OpMissing   = ("no-" <> s, "")

instance (ToText a) => OpParam (NumOp a) where
  opToParam s (OpEq v) = (s, toText v)
  opToParam s (OpLt v) = (s, '<' `Text.cons` toText v)
  opToParam s (OpGt v) = (s, '>' `Text.cons` toText v)
  opToParam s  OpNa    = ("no-" <> s, "")

instance OpParam TypeOp where
  opToParam s = (s, ) . showType

instance OpParam ImageStatus where
  opToParam s = (s, ) . showImageStatus

instance OpParam FolderClass where
  opToParam s = (s, ) . showFolderClass

formatParam :: (OpParam a) => Symbol -> a -> (Text, Text)
formatParam s = opToParam (symbolName s)

atomToParams :: Atom -> [(Text, Text)]
atomToParams (Country  v) = [formatParam TCountry  v]
atomToParams (Province v) = [formatParam TProvince v]
atomToParams (City     v) = [formatParam TCity     v]
atomToParams (Location v) = [formatParam TLocation v]
atomToParams (Person   v) = [formatParam TPerson   v]
atomToParams (Keyword  v) = [formatParam TKeyword  v]
atomToParams (Title    v) = [formatParam TTitle    v]
atomToParams (Caption  v) = [formatParam TCaption  v]
atomToParams (Year     n) = [formatParam TYear     n]
atomToParams (Camera   v) = [formatParam TCamera   v]
atomToParams (Lens     v) = [formatParam TLens     v]
atomToParams (Problem  v) = [formatParam TProblem  v]
atomToParams (Type     v) = [formatParam TType     v]
atomToParams (Folder   v) = [formatParam TFolder   v]
atomToParams (FileName v) = [formatParam TFileName v]
atomToParams (Status   v) = [formatParam TStatus   v]
atomToParams (FClass   v) = [formatParam TFClass   v]
atomToParams (And a b)    =
  concat [atomToParams a, atomToParams b, [("and", "")]]
atomToParams (Or a b)     =
  concat [atomToParams a, atomToParams b, [("or", "")]]
atomToParams (Not a)      =
  atomToParams a ++ [("not", "")]
atomToParams (All xs)     =
  concatMap atomToParams xs ++ [("all", sformat int $ length xs)]
atomToParams (Any xs)     =
  concatMap atomToParams xs ++ [("any", sformat int $ length xs)]
atomToParams ConstTrue    = atomToParams (All [])

-- | Build image map (with static sorting).
buildImageMap :: Atom -> Repository -> SearchResults
buildImageMap atom =
  foldl' (\m img ->
             Map.insert (imgParent img, imageTimeKey img) img m
         ) Map.empty .
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
