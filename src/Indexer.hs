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
{-# LANGUAGE ViewPatterns       #-}

module Indexer ( Symbol(..)
               , Atom(..)
               , symbolNames
               , symbolName
               , negSymbolName
               , atomTypeDescriptions
               , atomDescription
               , parseAtom
               , buildMissingAtom
               , folderSearchFunction
               , imageSearchFunction
               , getAtoms
               , parseAtomParams
               , buildImageMap
               , searchImages
               , genQuickSearchParams
               ) where

import           Control.Monad              (foldM)
import           Control.Monad.Trans.Except
import           Data.List                  (foldl')
import qualified Data.Map                   as Map
import           Data.Maybe                 (isNothing)
import           Data.Semigroup             ((<>))
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Read             as Text
import           Yesod                      (PathPiece (..))

import           Exif
import           Pics
import           Types                      (UrlParams)

data Symbol = TCountry
              | TProvince
              | TCity
              | TLocation
              | TPerson
              | TKeyword
              | TYear
              | TCamera
                deriving (Enum, Bounded, Show, Read, Eq)

instance PathPiece Symbol where
  toPathPiece  = atomTypeDescriptions
  fromPathPiece "countries" = Just TCountry
  fromPathPiece "provinces" = Just TProvince
  fromPathPiece "cities"    = Just TCity
  fromPathPiece "locations" = Just TLocation
  fromPathPiece "people"    = Just TPerson
  fromPathPiece "keywords"  = Just TKeyword
  fromPathPiece "years"     = Just TYear
  fromPathPiece "cameras"   = Just TCamera
  fromPathPiece _           = Nothing

-- TODO: Replace this with Data.CaseInsensitive from case-insensitive
-- package, once the actual image metadata uses it too.
newtype FuzzyText = FuzzyText { unFuzzy :: Text }
  deriving (Show)

makeFuzzy :: Text -> FuzzyText
makeFuzzy = FuzzyText . Text.toCaseFold

data StrOp = OpEqual Text
           | OpFuzzy FuzzyText
           | OpMissing
           deriving (Show)

data NumOp a where
  OpEq :: (Eq a) => a -> NumOp a
  OpNa :: NumOp a
  OpLt :: (Num a, Ord a) => a -> NumOp a
  OpGt :: (Num a, Ord a) => a -> NumOp a

deriving instance Show a => Show (NumOp a)

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

data Atom = Country  StrOp
          | Province StrOp
          | City     StrOp
          | Location StrOp
          | Person   StrOp
          | Keyword  StrOp
          | Year     (NumOp Integer)
          | Camera   StrOp
          | And Atom Atom
          | Or  Atom Atom
          | Not Atom
          | All [Atom]
          | Any [Atom]
          deriving (Show)

symbolNames :: [(Symbol, Text)]
symbolNames = map (\t -> (t, symbolName t)) [minBound..maxBound]

symbolName :: Symbol -> Text
symbolName TCountry  = "country"
symbolName TProvince = "province"
symbolName TCity     = "city"
symbolName TLocation = "location"
symbolName TPerson   = "person"
symbolName TKeyword  = "keyword"
symbolName TYear     = "year"
symbolName TCamera   = "camera"

negSymbolName :: Symbol -> Text
negSymbolName atom = "no-" <> symbolName atom

parseSymbol :: Text -> Maybe Symbol
parseSymbol "country"  = Just TCountry
parseSymbol "province" = Just TProvince
parseSymbol "city"     = Just TCity
parseSymbol "location" = Just TLocation
parseSymbol "person"   = Just TPerson
parseSymbol "keyword"  = Just TKeyword
parseSymbol "year"     = Just TYear
parseSymbol "camera"   = Just TCamera
parseSymbol _          = Nothing

buildMissingAtom :: Symbol -> Atom
buildMissingAtom s =
  case s of
    TCountry  -> Country   OpMissing
    TProvince -> Province  OpMissing
    TCity     -> City      OpMissing
    TLocation -> Location  OpMissing
    TPerson   -> Person    OpMissing
    TKeyword  -> Keyword   OpMissing
    TYear     -> Year      OpNa
    TCamera   -> Camera    OpMissing

parseAtom :: Text -> Text -> Maybe Atom
parseAtom (Text.splitAt 3 -> ("no-", v)) _ =
  buildMissingAtom <$> parseSymbol v

parseAtom a v = do
  s <- parseSymbol a
  let dec = parseDecimal v
      str = parseString v
  case s of
    TCountry  -> str >>= Just . Country
    TProvince -> str >>= Just . Province
    TCity     -> str >>= Just . City
    TLocation -> str >>= Just . Location
    TPerson   -> str >>= Just . Person
    TKeyword  -> str >>= Just . Keyword
    TYear     -> dec >>= Just . Year
    TCamera   -> str >>= Just . Camera

quickSearch :: Symbol -> Text -> Maybe Atom
quickSearch s v =
  case s of
    TCountry  -> Just . Country  . OpFuzzy $ f
    TProvince -> Just . Province . OpFuzzy $ f
    TCity     -> Just . City     . OpFuzzy $ f
    TLocation -> Just . Location . OpFuzzy $ f
    TPerson   -> Just . Person   . OpFuzzy $ f
    TKeyword  -> Just . Keyword  . OpFuzzy $ f
    TCamera   -> Just . Camera   . OpFuzzy $ f
    TYear     ->
      case Text.decimal v of
        Right (w', "") -> Just . Year . OpEq $ w'
        _              -> Nothing
  where f = makeFuzzy v

atomTypeDescriptions :: Symbol -> Text
atomTypeDescriptions TCountry  = "countries"
atomTypeDescriptions TProvince = "provinces"
atomTypeDescriptions TCity     = "cities"
atomTypeDescriptions TLocation = "locations"
atomTypeDescriptions TPerson   = "people"
atomTypeDescriptions TKeyword  = "keywords"
atomTypeDescriptions TYear     = "years"
atomTypeDescriptions TCamera   = "cameras"

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
  "tagged with a person with a name containing " <> unFuzzy v
atomDescription (Person OpMissing) = "has no person information"
atomDescription (Keyword (OpEqual keyword)) =
  case keyword of
    "" -> "tagged with an empty keyword"
    k  -> "tagged with keyword " <> k <> ""
atomDescription (Keyword (OpFuzzy v)) =
  "tagged with a keyword containing " <> unFuzzy v
atomDescription (Keyword OpMissing) = "not tagged with any keywords"
atomDescription (Year (OpEq year))    = "taken in the year " <> toText year
atomDescription (Year (OpLt year))    = "taken before the year " <> toText year
atomDescription (Year (OpGt year))    = "taken after the year " <> toText year
atomDescription (Year OpNa)           = "does not have date information"
atomDescription (Camera OpMissing)    = "has no camera information"
atomDescription (Camera (OpEqual "")) = "has defined but empty camera information"
atomDescription (Camera (OpEqual v))  = "shot with a " <> v <> " camera"
atomDescription (Camera (OpFuzzy v))  =
  "shot with a camera named like " <> unFuzzy v
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
atomDescription (Not a) = "(not " <> atomDescription a <> ")"

atomDescription (All as) = "(all of: " <> Text.intercalate ", " (map atomDescription as) <> ")"
atomDescription (Any as) = "(any of: " <> Text.intercalate ", " (map atomDescription as) <> ")"

-- | Set search function for either membership or null set checking.
setSearch :: StrOp -> Set Text -> Bool
setSearch OpMissing = Set.null
setSearch (OpEqual v) = (v `Set.member`)
setSearch (OpFuzzy f)=
  Set.foldr' (\a v -> v || fuzzyMatch f a) False

folderSearchFunction :: Atom -> PicDir -> Bool
folderSearchFunction a =
  any (imageSearchFunction a) . pdImages

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

imageSearchFunction (Year year) =
  evalNum year . imageYear

imageSearchFunction (Camera camera) =
  evalStr camera . exifCamera . imgExif

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

getAtoms :: Symbol -> Repository -> NameStats
getAtoms TCountry  = gExifCountries . repoExif
getAtoms TProvince = gExifProvinces . repoExif
getAtoms TCity     = gExifCities    . repoExif
getAtoms TLocation = gExifLocations . repoExif
getAtoms TPerson   = gExifPeople    . repoExif
getAtoms TKeyword  = gExifKeywords  . repoExif
getAtoms TCamera   = gExifCameras   . repoExif
getAtoms TYear     = const Map.empty

-- | Builder for all atom.
--
-- Builds an 'All' atom, with special casing for small values.
allAtom :: [Atom] -> Atom
allAtom [x]    = x       -- no sense in over-wrapping.
allAtom [x, y] = And x y -- 'x and y' is better than 'all of [x, y]'.
allAtom xs     = All xs  -- and normal wrapping.

-- | Builder for any atom.
-- Builds an 'Any' atom, with special casing for small values.
anyAtom :: [Atom] -> Atom
anyAtom [x]    = x
anyAtom [x, y] = Or x y
anyAtom xs     = Any xs

rpnParser :: [Atom] -> (Text, Text) -> Except Text [Atom]
rpnParser (x:y:ys) ("and",_) = return $ And x y:ys
rpnParser (x:y:ys) ("or",_) = return $ Or x y:ys
rpnParser (x:xs) ("not", _) =
  let a = case x of
            Not y -> y
            _     -> Not x
  in return $ a:xs
rpnParser xs ("all", _) = return [allAtom xs]
rpnParser xs ("any", _) = return [anyAtom xs]
rpnParser xs (an, av) =
  let v = parseAtom an av
  in case v of
    Just v' -> return $ v':xs
    Nothing -> throwE $ "Failed to parse the atom " <>
               an <> "=" <> av <>
               " with stack " <> Text.pack (show xs)

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

parseAtomParams :: [(Text, Text)] -> Either Text Atom
parseAtomParams params = runExcept $
  allAtom <$> foldM rpnParser [] params

numOpToParam :: (ToText a) => Text -> NumOp a -> (Text, Text)
numOpToParam s (OpEq v) = (s, toText v)
numOpToParam s (OpLt v) = (s, '<' `Text.cons` toText v)
numOpToParam s (OpGt v) = (s, '>' `Text.cons` toText v)
numOpToParam s  OpNa    = ("no-" <> s, "")

strOpToParam :: Text -> StrOp -> (Text, Text)
strOpToParam s (OpEqual v) = (s, v)
strOpToParam s (OpFuzzy v) = (s, '~' `Text.cons` unFuzzy v)
strOpToParam s OpMissing   = ("no-" <> s, "")

atomToParams :: Atom -> [(Text, Text)]
atomToParams (Country  v) = [strOpToParam (symbolName TCountry) v]
atomToParams (Province v) = [strOpToParam (symbolName TProvince) v]
atomToParams (City v) = [strOpToParam (symbolName TCity) v]
atomToParams (Location v) = [strOpToParam (symbolName TLocation) v]
atomToParams (Person v) = [strOpToParam (symbolName TPerson) v]
atomToParams (Keyword v) = [strOpToParam (symbolName TKeyword) v]
atomToParams (Year n) = [numOpToParam (symbolName TYear) n]
atomToParams (Camera v) = [strOpToParam (symbolName TCamera) v]
atomToParams (And a b) =
  concat $ [atomToParams a, atomToParams b, [("and", "")]]
atomToParams (Or a b) =
  concat $ [atomToParams a, atomToParams b, [("or", "")]]
atomToParams (Not a) =
  concat $ [atomToParams a, [("not", "")]]
atomToParams (All xs) =
  reverse $ ("all", ""):concatMap atomToParams xs
atomToParams (Any xs) =
  reverse $ ("any", ""):concatMap atomToParams xs

-- | Build image map (with static sorting).
buildImageMap :: Atom -> Repository -> SearchResults
buildImageMap atom =
  foldl' (\m img ->
             Map.insert (imgParent img, imgName img) img m
         ) Map.empty .
  filterImagesBy (imageSearchFunction atom)

searchImages :: UrlParams -> Atom -> Repository -> IO SearchResults
searchImages params atom pics = do
  -- Note: the call to buildImageMap must *not* be evaluated,
  -- otherwise we don't gain anything from caching it.
  let lazyimages = buildImageMap atom pics
  getSearchResults lazyimages params

-- | Generates a quick search atom.
genQuickSearchParams :: Text -> Either Text [(Text, Text)]
genQuickSearchParams q = runExcept $ do
  search <- case q of
              "" -> throwE $ "Empty search parameter"
              _  -> return q
  let params = foldl' (\p s -> case quickSearch s search of
                                 Nothing -> p
                                 Just a  -> a:p
                      ) [] [minBound..maxBound]
      p' = Any params
  return $ atomToParams p'
