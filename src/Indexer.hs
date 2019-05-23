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
               , searchImages
               , genQuickSearchParams
               ) where

import           Control.Monad  (foldM)
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
            | TYear
            | TCamera
            | TLens
            | TProblem
            | TType
            | TPath
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
  toPathPiece TYear     = "years"
  toPathPiece TCamera   = "cameras"
  toPathPiece TLens     = "lenses"
  toPathPiece TProblem  = "problems"
  toPathPiece TType     = "types"
  toPathPiece TPath     = "paths"
  toPathPiece TStatus   = "image-status"
  toPathPiece TFClass   = "folder-class"
  fromPathPiece "countries"    = Just TCountry
  fromPathPiece "provinces"    = Just TProvince
  fromPathPiece "cities"       = Just TCity
  fromPathPiece "locations"    = Just TLocation
  fromPathPiece "people"       = Just TPerson
  fromPathPiece "keywords"     = Just TKeyword
  fromPathPiece "years"        = Just TYear
  fromPathPiece "cameras"      = Just TCamera
  fromPathPiece "lenses"       = Just TLens
  fromPathPiece "problems"     = Just TProblem
  fromPathPiece "types"        = Just TType
  fromPathPiece "image-status" = Just TStatus
  fromPathPiece "folder-class" = Just TFClass
  -- TODO: hmm, what about paths?
  fromPathPiece _              = Nothing

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
          | Year     (NumOp Integer)
          | Camera   StrOp
          | Lens     StrOp
          | Problem  StrOp
          | Type     TypeOp
          | Path     StrOp
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
symbolName TYear     = "year"
symbolName TCamera   = "camera"
symbolName TLens     = "lens"
symbolName TProblem  = "problem"
symbolName TType     = "type"
symbolName TPath     = "path"
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
parseSymbol "year"         = Just TYear
parseSymbol "camera"       = Just TCamera
parseSymbol "lens"         = Just TLens
parseSymbol "problem"      = Just TProblem
parseSymbol "type"         = Just TType
parseSymbol "path"         = Just TPath
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
    TYear     -> Year      OpNa
    TCamera   -> Camera    OpMissing
    TLens     -> Lens      OpMissing
    TProblem  -> Problem   OpMissing
    TType     -> Type      TypeUnknown
    -- FIXME: this should fail instead (using Maybe).
    TPath     -> Path      OpMissing
    -- FIXME: what to return here?
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
    TYear     -> Year     <$> dec
    TCamera   -> Camera   <$> str
    TLens     -> Lens     <$> str
    TProblem  -> Problem  <$> str
    TType     -> Type     <$> typ
    TPath     -> Path     <$> str
    TStatus   -> Status   <$> sta
    TFClass   -> FClass   <$> parseFolderClass v

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
    TLens     -> Just . Lens     . OpFuzzy $ f
    TProblem  -> Just . Problem  . OpFuzzy $ f
    TYear     ->
      case Text.decimal v of
        Right (w', "") -> Just . Year . OpEq $ w'
        _              -> Nothing
    TType     -> Type <$> parseType v
    TPath     -> Just . Path . OpFuzzy $ f
    TStatus   -> Status <$> parseImageStatus v
    TFClass   -> FClass <$> parseFolderClass v
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
atomTypeDescriptions TLens     = "lenses"
atomTypeDescriptions TProblem  = "problems"
atomTypeDescriptions TType     = "types"
atomTypeDescriptions TPath     = "paths"
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
atomDescription (Path s)               = describeStr "path" s
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

-- TODO: implement searching type=unknown after untracked merging into image.
folderSearchFunction :: Atom -> PicDir -> Bool
folderSearchFunction ConstTrue = const True
folderSearchFunction a@(Year OpNa) =
  \p -> imagesMatchAtom a (pdImages p) ||
        isNothing (pdYear p)
folderSearchFunction (FClass c) =
  ((== c) . folderClass)
folderSearchFunction a = imagesMatchAtom a . pdImages

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

imageSearchFunction (Path p) =
  \img -> (evalStr p  . Just . imgName)   img ||
          (evalStr p  . Just . imgParent) img

imageSearchFunction (Status v) =
  ((== v) . imgStatus)

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

getAtoms :: Symbol -> Repository -> NameStats
getAtoms TCountry  = gExifCountries . repoExif
getAtoms TProvince = gExifProvinces . repoExif
getAtoms TCity     = gExifCities    . repoExif
getAtoms TLocation = gExifLocations . repoExif
getAtoms TPerson   = gExifPeople    . repoExif
getAtoms TKeyword  = gExifKeywords  . repoExif
getAtoms TCamera   = gExifCameras   . repoExif
getAtoms TLens     = gExifLenses    . repoExif
getAtoms TYear     = yearStats
getAtoms TProblem  = repoProblems
getAtoms TType     = typeStats
getAtoms TPath     = const Map.empty
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

numOpToParam :: (ToText a) => Text -> NumOp a -> (Text, Text)
numOpToParam s (OpEq v) = (s, toText v)
numOpToParam s (OpLt v) = (s, '<' `Text.cons` toText v)
numOpToParam s (OpGt v) = (s, '>' `Text.cons` toText v)
numOpToParam s  OpNa    = ("no-" <> s, "")

strOpToParam :: Text -> StrOp -> (Text, Text)
strOpToParam s (OpEqual v) = (s, v)
strOpToParam s (OpFuzzy v) = (s, '~' `Text.cons` unFuzzy v)
strOpToParam s OpMissing   = ("no-" <> s, "")

typeOpToParam :: Text -> TypeOp -> (Text, Text)
typeOpToParam s = (s, ) . showType

statusOpToParam :: Text -> ImageStatus -> (Text, Text)
statusOpToParam s = (s, ) . showImageStatus

fClassToParam :: Text -> FolderClass -> (Text, Text)
fClassToParam s = (s, ) . showFolderClass

atomToParams :: Atom -> [(Text, Text)]
atomToParams (Country  v) = [strOpToParam (symbolName TCountry ) v]
atomToParams (Province v) = [strOpToParam (symbolName TProvince) v]
atomToParams (City     v) = [strOpToParam (symbolName TCity    ) v]
atomToParams (Location v) = [strOpToParam (symbolName TLocation) v]
atomToParams (Person   v) = [strOpToParam (symbolName TPerson  ) v]
atomToParams (Keyword  v) = [strOpToParam (symbolName TKeyword ) v]
atomToParams (Year     n) = [numOpToParam (symbolName TYear    ) n]
atomToParams (Camera   v) = [strOpToParam (symbolName TCamera  ) v]
atomToParams (Lens     v) = [strOpToParam (symbolName TLens    ) v]
atomToParams (Problem  v) = [strOpToParam (symbolName TProblem ) v]
atomToParams (Type     v) = [typeOpToParam (symbolName TType   ) v]
atomToParams (Path     v) = [strOpToParam (symbolName TPath    ) v]
atomToParams (Status   v) = [statusOpToParam (symbolName TStatus ) v]
atomToParams (FClass   v) = [fClassToParam (symbolName TFClass) v]
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

searchImages :: Ctx -> Atom -> Repository -> IO SearchResults
searchImages ctx atom pics = do
  -- Note: the call to buildImageMap must *not* be evaluated,
  -- otherwise we don't gain anything from caching it.
  -- TODO: remove repository argument, read from ctx? [cleanup]
  let lazyimages = buildImageMap atom pics
  getSearchResults ctx lazyimages (atomToParams atom)

-- | Generates a quick search atom.
genQuickSearchParams :: Repository -> Text ->
                        Either Text ([Atom], Maybe [(Text, Text)])
genQuickSearchParams _ "" = Left "Empty search parameter"
genQuickSearchParams pics search =
  let swords = nub $ Text.words search
      -- TODO: this way of checking search is too slow, especially for
      -- negative searches, as it really needs to scan the entire
      -- repository. Improve by checking membership in sum of
      -- atom-values (e.g. a set with all defined cities, so on) where
      -- possible. [performance]
      findsAny a = not . null . filterImagesBy (imageSearchFunction a) $ pics
      -- Algorithm: for each symbol, try all words (that can be
      -- converted). Combine all words that find matches using the Any
      -- atom, and at the end, combine all symbol findings with the
      -- 'All' atom. This means that a search of type "2018 Spain"
      -- finds correctly all pictures taken in Spain in the year 2018,
      -- but a search of type "Italy England" won't find anything,
      -- since no picture will have a location of both. It would be
      -- possible to understand that in this case the likely meaning
      -- is Italy *or* England, but with any complex filters it would
      -- become a headache.
      params = foldl' (\(pf, pm) w ->
                         let allA = mapMaybe (`quickSearch` w)
                                      [minBound..maxBound]
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
             else Just . atomToParams . allAtom $ found
  in Right (notfound, p')
