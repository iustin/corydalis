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

{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Reading image names from file names: copy suffixes and ranges.
module Repository.Names
  ( dropCopySuffix
  , parseImageName
  , rangeSize
  , rangeNames
  , rangeName
  ) where

import           Data.Semigroup
import qualified Data.Text               as Text
import qualified Data.Text.Short         as TS
import           System.FilePath
import qualified Text.Regex.TDFA         as TDFA

import           Compat.Orphans          ()
import           Import.NoFoundation     hiding (fileName, fileSize, path)
import           Repository              (ImageRange (..))
import           Utils.Parsing           (parseDecimal)

-- | Drops the copy suffix (as matched by the copy regex) from a name,
-- which can be a path relative to its folder: only its last component
-- is matched, so that the regex never applies to directory names.
dropCopySuffix :: Config -> String -> String
dropCopySuffix cfg path =
  let name = takeFileName path
  in case TDFA.match (reRegex $ cfgCopyRegex cfg) name of
       [_:b:_] -> replaceFileName path b
       _       -> path

-- | Reads the name of an image from a file's name without its
-- extension (which can be a path relative to its folder; only its
-- last component is matched): the image name, and the range in it, if
-- it is a composite image.
--
-- The range regex is tried first, on the name before any copy suffix
-- is removed, and the range is accepted when the two numbers allow it
-- (see 'readRange'). Otherwise the copy suffix is removed, and the
-- range is tried again on what is left: @dsc_1920-2@ is a copy of @dsc_1920@ (2 is below 1920), while
-- @dsc_1920-1925@ is a range, and @d_01-03-Edit@ a copy of the
-- composite image @d_01-03@.
parseImageName :: Config -> FilePath -> (FilePath, Maybe ImageRange)
parseImageName cfg path =
  case readRange cfg path of
    Just range -> (path, Just range)
    Nothing    -> let original = dropCopySuffix cfg path
                  in (original, readRange cfg original)

-- | The width of the numbers of a camera counter, which counts from
-- 0001 to 9999 and then starts again (as the DCF standard says).
cameraCounterWidth :: Int
cameraCounterWidth = 4

-- | Reads the range in a name, if it has one that the numbers allow:
-- the last number not below the first, or a camera counter that
-- wrapped (both numbers of 'cameraCounterWidth' digits, and the last
-- below the first, but not zero); and at most 'cfgMaxRangeSize' names
-- in it. The limit keeps names that only look like ranges, such as
-- @img_3982-1773958749527@ (a timestamp), from covering most of a
-- folder, and from listing that many names (the links and the image
-- page list the names of a range).
readRange :: Config -> FilePath -> Maybe ImageRange
readRange cfg path =
  -- The directory part, with its trailing separator (empty if none).
  let dir = replaceFileName path ""
  in case TDFA.match (reRegex $ cfgRangeRegex cfg) (Text.pack $ takeFileName path) of
       [[_, root, begin, end]] -> do
         first' <- readNumber begin
         last' <- readNumber end
         let range = ImageRange { irRoot  = TS.fromText (Text.pack dir <> root)
                                , irFirst = first'
                                , irLast  = last'
                                , irWidth = Text.length begin
                                }
             wrapped = Text.length begin == cameraCounterWidth &&
                       Text.length end == cameraCounterWidth &&
                       last' >= 1
         guard $ last' >= first' || wrapped
         guard $ rangeSize range <= toInteger (cfgMaxRangeSize cfg)
         return range
       _ -> Nothing
  where
    -- Read as an Integer first, as reading as an Int would wrap
    -- around for numbers that are too large.
    readNumber t = do
      n <- either (const Nothing) Just (parseDecimal t) :: Maybe Integer
      guard $ n <= toInteger (maxBound :: Int)
      return $ fromInteger n

-- | The numbers in a range, in order. A wrapped range (see
-- 'readRange') runs up to the largest number of its width, then from 1.
rangeNumbers :: ImageRange -> [Int]
rangeNumbers ImageRange {..}
  | irFirst <= irLast = [irFirst..irLast]
  | otherwise         = [irFirst..10 ^ irWidth - 1] ++ [1..irLast]

-- | The number of names in a range, computed without listing them
-- (the numbers can be large).
rangeSize :: ImageRange -> Integer
rangeSize ImageRange {..}
  | irFirst <= irLast = toInteger irLast - toInteger irFirst + 1
  | otherwise         = 10 ^ irWidth - toInteger irFirst + toInteger irLast

-- | The names in a range, in order (see 'rangeNumbers').
rangeNames :: ImageRange -> [ImageName]
rangeNames range = map (rangeName range) (rangeNumbers range)

-- | The name of a number in a range: its root and the number, padded.
rangeName :: ImageRange -> Int -> ImageName
rangeName range =
  ImageName . (irRoot range <>) . TS.fromText . sformat (left (irWidth range) '0' %. int)
