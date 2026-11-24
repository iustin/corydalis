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

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stats ( DateRange
             , DayRange
             , Occurrence(..)
             , Trends
             , ocFromSize
             , CameraInfo(..)
             , keeperRate
             , mergeMinMaxPair
)  where

import           Control.DeepSeq
import qualified Data.Map.Strict     as Map
import           Data.Semigroup
import           Data.Store          (Store)
import           Data.Time.LocalTime
import           System.Posix.Types  (FileOffset)

import           Import.NoFoundation hiding (fileName, fileSize)


-- | Helper type alias for a (start date, end date) interval.
type DateRange = (LocalTime, LocalTime)

-- | Helper type alias for a (start day, end day) interval.
--
-- This is more useful for the day-level statistics.
type DayRange = (Day, Day)

-- | Type alias for the trends keys.
type TrendsKey = (Int, Int)

-- | Type alias for per-month statistics.
type Trends = Map TrendsKey Integer

-- | Min-max merge of two pairs.
--
-- Both ends of the result are evaluated along with the pair: these
-- are merged repeatedly (e.g. once per image), so otherwise each end
-- would be a chain of pending comparisons, holding all the values
-- compared.
minMaxPairMerge :: (Ord a) => (a, a) -> (a, a) -> (a, a)
minMaxPairMerge (xmin, xmax) (ymin, ymax) =
  let !lo = xmin `min` ymin
      !hi = xmax `max` ymax
  in (lo, hi)

-- | Merge two Maybe values using a custom function, whose result is
-- evaluated along with the merged value (and not left pending inside
-- its Just).
mergeMM :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
mergeMM _ Nothing x          = x
mergeMM _ x Nothing          = x
mergeMM fn (Just x) (Just y) = Just $! fn x y

-- | Helper for merging two Maybe values using minxMaxPairMerge.
mergeMinMaxPair :: (Ord a) => Maybe (a, a) -> Maybe (a, a) -> Maybe (a, a)
mergeMinMaxPair = mergeMM minMaxPairMerge

data Occurrence a = Occurrence
  { ocFiles     :: !Integer
  , ocFileSize  :: !FileOffset
  , ocFolders   :: !Integer
  , ocData      :: !a
  , ocTrends    :: !Trends
  , ocDateRange :: !(Maybe DateRange)
  } deriving (Show, Eq, Generic)

instance (Store a) => Store (Occurrence a)

instance Default a => Default (Occurrence a) where
  def = Occurrence 0 0 0 def Map.empty Nothing

instance (Semigroup a, Default a) => Monoid (Occurrence a) where
  mempty = def

instance Semigroup a => Semigroup (Occurrence a) where
  x <> y = Occurrence { ocFiles = ocFiles x + ocFiles y
                      , ocFileSize = ocFileSize x + ocFileSize y
                      , ocFolders = ocFolders x + ocFolders y
                      , ocData = ocData x <> ocData y
                      , ocTrends = Map.unionWith (+) (ocTrends x) (ocTrends y)
                      , ocDateRange = mergeMinMaxPair (ocDateRange x) (ocDateRange y)
                      }

instance NFData a => NFData (Occurrence a) where
  rnf Occurrence{..} = rnf ocData      `seq`
                       rnf ocTrends    `seq`
                       rnf ocDateRange

ocFromSize :: FileOffset -> a -> Maybe TrendsKey -> Maybe DateRange -> Occurrence a
ocFromSize size d tk dr =
  Occurrence { ocFiles = 1
             , ocFileSize = size
             , ocFolders = 0
             , ocData = d
             , ocTrends = maybe Map.empty (`Map.singleton` 1) tk
             , ocDateRange = dr
             }

data CameraInfo = CameraInfo
  { ciName         :: !SymbolizedItem
  , ciShutterCount :: !(Maybe (Integer, Integer))
  } deriving (Eq, Show, Ord, Generic)

instance Store CameraInfo

-- | The keeper rate of a camera, if its shutter counts are known: its
-- number of images divided by the number of shots in its range of
-- shutter counts (both ends included).
--
-- All its images are counted, including those without a shutter count,
-- so this is an approximation, which can exceed 1.
keeperRate :: Occurrence CameraInfo -> Maybe Double
keeperRate Occurrence{..} =
  case ciShutterCount ocData of
    Nothing -> Nothing
    Just (scMin, scMax) ->
      let numImgs = fromIntegral ocFiles
          delta = fromIntegral $ scMax - scMin + 1
      in Just $ numImgs / delta

instance NFData CameraInfo where
  rnf CameraInfo{..} = rnf ciName `seq`
                       rnf ciShutterCount

instance Semigroup CameraInfo where
  x <> y = x { ciShutterCount = mergeMinMaxPair (ciShutterCount x) (ciShutterCount y)
             }

instance Default CameraInfo where
  -- Fixme: remove the duplication with Exif.hs
  def = CameraInfo (mkSymbolizedItem ("unknown"::Text)) Nothing
