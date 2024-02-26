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

{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}


module Stats ( DateRange
            ,  Occurrence(..)
            , Trends
            , ocFromSize
            , CameraInfo(..)
)  where

import           Control.DeepSeq
import qualified Data.Map.Strict     as Map
import           Data.Semigroup
import           Data.Store.TH       (makeStore)
import           Data.Time.LocalTime
import           System.Posix.Types  (FileOffset)

import           Import.NoFoundation hiding (fileName, fileSize)


-- | Helper type alias for a (start date, end date) interval.
type DateRange = (LocalTime, LocalTime)

-- | Type alias for the trends keys.
type TrendsKey = (Int, Int)

-- | Type alias for per-month statistics.
type Trends = Map TrendsKey Integer

-- | Min-max merge of two pairs.
minMaxPairMerge :: (Ord a) => (a, a) -> (a, a) -> (a, a)
minMaxPairMerge (xmin, xmax) (ymin, ymax) =
  (xmin `min` ymin, xmax `max` ymax)

-- | Merge two Maybe values using a custom function.
mergeMM :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
mergeMM _ Nothing x          = x
mergeMM _ x Nothing          = x
mergeMM fn (Just x) (Just y) = Just $ fn x y

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
  } deriving (Show)

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
  rnf occ = rwhnf occ `seq` rnf (ocData occ)

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
  { ciName         :: !Text
  , ciShutterCount :: !(Maybe (Integer, Integer))
  } deriving (Eq, Show, Ord)

instance NFData CameraInfo where
  rnf CameraInfo{..} = rnf ciName `seq`
                       rnf ciShutterCount

instance Semigroup CameraInfo where
  x <> y = x { ciShutterCount = mergeMinMaxPair (ciShutterCount x) (ciShutterCount y)
             }

instance Default CameraInfo where
  -- Fixme: remove the duplication with Exif.hs
  def = CameraInfo "unknown" Nothing

$(makeStore ''CameraInfo)
$(makeStore ''Occurrence)
