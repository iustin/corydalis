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

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Reading image names from file names: copy suffixes and ranges.
module Repository.Names
  ( dropCopySuffix
  , expandRangeFile
#ifdef TEST
  , maxRangeExpansion
#endif
  ) where

import           Data.Semigroup
import qualified Data.Text               as Text
import           System.FilePath
import qualified Text.Regex.TDFA         as TDFA

import           Compat.Orphans          ()
import           Import.NoFoundation     hiding (fileName, fileSize, path)
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

-- | Upper bound on names generated from a range file.
--
-- Filenames like @img_3982-1773958749527@ match the range regex but
-- are timestamps, not brackets; expanding them OOMs.
maxRangeExpansion :: Int
maxRangeExpansion = 1024

-- | Expands a range file's name (which can be a path relative to its
-- folder) into the names of the images it is composed from. As for
-- copies, only the last component of the path is matched.
expandRangeFile :: Config -> FilePath -> [Text]
expandRangeFile cfg path =
  -- The directory part, with its trailing separator (empty if none).
  let dir = Text.pack $ replaceFileName path ""
  in case TDFA.match (reRegex $ cfgRangeRegex cfg) (Text.pack $ takeFileName path) of
       [[_, root, begin, end]] -> let ib = parseDecimal begin::Either Text Int
                                      ie = parseDecimal end
                                      formatPadded = sformat (left (length begin) '0' %. int)
                                  in case (ib, ie) of
                                       (Right b, Right e)
                                         | let n = toInteger e - toInteger b + 1
                                         , n >= 1
                                         , n <= toInteger maxRangeExpansion ->
                                             [dir <> root <> formatPadded i | i <- [b..e]]
                                       _ -> []
       _ -> []
