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

module TemplatesSpec (spec) where

import qualified Data.Text        as T
import           System.Directory (listDirectory)
import           System.FilePath  (takeExtension)

import           TestImport

-- | The table columns allowed to use a select filter, as (template file,
-- header line). Tablesorter builds the select options as HTML from the
-- cell texts without escaping them, so only columns whose values are
-- fixed by the server code (and not e.g. file names or EXIF data) may be
-- listed here.
allowedSelectFilters :: [(FilePath, Text)]
allowedSelectFilters =
  [ ("imagelist.hamlet", "<th .filter-select>Status")
  , ("lensstats.hamlet", "<th .sorter-text .filter-select>Type")
  ]

-- | Returns all template lines that use a select filter.
selectFilterLines :: IO [(FilePath, Text)]
selectFilterLines = do
  files <- sort . filter ((== ".hamlet") . takeExtension) <$>
             listDirectory "templates"
  concat <$> forM files (\file -> do
    contents <- readFileUtf8 ("templates" </> file)
    return [ (file, line') | line <- lines contents
                           , let line' = T.strip line
                           , not ("$#" `isPrefixOf` line') -- comments
                           , "filter-select" `isInfixOf` line' ])

spec :: Spec
spec = describe "table templates" $
  it "only use select filters on columns with trusted values" $
    selectFilterLines `shouldReturn` allowedSelectFilters
