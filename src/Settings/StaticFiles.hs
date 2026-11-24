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
{-# LANGUAGE TemplateHaskell   #-}

module Settings.StaticFiles where

import           Control.Monad              (forM, unless)
import           Data.List                  (isPrefixOf, sort)
import           Language.Haskell.TH.Syntax (addDependentFile, runIO)
import           Prelude
import           System.Directory           (doesDirectoryExist,
                                             doesFileExist,
                                             getCurrentDirectory,
                                             listDirectory)
import           System.FilePath            ((</>))
import           Yesod.EmbeddedStatic       (embedFileAt, mkEmbeddedStatic)

import           Settings.Development       (development)

-- The files are listed (and, in production builds, read, see below)
-- at compile time, but GHC doesn't know that this module depends on
-- them. So depend instead on the stamp file written by @npm run build@
-- (and @npm run watch@ and @npm run stamp@), whose contents change
-- whenever the files do, provided it is regenerated after they change
-- (see "Static files" in docs/devel.md).
$(do
    let stamp = ".static-files.stamp"
    exists <- runIO $ doesFileExist stamp
    cwd <- runIO getCurrentDirectory
    unless exists $ fail $ unlines
      [ "The stamp file of the static files, '" <> stamp <>
        "', is missing (in " <> cwd <> ")."
      , "Run 'make bootstrap' (or 'npm run build') to create it."
      ]
    addDependentFile stamp
    return [])

-- Defines the static subsite, 'corydalisStatic', serving the files
-- in the "static" directory, and a route name for each file (e.g.
-- @js_viewer_js@ for @static/js/viewer.js@).
--
-- In production builds, the files' contents are embedded in the
-- binary, (gzip-)compressed if useful, and their URLs include a hash of
-- their contents, in the @etag@ parameter. In development builds
-- (with the @dev@ flag, as used by the tests, or the @library-only@
-- one, as used by @yesod devel@), the
-- files are instead read from disk on each request, so changes to them
-- need no recompilation (unless files are added or removed).
--
-- Hidden files and directories are skipped, at any level, as editors
-- and tools may leave them around.
$(let listFiles loc dir = do
        names <- sort <$> listDirectory dir
        fmap concat $ forM [n | n <- names, not ("." `isPrefixOf` n)] $ \name -> do
          let path = dir </> name
              loc' = if loc == "" then name else loc <> "/" <> name
          isDir <- doesDirectoryExist path
          if isDir
            then listFiles loc' path
            else return [(loc', path)]
  in mkEmbeddedStatic development "corydalisStatic"
       [ runIO (listFiles "" "static") >>=
           fmap concat . mapM (uncurry embedFileAt) ])
