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

import           Control.Monad              (unless)
import           Language.Haskell.TH.Syntax (addDependentFile, runIO)
import           Prelude                    (fail, return, unlines, ($), (<>))
import           System.Directory           (doesFileExist,
                                             getCurrentDirectory)
import           Yesod.Static               (staticFiles)

-- The hashes of the files' contents, in the 'etag' parameter of their
-- URLs, are computed at compile time, but GHC doesn't know that this
-- module depends on the files. So depend instead on the stamp file
-- written by @npm run build@ (and @npm run watch@ and @npm run stamp@),
-- whose contents change whenever the files do, provided it is
-- regenerated after they change (see "Static files" in docs/devel.md).
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

-- This uses hardcoded "static", which is the git location of the
-- static files. At run-time, this directory can be changed via the
-- configuration file to a different path, but that doesn't affect the
-- static file values - they still point to the same sub-path in
-- whatever new directory appStaticDir points to.
staticFiles "static"
