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

module Handler.LensSpec (spec) where

import           TestImport

spec :: Spec
spec = withApp $ do
  it "loads the lens page and checks it looks right" $ do
    login
    checkRoute LensStatsR
    htmlAllContain "h1" "Lens statistics"
  it "loads a wrong lens info page and returns not found" $ do
    login
    checkNotFound $ LensInfoR "foo"
  it "load a lens info page and checks it looks right" $
    const pending
