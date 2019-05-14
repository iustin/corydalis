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

module Handler.CameraSpec (spec) where

import           TestImport

spec :: Spec
spec = withApp $ do
  it "loads the camera stats page and checks it looks right" $ do
    login
    checkRoute CameraStatsR
    htmlAllContain "h1" "Camera statistics"
    htmlAnyContain "p" "No camera information found."
  it "load a wrong camera details page" $ do
    login
    checkNotFound $ CameraInfoR "foo"
  it "load a camera details page" $
    const pending
