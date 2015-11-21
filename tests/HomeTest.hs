{-# LANGUAGE OverloadedStrings #-}

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

module HomeTest
    ( homeSpecs
    ) where

import TestImport

homeSpecs :: Spec
homeSpecs =
    ydescribe "These are some example tests" $ do

        yit "loads the index and checks it looks right" $ do
            get HomeR
            statusIs 200
            htmlAllContain "h1" "Hello"

            request $ do
                setMethod "POST"
                setUrl HomeR
                addNonce
                fileByLabel "Choose a file" "tests/main.hs" "text/plain" -- talk about self-reference
                byLabel "What's on the file?" "Some Content"

            statusIs 200
            printBody
            htmlCount ".message" 1
            htmlAllContain ".message" "Some Content"
            htmlAllContain ".message" "text/plain"
