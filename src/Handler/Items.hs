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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoCPP                 #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Handler.Items
  ( itemDiv
  ) where

import           Exif          (formatPerson)
import           Handler.Utils
import           Import
import           Indexer

symbolPlCap :: Symbol -> Text
symbolPlCap TCountry  = "Countries"
symbolPlCap TProvince = "Provinces"
symbolPlCap TCity     = "Cities"
symbolPlCap TLocation = "Locations"
symbolPlCap TPerson   = "People"
symbolPlCap TKeyword  = "Keywords"
symbolPlCap TProblem  = "Problems"
symbolPlCap TYear     = "Years"
symbolPlCap TCamera   = "Cameras"

divClassForNoAtom :: Symbol -> Text
divClassForNoAtom TProblem = "border-info"
divClassForNoAtom _        = "border-warning"

divClassForAtom :: Symbol -> Text
divClassForAtom TProblem = "border-warning"
divClassForAtom _        = ""

formatter :: Symbol -> (Text -> Text)
formatter TPerson = formatPerson True
formatter _       = id

itemDiv :: Symbol -> ([(Text, Integer)], [(Text, Integer)]) -> Widget
itemDiv symbol (items, length -> rcount) =
  $(widgetFile "itemdiv")
