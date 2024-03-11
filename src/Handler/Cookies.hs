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
{-# LANGUAGE NoCPP             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Cookies
  ( viewCookieName
  , lastViewCookie
  ) where

import           Import.NoFoundation

import           Data.Time
import           Web.Cookie

-- | Name of the last view cookie.
viewCookieName :: Text
viewCookieName = "corydalis.last-view"

-- | Template for the view cookie.
baseViewCookie :: SetCookie
baseViewCookie =
  def { setCookieName = encodeUtf8 viewCookieName
      , setCookiePath = Just $ encodeUtf8 ("/"::Text)
      , setCookieMaxAge = Just . secondsToDiffTime $ 86400 * 7 * 52
      , setCookieDomain = Nothing
      , setCookieHttpOnly = True
      , setCookieSecure = True
      , setCookieSameSite = Just sameSiteStrict
      }

-- | Actual view cookie for a view mode.
lastViewCookie :: Bool -> ViewMode -> SetCookie
lastViewCookie secureCookies vm =
  baseViewCookie { setCookieValue  = formatViewMode vm
                 , setCookieSecure = secureCookies
                 }
