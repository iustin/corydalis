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
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Common handler functions.
module Handler.Common where

import           Data.FileEmbed (embedFile)
import           Import

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

-- | The favicon, cached for a month.
--
-- Private because Yesod attaches the session cookie to handler
-- responses; it can be public once routes can go without a session,
-- see <https://github.com/yesodweb/yesod/issues/1938>.
getFaviconR :: Handler TypedContent
getFaviconR = do addHeader "Cache-Control" "private, max-age=2592000"
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")
