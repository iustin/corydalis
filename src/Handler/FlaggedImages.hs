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
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Handler.FlaggedImages
  ( getFlaggedImagesR
  , getFlaggedImagesListR
  , putImageFlagR
  , deleteImageFlagR
  )
where

import qualified Data.Text       as Text

import           Handler.Utils
import           Handler.Widgets
import           Import
import           Pics

getFlaggedImagesR :: Handler Html
getFlaggedImagesR = do
  flagged <- runDB $
    selectList [] [Asc FlaggedImageFolder, Asc FlaggedImageName]
  pics <- getPics
  let flagged' = map ((\fe -> (fe, lookupImage pics (flaggedImageFolder fe) (flaggedImageName fe))) . entityVal) flagged
  defaultLayout $ do
    setHtmlTitle "listing flagged images"
    $(widgetFile "flaggedimages")

getFlaggedImagesListR :: Handler Text
getFlaggedImagesListR = do
  flagged <- runDB $
    selectList [] [Asc FlaggedImageFolder, Asc FlaggedImageName]
  let flagged' = map entityVal flagged
  return . Text.unlines . map flaggedImageFolder $ flagged'

putImageFlagR :: Text -> Text -> Handler Html
putImageFlagR folder iname = do
  _ <- getImage folder iname
  cuser <- requireAuthId
  r <- runDB $
    insertUnique $ FlaggedImage folder iname cuser
  case r of
    Just _ -> do
      setMessage "Image flagged"
      setSession msgTypeKey msgSuccess
    Nothing -> do
      setMessage "Image already flagged!"
      setSession msgTypeKey msgWarning
  setUltDestReferer
  redirectUltDest $ ImageR folder iname

deleteImageFlagR :: Text -> Text -> Handler Html
deleteImageFlagR folder iname = do
  r <- runDB $ do
    let u = UniqueFlaggedImage folder iname
    fi <- getBy u
    case fi of
      Just (Entity fii _) -> delete fii >> return True
      Nothing             -> return False
  if r
    then do
      setMessage "Image flag removed"
      setSession msgTypeKey msgSuccess
    else do
      setMessage "Image was not flagged!"
      setSession msgTypeKey msgWarning
  setUltDestReferer
  redirectUltDest $ ImageR folder iname
