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

import qualified Data.Text          as Text
import           Database.Esqueleto ((?.), (^.))
import qualified Database.Esqueleto as E

import           Handler.Utils
import           Handler.Widgets
import           Import
import           Pics

getFlaggedImagesR :: Handler Html
getFlaggedImagesR = do
  flagged <- runDB
    $ E.select
    $ E.from $ \(flaggedimage `E.LeftOuterJoin` user) -> do
      E.on $ E.just (flaggedimage ^. FlaggedImageAuthorId) E.==. user ?. UserId
      return ( flaggedimage ^. FlaggedImageFolder
             , flaggedimage ^. FlaggedImageName
             , user ?. UserName
             )
  pics <- getPics
  let flagged' = map (\(E.Value dir, E.Value name, E.Value author) ->
                         let img = lookupImage pics dir name
                         in (dir, name, author, img)) flagged
  defaultLayout $ do
    setHtmlTitle "listing flagged images"
    $(widgetFile "flaggedimages")

getFlaggedImagesListR :: Handler Text
getFlaggedImagesListR = do
  flagged <- runDB $
    selectList [] [Asc FlaggedImageFolder, Asc FlaggedImageName]
  let flagged' = map entityVal flagged
  return . Text.unlines . map flaggedImageFolder $ flagged'

flagImageMsg :: Bool -> Text
flagImageMsg True  = "Image flagged"
flagImageMsg False = "Image already flagged!"

unFlagImageMsg :: Bool -> Text
unFlagImageMsg True  = "Image flag removed"
unFlagImageMsg False = "Image was not flagged!"

flagImage :: Text -> ImageName -> Handler Bool
flagImage folder iname = do
  _     <- getImage folder iname
  cuser <- requireAuthId
  r     <- runDB $ insertUnique $ FlaggedImage folder iname cuser
  return $ isJust r

unFlagImage :: Text -> ImageName -> Handler Bool
unFlagImage folder iname = runDB $ do
  let u = UniqueFlaggedImage folder iname
  fi <- getBy u
  case fi of
    Just (Entity fii _) -> delete fii >> return True
    Nothing             -> return False

flagHtml :: Text -> ImageName -> Text -> Text -> Handler Html
flagHtml folder iname msg kind = do
  setMessage $ toHtml msg
  setSession msgTypeKey kind
  setUltDestReferer
  redirectUltDest $ ImageR folder iname

flagJson :: Text -> Handler Value
flagJson msg = return $ object ["text" .= msg]

flagHandler
  :: (Text -> ImageName -> Handler Bool)
  -> (Bool -> Text)
  -> Text
  -> ImageName
  -> Handler TypedContent
flagHandler action msggen folder iname = do
  r <- action folder iname
  let msg  = msggen r
      kind = if r then msgSuccess else msgWarning
  selectRep $ do
    provideRep $ flagJson msg
    provideRep $ flagHtml folder iname msg kind

putImageFlagR :: Text -> ImageName -> Handler TypedContent
putImageFlagR = flagHandler flagImage flagImageMsg

deleteImageFlagR :: Text -> ImageName -> Handler TypedContent
deleteImageFlagR = flagHandler unFlagImage unFlagImageMsg
