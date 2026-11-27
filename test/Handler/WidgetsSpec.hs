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

module Handler.WidgetsSpec (spec) where

import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Yesod.Core

import           Data.Default                  (def)
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Data.Time                     (LocalTime (..), midnight)

import           Exif
import           Handler.Widgets
import           Indexer                       (Atom (..), NumOp (..), StrOp (..))
import           Repository                    (Image (..), MediaType (..))
import           TestImport

-- | Renders a widget's body.
renderWidget :: Widget -> YesodExample App Text
renderWidget w =
  runHandler $ do
    page <- widgetToPageContent w
    body <- withUrlRenderer $ pageBody page
    return . toStrict $ renderHtml body

-- | Checks that the rendered widget contains all the given texts.
shouldContainAll :: Text -> [Text] -> YesodExample App ()
shouldContainAll html = liftIO . mapM_ (\t -> html `shouldSatisfy` isInfixOf t)

-- | Checks that the rendered widget contains none of the given texts.
shouldContainNone :: Text -> [Text] -> YesodExample App ()
shouldContainNone html = liftIO . mapM_ (\t -> html `shouldNotSatisfy` isInfixOf t)

spec :: Spec
spec = parallel $ withApp $ do
  describe "showExif" $ do
    it "shows missing information" $ do
      html <- renderWidget $ showExif def
      html `shouldContainAll`
        [ "unknown focal length", "f/?", "?s", "ISO unknown", "unrated"
        , "dimensions missing entirely", "<td>unknown</td>"
        , "<i>no title</i>", "<i>empty</i>"
        ]
    it "shows the exif information" $ do
      html <- renderWidget . showExif $
        def { exifFocalLength = Just 50, exifFL35mm = Just 75
            , exifAperture = Just 2.8, exifISO = Just 200
            , exifSSpeedDesc = Just (mkSym "1/100"), exifRating = Just 4
            , exifWidth = Just 6000, exifHeight = Just 4000
            , exifMegapixels = Just 24, exifShutterCount = Just 1234
            , exifTitle = Just (mkSym "A title")
            , exifPeople = Set.fromList [mkSym "Ann", mkSym "Bob"]
            , exifMake = Just (mkSym "Nikon"), exifLensMake = Just (mkSym "Sigma")
            , exifCreateDate = Just (exifDate 2024 3 5)
            , exifFlashInfo = FlashInfo (Just FlashSourceInternal) (Just (mkSym "auto"))
            , exifWarning = Set.fromList ["bad tag", "other"]
            }
      html `shouldContainAll`
        [ "50.0mm (75.0mm equiv.)", "1/100s @ f/2.8, ISO 200", "rated 4 stars"
        , "6000×4000", "24.0 MP", "1234", "A title", "Ann, Bob"
        , "Nikon / Sigma", "2024-03-05", "camera internal flash", "auto"
        ]
    it "shows the full frame focal length" $ do
      html <- renderWidget . showExif $ def { exifFocalLength = Just 35, exifFL35mm = Just 35 }
      html `shouldContainAll` ["35.0mm (FF)"]
      html' <- renderWidget . showExif $ def { exifFL35mm = Just 28 }
      html' `shouldContainAll` ["28.0mm (FF)"]
      html'' <- renderWidget . showExif $ def { exifFocalLength = Just 12 }
      html'' `shouldContainAll` ["12.0mm (unknown equiv.)"]
    it "shows partial dimensions and zero ratings" $ do
      html <- renderWidget . showExif $ def { exifWidth = Just 10, exifRating = Just 0 }
      html `shouldContainAll` ["10 wide, height missing", "unrated"]
      html' <- renderWidget . showExif $ def { exifHeight = Just 20, exifTitle = Just (mkSym "") }
      html' `shouldContainAll` ["width missing, 20 high", "<i>empty</i>"]
  describe "small widgets" $ do
    it "shows camera links" $ do
      html <- renderWidget $ showCameraLink (Just "Nikon")
      html `shouldContainAll` ["/stats/camera/Nikon", ">Nikon<"]
      html' <- renderWidget $ showCameraLink Nothing
      html' `shouldContainAll` ["unknown"]
    it "shows date ranges" $ do
      let day d = LocalTime (fromGregorian 2024 3 d) midnight
      none <- renderWidget $ showDateRange "folder" Nothing
      none `shouldContainAll` ["No date information."]
      one <- renderWidget $ showDateRange "folder" (Just (day 5, day 5))
      one `shouldContainAll` ["This folder contains pictures taken on", "Tuesday, March 05 2024"]
      range <- renderWidget $ showDateRange "event" (Just (day 1, day 5))
      range `shouldContainAll`
        [ "This event contains pictures taken between", "Friday, March 01 2024"
        , "Tuesday, March 05 2024", "(5 days)" ]
    it "shows lists with a header" $ do
      empty' <- renderWidget $ showListWithHeader "Warnings" []
      empty' `shouldContainAll` ["Warnings", "<i>none"]
      full <- renderWidget $ showListWithHeader "Warnings" ["a", "b"]
      full `shouldContainAll` ["colspan=\"1\"", "<td>a</td>", "<td>b</td>"]
    it "explains empty searches" $ do
      html <- renderWidget $ noItemsFound "camera Foo" True
      html `shouldContainAll` ["Nothing found", "camera Foo", "any images"]
      html' <- renderWidget $ noItemsFound "camera Foo" False
      html' `shouldContainAll` ["any folders"]
      html'' <- renderWidget $ noImageSearchPossible "folder x"
      html'' `shouldContainAll` ["No image search possible", "folder x"]
    it "shows the actions for each media type" $ do
      config <- ctxConfig . appContext <$> getTestYesod
      let img = simpleUntrackedImage config "f" "a"
      image <- renderWidget $ actionsWidget [] img
      image `shouldContainAll` ["/view/f/a", "/image/bytes/f/a", "download=\"a\""]
      movie <- renderWidget $ actionsWidget [] img { imgType = MediaMovie }
      movie `shouldContainAll` ["/view/f/a", "/image/movie/f/a", "Play movie"]
      unknown' <- renderWidget $ actionsWidget [] img { imgType = MediaUnknown }
      unknown' `shouldContainAll` ["No actions for files of unknown type"]
  describe "folder covers" $ do
    it "shows a folder without viewable images" $ do
      let dir = createTestPicDir "empty-folder"
      browsing <- renderWidget $ folderCover 100 True [] (All []) dir
      browsing `shouldContainAll` ["Folder 'empty-folder' contains no viewable images"]
      listing <- renderWidget $ folderCover 100 False [] (All []) dir
      listing `shouldContainAll` ["N/A"]
    it "shows a hidden component only for a search about the files" $ do
      config <- ctxConfig . appContext <$> getTestYesod
      -- d_01 comes first by name, but is hidden by d_01-03.
      let dir = compositeFolder config [[("d_01.nef", 10), ("d_02.nef", 10)]]
                  [[("d_01-03.jpg", 50)]]
      cover <- renderWidget $ folderCover 100 True [] (All []) dir
      cover `shouldContainAll` ["/f/d_01-03"]
      cover `shouldContainNone` ["/f/d_01?"]
      -- A search that finds only a hidden component shows its
      -- composite image, even if another image comes first.
      let dir' = compositeFolderWith (Map.fromList [("d_02.nef", Right def { exifISO = Just 200 })])
                   config [[("d_01.nef", 10), ("d_02.nef", 10)]]
                   [[("a.jpg", 5), ("d_01-03.jpg", 50)]]
      found <- renderWidget $ folderCover 100 True [] (Iso (OpEq 200)) dir'
      found `shouldContainAll` ["/f/d_01-03"]
      found `shouldContainNone` ["/f/a?", "/f/d_02?"]
      -- But one about the files shows the hidden component itself.
      named <- renderWidget $ folderCover 100 True [] (FileName (OpEqual "d_02")) dir'
      named `shouldContainAll` ["/f/d_02?"]
      named `shouldContainNone` ["/f/a?", "/f/d_01-03"]
  describe "imageSrcSet" $
    it "lists the sizes from the given minimum" $ do
      config <- ctxConfig . appContext <$> getTestYesod
      let render :: Route App -> [(Text, Text)] -> Text
          render r ps = tshow r <> tshow ps
          sizes = Set.toList (cfgAutoImageSizes config)
          srcset = imageSrcSet config render "f" "a" 0
      liftIO $ do
        srcset `shouldSatisfy` isInfixOf (render (ImageBytesR "f" "a") [])
        forM_ sizes $ \size -> srcset `shouldSatisfy` isInfixOf (tshow size <> "w")
        imageSrcSet config render "f" "a" (maybe 0 (+ 1) (lastMay sizes))
          `shouldBe` render (ImageBytesR "f" "a") []
  describe "imageFlagActions" $
    forM_ [True, False] $ \flag -> do
      let render = runHandler $ do
            urlRender <- getUrlRender
            page <- widgetToPageContent $ imageFlagActions "a" "b" flag
            body <- withUrlRenderer $ pageBody page
            return (urlRender $ ImageFlagR "a" "b", toStrict $ renderHtml body)
      it ("points the buttons to the flag URL, flag=" ++ show flag) $ do
        (url, html) <- render
        liftIO $ html `shouldSatisfy`
          isInfixOf ("data-flag-url=\"" <> url <> "\"")
      it ("has the right buttons, flag=" ++ show flag) $ do
        (_, html) <- render
        liftIO $ do
          html `shouldSatisfy` isInfixOf "data-flag=\"false\""
          isInfixOf "data-flag=\"true\"" html `shouldBe` flag
      it ("is not a form, flag=" ++ show flag) $ do
        (_, html) <- render
        liftIO $ do
          html `shouldNotSatisfy` isInfixOf "<form"
          html `shouldNotSatisfy` isInfixOf "_method"
