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
{-# LANGUAGE ViewPatterns      #-}

module CopyrightSpec (spec) where

import qualified Data.Text        as T
import           System.Directory (doesDirectoryExist, listDirectory)
import           System.FilePath  (takeExtension)

import           TestImport

-- | The source directories, and the extension of their source files.
sourceDirs :: [(FilePath, String)]
sourceDirs =
  [ ("src",  ".hs")
  , ("test", ".hs")
  , ("app",  ".hs")
  , ("js",   ".ts")
  ]

-- | The lines every source file must start with (after the opening of the
-- comment), see AGENTS.md.
headerLines :: [Text]
headerLines =
  [ "Copyright (C) 2013 Iustin Pop"
  , ""
  , "This program is free software: you can redistribute it and/or modify"
  , "it under the terms of the GNU Affero General Public License as published by"
  ]

-- | Returns all files with the given extension under a directory.
findFiles :: String -> FilePath -> IO [FilePath]
findFiles ext dir = do
  entries <- sort <$> listDirectory dir
  concat <$> forM entries (\entry -> do
    let path = dir </> entry
    isDir <- doesDirectoryExist path
    if isDir
      then findFiles ext path
      else return [path | takeExtension path == ext])

-- | Whether a file starts with the copyright header: its first line opens
-- a comment (@{-@ or @/*@), and the notice follows, as in the existing
-- files (Haskell has the notice on the line after an empty one, TypeScript
-- on the opening line itself).
hasHeader :: Text -> Bool
hasHeader contents =
  case T.lines contents of
    "{-" : "" : rest       -> headerLines `isPrefixOf` rest
    (T.stripPrefix "/* " -> Just firstLine) : rest ->
      headerLines `isPrefixOf` (firstLine : rest)
    _                      -> False

spec :: Spec
spec = describe "source files" $ do
  it "all start with the copyright header" $ do
    files <- concat <$> mapM (uncurry $ flip findFiles) sourceDirs
    missing <- filterM (fmap (not . hasHeader) . readFileUtf8) files
    missing `shouldBe` []
  describe "hasHeader" $ do
    let notice = T.unlines headerLines
    it "accepts the Haskell header" $
      hasHeader ("{-\n\n" <> notice <> "\n-}\n") `shouldBe` True
    it "accepts the TypeScript header" $
      hasHeader ("/* " <> notice <> "\n*/\n") `shouldBe` True
    it "rejects files without a header" $
      hasHeader "module Foo where\n" `shouldBe` False
    it "rejects a header that is not at the start" $
      hasHeader ("module Foo where\n{-\n\n" <> notice <> "-}\n")
        `shouldBe` False
    it "rejects another copyright year" $
      hasHeader ("{-\n\n" <> T.replace "2013" "2026" notice <> "-}\n")
        `shouldBe` False
