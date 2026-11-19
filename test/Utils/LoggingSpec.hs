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

module Utils.LoggingSpec (spec) where

import qualified Data.Text             as Text
import           Data.Text.Arbitrary   ()
import           System.Log.FastLogger (LogStr, fromLogStr, toLogStr)
import           Test.QuickCheck       (property)

import           TestImport
import           Utils.Logging

-- | The bytes of a log message.
logged :: LogStr -> ByteString
logged = fromLogStr

spec :: Spec
spec = do
  describe "sanitizeLogText" $ do
    it "keeps printable text, including non-ASCII letters and symbols" $
      forM_ [ "plain text: /pics/2020 (1).jpg"
            , "Bäume über Größe, çà, ñ, ø, ő, ł"
            , "Ελληνικά, кириллица, 日本語, 📷"
            ] $ \t -> sanitizeLogText t `shouldBe` t
    it "escapes newlines, tabs and other control characters" $ do
      sanitizeLogText "a\nb" `shouldBe` "a\\x0ab"
      sanitizeLogText "a\r\nb\tc" `shouldBe` "a\\x0d\\x0ab\\x09c"
      sanitizeLogText "\0" `shouldBe` "\\x00"
      sanitizeLogText "\DEL" `shouldBe` "\\x7f"
    it "escapes terminal escape sequences" $
      sanitizeLogText "\ESC[31mred\ESC[0m" `shouldBe` "\\x1b[31mred\\x1b[0m"
    it "escapes the C1 controls, including the next line one" $ do
      sanitizeLogText "a\x85\&b" `shouldBe` "a\\x85b"
      sanitizeLogText "\x9b\&31m" `shouldBe` "\\x9b31m"
    it "escapes the line and paragraph separators" $
      sanitizeLogText "a\x2028\&b\x2029" `shouldBe` "a\\u{2028}b\\u{2029}"
    it "escapes the bidirectional text controls" $
      sanitizeLogText "a\x202e\&b\x2066\&c" `shouldBe` "a\\u{202e}b\\u{2066}c"
    it "escapes invisible formatting characters" $ do
      sanitizeLogText "a\x200b\&b" `shouldBe` "a\\u{200b}b"
      sanitizeLogText "\xfeff" `shouldBe` "\\u{feff}"
      sanitizeLogText "\xad" `shouldBe` "\\xad"
    it "escapes spaces other than the ASCII one" $ do
      sanitizeLogText "a b" `shouldBe` "a b"
      sanitizeLogText "a\xa0\&b" `shouldBe` "a\\xa0b"
      sanitizeLogText "a\x3000\&b" `shouldBe` "a\\u{3000}b"
    it "escapes private use and unassigned characters" $ do
      sanitizeLogText "\xe000" `shouldBe` "\\u{e000}"
      sanitizeLogText "\x10ffff" `shouldBe` "\\u{10ffff}"
    it "escapes the backslash, so that escapes are unambiguous" $
      sanitizeLogText "a\\x0ab" `shouldBe` "a\\x5cx0ab"
    it "leaves no character that needs escaping, except escapes' backslashes" $
      property $ \t ->
        let escaped = sanitizeLogText t
        in not (Text.any (\c -> c /= '\\' && needsLogEscape c) escaped)
           && Text.count "\\" escaped == Text.count "\\x" escaped
                                          + Text.count "\\u{" escaped
  describe "mayNeedLogEscape" $ do
    it "matches needsLogEscape for ASCII bytes" $
      forM_ [0..0x7f :: Word8] $ \b ->
        (b, mayNeedLogEscape b) `shouldBe` (b, needsLogEscape (toEnum (fromIntegral b)))
    it "holds for all the other bytes" $
      filter (not . mayNeedLogEscape) [0x80..0xff] `shouldBe` []
  describe "sanitizeLogStr" $ do
    it "escapes a message" $
      logged (sanitizeLogStr "a\nb") `shouldBe` "a\\x0ab"
    it "keeps UTF-8 text" $
      logged (sanitizeLogStr (toLogStr ("Größe" :: Text)))
        `shouldBe` encodeUtf8 "Größe"
    it "replaces invalid UTF-8" $
      logged (sanitizeLogStr (toLogStr ("a\xff\&b" :: ByteString)))
        `shouldBe` encodeUtf8 "a\xfffd\&b"
  describe "sanitizeLogLines" $
    it "keeps the newlines, and escapes each line" $
      logged (sanitizeLogLines "GET /a\ESC[2J\n  Accept: \x202e\n")
        `shouldBe` "GET /a\\x1b[2J\n  Accept: \\u{202e}\n"
