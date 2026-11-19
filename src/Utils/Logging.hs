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

-- | Escaping of logged text.
--
-- Log messages contain text from outside the application: file names
-- (from the picture folders), the output of external tools, and parts
-- of requests (paths, headers). Such text could contain newlines, which
-- would make a single message look like several log lines (possibly
-- forged ones), terminal escape sequences, which could act on the
-- terminal of whoever reads the logs, or invisible or bidirectional
-- control characters, which can make a line display differently from
-- what it contains. These are therefore escaped, in all logged text.
module Utils.Logging ( sanitizeLogText
                     , sanitizeLogStr
                     , sanitizeLogLines
                     , needsLogEscape
                     , mayNeedLogEscape
                     ) where

import qualified Data.ByteString          as BS
import           Data.Char                (GeneralCategory (..),
                                           generalCategory, ord)
import qualified Data.Text                as Text
import           Data.Text.Encoding       (decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import           Numeric                  (showHex)
import           System.Log.FastLogger    (LogStr, fromLogStr, toLogStr)

import           Import.NoFoundation

-- | Whether a character is escaped in logged text.
--
-- These are: the backslash (which starts the escapes, so that these are
-- unambiguous); control characters (including newlines, tabs, the
-- escape character of terminal sequences, and the C1 controls); the
-- line and paragraph separators; invisible formatting characters (e.g.
-- zero-width spaces and joiners, and the bidirectional text controls);
-- spaces other than the ASCII one; and private use, surrogate and
-- unassigned code points. Other characters, e.g. accented letters, are
-- kept as they are.
needsLogEscape :: Char -> Bool
needsLogEscape c
  | c == '\\' = True
  | c == ' ' = False
  | otherwise = case generalCategory c of
      Control            -> True
      Format             -> True
      LineSeparator      -> True
      ParagraphSeparator -> True
      Space              -> True
      PrivateUse         -> True
      Surrogate          -> True
      NotAssigned        -> True
      _                  -> False

-- | Whether a byte of UTF-8 text may be part of a character that needs
-- escaping (see 'needsLogEscape'), to skip decoding most messages: an
-- ASCII byte does exactly when its character does, while the others,
-- of characters beyond ASCII, always may.
mayNeedLogEscape :: Word8 -> Bool
mayNeedLogEscape b = b < 0x20 || b >= 0x7f || b == 0x5c

-- | Escapes a character, as @\\xHH@ (two hex digits) for those below
-- U+0100, and as @\\u{HHHH}@ for the others.
escapeChar :: Char -> Text
escapeChar c
  | n < 0x100 = "\\x" <> Text.justifyRight 2 '0' digits
  | otherwise = "\\u{" <> Text.justifyRight 4 '0' digits <> "}"
  where n = ord c
        digits = Text.pack $ showHex n ""

-- | Escapes the characters that shouldn't be logged as they are (see
-- 'needsLogEscape').
sanitizeLogText :: Text -> Text
sanitizeLogText t
  | Text.any needsLogEscape t = Text.concatMap escape t
  | otherwise = t
  where escape c = if needsLogEscape c then escapeChar c else Text.singleton c

-- | Escapes a log message (see 'sanitizeLogText'), which must be a
-- single line: newlines in it are escaped too.
--
-- Invalid UTF-8 (e.g. from a file name) is replaced by U+FFFD.
sanitizeLogStr :: LogStr -> LogStr
sanitizeLogStr (fromLogStr -> raw)
  | BS.any mayNeedLogEscape raw =
      toLogStr . sanitizeLogText . decodeUtf8With lenientDecode $ raw
  | otherwise = toLogStr raw

-- | Escapes each line of a log text (see 'sanitizeLogText'), keeping
-- the newlines between them; for log entries spanning several lines.
sanitizeLogLines :: LogStr -> LogStr
sanitizeLogLines =
  toLogStr . Text.intercalate "\n" . map sanitizeLogText . Text.splitOn "\n"
  . decodeUtf8With lenientDecode . fromLogStr
