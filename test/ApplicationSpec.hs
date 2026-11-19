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

module ApplicationSpec (spec) where

import           Network.HTTP.Types                   (status200)
import qualified Data.ByteString                      as BS
import           Network.Wai                          (Request,
                                                       defaultRequest,
                                                       rawPathInfo,
                                                       requestHeaderUserAgent,
                                                       requestHeaders,
                                                       requestMethod,
                                                       responseLBS)
import           Network.Wai.Middleware.RequestLogger (Destination (Callback),
                                                       mkRequestLogger)
import           Network.Wai.Test                     (SRequest (..),
                                                       runSession, srequest)
import           System.Log.FastLogger                (fromLogStr)

import           Application                          (redactPassword,
                                                       requestLoggerSettings)
import           Settings                             (RequestLogging (..))
import           TestImport

-- | Returns what the request logger logs for a request.
logOfRequest :: Bool -> RequestLogging -> Request -> LByteString
             -> IO ByteString
logOfRequest ipFromHeader logging req body = do
  logged <- newIORef []
  let dest = Callback $ \str -> modifyIORef logged (fromLogStr str:)
  logware <- mkRequestLogger $ requestLoggerSettings ipFromHeader logging dest
  let app _ respond = respond $ responseLBS status200 [] "ok"
  _ <- runSession (srequest $ SRequest req body) (logware app)
  concat . reverse <$> readIORef logged

-- | Returns what the request logger logs for a login request, coming
-- through a proxy (as given by a header).
logOfLogin :: Bool -> RequestLogging -> IO ByteString
logOfLogin ipFromHeader logging =
  logOfRequest ipFromHeader logging req "username=alice&password=secret"
  where req = defaultRequest
                { requestMethod = "POST"
                , requestHeaders = [ ("Content-Type", "application/x-www-form-urlencoded")
                                   , ("X-Forwarded-For", "192.0.2.7")
                                   ]
                }

-- | A request with terminal sequences, newlines and quotes, in its path
-- and in a header.
hostileRequest :: Request
hostileRequest = defaultRequest
  { rawPathInfo = "/a\ESC[2J"
  , requestHeaderUserAgent = Just "agent\"\ESC[31m\nforged line"
  }

spec :: Spec
spec = do
 describe "request logging" $ do
  it "doesn't log the password in the detailed format" $ do
    logged <- logOfLogin False RequestLoggingDetailed
    logged `shouldSatisfy` ("alice" `isInfixOf`)
    logged `shouldSatisfy` ("(hidden)" `isInfixOf`)
    logged `shouldNotSatisfy` ("secret" `isInfixOf`)
  it "doesn't log the parameters in the Apache format" $ do
    logged <- logOfLogin False RequestLoggingApache
    logged `shouldSatisfy` ("POST" `isInfixOf`)
    logged `shouldNotSatisfy` ("secret" `isInfixOf`)
  it "logs the client address from the headers only if configured" $ do
    direct <- logOfLogin False RequestLoggingApache
    direct `shouldNotSatisfy` ("192.0.2.7" `isInfixOf`)
    proxied <- logOfLogin True RequestLoggingApache
    proxied `shouldSatisfy` ("192.0.2.7" `isInfixOf`)
  it "logs nothing when disabled" $
    logOfLogin False RequestLoggingDisabled `shouldReturn` ""
  it "escapes the fields of the Apache lines, as Apache does" $ do
    logged <- logOfRequest False RequestLoggingApache hostileRequest ""
    logged `shouldSatisfy` ("/a\\x1b[2J" `isInfixOf`)
    logged `shouldSatisfy` ("\"agent\\\"\\x1b[31m\\nforged line\"" `isInfixOf`)
    logged `shouldNotSatisfy` BS.elem 0x1b
    BS.count 0x0a logged `shouldBe` 1
  it "logs only the first address of the header" $ do
    let req = defaultRequest
                { requestHeaders = [("X-Forwarded-For", " 192.0.2.7 - forged, 10.0.0.1")] }
    logged <- logOfRequest True RequestLoggingApache req ""
    logged `shouldSatisfy` ("192.0.2.7 - - [" `BS.isPrefixOf`)
    logged `shouldNotSatisfy` ("forged" `isInfixOf`)
    logged `shouldNotSatisfy` ("10.0.0.1" `isInfixOf`)
 describe "redactPassword" $ do
  it "hides the password" $
    redactPassword ("password", "secret") `shouldBe` Just ("password", "(hidden)")
  it "keeps the other parameters" $ do
    redactPassword ("username", "alice") `shouldBe` Just ("username", "alice")
    redactPassword ("_token", "abc") `shouldBe` Just ("_token", "abc")
