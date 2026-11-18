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
import           Network.Wai                          (defaultRequest,
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

-- | Returns what the request logger logs for a login request.
logOfLogin :: RequestLogging -> IO ByteString
logOfLogin logging = do
  logged <- newIORef []
  let dest = Callback $ \str -> modifyIORef logged (fromLogStr str:)
  logware <- mkRequestLogger $ requestLoggerSettings False logging dest
  let app _ respond = respond $ responseLBS status200 [] "ok"
      req = defaultRequest
              { requestMethod = "POST"
              , requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
              }
  _ <- runSession (srequest $ SRequest req "username=alice&password=secret")
                  (logware app)
  concat . reverse <$> readIORef logged

spec :: Spec
spec = do
 describe "request logging" $ do
  it "doesn't log the password in the detailed format" $ do
    logged <- logOfLogin RequestLoggingDetailed
    logged `shouldSatisfy` ("alice" `isInfixOf`)
    logged `shouldSatisfy` ("(hidden)" `isInfixOf`)
    logged `shouldNotSatisfy` ("secret" `isInfixOf`)
  it "doesn't log the parameters in the Apache format" $ do
    logged <- logOfLogin RequestLoggingApache
    logged `shouldSatisfy` ("POST" `isInfixOf`)
    logged `shouldNotSatisfy` ("secret" `isInfixOf`)
  it "logs nothing when disabled" $
    logOfLogin RequestLoggingDisabled `shouldReturn` ""
 describe "redactPassword" $ do
  it "hides the password" $
    redactPassword ("password", "secret") `shouldBe` Just ("password", "(hidden)")
  it "keeps the other parameters" $ do
    redactPassword ("username", "alice") `shouldBe` Just ("username", "alice")
    redactPassword ("_token", "abc") `shouldBe` Just ("_token", "abc")
