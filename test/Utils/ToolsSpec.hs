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

module Utils.ToolsSpec (spec) where

import           Control.Concurrent.STM.TSem (signalTSem, waitTSem)
import           System.Exit                 (ExitCode (..))
import           System.Process.Typed        (ProcessConfig, proc)

import           TestImport
import           Utils.Tools

-- | Runs a tool in a slot of the given limits.
runIn :: ToolLimits -> ProcessConfig stdin stdout stderr
      -> IO (Either Text (ExitCode, LByteString, LByteString))
runIn tools pconfig = withToolSlot tools $ \slot -> runTool slot pconfig

spec :: Spec
spec = do
  describe "external tools" $ do
    it "returns an error if a tool cannot be run" $ do
      result <- readToolProcess (proc "corydalis-no-such-tool" [])
      -- The error names the tool, to make the failure easy to diagnose.
      result `shouldSatisfy` either ("corydalis-no-such-tool" `isInfixOf`) (const False)
    it "decodes invalid UTF-8 in tool output without failing" $ do
      -- 0xff is never valid in UTF-8; forcing the text must not throw.
      text <- evaluate . force $ decodeToolOutput "bad \xff name: no such file"
      text `shouldBe` "bad \xfffd name: no such file"
    it "returns the exit code and output of a tool" $ do
      result <- readToolProcess (proc "sh" ["-c", "echo out; echo err >&2; exit 3"])
      result `shouldBe` Right (ExitFailure 3, "out\n", "err\n")
  describe "runTool" $ do
    it "returns the result of a tool within the time limit" $ do
      tools <- testToolLimits
      result <- runIn tools (proc "sh" ["-c", "echo out; exit 2"])
      result `shouldBe` Right (ExitFailure 2, "out\n", "")
    it "stops a tool running for too long" $ do
      tools <- atomically $ newToolLimits 1 200000
      -- A distinctive argument, to find the process afterwards.
      let marker = "86413"
      result <- runIn tools (proc "sleep" [marker])
      result `shouldBeLeftWithMessage` "timed out after 1 second"
      -- The tool has been stopped (pgrep finds no such process).
      found <- readToolProcess (proc "pgrep" ["-x", "-f", "sleep " ++ marker])
      fmap (\(code, _, _) -> code) found `shouldBe` Right (ExitFailure 1)
    it "gives what a stopped tool wrote to its error output" $ do
      -- The tool writes it right away, well within the time limit.
      tools <- atomically $ newToolLimits 1 1000000
      result <- runIn tools $
                  proc "sh" ["-c", "echo reading photo.jpg >&2; exec sleep 86416"]
      result `shouldBeLeftWithMessage` "timed out"
      result `shouldBeLeftWithMessage` "reading photo.jpg"
    it "stops the processes started by a tool" $ do
      tools <- atomically $ newToolLimits 1 200000
      let marker = "86415"
      -- The shell starts sleep, which keeps the output pipes open: if
      -- only the shell were killed, the run would wait for sleep to
      -- finish (and couldn't be interrupted, even by a timeout here).
      result <- runIn tools $
                  proc "sh" ["-c", "sleep " ++ marker ++ " & wait"]
      result `shouldBeLeftWithMessage` "timed out"
      found <- readToolProcess (proc "pgrep" ["-x", "-f", "sleep " ++ marker])
      fmap (\(code, _, _) -> code) found `shouldBe` Right (ExitFailure 1)
    it "allows at least one tool to run" $ do
      tools <- atomically $ newToolLimits 0 defaultRenderToolTimeout
      runIn tools (proc "true" []) >>= (`shouldBe` Right (ExitSuccess, "", ""))
    it "waits for a free slot" $ do
      tools <- atomically $ newToolLimits 1 defaultRenderToolTimeout
      let run = runIn tools (proc "true" [])
      -- Taking the only slot, as a running tool would.
      atomically . waitTSem $ tlSlots tools
      -- The run waits for the slot; cancelled while waiting, it
      -- doesn't keep one.
      waiting <- async run
      waitUntilBlockedInSTM (asyncThreadId waiting)
      cancel waiting
      atomically . signalTSem $ tlSlots tools
      run >>= (`shouldBe` Right (ExitSuccess, "", ""))
      -- The slot is free again, even after the cancelled run.
      takeFreeSlots tools `shouldReturn` 1
    it "stops a tool when interrupted" $ do
      tools <- testToolLimits
      let marker = "86414"
      -- E.g. as when the request is cancelled; this would wait for
      -- the tool to finish by itself if it weren't stopped.
      result <- timeout 200000 $ runIn tools (proc "sleep" [marker])
      result `shouldBe` Nothing
      found <- readToolProcess (proc "pgrep" ["-x", "-f", "sleep " ++ marker])
      fmap (\(code, _, _) -> code) found `shouldBe` Right (ExitFailure 1)
  describe "withToolSlot" $ do
    it "frees the slot when the action fails" $ do
      tools <- atomically $ newToolLimits 1 defaultRenderToolTimeout
      withToolSlot tools (\_ -> throwIO (userError "failed"))
        `shouldThrow` anyIOException
      -- Taking it doesn't wait (and would fail, not hang, otherwise).
      atomically ((waitTSem (tlSlots tools) >> return True) `orElse` return False)
        `shouldReturn` True
