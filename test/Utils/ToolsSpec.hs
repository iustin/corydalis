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
import           System.Process.Typed        (proc)

import           TestImport
import           Utils.Tools

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
      result <- runTool tools (proc "sh" ["-c", "echo out; exit 2"])
      result `shouldBe` Right (ExitFailure 2, "out\n", "")
    it "stops a tool running for too long" $ do
      tools <- atomically $ newToolLimits 1 200000
      -- A distinctive argument, to find the process afterwards.
      let marker = "86413"
      result <- runTool tools (proc "sleep" [marker])
      result `shouldBeLeftWithMessage` "timed out after 1 second"
      -- The tool has been stopped (pgrep finds no such process).
      found <- readToolProcess (proc "pgrep" ["-x", "-f", "sleep " ++ marker])
      fmap (\(code, _, _) -> code) found `shouldBe` Right (ExitFailure 1)
    it "stops the processes started by a tool" $ do
      tools <- atomically $ newToolLimits 1 200000
      let marker = "86415"
      -- The shell starts sleep, which keeps the output pipes open: if
      -- only the shell were killed, the run would wait for sleep to
      -- finish (and couldn't be interrupted, even by a timeout here).
      result <- runTool tools $
                  proc "sh" ["-c", "sleep " ++ marker ++ " & wait"]
      result `shouldBeLeftWithMessage` "timed out"
      found <- readToolProcess (proc "pgrep" ["-x", "-f", "sleep " ++ marker])
      fmap (\(code, _, _) -> code) found `shouldBe` Right (ExitFailure 1)
    it "allows at least one tool to run" $ do
      tools <- atomically $ newToolLimits 0 defaultToolTimeout
      runTool tools (proc "true" []) >>= (`shouldBe` Right (ExitSuccess, "", ""))
    it "waits for a free slot" $ do
      tools <- atomically $ newToolLimits 1 defaultToolTimeout
      let run = runTool tools (proc "true" [])
      -- Taking the only slot, as a running tool would.
      atomically . waitTSem $ tlSlots tools
      -- The run can't start, and doesn't complete.
      timeout 200000 run >>= (`shouldBe` Nothing)
      atomically . signalTSem $ tlSlots tools
      run >>= (`shouldBe` Right (ExitSuccess, "", ""))
      -- The slot is free again, even after the interrupted run.
      run >>= (`shouldBe` Right (ExitSuccess, "", ""))
    it "stops a tool when interrupted" $ do
      tools <- testToolLimits
      let marker = "86414"
      -- E.g. as when the request is cancelled; this would wait for
      -- the tool to finish by itself if it weren't stopped.
      result <- timeout 200000 $ runTool tools (proc "sleep" [marker])
      result `shouldBe` Nothing
      found <- readToolProcess (proc "pgrep" ["-x", "-f", "sleep " ++ marker])
      fmap (\(code, _, _) -> code) found `shouldBe` Right (ExitFailure 1)
