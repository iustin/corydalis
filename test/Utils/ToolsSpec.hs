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

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils.ToolsSpec (spec) where

import           Control.Concurrent.STM.TSem (signalTSem, waitTSem)
import           GHC.Conc                    (BlockReason (..),
                                              ThreadStatus (..), threadStatus,
                                              yield)
import           System.Exit                 (ExitCode (..))
import qualified System.IO
import           System.Posix.Files          (createNamedPipe)
import           System.Posix.Signals        (nullSignal, sigKILL,
                                              signalProcess)
import           System.Posix.Types          (CPid (..))
import           System.Process.Typed        (ProcessConfig, StreamSpec,
                                              StreamType (..),
                                              mkPipeStreamSpec, proc, setStdout)

import           TestImport
import           Utils.Tools

-- | Runs a tool in a slot of the given limits.
runIn :: ToolLimits -> ProcessConfig stdin stdout stderr
      -> IO (Either Text (ExitCode, LByteString, LByteString))
runIn tools pconfig = withToolSlot tools $ \slot -> runTool slot pconfig

-- | Waits until the given process has exited and been reaped.
waitUntilGone :: CPid -> IO ()
waitUntilGone pid =
  timeout lastResort loop >>=
    maybe (expectationFailure "The process never exited") return
  where loop =
          tryIO (signalProcess nullSignal pid) >>= \case
            Left _  -> return ()
            Right _ -> yield >> loop

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
      withAsync run $ \waiting -> do
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
    it "stops a tool cancelled while starting" $ do
      tools <- atomically $ newToolLimits 1 defaultRenderToolTimeout
      let marker = "86417"
      inStart <- newEmptyMVar
      cancelQueued <- newEmptyMVar
      -- Runs inside startProcess, after the child has been forked and
      -- while the thread is still masked, so a cancellation queued
      -- here fires as soon as bracket unmasks for the body.
      let hook :: StreamSpec 'STOutput ()
          hook = mkPipeStreamSpec $ \_ h -> do
            uninterruptibleMask_ $ do
              putMVar inStart ()
              takeMVar cancelQueued
            return ((), System.IO.hClose h)
      withSystemTempDirectory "corydalis-tools-test" $ \dir -> do
        let ready = dir </> "ready"
        createNamedPipe ready 0o600
        withBinaryFile ready ReadWriteMode $ \h ->
          -- The tool ignores SIGTERM so that skipping our SIGKILL
          -- (falling back to typed-process's stopProcess) hangs even
          -- if hClose wins the race with the stderr reader thread.
          withAsync
            (withToolSlot tools $ \slot ->
               runToolWith slot (const $ return ()) $
                 setStdout hook $
                   proc "sh" [ "-c"
                             , "trap '' TERM; echo ready > \"$0\"; exec sleep " ++ marker
                             , ready
                             ]) $
            \running -> do
              (do takeMVar inStart
                  timeout lastResort (System.IO.hGetLine h) `shouldReturn` Just "ready"
                  withAsync (cancel running) $ \canceller -> do
                    waitUntilBlocked BlockedOnException (asyncThreadId canceller)
                    putMVar cancelQueued ()
                    timeout (5 * 1000000) (wait canceller) `shouldReturn` Just ())
                `finally` void (tryPutMVar cancelQueued ())
              waitCatch running >>= \case
                Left e  -> (fromException e :: Maybe AsyncCancelled) `shouldSatisfy` isJust
                Right r -> expectationFailure $ "Expected cancellation, got: " ++ show r
      found <- readToolProcess (proc "pgrep" ["-x", "-f", "sleep " ++ marker])
      fmap (\(code, _, _) -> code) found `shouldBe` Right (ExitFailure 1)
      takeFreeSlots tools `shouldReturn` 1
    it "stays interruptible if a background child keeps stderr open" $ do
      tools <- atomically $ newToolLimits 1 defaultRenderToolTimeout
      withSystemTempDirectory "corydalis-tools-test" $ \dir -> do
        let ready = dir </> "ready"
        createNamedPipe ready 0o600
        withBinaryFile ready ReadWriteMode $ \h ->
          -- The tool writes enough to stderr to exceed the pipe buffer
          -- so the stderr reader thread is running, leaves a child
          -- holding stderr open, and exits cleanly.
          withAsync
            (withToolSlot tools $ \slot ->
               runToolWith slot (const $ return ()) $
                 proc "sh" [ "-c"
                           , "sleep 86418 & head -c 70000 /dev/zero >&2; echo \"$$ $!\" > \"$0\"; exit 0"
                           , ready
                           ]) $
            \running -> do
              line <- timeout lastResort (System.IO.hGetLine h)
              (shPid, bgPid) <- case words <$> line of
                Just [s, b]
                  | Just s' <- readMay s
                  , Just b' <- readMay b -> return (CPid s', CPid b')
                other -> expectationFailure ("Unexpected PIDs: " ++ show other)
                         >> error "unreachable"
              (do -- Once the shell is reaped, getPid is Nothing during
                  -- cleanup and stopProcess's hClose blocks on the child.
                  waitUntilGone shPid
                  waitUntilBlockedInSTM (asyncThreadId running)
                  withAsync (cancel running) $ \c1 -> do
                    let waitCleanup = do
                          st <- threadStatus (asyncThreadId running)
                          case st of
                            ThreadBlocked BlockedOnMVar -> return ()
                            ThreadFinished              -> return ()
                            ThreadDied                  -> return ()
                            _                           -> yield >> waitCleanup
                    timeout lastResort waitCleanup `shouldReturn` Just ()
                    -- A second cancellation must interrupt stopProcess's
                    -- hClose (which would block under UnliftIO's
                    -- uninterruptibleMask_).
                    withAsync (cancel running) $ \c2 ->
                      timeout (5 * 1000000) (wait c2) `shouldReturn` Just ()
                    timeout (5 * 1000000) (wait c1) `shouldReturn` Just ())
                `finally` void (tryIO $ signalProcess sigKILL bgPid)
      takeFreeSlots tools `shouldReturn` 1
  describe "withToolSlot" $ do
    it "frees the slot when the action fails" $ do
      tools <- atomically $ newToolLimits 1 defaultRenderToolTimeout
      withToolSlot tools (\_ -> throwIO (userError "failed"))
        `shouldThrow` anyIOException
      -- Taking it doesn't wait (and would fail, not hang, otherwise).
      atomically ((waitTSem (tlSlots tools) >> return True) `orElse` return False)
        `shouldReturn` True
