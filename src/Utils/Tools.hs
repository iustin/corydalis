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

-- | Running the external tools (ImageMagick, ffmpeg and exiftool),
-- within limits.
module Utils.Tools ( readToolProcess
                   , runTool
                   , runToolToFile
                   , runToolWith
                   , decodeToolOutput
                   ) where

import           Control.Concurrent.STM.TSem (signalTSem, waitTSem)
import qualified Data.Text.Lazy          as TextL
import           Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Lazy.Encoding as Text (decodeUtf8With)
import qualified System.IO
import           System.Posix.Signals    (sigKILL, signalProcessGroup)
import           System.Process.Typed
import           UnliftIO.Exception

import           Import.NoFoundation     hiding (path)

-- | Runs an external tool, returning its exit code and output.
--
-- Failing to run the tool at all, e.g. because it isn't installed, is
-- returned as an error instead of being thrown: a single image's preview
-- must not abort the whole repository scan.
readToolProcess :: ProcessConfig stdin stdout stderr
                -> IO (Either Text (ExitCode, LByteString, LByteString))
readToolProcess pconfig =
  first describe <$> try (readProcess pconfig)
  where describe :: IOException -> Text
        describe = sformat ("Failed to run external tool: " % shown)

-- | Runs an external tool for building a preview, as 'readToolProcess'
-- does, but within the given limits (see 'ToolLimits'): it waits for a
-- free slot, if as many tools as allowed are running; and a tool
-- running for too long is killed, and its run returned as an error.
--
-- The tool is also killed if the run is interrupted otherwise (e.g.
-- the request is cancelled). This is needed as typed-process, when the
-- run is interrupted, first closes the tool's output pipes, which waits
-- for their reader threads, which only finish when the tool exits; so
-- it would wait for the tool to finish by itself before stopping it.
-- SIGKILL is used, as e.g. ffmpeg only stops at a safe point on
-- SIGTERM; the callers have the tools write to temporary files, which
-- they remove on failure. The tool runs in its own process group,
-- which is killed as a whole: a process it started would otherwise
-- keep running, and keep the output pipes open, so the run would still
-- wait for it.
--
-- Both the kill on interruption and the wait in @stop@ below work around
-- typed-process bugs, fixed upstream by
-- <https://github.com/fpco/typed-process/pull/93> (the wait for the
-- output pipes, <https://github.com/fpco/typed-process/issues/84>) and
-- <https://github.com/fpco/typed-process/pull/70> ("No child
-- processes", <https://github.com/fpco/typed-process/issues/69>).
runTool :: ToolLimits -> ProcessConfig stdin stdout stderr
        -> IO (Either Text (ExitCode, LByteString, LByteString))
runTool limits = runToolWith limits id . setStdout byteStringOutput

-- | Runs an external tool as 'runTool' does, but with its output
-- written to the given file, instead of read in memory. Failing to open
-- the file is thrown, not returned.
runToolToFile :: ToolLimits -> FilePath -> ProcessConfig stdin stdout stderr
              -> IO (Either Text (ExitCode, LByteString))
runToolToFile limits path pconfig =
  System.IO.withBinaryFile path System.IO.WriteMode $ \h ->
    fmap (\(code, (), err) -> (code, err)) <$>
      runToolWith limits return (setStdout (useHandleOpen h) pconfig)

-- | Runs an external tool as 'runTool' does, but with its output going
-- where the process configuration says (a pipe, or a file); the given
-- function returns the result of the output stream, once the tool
-- exited (e.g. the output read from a pipe, or @()@ for a file).
runToolWith :: ToolLimits -> (stdout -> STM a)
            -> ProcessConfig stdin stdout stderr
            -> IO (Either Text (ExitCode, a, LByteString))
runToolWith limits output pconfig =
  bracket_ (atomically . waitTSem $ tlSlots limits)
           (atomically . signalTSem $ tlSlots limits) $
    either (Left . describe) id <$> try run
  where
    pconfig' = setCreateGroup True
               . setStderr byteStringOutput
               $ pconfig
    run = withProcessTerm pconfig' $ \p -> do
      -- After killing the tool, this waits for typed-process to record
      -- its exit; otherwise, its cleanup can interrupt the thread
      -- waiting for the tool right after the tool was reaped, but
      -- before its exit was recorded, and then fail waiting for it
      -- again ("No child processes"). The group's id is the tool's
      -- process id. Failing to send the signal (e.g. the tool has just
      -- exited, and been reaped, along with the rest of its group) is
      -- ignored.
      let stop = do
            mpid <- getPid p
            case mpid of
              Just pid -> void . tryIO $ signalProcessGroup sigKILL pid
              Nothing  -> return ()
            void . atomically $ waitExitCodeSTM p
      result <- timeout (tlTimeout limits)
                  (atomically $ (,,) <$> waitExitCodeSTM p
                                     <*> output (getStdout p)
                                     <*> getStderr p)
                `onException` stop
      case result of
        Just r  -> return $ Right r
        Nothing -> stop >> return timedOut
    timedOut = Left $ sformat ("External tool timed out after " % int % " second" % string)
                 secs (if secs == 1 then "" else "s")
    -- Rounded up, so that a (test) limit under a second isn't shown
    -- as zero.
    secs = (tlTimeout limits + 999999) `div` 1000000
    describe :: IOException -> Text
    describe = sformat ("Failed to run external tool: " % shown)

-- | Decodes the output of an external tool, for error messages.
--
-- The output can contain anything, e.g. file names which are not valid
-- UTF-8; invalid bytes are replaced rather than throwing an exception,
-- which (being lazy) could otherwise happen much later, e.g. when the
-- error is logged during a scan.
decodeToolOutput :: LByteString -> Text
decodeToolOutput = TextL.toStrict . Text.decodeUtf8With lenientDecode
