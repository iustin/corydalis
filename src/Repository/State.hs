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

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The live state of the repository: the application context that
-- holds the current repository, the scan's status and the search
-- cache, and the functions that read and update them.
module Repository.State
  ( Ctx
  , initContext
  , SearchCache(..)
  , updateRepo
  , newRepo
  , OwnershipChanged(..)
  , tryUpdateRepo
  , updateRepoStatus
  , tryUpdateRepoStatus
  , checkRepoOwnership
  , checkScanOwnership
  , getSearchResults
  , getRepo
  , getProgress
  , getRenderProgress
  , getCleanProgress
  ) where

import           Control.DeepSeq
import           Data.LruCache           (LruCache)
import qualified Data.LruCache           as LRU
import           UnliftIO.Exception

import           Compat.Orphans          ()
import           Import.NoFoundation     hiding (fileName, fileSize, path)

import           Repository

type Ctx = Context Repository SearchCache

-- TODO: move this to IO, so that callers don't have to know about
-- atomically? [cleanup]
initContext :: Config -> LogFn
            -> ToolLimits  -- ^ The limits of the tools building previews.
            -> ToolLimits  -- ^ The limits of the scan's exiftool runs.
            -> STM Ctx
initContext config logfn renderTools scanTools =
  newContext config logfn renderTools scanTools startupRepository emptySearchCache

-- | The search cache: the results of the latest searches, computed
-- from the current data of the repository (see 'getSearchResults').
data SearchCache = SearchCache
  { scGeneration :: !Int
    -- ^ The generation of the repository's data, increased each time
    -- new data is published (see 'flushSearchCache').
  , scResults    :: !(LruCache UrlParams SearchResults)
    -- ^ The results, by search.
  }

-- | The number of searches whose results are kept.
searchCacheSize :: Int
searchCacheSize = 10

emptySearchCache :: SearchCache
emptySearchCache = SearchCache 0 (LRU.empty searchCacheSize)

updateRepo :: Ctx -> Repository -> IO Bool
updateRepo ctx new = atomically $ do
  let rc = ctxRepo ctx
  -- Note: the below can't be replaced via stateTVar, since it does write
  -- to the search cache too, thus the transformation function is not
  -- pure.
  current <- readTVar rc
  let do_update = repoSerial current <= repoSerial new
  -- TODO: add logging for prevented overwrites.
  when do_update $ do
    writeTVar rc $! new
    flushSearchCache (ctxSearchCache ctx)
  return do_update

-- | Marks the repository as being rescanned: it keeps its data, with
-- an increased serial number and the starting status. Returns the new
-- serial, which identifies the scan that owns the repository from now
-- on (see 'updateRepo' and 'updateRepoStatus').
--
-- Only the serial is returned, not the repository, so that the scan
-- can't keep the previous data alive once it has published its own.
--
-- The logic here needs to be kept in sync with updateRepo and
-- updateRepoStatus.
newRepo :: TVar Repository -> STM Int
newRepo rc = do
  old <- readTVar rc
  let new = old { repoSerial = repoSerial old + 1
                , repoStatus = RepoStarting
                }
  -- Note: it is safe to update the repository without flushing the
  -- cache, as the image data has not been changed, only the serial
  -- and status; hence, not going through updateRepo.
  writeTVar rc $! new
  return $ repoSerial new

-- | The failure of a scan that no longer owns the repository, as a
-- newer one was started (see 'newRepo'). It is its own type so that it
-- can be told apart from the scan's real failures (see runScanAction
-- in "Repository.Scan").
data OwnershipChanged = OwnershipChanged

instance Show OwnershipChanged where
  show OwnershipChanged = "Repository ownership changed, aborting"

instance Exception OwnershipChanged

-- | Fails with 'OwnershipChanged'.
ownershipChanged :: IO a
ownershipChanged = throwIO OwnershipChanged

tryUpdateRepo :: Ctx -> Repository -> IO Repository
tryUpdateRepo ctx new = do
  owning <- updateRepo ctx new
  unless owning ownershipChanged
  return new

-- | Sets the status of the current repository, if the scan with the
-- given serial (see 'newRepo') still owns it, and returns the updated
-- repository if so.
--
-- Unlike 'updateRepo', the caller doesn't pass a repository, only the
-- status: it can't bring back data it read earlier, and has no reason
-- to keep the repository it started from (which, during a scan, is
-- the previous one, in full).
--
-- The search cache is not flushed: the status is not searched, and the
-- data doesn't change. (The cache can't hold results computed from
-- earlier data either: see 'getSearchResults'.)
updateRepoStatus :: Ctx -> Int -> RepoStatus -> IO (Maybe Repository)
updateRepoStatus ctx !serial status = do
  -- Evaluated here, so that the transaction doesn't do it, and so that
  -- the repository doesn't keep what the status is computed from.
  status' <- evaluate $ force status
  atomically $ do
    let rc = ctxRepo ctx
    current <- readTVar rc
    if repoSerial current == serial
      then do
        let new = current { repoStatus = status' }
        writeTVar rc $! new
        return $ Just new
      else return Nothing

-- | Like 'updateRepoStatus', but failing if the scan no longer owns
-- the repository.
tryUpdateRepoStatus :: Ctx -> Int -> RepoStatus -> IO Repository
tryUpdateRepoStatus ctx serial status =
  updateRepoStatus ctx serial status >>= maybe ownershipChanged return

checkRepoOwnership :: Ctx -> Repository -> IO ()
checkRepoOwnership ctx candidate = do
  current <- readTVarIO (ctxRepo ctx)
  unless (repoSerial current <= repoSerial candidate) ownershipChanged

-- | Fails if the scan with the given serial (see 'newRepo') no longer
-- owns the repository.
checkScanOwnership :: Ctx -> Int -> IO ()
checkScanOwnership ctx serial = do
  current <- readTVarIO (ctxRepo ctx)
  unless (repoSerial current == serial) ownershipChanged

-- | Returns the results of a search, under the given key, from the
-- cache if present, otherwise computed from the current repository,
-- and cached.
--
-- The results are computed, and fully evaluated, outside any
-- transaction, so that a long search doesn't keep the cache from
-- changing, nor is run again because it changed. They are only cached
-- if no new data was published meanwhile (see 'scGeneration'), so that
-- the cache never holds results computed from earlier data: these
-- would list pictures a scan removed, and miss those it found, until
-- they leave the cache.
--
-- If new data was published meanwhile, the search is done once more,
-- on that data; if that happens again, the second results are returned
-- without caching them. Two identical searches at the same time both
-- compute their results.
--
-- The cache is evaluated as well before being written, on a hit
-- (which changes the order of the entries) as on a miss: an
-- unevaluated one would keep the previous cache, and with it, on a
-- miss with a full cache, the results just dropped.
getSearchResults :: Ctx
                 -> (Repository -> SearchResults)  -- ^ The search.
                 -> UrlParams                      -- ^ Its key in the cache.
                 -> IO SearchResults
getSearchResults ctx search key = attempt True
  where
    cacheVar = ctxSearchCache ctx
    attempt again = do
      found <- atomically $ do
        cache <- readTVar cacheVar
        case LRU.lookup key (scResults cache) of
          Just (results, entries) -> do
            writeTVar cacheVar $! cache { scResults = entries }
            return $ Right results
          Nothing -> do
            -- Read in the same transaction as the generation, so that
            -- the generation is that of the data searched.
            repo <- readTVar (ctxRepo ctx)
            let !generation = scGeneration cache
            return $ Left (generation, repo)
      case found of
        Right results -> return results
        Left (generation, repo) -> do
          results <- evaluate . force $ search repo
          stored <- atomically $ do
            cache <- readTVar cacheVar
            let current = scGeneration cache == generation
            when current $
              writeTVar cacheVar $!
                cache { scResults = LRU.insert key results (scResults cache) }
            return current
          if stored || not again
            then return results
            else attempt False

-- | Empties the search cache, as new data is published, and increases
-- its generation (see 'getSearchResults'). This must be done in the
-- transaction that publishes the data.
flushSearchCache :: TVar SearchCache -> STM ()
flushSearchCache cacheVar = do
  cache <- readTVar cacheVar
  writeTVar cacheVar $! SearchCache (scGeneration cache + 1)
                                    (LRU.empty searchCacheSize)

-- | Returns the current repository.
--
-- Nowadays this always returns the repository, which might be empty.
getRepo :: Ctx -> IO Repository
getRepo = readTVarIO . ctxRepo

-- | Returns the progress of the repository scanner.
--
-- If the scan has finished, this is the value of all files and
-- directories scanned (mildly interesting). If not, then this value
-- is the running state.
getProgress :: Ctx -> IO Progress
getProgress = readTVarIO . ctxScanProgress

-- | Returns the progress of the render thread.
getRenderProgress :: Ctx -> IO Progress
getRenderProgress = readTVarIO . ctxRenderProgress

getCleanProgress :: Ctx -> IO Progress
getCleanProgress = readTVarIO . ctxCleanProgress
