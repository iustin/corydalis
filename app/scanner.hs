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

import Import
import System.Mem
import System.Clock
import Pics

main :: IO ()
main = do
    -- Get the settings from all relevant sources
    settings <- loadYamlSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv

    let config = appConfig settings

    -- Scan repository
    t1m <- getTime Monotonic
    t1p <- getTime ProcessCPUTime
    repo <- scanAll config
    performGC
    t2m <- getTime Monotonic
    t2p <- getTime ProcessCPUTime
    let s1 = computeRepoStats (repoDirs repo)
    putStrLn $ "Repository stats #1:"
    print s1
    t3m <- getTime Monotonic
    t3p <- getTime ProcessCPUTime
    let s2 = computeRepoStats (repoDirs repo)
    putStrLn $ "Repository stats #1:"
    print s2
    t4m <- getTime Monotonic
    t4p <- getTime ProcessCPUTime
    putStrLn $ "Repo scan time (m/c):"
    print $ diffTimeSpec t2m t1m
    print $ diffTimeSpec t2p t1p
    putStrLn $ "Stats time #1 (m/c):"
    print $ diffTimeSpec t3m t2m
    print $ diffTimeSpec t3p t2p
    putStrLn $ "Stats time #2 (m/c):"
    print $ diffTimeSpec t4m t3m
    print $ diffTimeSpec t4p t3p
    -- threadDelay 5000000
    performGC
