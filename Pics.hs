{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Pics where

import Import

import Control.Monad
import qualified Data.Map as Map
import Data.List
import System.Directory
import System.FilePath
import System.Posix.Files
import Text.Regex.PCRE as PCRE

nefBase :: String
nefBase = undefined

jpegBase :: String
jpegBase = undefined

nefExts :: [String]
nefExts = ["nef"]

nefExtsRev :: [String]
nefExtsRev = map reverse nefExts

jpegExts :: [String]
jpegExts = ["jpg", "jpeg"]

jpegExtsRev :: [String]
jpegExtsRev = map reverse jpegExts

xmpExts :: [String]
xmpExts = ["xmp"]

xmpExtsRev :: [String]
xmpExtsRev = map reverse xmpExts

fileExts :: [String]
fileExts = nefExts ++ xmpExts ++ ["png", "jpg", "jpeg"]

fileDotExts :: [String]
fileDotExts = map ('.':) fileExts

dateREString :: String
dateREString = "([0-9]{4}-[0-9]{2}-[0-9]{2})(.*)"

dirRegex :: PCRE.Regex
dirRegex = PCRE.makeRegex dateREString

blacklistedDirs :: [String]
blacklistedDirs = [".", "..", ".thumbnails"]

isOKDir :: String -> Bool
isOKDir = PCRE.match dirRegex

data NefDir = NefDir { nfName :: String
                     , nfPath :: String
                     , nfWritten :: Bool
                     , nfNefCount :: Int
                     , nfXmpCount :: Int
                     }

data JpegDir = JpegDir { jpName :: String
                       , jpPath :: String
                       , jpJpegCount :: Int
                       }

data PicDir = PicNef NefDir
            | PicJpeg JpegDir
            | PicBoth NefDir JpegDir

type Repository = Map.Map String PicDir

class HasName a where
  nameOf :: a -> String

instance HasName NefDir where
  nameOf = nfName

instance HasName JpegDir where
  nameOf = jpName

instance HasName PicDir where
  nameOf (PicNef v) = nameOf v
  nameOf (PicJpeg v) = nameOf v
  nameOf (PicBoth v _) = nameOf v


getDownContents :: FilePath -> IO [FilePath]
getDownContents base = do
  contents <- getDirectoryContents base
  return $ filter (\d -> d `notElem` blacklistedDirs) contents

isDir :: FilePath -> IO Bool
isDir = liftM isDirectory . getSymbolicLinkStatus

scanDir :: (HasName a) =>
           String
        -> (String -> String -> IO a)
        -> IO (Map.Map String a)
scanDir base builder = do
  paths <- getDownContents base
  dirs <- filterM (isDir . (base </>)) paths
  ndirs <- mapM (\p -> scanSubDir (base </> p) builder) dirs
  let ndirs' = concat ndirs
  return $ foldl (\a v -> Map.insert (nameOf v) v a) Map.empty ndirs'

scanSubDir :: (HasName a) => String -> (String -> String -> IO a) -> IO [a]
scanSubDir path builder = do
  allpaths <- getDownContents path
  dirpaths <- filterM (isDir . (path </>)) allpaths
  let allpaths' = filter isOKDir dirpaths
  mapM (\s -> builder s (path </> s)) allpaths'

mergeDirs :: Map.Map String NefDir -> Map.Map String JpegDir ->
             Map.Map String PicDir
mergeDirs nefs jpegs =
  let withnefs = foldl (\a n ->
                          let name = nameOf n in
                          case Map.lookup name jpegs of
                            Nothing -> Map.insert name (PicNef n) a
                            Just j -> Map.insert name (PicBoth n j) a
                       ) Map.empty $ Map.elems nefs
      withjpegs = foldl (\a j ->
                           let name = nameOf j in
                           case Map.lookup name a of
                             Nothing -> Map.insert name (PicJpeg j) a
                             Just _ -> a) withnefs $ Map.elems jpegs
  in withjpegs

recursiveScanDir :: FilePath -> IO [FilePath]
recursiveScanDir base = do
  contents <- getDownContents base
  let potentialdirs =
        filter (\s -> all (\e -> not (e `isSuffixOf` s)) fileDotExts) contents
  dirs <- filterM (isDir . (base </>)) potentialdirs
  subdirs <- mapM (\s -> recursiveScanDir (base </> s)) dirs
  return $ contents ++ concat subdirs

loadNefDir :: String -> FilePath -> IO NefDir
loadNefDir name path = do
  contents <- recursiveScanDir path
  let rcontent = map reverse contents
  let nefs = length $ filter (\s -> any (`isPrefixOf` s) nefExtsRev) rcontent
      xmps = length $ filter (\s -> any (`isPrefixOf` s) xmpExtsRev) rcontent
  return $ NefDir name path False nefs xmps

loadJpegDir :: String -> FilePath -> IO JpegDir
loadJpegDir name path = do
  contents <- recursiveScanDir path
  let rcontent = map reverse contents
  let jpegs = length $ filter (\s -> any (`isPrefixOf` s) jpegExtsRev) rcontent
  return $ JpegDir name path jpegs

numUnprocessed :: NefDir -> Int
numUnprocessed n =
  if delta > 0
    then delta
    else 0
  where delta = nfNefCount n - nfXmpCount n

showUnprocessed :: NefDir -> String
showUnprocessed n =
  if u > 0 then "(" ++ show u ++ " unprocessed )" else ""
    where u = numUnprocessed n

scanAll :: IO (Map.Map String PicDir)
scanAll = do
  nefs <- scanDir nefBase loadNefDir
  jpegs <- scanDir jpegBase loadJpegDir
  let pics = mergeDirs nefs jpegs
  return pics

computeUnprocessed :: Repository -> [NefDir]
computeUnprocessed =
  reverse .
  foldl (\a e -> case e of
                   -- it could be (rare) that a given directory only
                   -- has movies, so nothing to process
                   PicNef n | nfNefCount n > 0 -> n:a
                   _ -> a) [] .
  Map.elems


computeStandaloneJpegs :: Repository -> [JpegDir]
computeStandaloneJpegs =
  reverse .
  foldl (\a e -> case e of
                   -- it could be (rare) that a given directory only
                   -- has movies, so nothing to process
                   PicJpeg j -> j:a
                   _ -> a) [] .
  Map.elems
