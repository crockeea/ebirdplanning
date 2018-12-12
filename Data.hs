{-# LANGUAGE RecordWildCards #-}

module Data (nameMapping, readHistogram, Hotspot(..)) where

import qualified Codec.Binary.UTF8.String as U
import qualified Data.ByteString.Lazy as C
import Data.List (intersperse, sortOn, reverse, stripPrefix)
import Data.Map hiding (drop, splitAt, take, map)
import Data.Maybe (catMaybes)
import Data.Tuple (swap)
import Network.HTTP.Conduit
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix (pathSeparator)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read

pathSep :: String
pathSep = [pathSeparator]

nameMapping :: Map String String
nameMapping = unsafePerformIO $ do
  let dir = "data"
      filePath = "idmap.txt"
      url = "https://ebird.org/GuideMe?reportType=location&bMonth" ++
            "=08&bYear=1900&eMonth=08&eYear=2018&parentState=US-WA" ++
            "&countries=US&states=US-WA&getLocations=hotspots&" ++ 
            "continue.x=31&continue.y=6"
  readOrDownload dir filePath parseHotspotMapping url

parseHotspotMapping :: String -> Map String String
parseHotspotMapping rawData =
  let rawData' = lines rawData
      mapLinePrefix = "\t\t\t\t\t\t\t\t<td valign=\"top\"><input type=\"checkbox\" name=\"hotspots\" value=\""
      mapLines = catMaybes $ (stripPrefix mapLinePrefix) <$> rawData'
      parseMapping str = 
        let (lhs,rhs) = span (/= '\"') str
        in (lhs, drop 4 $ take (length rhs - 5) rhs)
      parsedLines = swap <$> parseMapping <$> mapLines
  in fromList parsedLines

data Hotspot = HS {region::String, hsName::String, code::String}
  deriving (Show,Ord,Eq)

readHistogram :: Hotspot -> IO (Int,Map String Double)
readHistogram HS{..} = do
  let dir = "data" ++ pathSep ++ region
      filePath = code ++ ".txt"
      url = "https://ebird.org/barchartData?r=" ++ code ++ 
            "&bmo=1&emo=12&byr=1900&eyr=2018&fmt=tsv"
  readOrDownload dir filePath parseHistogram url

readOrDownload :: String -> String -> (String -> a) -> String -> IO a
readOrDownload dir filePath parse url = do
  let fullPath = dir ++ pathSep ++ filePath
  fexists <- doesFileExist fullPath
  parse <$> 
    if fexists
    then do
      putStrLn $ "Reading " ++ fullPath
      readFile fullPath
    else do
      putStrLn $ "Downloading " ++ filePath
      rawData <- U.decode <$> C.unpack <$> simpleHttp url
      createDirectoryIfMissing True dir
      writeFile fullPath rawData
      return rawData

parseHistogram :: String -> (Int, Map String Double)
parseHistogram rawData = 
  -- raw files include 14 lines of cruft
  -- then one blank line
  -- then the number of checklists for each month
  -- followed by the data for each bird
  -- followed by two empty lines
      -- split each line into a separate string,
      -- then drop the first 14 lines and the last two lines
  let rawData' = init $ init $ drop 14 $ lines rawData
      -- first line of rawData' has info about number of checklists for each week,
      -- followed by an empty line
      checklistCount = snd $ parseHistLine $ head rawData'
      dataLines = drop 2 rawData'
  -- parse each line, dropping unwanted columns
  -- and convert to a map
  in (round checklistCount, fromList $ parseHistLine <$> dataLines)

parseHistLine :: String -> (String, Double)
parseHistLine str = 
  -- split the line into words
  let ws = words str
  in if length ws < 49 -- one word name plus 48 data values
     then error $ "line too short!\n\n" ++ str
     else 
       let (birdname,vals) = splitAt (length ws - 48) ws
           bird = concat $ intersperse "-" birdname
           -- read the value for the first week of August
           augVal = case readMaybe $ vals !! 28 of
                      Nothing -> error $ str
                      Just x -> x
       in (bird,augVal)