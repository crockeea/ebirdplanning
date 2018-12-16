{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data

import Control.Arrow ((***))
import Control.Monad (join)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List (intersperse, sortOn, reverse, partition, stripPrefix, delete, filter, groupBy, dropWhile, reverse)
import Data.Map hiding (splitAt,drop,map,take,delete,filter,partition)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tuple (swap)
import GHC.IO.Encoding
import Prelude hiding (lookup, null)
import System.Directory (listDirectory)
import System.IO

updateKeys :: (Ord k, Ord k') => (k -> k') -> Map k a -> Map k' a
updateKeys f m = 
  let l = swap <$> toList m
      l' = swap <$> (f <$>) <$> l
  in fromList l'

histograms :: IO (Map Hotspot (Map String Double))
histograms = do
  hss <- hotspots
  -- histpairs :: [(Hotspot,(Int,Map))]
  histpairs <- zip hss <$> mapM readHistogram hss
  -- histograms include birds seen in *any* week of the year. Many have not been seen
  -- in the first week of August, so let's remove them.
  let filteredHSs = ((M.filter (/=0) <$>) <$>) <$> histpairs
      groupedHSs :: [[(Hotspot,(Int,Map String Double))]]
      groupedHSs = groupBy (\a b -> region (fst a) == region (fst b)) filteredHSs
      sortedGroups :: [([(Hotspot,(Int,Map String Double))],[(Hotspot,(Int,Map String Double))])]
      sortedGroups = partition ((< 5) . fst . snd) <$> groupedHSs
      printStats (a,b) = 
        putStrLn $ region (fst $ head b) ++ ": Keeping " ++ show (length b) ++ "/" ++ show (length a)
  mapM_ printStats sortedGroups
  putStrLn $ "Total: Kept " ++ show (sum $ length <$> snd <$> sortedGroups) ++ "/" ++ show (length histpairs) ++ " hotspots"
  let volHSs' = join (***) concat $ unzip sortedGroups
      -- remove the checklist count
      volHSs@(loVolMaps, hiVolMaps) = join (***) (map (snd <$>)) volHSs'
      (loVolKeys, hiVolKeys) = 
        join (***) (S.unions . map (keysSet . snd)) volHSs
      loVolOnly = loVolKeys S.\\ hiVolKeys
  putStrLn $ "Number of birds only seen at low-volume hotspots: " ++ show (length loVolOnly)
  let loVolRare = filter (not . null . snd) $ map (flip restrictKeys loVolOnly <$>) loVolMaps
      -- adjust bird name to indicate a rarity
      loVolRare' = (updateKeys ("RARE: "++) <$>) <$> loVolRare
  --    largeMaps = concat $ snd <$> sortedGroups

  return $ fromList $ hiVolMaps -- loVolRare' -- ++ 

main :: IO ()
main = do
  setLocaleEncoding utf8
  hist <- histograms
      -- already sorted using `toList`, so no need to resort. Just group.
  let histGroups = groupWith (region . fst) $ toList hist :: [[(Hotspot,Map String Double)]]
      compressRegion regMaps =
        let regionRepr = fst $ head regMaps
            mapsOnly = snd <$> regMaps
        in (regionRepr, unionsWith max mapsOnly)
      compressedHists = fromList $ compressRegion <$> histGroups
  let str = showTable compressedHists
  writeFile "out.txt" str
  putStrLn "done"

invertMap :: forall a b . (Ord b) => Map a (Map b Double) -> Map b (Map a Double)
invertMap m =
  let allBs :: S.Set b
      allBs = S.unions $ keysSet <$> m
      indexb :: b -> Map b Double -> Double
      indexb b m = case lookup b m of Nothing -> 0; (Just x) -> x
      bmap :: b -> Map a Double
      bmap b = indexb b <$> m
  in fromSet bmap allBs

-- one row of the table
showRow :: (String,[(Hotspot, Double)]) -> String
showRow (k,m) = concat $ intersperse "\t" $ k : (show <$> snd <$> m)

hsAvg :: [(a,Double)] -> Double
hsAvg xs = (sum $ snd <$> xs) / (fromIntegral $ length xs)

-- you probably want to sort your input first!
groupWith :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupWith f = groupBy (\a b -> f a == f b)

sortAndGroup :: (Eq b, Ord b) => (a -> b) -> [a] -> [[a]]
sortAndGroup f = groupWith f . sortOn f

showTable :: Map Hotspot (Map String Double) -> String
showTable m = 
  let header = concat $ intersperse "\t" $ "" : (show <$> keys m)
      invLists = toList <$> invertMap m :: Map String [(Hotspot,Double)]
      -- remove 0s from each row, and sort from largest to smallest
      invLists' = reverse <$> sortOn snd <$> (filter ((/= 0) . snd)) <$> invLists :: Map String [(Hotspot,Double)]
      invLists'' = sortAndGroup (length . snd) $ toList invLists' -- :: [[(String,[(Hotspot,Double)])]]
      sortedGroups = concat $ reverse $ sortOn (length . snd . head) $ reverse <$> sortOn (hsAvg . snd) <$> invLists''
      dataStrs = showRow <$> sortedGroups
  in concat $ intersperse "\n" $ header : dataStrs






{-
  let sortedData = reverse $ dropWhile ((<0.25) . snd) $ sortOn snd $ toList m
      strData = (show <$>) <$> sortedData
      birdLines = concat <$> intersperse "\t" <$> (\(a,b)->[a,b]) <$> strData
  in unlines birdLines


-- process all hotspots in one region and report the maximum 
-- probability of each bird at any of the hotspots, ignoring 
-- spots with only a few checklists for the target week
processArea :: [String] -> IO (Map String Double)
processArea hotspotIDs = do
  -- create a map from hotspot ID to the processed data for that
  -- hotspot (a map from bird name to probability for that bird 
  -- in the first week of August)
  hsMap <- sequence $ fromSet createHotspot $ S.fromList hotspotIDs
  -- filter out hotspots which don't have enough checklists
  largeHSMap <- filterSmallHotspots hsMap
  -- merge the data from the remaining hotspots to find the maximum
  -- probability for each bird at *any* hotspot in the region
  let finalMap = unionsWith max largeHSMap
  -- write the data to a file
  return finalMap
-}
