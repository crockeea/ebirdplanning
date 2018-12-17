{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data

import Control.Arrow ((***))
import Control.Monad (join, when)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List (intersperse, sortOn, reverse, partition, stripPrefix, delete, filter, groupBy, dropWhile, reverse)
import Data.Map hiding (splitAt,drop,map,take,delete,filter,partition)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tuple (swap)
import GHC.IO.Encoding
import Prelude hiding (lookup, null, showList)
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
      -- group the (pre-sorted) histograms by region
      groupedHSs :: [[(Hotspot,(Int,Map String Double))]]
      groupedHSs = groupBy (\a b -> region (fst a) == region (fst b)) filteredHSs
      -- sort into large and small hotspots
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
  showList "" $ S.toList loVolOnly
  let loVolRare = filter (not . null . snd) $ map (flip restrictKeys loVolOnly <$>) loVolMaps
      -- adjust bird name to indicate a rarity
      loVolRare' = (updateKeys ("RARE: "++) <$>) <$> loVolRare
  --    largeMaps = concat $ snd <$> sortedGroups

  return $ fromList $ hiVolMaps -- loVolRare' -- ++ 

data Category =
  Common
  | Target
  | Rare
  | Vagrant
  deriving (Ord,Show,Eq)

categorize :: [(Hotspot, Double)] -> Category
categorize probs = 
  case sum $ snd <$> probs of
    x | x < 0.1   -> Vagrant
      | x < 0.34  -> Rare
      | x < 5     -> Target
      | otherwise -> Common

regionalAnalysis :: IO ()
regionalAnalysis = do
  hist <- histograms
  -- already sorted using `toList`, so no need to resort. Just group.
  let histGroups = groupWith (region . fst) $ toList hist :: [[(Hotspot,Map String Double)]]
      compressRegion regMaps =
        let regionRepr = fst $ head regMaps
            mapsOnly = snd <$> regMaps
        in (regionRepr, unionsWith max mapsOnly) :: (Hotspot,Map String Double)
      compressedHists = fromList $ compressRegion <$> histGroups :: Map Hotspot (Map String Double)
      birdMap = transposeMaps compressedHists :: Map String (Map Hotspot Double)
      catMap = invertMap $ (categorize . toList) <$> birdMap
  showList "Common" $ catMap ! Common
  showList "Vagrants" $ catMap ! Vagrant

showList :: (Show a) => String -> [a] -> IO ()
showList str as = do
  when (str /= "") $ putStrLn str
  mapM_ (putStrLn . show) as
  putStrLn "\n\n"

showTable :: Map Hotspot (Map String Double) -> String
showTable m = 
  let header = concat $ intersperse "\t" $ "" : (show <$> keys m)
      invLists = toList <$> transposeMaps m :: Map String [(Hotspot,Double)]
      -- remove 0s from each row, and sort from largest to smallest
      invLists' = reverse <$> sortOn snd <$> filter ((>0) . snd) <$> invLists :: Map String [(Hotspot,Double)]
      -- sort by length of the list
      invLists'' = sortAndGroup (length . snd) $ toList invLists' -- :: [[(String,[(Hotspot,Double)])]]
      sortedGroups = concat $ reverse $ sortOn (length . snd . head) $ reverse <$> sortOn (map snd . snd) <$> invLists''
      dataStrs = showRow <$> sortedGroups
  in concat $ intersperse "\n" $ header : dataStrs

main :: IO ()
main = do
  setLocaleEncoding utf8
  regionalAnalysis
  --let str = showTable compressedHists
  --writeFile "out.txt" str
  putStrLn "done"

-- you probably want to sort your input first!
groupWith :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupWith f = groupBy (\a b -> f a == f b)

sortAndGroup :: (Eq b, Ord b) => (a -> b) -> [a] -> [[a]]
sortAndGroup f = groupWith f . sortOn f

groupKeysList :: (Eq k, Ord k) => [(k,a)] -> Map k [a]
groupKeysList ys = fromList $ (\xs -> (fst $ head xs, snd <$> xs)) <$> sortAndGroup fst ys

keyMap :: (Ord k') => (k -> k') -> Map k a -> Map k' [a]
keyMap f m = groupKeysList $ (\(k,a) -> (f k,a)) <$> toList m

invertMap :: (Ord a) => Map k a -> Map a [k]
invertMap m = groupKeysList $ swap <$> toList m

transposeMaps :: forall a b . (Ord b) => Map a (Map b Double) -> Map b (Map a Double)
transposeMaps m =
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
