{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

import Data

import Control.Arrow ((***))
import Control.Monad (join, when, void)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List (intersperse, sortOn, reverse, partition, stripPrefix, delete, filter, groupBy, dropWhile, reverse, (\\))
import Data.Map.Strict hiding (splitAt,drop,map,take,delete,filter,partition,(\\))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Tuple (swap)
import GHC.IO.Encoding
import Prelude hiding (lookup, null, showList)
import System.Directory (listDirectory)
import System.IO

-- returns a map from hotspot to histograms for a given week ID
-- note that the probability is the maximum for the three week period surrounding the desired week
-- only return data for hotspots with 5 or more checklists in that three week period
histograms :: Int -> IO (Map Hotspot (Map BirdName Double))
histograms weekID = do
  hss <- hotspots
  -- histpairs :: [(Hotspot,(Int,Map))]
  histpairs <- zip hss <$> mapM (readHistogram weekID) hss
  -- histograms include birds seen in *any* week of the year. Many have not been seen
  -- in the first week of August, so let's remove them.
  let filteredHSs = ((M.filter (/=0) <$>) <$>) <$> histpairs
      -- group the (pre-sorted) histograms by region
      groupedHSs :: [[(Hotspot,(Int,Map BirdName Double))]]
      groupedHSs = groupBy (\a b -> region (fst a) == region (fst b)) filteredHSs
      -- sort into large and small hotspots
      sortedGroups :: [([(Hotspot,(Int,Map BirdName Double))],[(Hotspot,(Int,Map BirdName Double))])]
      sortedGroups = partition ((< 5) . fst . snd) <$> groupedHSs
      printStats (a,b) = 
        putStrLn $ region (fst $ head b) ++ ": Keeping " ++ show (length b) ++ ", discarding " ++ show (length a)
  mapM_ printStats sortedGroups
  putStrLn $ "Total: Kept " ++ show (sum $ length <$> snd <$> sortedGroups) ++ "/" ++ show (length histpairs) ++ " hotspots"
  -- sortedGroups is a list of pairs, where each pair corresponds to the small and large hotspots within a region
  -- join (***) concat (unzip sortedGroups) merges all large hotspots for all regions and all small hotspots in all regions
  let volHSs' = join (***) concat $ unzip sortedGroups
      -- volHSs' is a pair of lists, where each list is a triple (Hotspot, (Int, Map BirdName Double))
      -- where the Int is the number of checklists for the period at that hotspot
      -- remove the checklist count
      volHSs@(loVolMaps, hiVolMaps) = join (***) (map (snd <$>)) volHSs'
      -- volHSs is a pair of lists, where each list is (Hotspot, Map BirdName Double)
      (loVolKeys, hiVolKeys) = 
        join (***) (S.unions . map (keysSet . snd)) volHSs
      -- loVolKeys is the set of all birds seen at low-volume hotspots
      loVolOnly = loVolKeys S.\\ hiVolKeys
  putStrLn $ "Number of birds only seen at low-volume hotspots: " ++ show (length loVolOnly)
  showList "" $ S.toList loVolOnly
  --let loVolRare = filter (not . null . snd) $ map (flip restrictKeys loVolOnly <$>) loVolMaps
      -- adjust bird name to indicate a rarity
  --    loVolRare' = (updateKeys ("RARE: "++) <$>) <$> loVolRare
  --    largeMaps = concat $ snd <$> sortedGroups
  return $ fromList $ hiVolMaps -- loVolRare' -- ++ 

-- merges all hotspots in a region into a single "regional hotspot", 
-- using the maximum probabilty in any hotspot in the region.
regionalAnalysis :: Map Hotspot (Map BirdName Double) -> Map RegionName (Map BirdName Double)
regionalAnalysis hist =
  -- group histograms by region
  -- already sorted using `toList`, so no need to resort. Just group.
  let histGroups = groupWith (region . fst) $ toList hist :: [[(Hotspot,Map BirdName Double)]]
      -- given a list of (Hotspot, Map BirdName Double) pairs, where all hotspots are in the same region,
      -- returns a single pair (Hotspot, Map BirdName Double) where the probability for each bird is the
      -- maximum of all hotspots in the region
      compressRegion :: [(Hotspot,Map BirdName Double)] -> (RegionName, Map BirdName Double)
      compressRegion regMaps =
        let regionRepr = region $ fst $ head regMaps
            mapsOnly = snd <$> regMaps :: [Map BirdName Double]
        in (regionRepr, unionsWith max mapsOnly) :: (RegionName,Map BirdName Double)
      -- we treat each region as the (maximum for each bird) union of all of its hotspots
  in fromList $ compressRegion <$> histGroups

categorize :: Map RegionName (Map BirdName Double) -> Map Category [BirdName]
categorize regList = 
  -- a map from birds to their probability for each hotspot
  let birdMap = transposeMaps regList :: Map BirdName (Map RegionName Double)
      categorize' probs = 
        case sum $ snd <$> probs of
          x | x < 0.1   -> Vagrant
            | x < 0.34  -> Rare
            | x < 5     -> Target
            | otherwise -> Common
      -- categorize each bird by its probability of being seen in *any* region
  in invertMap $ ((:[]) . categorize' . toList) <$> birdMap :: Map Category [BirdName]

data Category =
  Common
  | Target
  | Rare
  | Vagrant
  deriving (Ord,Show,Eq)

type RegionName = String
type BirdName = String

filterLowProbRegions :: [(RegionName, Double)] -> [(RegionName, Double)]
filterLowProbRegions xs = 
  let maxProb = maximum $ snd <$> xs
  in filter (\(_,y) ->  y > (maxProb * 0.75)) xs

computeTargets :: Map RegionName (Map BirdName Double) -> Map RegionName [BirdName]
computeTargets regionalProbMap =
  let birdMap = flip withoutKeys (S.fromList gaBirds) $ 
        filterLowProbRegions <$> toList <$> transposeMaps regionalProbMap :: Map BirdName [(RegionName,Double)]
      -- remove regions with lower probability
      birdMap' = transposeMaps $ fromList <$> birdMap
  in keys <$> birdMap' :: Map RegionName [BirdName]

-- given a target bird in a particular region,
-- look at the probability of that bird for all hotspots in the region
-- and output a list which includes the top 10 percent of hotspots
likelyHotspots :: Map BirdName (Map Hotspot Double) -> Map RegionName [BirdName] -> Map RegionName (Map BirdName [Hotspot])
likelyHotspots rawHist targetBirds = mapWithKey go targetBirds
  where 
    go :: RegionName -> [BirdName] -> Map BirdName [Hotspot]
    go r bs = 
      let m = (filterWithKey $ \h _ -> region h == r) <$> rawHist
      in fromSet (go' m) (S.fromList bs)

    go' :: Map BirdName (Map Hotspot Double) -> BirdName -> [Hotspot]
    go' m b =
      let hsMap = m ! b
          probs = elems hsMap
          maxProb = maximum probs
          hsMap' = M.filter (> 0.8*maxProb) hsMap
      in keys hsMap'

main :: IO ()
main = do
  -- the ebird data files contain utf8 characters. This ensures that Haskell handles them properly.
  setLocaleEncoding utf8
  --mapM_ regionalAnalysis [24..28]
  let weekID = 28 -- first week of August
  rawHistogram <- histograms weekID

  -- the probability that a bird is seen in a particular region
  let regionalBirdProbs = regionalAnalysis rawHistogram

  -- categorize each bird as "common" "target" etc based on the probability it
  -- will be seen in *any* region during the entire trip
  let birdCats = categorize regionalBirdProbs

  putStrLn $ "week " ++ show weekID ++ ":" ++
    "\tcommon " ++ show (length $ birdCats ! Common) ++
    "\t+target " ++ show (length $ birdCats ! Target) ++
    "\t+rare " ++ show (length $ birdCats ! Rare)

  showList "Common birds" $ birdCats ! Common

  -- compute target birds for each region, but don't count birds which should
  -- be "common" on our trip
  let bset = S.fromList $ (birdCats ! Target) ++ (birdCats ! Rare)
      uncommonHist = flip restrictKeys bset <$> regionalBirdProbs
      targetBirds = computeTargets uncommonHist
  void $ sequence $ mapWithKey showList targetBirds

  --let str = showTable birdMap'
  --writeFile "out.txt" str

showTable :: Map RegionName (Map BirdName Double) -> String
showTable m = 
  let header = concat $ intersperse "\t" $ "" : (show <$> keys m)
      invLists = toList <$> transposeMaps m :: Map BirdName [(RegionName,Double)]
      -- remove 0s from each row, and sort from largest to smallest
      invLists' = reverse <$> sortOn snd <$> filter ((>0) . snd) <$> invLists :: Map BirdName [(RegionName,Double)]
      -- sort by length of the list
      invLists'' = sortAndGroup (length . snd) $ toList invLists' -- :: [[(BirdName,[(RegionName,Double)])]]
      sortedGroups = concat $ reverse $ sortOn (length . snd . head) $ reverse <$> sortOn (map snd . snd) <$> invLists''
      dataStrs = showRow <$> sortedGroups
  in concat $ intersperse "\n" $ header : dataStrs

-- one row of the table
showRow :: (BirdName,[(RegionName, Double)]) -> String
showRow (k,m) = concat $ intersperse "\t" $ k : (show <$> snd <$> m)




-- helper functions

showList :: String -> [String] -> IO ()
showList str as = do
  when (str /= "") $ putStrLn $ show (length as) ++ " " ++ str
  mapM_ (putStrLn) as
  putStrLn "\n\n"

groupKeysList :: (Eq k, Ord k) => [(k,a)] -> Map k [a]
groupKeysList ys = fromList $ (\xs -> (fst $ head xs, snd <$> xs)) <$> sortAndGroup fst ys

transposeMaps :: forall a b c . (Ord b, Num c) => Map a (Map b c) -> Map b (Map a c)
transposeMaps m =
  let allBs :: S.Set b
      allBs = S.unions $ keysSet <$> m
      bmap :: b -> Map a c
      bmap b = mapMaybe (lookup b) m
  in fromSet bmap allBs

invertMap :: (Ord a) => Map k [a] -> Map a [k]
invertMap m = groupKeysList $ concat $ (\(ls,k) -> (,k) <$> ls) <$> swap <$> toList m

-- you probably want to sort your input first!
groupWith :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupWith f = groupBy (\a b -> f a == f b)

sortAndGroup :: (Eq b, Ord b) => (a -> b) -> [a] -> [[a]]
sortAndGroup f = groupWith f . sortOn f


keyMap :: (Ord k') => (k -> k') -> Map k a -> Map k' [a]
keyMap f m = groupKeysList $ (\(k,a) -> (f k,a)) <$> toList m

updateKeys :: (Ord k, Ord k') => (k -> k') -> Map k a -> Map k' a
updateKeys f m = 
  let l = swap <$> toList m
      l' = swap <$> (f <$>) <$> l
  in fromList l'
