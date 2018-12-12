{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data

import Control.Arrow ((***))
import Control.Monad (join)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List (intersperse, sortOn, reverse, partition, stripPrefix, delete, filter, groupBy)
import Data.Map hiding (splitAt,drop,map,take,delete,filter,partition)
import qualified Data.Set as S
import Data.Tuple (swap)
import GHC.IO.Encoding
import System.Directory (listDirectory)
import System.IO

hotspots :: [Hotspot]
hotspots = 
  let nameToHS region hsName = 
        if member hsName nameMapping
        then HS{code=nameMapping!hsName,..}
        else error hsName
      regionList = [
       ("neahBay", neahBay),
       ("portAngeles", portAngeles),
       ("lakeCrescent", lakeCrescent),
       ("highOlympics", highOlympics),
       ("portTownsend", portTownsend),
       ("kitsapPeninsula", kitsapPeninsula),
       ("whidbeyIsland", whidbeyIsland),
       ("discoveryBay", discoveryBay),
       ("dungeness", dungeness)]
  in concatMap (\(str,lst) -> nameToHS str <$> lst) regionList

updateKeys :: (Ord k, Ord k') => (k -> k') -> Map k a -> Map k' a
updateKeys f m = 
  let l = swap <$> toList m
      l' = swap <$> (f <$>) <$> l
  in fromList l'

histograms :: IO (Map Hotspot (Map String Double))
histograms = do
  -- histpairs :: [(Hotspot,(Int,Map))]
  histpairs <- zip hotspots <$> mapM readHistogram hotspots
  let groupedHSs :: [[(Hotspot,(Int,Map String Double))]]
      groupedHSs = groupBy (\a b -> region (fst a) == region (fst b)) histpairs
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
  let loVolRare = map (flip restrictKeys loVolOnly <$>) loVolMaps
      -- adjust bird name to indicate a rarity
      loVolRare' = (updateKeys ("RARE"++) <$>) <$> loVolRare
  putStrLn $ show loVolRare
  let largeMaps = concat $ snd <$> sortedGroups

  return $ fromList $ loVolRare' ++ hiVolMaps

main :: IO ()
main = do

  setLocaleEncoding utf8
  _ <- histograms
  putStrLn "done"

{-
mapToFile :: Map String Double -> String
mapToFile m = 
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


neahBay :: [String]
neahBay = [
  "Cape Flattery",
  "Cape Flattery Road--Hobuck Overlook",
  "Neah Bay--Cape Loop Road",
  "Shi Shi Beach",
  "Makah Fish Hatchery",
  "Tsoo-Yess Beach",
  "Tsoo-Yess River Valley",
  "Tsoo-Yess River Valley--Makah Passage Rd",
  "Neah Bay--Wa'Atch Beach Drive",
  "Hobuck Beach",
  "Hobuck / Hobuck Loop / Wa'atch Peak Rds",
  "Hobuck Lake",
  "Neah Bay--Sewage Treatment Ponds (open most weekdays)",
  "Wa'atch River Valley",
  "Neah Bay--general area (*move to more refined location if appropriate)",
  "Neah Bay--Bahokus Peak",
  "Neah Bay--town and bay only",
  "Neah Bay--Ba'adah Village Loop Road",
  "Neah Bay--Greenhouse seawatch",
  "Neah Bay--housing development/clearcuts",
  "Neah Bay—The Wedge",
  "Snow Creek",
  "Bullman Beach",
  "Shipwreck Point",
  "Sekiu River mouth",
  "Hoko River SP--Cowan Ranch Heritage Site",
  "Sekiu",
  "Middle Point",
  "Clallam Bay County Park",
  "Clallam Bay",
  "Lake Pleasant Recreation Area",
  "Sol Duc Hatchery",
  "Bear Creek Campgrond",
  "Beaver Lake (Clallam Co.)",
  "Pysht River Conservation Area",
  "Pillar Point Recreation Area"]

portAngeles :: [String]
portAngeles = [
  "Elwha River Mouth -- west access",
  "Elwha River mouth",
  "Elwha River Mouth -- east access",
  "Port Angeles Landfill",
  "Lincoln Park (Port Angeles)",
  "Ediz Hook",
  "Port Angeles waterfront",
  "Valley Creek Estuary Park",
  "Port Angeles City Pier",
  "Olympic Discovery Trail - PA City Pier to Ennis Creek",
  "Francis Street Park",
  "Webster Park",
  "Olympic NP--Headquarters"]

lakeCrescent :: [String]
lakeCrescent = [
  "West Twin River mouth",
  "Olympic National Park--Lake Crescent Campground",
  "Olympic NF--Salmon Cascades",
  "Lyre Conservation Area",
  "Salt Creek County Park",
  "Freshwater Bay",
  "Lake Crescent",
  "Olympic NP--Lake Crescent Lodge grounds and trails",
  "Olympic NP--Lake Crescent--Marymere Falls Trail",
  "Storm King Ranger station",
  "Olympic NP--Sol Duc Hot Springs Resort",
  "Sol Duc Campground",
  "Olympic NP--Sol Duc Falls Trail"]

highOlympics :: [String]
highOlympics = [
  "Olympic NP--Elwha area",
  "Elwha River -- Aldwell area",
  "Olympic NP--Hurricane Hill Trail",
  "Olympic NP--Hurricane Ridge",
  "Olympic NP--Meadow Loop Trail, Hurricane Ridge",
  "Olympic NP--Overlook near Heart o' the Hills CG",
  "Olympic NP -- Heart o' the Hills CG",
  "Olympic NP--Morse Creek Overlook",
  "Olympic NP--Deer Park",
  "Olympic NP -- Obstruction Point Rd.",
  "Blue Mountain"]

dungeness :: [String]
dungeness = [
  "Olympic Peninsula--Sieberts Creek",
  "Seibert Creek Conservation Area",
  "Robin Hill Farm County Park",
  "Kitchen-Dick Ponds",
  "Olson Rd., Sequim",
  "Dungeness Recreation Area",
  "Dungeness NWR--forest trails",
  "Dungeness NWR--Harbor side",
  "Dungeness Spit--Strait of Juan de Fuca side",
  "Dungeness NWR",
  "Dungeness Bay",
  "Dungeness--Corner of Lotzgesell and E.Anderson",
  "Cline Spit",
  "Lotzgesell Rd. Area (between Lotzgesell Rd., Cay's Rd., and E. A",
  "Dungeness Landing Park/Oyster House",
  "Lower Dungeness R. (Creamery) Trail",
  "North Olympic WA--Lower Dungeness Unit",
  "Three Crabs",
  "Helen's Pond, Dungeness",
  "Roberta's Pond, Dungeness",
  "Gaskell Slough",
  "Jamestown Beach",
  "Graysmarsh",
  "Sequim View Cemetery",
  "Port Williams--Holland, Schmuck, John Scott Rds.",
  "Port Williams & Schmuck Rds",
  "Railroad Bridge Park",
  "Lower Dungeness River Riparian Corridor",
  "Marlyn Nelson County Park",
  "Gibson Spit",
  "Schmuck Road",
  "Maple View Farm",
  "Carrie Blake Park",
  "Pacific Northwest National Laboratory",
  "John Wayne Marina",
  "Pitship Pocket Estuary",
  "Sequim Bay State Park",
  "Dungeness Forks Campground"]

discoveryBay :: [String]
discoveryBay = [
  "Jimmy-Come-Lately Creek Estuary",
  "Jimmy-come-lately Creek",
  "Panorama Vista and Travis Spit",
  "Miller Peninsula State Park and Thompson's Spit",
  "Diamond Point",
  "Thompson Road",
  "Knapp Road (Clallam Co.)",
  "Cat Lake Road",
  "Miller Peninsula State Park",
  "Gardiner Beach",
  "Discovery Bay",
  "Crocker Lake"]

portTownsend :: [String]
portTownsend = [
  "Port Townsend -- Tibbals Lake Park",
  "Port Townsend--Quimper Wildlife Corridor",
  "Port Townsend -- North Beach",
  "Fort Worden SP--Chinese Gardens Pond",
  "Fort Worden SP",
  "Port Townsend Marine Science Center",
  "Point Wilson",
  "Protection Island",
  "Port Townsend--Glen Cove Sewage Pond",
  "Port Townsend--Larry Scott Trail--bluff",
  "Port Townsend--Larry Scott Trail - boat haven end",
  "Kah Tai Lagoon",
  "Port Townsend--Port of Port Townsend",
  "Port Townsend ferry terminal",
  "Port Townsend--Union Wharf",
  "Chetzemoka Park",
  "Point Hudson",
  "Discovery Bay--Discovery Road Pond",
  "Fort Townsend State Park",
  "Kala Point",
  "Anderson Lake SP",
  "West Valley (Jefferson Co.)",
  "Beaver Valley Rd, Chimacum, WA",
  "HJ Carroll Park",
  "Rick Tollefson Trail",
  "Chimacum Creek",
  "Irondale Beach Park",
  "Port Hadlock--Ajax Cafe/NW School Boatbuilding",
  "Hadlock Bay Road",
  "Oak Bay County Park",
  "Oak Bay--Lagoon Trail",
  "Lagoon Beach",
  "Lagoon Trail Park",
  "Indian/Marrowstone Island isthmus",
  "Nordland",
  "East Beach Park, Marrowstone Island",
  "Mystery Bay SP",
  "Fort Flagler SP - Campground/Beach",
  "Fort Flagler SP",
  "Fort Flagler SP -- Marrowstone Point",
  "Mats Mats Bay",
  "Port Ludlow",
  "Resort at Port Ludlow",
  "Hood Canal--Paradise Bay",
  "Hood Canal--Squamish Harbor",
  "Shine Tidelands",
  "Irondale Chimacum Creek Bay Park",
  "Bywater Bay Spit & Tidelands"]

kitsapPeninsula :: [String]
kitsapPeninsula = [
  "Salsbury Point Park",
  "Kitsap Memorial SP",
  "Port Gamble",
  "Port Gamble Forest Heritage Park",
  "Port Gamble Bay",
  "Foulweather Bluff",
  "Nature Conservancy Foulweather Bluff Preserve",
  "Driftwood Key",
  "Hansville Greenway--Lower Hawk's Pond Viewing Platform",
  "Hansville Greenway",
  "Buck Lake",
  "Hansville",
  "Norwegian Point",
  "Point No Point",
  "Port of Eglon",
  "North Kitsap Heritage Park",
  "Arness Roadside Park, Kingston",
  "Kingston -- Village Green Comm Park",
  "Kingston Ferry Terminal",
  "Appletree Cove",
  "Apple Cove Point"]

whidbeyIsland :: [String]
whidbeyIsland = []
