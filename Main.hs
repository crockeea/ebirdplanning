
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List (intersperse, sortOn, reverse, stripPrefix)
import Data.Map hiding (splitAt,drop,map,take)
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Data.Tuple (swap)
import GHC.IO.Encoding
import Network.HTTP.Conduit
import System.IO

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
  








data Hotspot = HS {region::String, hsName::String, code::String}



neahBay :: [String]
neahBay = ["L267161", "L267164", "L6517534","L800185"] {-, "L1209931","L3076841","L1585910",
           "L3075713","L2818141","L5253818","L1803694","L3075795","L5254015","L1567344",
           "L3853712","L6579451","L3374604","L2180292","L2763900","L6299347","L5056186"]-}

highOlympics :: [String]
highOlympics = ["L4176035","L1004137","L982949","L4167863","L608380","L1482533",
                "L1225268","L967793","L276737","L1777266","L4956910","L3746131",
                "L1512139","L4741791","L4167881","L3761589"]


hotspotToURL :: String -> String
hotspotToURL hid = 
  "https://ebird.org/barchartData?r=" ++ hid ++ 
  "&bmo=1&emo=12&byr=1900&eyr=2018&fmt=tsv"

parseLine :: String -> (String, Double)
parseLine str = 
  -- split the line into words
  let ws = words str
  in if length ws < 49 -- one word name plus 48 data values
     then error $ "line too short!\n\n" ++ str
     else 
       let (birdname,vals) = splitAt (length ws - 48) ws
           bird = concat $ intersperse "-" birdname
           -- read the value for the first week of August
           augVal = read $ vals !! 28
       in (bird,augVal)

createHotspot :: String -> IO (Map String Double)
createHotspot hid = do
  putStrLn $ "Parsing " ++ hotspotToURL hid
  rawData <- C.unpack <$> simpleHttp (hotspotToURL hid)
  -- raw files include 14 lines of cruft
  -- then one blank line
  -- then the number of checklists for each month
  -- followed by the data for each bird
  -- followed by two empty lines
      -- split each line into a separate string,
      -- then drop the first 14 lines and the last two lines
  let rawData' = init $ init $ drop 14 $ lines rawData
      -- skip the second. The rest of the lines
      -- have some string at the left followed by 48 numbers (4 for each month)
      dataLines = head rawData' : drop 2 rawData'
  -- parse each line, dropping unwanted columns
  -- and convert to a map
  return $ fromList $ parseLine <$> dataLines

filterSmallHotspots :: Map String (Map String Double) -> IO [Map String Double]
filterSmallHotspots m = do
  let (small,large) = partition ((<5) . (! "Sample-Size:")) m
      small' = round <$> (! "Sample-Size:") <$> small :: Map String Int
      large' = round <$> (! "Sample-Size:") <$> large :: Map String Int
  putStrLn $ "Dropping " ++ show (length (keys small)) ++ " hotspots: " ++ show small'
  putStrLn $ "Keeping " ++ show (length (keys large)) ++ " hotspots: " ++ show large'
  return $ elems large

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

processAreas :: IO ()
processAreas = do
  neahMap <- processArea  neahBay
  writeFile ("neahbay.txt") $ mapToFile neahMap

  olympicsMap <- processArea  highOlympics
  writeFile ("higholympics.txt") $ mapToFile olympicsMap

--computeTargets :: Map String (Map String Double) -> Map String (Map String Double)
--computeTargets
  -- a target bird is one that
  --   1) doesn't appear on all/half/? of areas
  --   2) has a significantly higher probability in one area than another
  --   3) 


getHotspotMapping :: IO (Map String String)
getHotspotMapping = do
  rawData <- C.unpack <$> simpleHttp
               "https://ebird.org/GuideMe?reportType=location&bMonth=08&bYear=1900&eMonth=08&eYear=2018&parentState=US-WA&countries=US&states=US-WA&getLocations=hotspots&continue.x=31&continue.y=6"
  -- writeFile "idmap.txt" rawData
  let rawData' = lines rawData
      mapLinePrefix = "\t\t\t\t\t\t\t\t<td valign=\"top\"><input type=\"checkbox\" name=\"hotspots\" value=\""
      mapLines = catMaybes $ (stripPrefix mapLinePrefix) <$> rawData'
      parseMapping str = 
        let (lhs,rhs) = span (/= '\"') str
        in (lhs, drop 4 $ take (length rhs - 5) rhs)
      parsedLines = swap <$> parseMapping <$> mapLines
  -- putStrLn $ show parsedLines
  return $ fromList parsedLines

main :: IO ()
main = do
  setLocaleEncoding utf8
  hotspotMap <- getHotspotMapping
  processAreas
  
