{-# LANGUAGE RecordWildCards #-}

module Data (nameMapping, readHistogram, Hotspot(..), hotspots, gaBirds) where

import qualified Codec.Binary.UTF8.String as U
import qualified Data.ByteString.Lazy as C
import Data.List (intersperse, sortOn, reverse, stripPrefix)
import Data.Map.Strict hiding (drop, splitAt, take, map)
import Data.Maybe (catMaybes, fromJust)
import Data.Tuple (swap)
import Network.HTTP.Conduit
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix (pathSeparator)
import Text.Read

-- a hotspot has a (custom-defined) region, name, and code
data Hotspot = HS {region::String, hsName::String, code::String}
  deriving (Show,Ord,Eq)

pathSep :: String
pathSep = [pathSeparator]

-- mapping of human-readable hotspot names to ebird hotspot IDs
nameMapping :: IO (Map String String)
nameMapping =
  let dir = "data"
      filePath = "idmap.txt"
      url = "https://ebird.org/GuideMe?reportType=location&bMonth" ++
            "=08&bYear=1900&eMonth=08&eYear=2018&parentState=US-WA" ++
            "&countries=US&states=US-WA&getLocations=hotspots&" ++ 
            "continue.x=31&continue.y=6"
  in readOrDownload dir filePath parseHotspotMapping url

-- parse the mapping file
parseHotspotMapping :: String -> Map String String
parseHotspotMapping rawData =
  let rawData' = lines rawData
      mapLinePrefix = "\t\t\t\t\t\t\t\t<td valign=\"top\"><input type=\"checkbox\" name=\"hotspots\" value=\""
      mapLines = catMaybes $ (stripPrefix mapLinePrefix) <$> rawData'
      parseMapping str = 
        let (lhs,rhs) = span (/= '\"') str
        in (lhs, drop 4 $ take (length rhs - 6) rhs)
      parsedLines = swap <$> parseMapping <$> mapLines
  in fromList parsedLines

-- 
readHistogram :: Int -> Hotspot -> IO (Int, Map String Double)
readHistogram weekID HS{..} = do
  let dir = "data" ++ pathSep ++ region
      filePath = code ++ ".txt"
      url = "https://ebird.org/barchartData?r=" ++ code ++ 
            "&bmo=1&emo=12&byr=1900&eyr=2018&fmt=tsv"
  --putStrLn dir
  readOrDownload dir filePath (parseHistogram weekID) url

readOrDownload :: String -> String -> (String -> a) -> String -> IO a
readOrDownload dir filePath parse url = do
  let fullPath = dir ++ pathSep ++ filePath
  fexists <- doesFileExist fullPath
  parse <$> 
    if fexists
    then readFile fullPath
    else do
      putStrLn $ "Downloading " ++ filePath
      rawData <- U.decode <$> C.unpack <$> simpleHttp url
      createDirectoryIfMissing True dir
      writeFile fullPath rawData
      return rawData

-- takes the ID of the week to get data for.
-- returns the number of checklists recorded in the given week for this hotspot,
-- and a map from bird names to probability of that bird occuring on a checklist during that week
parseHistogram :: Int -> String -> (Int, Map String Double)
parseHistogram weekID rawData = 
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
      checklistCount = snd $ fromJust $ parseHistLine weekID $ head rawData'
      dataLines = drop 2 rawData'
  -- parse each line, dropping unwanted columns
  -- and convert to a map
  in (round checklistCount, fromList $ catMaybes $ parseHistLine weekID <$> dataLines)

-- parse one (data) line of a histogram
-- the probability is the maximum of a three week period for the weeks surrounding the target week
-- and we drop any lines corresponding to a 'sp.' bird or '/' bird (like western/Glaucous-winged gull)
parseHistLine :: Int -> String -> Maybe (String, Double)
parseHistLine weekID str = 
  -- split the line into words
  let ws = words str
  in if length ws < 49 -- one word name plus 48 data values
     then error $ "line too short!\n\n" ++ str
     else 
       let (birdname,vals) = splitAt (length ws - 48) ws
           bird = concat $ intersperse " " birdname
           -- read the value for the specified week
           augVal = case mapM readMaybe $ (vals !!) <$> [weekID-1,weekID,weekID+1] of
                      Nothing -> error $ str
                      Just x -> maximum x
       in if last birdname == "sp." || elem '/' bird
          then Nothing
          else Just (bird,augVal)

hotspots :: IO [Hotspot]
hotspots = do
  nameMap <- nameMapping
  let nameToHS region hsName = 
        if member hsName nameMap
        then HS{code=nameMap!hsName,..}
        else error hsName
      regionList = [
       ("Neah Bay", neahBay),
       ("Port Angeles", portAngeles),
       ("Lake Crescent", lakeCrescent),
       ("High Olympics", highOlympics),
       ("Port Townsend", portTownsend),
       ("Kitsap Peninsula", kitsapPeninsula),
       ("Whidbey Island", whidbeyIsland),
       ("Discovery Bay", discoveryBay),
       ("Dungeness", dungeness)]
  return $ concatMap (\(str,lst) -> nameToHS str <$> lst) regionList

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
whidbeyIsland = [
  "Port Townsend-Keystone Ferry (Jefferson Co.)",
  "Port Townsend-Keystone Ferry (Island Co.)",
  "Port Townsend ferry terminal",
  "Keystone Ferry Landing",
  "Fort Casey State Park",
  "Crockett Lake",
  "Admiralty Bay",
  "Whidbey Island--Keystone Spit Rake Station",
  "Ebey's Landing NHP--Pacific Rim Institute",
  "Rhododendrom Park",
  "Coupeville",
  "Ebey's Landing",
  "Robert Pratt Preserve-Nature Conservancy",
  "Ebey's Landing NHP--Fort Ebey--Gun Battery",
  "Fort Ebey State Park",
  "Libbey Beach County Park",
  "Penn Cove",
  "Penn Cove--Grasser's Lagoon",
  "Hastie Lake Rd beach access",
  "Del Fairfax Preserve (WCLT)",
  "Swantown / Bos Lake",
  "Joseph Whidbey SP",
  "Freund Marsh",
  "Oak Harbor",
  "Oak Harbor Beach Park",
  "Flintstone Park",
  "Oak Harbor Marina",
  "Dugualla Bay",
  "Ala Spit",
  "Deception Pass SP --  Cornet Bay/Hoypus Hill Unit",
  "Deception Pass SP--Trail Cornet Bay around Goose Rock to bridge",
  "Cornet Bay",
  "Deception Pass SP -- Cranberry Lake",
  "Deception Pass SP",
  "Deception Pass SP -- West Beach",
  "Deception Pass SP--Dunes Interpretive Trail at West Beach",
  "Deception Pass SP -- North Beach",
  "Deception Pass SP-- Pass Island",
  "Rosario Beach / Rosario Head",
  "Deception Pass SP -- Bowman Bay",
  "Pass Lake",
  "Lake Campbell",
  "Fidalgo Island--South Lake Campbell Road",
  "Donnell Road",
  "Sharpe County Park, near Anacortes, WA"]

gaBirds :: [String]
gaBirds =
 ["American Coot",
  "American Crow",
  "American Goldfinch",
  "American Kestrel",
  "American Pipit",
  "American Robin",
  "American Wigeon",
  "Bald Eagle",
  "Barn Swallow",
  "Barred Owl",
  "Belted Kingfisher",
  "Blue-winged Teal",
  "Brewer's Blackbird",
  "Brown Creeper",
  "Brown-headed Cowbird",
  "Bufflehead",
  "Canada Goose",
  "Cedar Waxwing",
  "Chipping Sparrow",
  "Common Loon",
  "Common Nighthawk",
  "Common Yellowthroat",
  "Cooper's Hawk",
  "Dark-eyed Junco",
  "Domestic goose sp. (Domestic type)",
  "Double-crested Cormorant",
  "Downy Woodpecker",
  "Eurasian Collared-Dove",
  "European Starling",
  "Gadwall",
  "Golden-crowned Kinglet",
  "Great Blue Heron",
  "Great Egret",
  "Great Horned Owl",
  "Green Heron",
  "Green-winged Teal",
  "Hairy Woodpecker",
  "Hermit Thrush",
  "Hooded Merganser",
  "Horned Grebe",
  "Horned Lark",
  "House Finch",
  "House Sparrow",
  "House Wren",
  "Killdeer",
  "Lesser Scaup",
  "Mallard",
  "Mallard (Domestic type)",
  "Merlin",
  "Mourning Dove",
  "Muscovy Duck (Domestic type)",
  "Northern Flicker",
  "Northern Harrier",
  "Northern Pintail",
  "Northern Rough-winged Swallow",
  "Northern Shoveler",
  "Orange-crowned Warbler",
  "Osprey",
  "Peregrine Falcon",
  "Pied-billed Grebe",
  "Pileated Woodpecker",
  "Pine Siskin",
  "Purple Finch",
  "Purple Martin",
  "Red-breasted Nuthatch",
  "Red-tailed Hawk",
  "Red-winged Blackbird",
  "Redhead",
  "Ring-billed Gull",
  "Ring-necked Duck",
  "Rock Pigeon",
  "Ruby-crowned Kinglet",
  "Ruddy Duck",
  "Savannah Sparrow",
  "Sharp-shinned Hawk",
  "Song Sparrow",
  "Sora",
  "Spotted Sandpiper",
  "Tree Swallow",
  "Turkey Vulture",
  "Virginia Rail",
  "Warbling Vireo",
  "White-crowned Sparrow",
  "Wilson's Snipe",
  "Wood Duck",
  "Yellow Warbler",
  "Yellow-rumped Warbler"]