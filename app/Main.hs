{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Control.Exception (catch, SomeException)
import Control.Monad (forM_)
import Data.List (sortBy, tails, isPrefixOf)
import Data.Ord (comparing)
import Data.Char (toLower)
import System.Exit (exitSuccess)

import Fetch (downloadData, fetchStations, fetchJourney)
import Parse (parseLines, writeJson, parseStations, parseJourney)
import Database (initialiseDB, saveData, retrieveData, saveStations, searchStations, getSevereDelays, queryLinesBySeverity)
import Types (Line(..), Station(..), LineStatus(..), JourneyResponse(..), Journey(..), Leg(..), Instruction(..), Mode(..), Point(..))

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["create"]           -> handleCreate
        ["loaddata"]         -> handleLoadData
        ["dumpdata"]         -> handleDumpData
        ("filter-status":as) -> handleFilterStatus as
        ["severe-delays"]    -> handleSevereDelays
        ["search", queryStr] -> handleSearch queryStr
        ["plan-journey"]     -> handlePlanJourney
        _                    -> printUsage

-- | Print usage information
printUsage :: IO ()
printUsage = putStrLn "Usage: stack run -- [create|loaddata|dumpdata|filter-status <code >|plan-journey]"

-- | Create the database tables
handleCreate :: IO ()
handleCreate = withErrorHandling "Error creating database" $ do
    putStrLn "Started Creating the Database"
    initialiseDB
    putStrLn "Database created."

-- | Download and save line/station data
handleLoadData :: IO ()
handleLoadData = withErrorHandling "Error while loading data" $ do
    putStrLn "Downloading data..."
    json <- downloadData
    putStrLn "Parsing the data for the lines"
    case parseLines json of
        Left err -> putStrLn $ "Error parsing JSON: " ++ err
        Right lines -> do
            putStrLn $ "Saving " ++ show (length lines) ++ " lines to database..."
            saveData lines
            
            putStrLn "Fetching the stations details for each line -----"
            forM_ lines $ \line -> do
                putStrLn $ "Fetching the stations details for " ++ show (Types.name line)
                stationJson <- fetchStations (Types.id line)
                case parseStations stationJson of
                    Left err -> putStrLn $ "Getting Error while parsing stations for line " ++ show (Types.name line) ++ ": " ++ err
                    Right stations -> do
                        saveStations (Types.id line) stations
                        putStrLn $ "Saved " ++ show (length stations) ++ " stations."
                        
            putStrLn "Data saved."

-- | Dump database content to JSON file
handleDumpData :: IO ()
handleDumpData = withErrorHandling "Getting Error while dumping data" $ do
    putStrLn "Dumping data to data.json..."
    lines <- retrieveData
    writeJson "data.json" lines
    putStrLn "Data dumped."

-- | Filter lines by status severity
handleFilterStatus :: [String] -> IO ()
handleFilterStatus qArgs = do
    severity <- case qArgs of
        [severityStr] -> case readMaybe severityStr of
            Just s -> return $ Just s
            Nothing -> do
                putStrLn "Invalid severity code. Please provide an integer."
                return Nothing
        [] -> showFilterMenu
        _ -> do
             putStrLn "Usage: stack run -- filter-status [severity_code]"
             return Nothing

    case severity of
        Just sev -> do
            results <- queryLinesBySeverity sev
            if null results
                then putStrLn $ "No lines found with severity " ++ show sev ++ "."
                else do
                    putStrLn $ "Lines with severity " ++ show sev ++ ":"
                    mapM_ (\(name, s) -> putStrLn $ "  - " ++ T.unpack name ++ ": " ++ show (Types.statusSeverityDescription s) ++ " (" ++ show (severityReason s) ++ ")") results
        Nothing -> return ()
  where
    severityReason s = maybe "No Reason" Prelude.id (Types.reason s)

    showFilterMenu :: IO (Maybe Int)
    showFilterMenu = do
        putStrLn "Select a status to filter by:"
        putStrLn "1. Good Service (10)"
        putStrLn "2. Minor Delays (9)"
        putStrLn "3. Severe Delays (6)"
        putStrLn "4. Part Closure (5)"
        putStrLn "5. Planned Closure (4)"
        putStrLn "6. Service Closed (20)"
        putStrLn "7. Enter Custom Code"
        putStr "Selection: "
        hFlush stdout
        input <- getLine
        case input of
            "1" -> return $ Just 10
            "2" -> return $ Just 9
            "3" -> return $ Just 6
            "4" -> return $ Just 5
            "5" -> return $ Just 4
            "6" -> return $ Just 20
            "7" -> do
                putStr "Enter severity code: "
                hFlush stdout
                code <- getLine
                case readMaybe code of
                    Just c -> return $ Just c
                    Nothing -> do
                        putStrLn "Invalid code."
                        return Nothing
            _ -> do
                putStrLn "Invalid selection."
                return Nothing

-- | Check for severe delays
handleSevereDelays :: IO ()
handleSevereDelays = withErrorHandling "Getting Error while checking delays" $ do
    putStrLn "Checking for severe delays..."
    delays <- getSevereDelays
    if null delays
        then putStrLn "There is no severe delays found.The service is good on all lines!"
        else mapM_ (\s -> putStrLn $ show (Types.statusSeverityDescription s) ++ ": " ++ show (Types.reason s)) delays

-- | Search for a station by name
handleSearch :: String -> IO ()
handleSearch queryStr = withErrorHandling "Getting Error while searching stations" $ do
    putStrLn $ "Searching for stations matching: " ++ queryStr
    results <- searchStations queryStr
    mapM_ (\s -> putStrLn $ show (Types.commonName s) ++ " (" ++ show (Types.stationId s) ++ ")") results

-- | Interactive journey planner
handlePlanJourney :: IO ()
handlePlanJourney = withErrorHandling "Getting Error while planning journey" $ do
    -- 1. Get Start Station
    putStr "From (or 'exit' to quit): "
    hFlush stdout
    fromInput <- getLine
    checkExit fromInput
    from <- resolveStation fromInput
    
    -- 2. Get Destination Station
    putStr "To (or 'exit' to quit): "
    hFlush stdout
    toInput <- getLine
    checkExit toInput
    to <- resolveStation toInput
    
    -- 3. Get User Preferences
    modes <- selectModes
    preference <- selectPreference
    
    -- 4. Fetch and Display Results
    putStrLn "Fetching journey options..."
    json <- fetchJourney from to modes preference
    case parseJourney json of
        Left err -> putStrLn $ "Getting Error while parsing journey data: " ++ err
        Right (JourneyResponse journeys) -> do
            let sortedJourneys = sortJourneys preference journeys
            putStrLn $ "Found " ++ show (length sortedJourneys) ++ " options:--"
            
            -- Display options
            forM_ (zip [1..] sortedJourneys) $ \(i, journey) -> do
                let modes = map (Types.mName . Types.mode) (Types.legs journey)
                let modeStr = T.intercalate (T.pack ", ") modes
                putStrLn $ show i ++ ". Duration: " ++ show (Types.duration journey) ++ " min (" ++ T.unpack modeStr ++ ")"
                putStrLn $ "   Start: " ++ show (Types.startDateTime journey) ++ ", Arrival: " ++ show (Types.arrivalDateTime journey)
            
            -- 5. User Selection for Details
            putStr "Select an option (number) or 'exit': "
            hFlush stdout
            selection <- getLine
            checkExit selection
            case readMaybe selection of
                Just n | n > 0 && n <= length sortedJourneys -> do
                    let selectedJourney = sortedJourneys !! (n - 1)
                    putStrLn "Journey Details:"
                    forM_ (Types.legs selectedJourney) $ \leg -> do
                        putStrLn $ "  - " ++ show (Types.legDuration leg) ++ " min (" ++ show (Types.mName $ Types.mode leg) ++ ")"
                        putStrLn $ "    From: " ++ show (Types.pointName $ Types.departurePoint leg)
                        putStrLn $ "    To:   " ++ show (Types.pointName $ Types.arrivalPoint leg)
                        putStrLn $ "    " ++ show (Types.summary $ Types.instruction leg)
                _ -> putStrLn "Invalid selection."

withErrorHandling :: String -> IO () -> IO ()
withErrorHandling msg action = catch action handler
  where
    handler :: SomeException -> IO ()
    handler e
        | show e == "ExitSuccess" = exitSuccess
        | "StatusCodeException" `isInfixOf` show e = putStrLn $ msg ++ ": Invalid Station ID or API Error (404/400). Please check your station selection."
        | "HttpExceptionRequest" `isInfixOf` show e = putStrLn $ msg ++ ": No Internet Connection or API is down."
        | "ConnectionFailure" `isInfixOf` show e = putStrLn $ msg ++ ": No Internet Connection."
        | "SQLite" `isInfixOf` show e = putStrLn $ msg ++ ": Database error."
        | otherwise = putStrLn $ msg ++ ": " ++ show e

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)



checkExit :: String -> IO ()
checkExit input = do
    let lowerInput = map toLower input
    if lowerInput == "exit" || lowerInput == "quit"
        then do
            putStrLn "Goodbye!"
            exitSuccess
        else return ()

resolveStation :: String -> IO String
resolveStation input = do
    results <- searchStations input
    let sortedResults = sortBy (comparing Types.commonName) results
    
    if length sortedResults > 25
        then do
            putStrLn $ "Found " ++ show (length sortedResults) ++ " stations matching \"" ++ input ++ "\"."
            putStr "Too many results. Show all? (y/n) or 'exit': "
            hFlush stdout
            choice <- getLine
            checkExit choice
            if choice == "y" || choice == "yes"
                then showStationList sortedResults input
                else do
                    putStrLn "Please refine your search."
                    putStr "Enter station name (or 'exit'): "
                    hFlush stdout
                    newInput <- getLine
                    checkExit newInput
                    resolveStation newInput
        else showStationList sortedResults input

showStationList :: [Types.Station] -> String -> IO String
showStationList stations originalInput = case stations of
    [station] -> do
        putStrLn $ "Selected station: " ++ show (Types.commonName station)
        return (T.unpack $ Types.stationId station)
    [] -> do
        putStrLn "No matching stations found. Please try again."
        putStr "Enter station name (or 'exit'): "
        hFlush stdout
        newInput <- getLine
        checkExit newInput
        resolveStation newInput
    _ -> do
        putStrLn "Found matching stations:"
        forM_ (zip [1..] stations) $ \(i, station) -> do
            putStrLn $ show i ++ ". " ++ show (Types.commonName station) ++ " (" ++ show (Types.stationId station) ++ ")"
        
        putStr "Select a station (number) or 'exit': "
        hFlush stdout
        selection <- getLine
        checkExit selection
        case readMaybe selection of
            Just n | n > 0 && n <= length stations -> do
                let selected = stations !! (n - 1)
                return (T.unpack $ Types.stationId selected)
            _ -> do
                putStrLn "Invalid selection. Please select a number from the list."
                showStationList stations originalInput

selectModes :: IO (Maybe String)
selectModes = do
    putStrLn "Select Modes (comma separated numbers, e.g. 1,3 or enter for all, 'exit' to quit):"
    putStrLn "1. Tube"
    putStrLn "2. Bus"
    putStrLn "3. DLR"
    putStrLn "4. Overground"
    putStrLn "5. Elizabeth Line"
    putStrLn "6. National Rail"
    putStrLn "7. All Rail Services (Tube, DLR, Overground, Elizabeth Line, National Rail)"
    putStr "Selection: "
    hFlush stdout
    input <- getLine
    checkExit input
    if null input
        then do
            putStrLn "Selected: All Modes"
            return Nothing
        else do
            let selections = map T.strip (T.splitOn (T.pack ",") (T.pack input))
            if all isValidSelection selections
                then do
                    let modes = map mapMode selections
                    putStrLn $ "Selected: " ++ T.unpack (T.intercalate (T.pack ", ") selections)
                    return $ Just (T.unpack $ T.intercalate (T.pack ",") modes)
                else do
                    putStrLn "Invalid selection. Please use numbers 1-7 (comma separated)."
                    selectModes
  where
    isValidSelection :: T.Text -> Bool
    isValidSelection t = t `elem` map (T.pack . show) [1..7]

    mapMode :: T.Text -> T.Text
    mapMode t
        | t == T.pack "1" = T.pack "tube"
        | t == T.pack "2" = T.pack "bus"
        | t == T.pack "3" = T.pack "dlr"
        | t == T.pack "4" = T.pack "overground"
        | t == T.pack "5" = T.pack "elizabeth-line"
        | t == T.pack "6" = T.pack "national-rail"
        | t == T.pack "7" = T.pack "tube,dlr,overground,elizabeth-line,national-rail"
        | otherwise       = t

selectPreference :: IO (Maybe String)
selectPreference = do
    putStrLn "Select Preference (enter number, enter for none, or 'exit'):"
    putStrLn "1. Fastest"
    putStrLn "2. Fewest Changes"
    putStrLn "3. Least Walking"
    putStr "Selection: "
    hFlush stdout
    input <- getLine
    checkExit input
    case input of
        "1" -> do
            putStrLn "Selected: Fastest"
            return $ Just "leasttime"
        "2" -> do
            putStrLn "Selected: Fewest Changes"
            return $ Just "leastinterchange"
        "3" -> do
            putStrLn "Selected: Least Walking"
            return $ Just "leastwalking"
        ""  -> do
            putStrLn "Selected: No Preference"
            return Nothing
        _   -> do
            putStrLn "Invalid input, defaulting to: No Preference"
            return Nothing



sortJourneys :: Maybe String -> [Types.Journey] -> [Types.Journey]
sortJourneys (Just "leasttime") = sortBy (comparing Types.duration)
sortJourneys (Just "leastinterchange") = sortBy (comparing (length . Types.legs))
sortJourneys (Just "leastwalking") = sortBy (comparing walkingTime)
  where
    walkingTime :: Types.Journey -> Int
    walkingTime j = sum [Types.legDuration l | l <- Types.legs j, Types.modeId (Types.mode l) == T.pack "walking"]
sortJourneys _ = Prelude.id
