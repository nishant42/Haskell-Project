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
import Database (initialiseDB, saveData, retrieveData, saveStations, searchStations, getSevereDelays)
import Types (Line(..), Station(..), LineStatus(..), JourneyResponse(..), Journey(..), Leg(..), Instruction(..), Mode(..), Point(..))

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["create"] -> withErrorHandling "Error creating database" $ do
            putStrLn "Creating database..."
            initialiseDB
            putStrLn "Database created."
            
        ["loaddata"] -> withErrorHandling "Error loading data" $ do
            putStrLn "Downloading data..."
            json <- downloadData
            putStrLn "Parsing data..."
            case parseLines json of
                Left err -> putStrLn $ "Error parsing JSON: " ++ err
                Right lines -> do
                    putStrLn $ "Saving " ++ show (length lines) ++ " lines to database..."
                    saveData lines
                    
                    -- Fetch stations for each line
                    putStrLn "Fetching stations for each line (this might take a while)..."
                    forM_ lines $ \line -> do
                        putStrLn $ "Fetching stations for " ++ show (Types.name line)
                        stationJson <- fetchStations (Types.id line)
                        case parseStations stationJson of
                            Left err -> putStrLn $ "Error parsing stations for line " ++ show (Types.name line) ++ ": " ++ err
                            Right stations -> do
                                saveStations (Types.id line) stations
                                putStrLn $ "Saved " ++ show (length stations) ++ " stations."
                                
                    putStrLn "Data saved."

        ["dumpdata"] -> withErrorHandling "Error dumping data" $ do
            putStrLn "Dumping data to data.json..."
            lines <- retrieveData
            writeJson "data.json" lines
            putStrLn "Data dumped."
            
        ("query":qArgs) -> do
            putStrLn $ "Running query: " ++ unwords qArgs
            
            putStrLn "Query functionality not yet implemented."

        ["severe-delays"] -> withErrorHandling "Error checking delays" $ do
            putStrLn "Checking for severe delays..."
            delays <- getSevereDelays
            if null delays
                then putStrLn "No severe delays found. Good Service on all lines!"
                else mapM_ (\s -> putStrLn $ show (Types.statusSeverityDescription s) ++ ": " ++ show (Types.reason s)) delays

        ["search", queryStr] -> withErrorHandling "Error searching stations" $ do
            putStrLn $ "Searching for stations matching: " ++ queryStr
            results <- searchStations queryStr
            mapM_ (\s -> putStrLn $ show (Types.commonName s) ++ " (" ++ show (Types.stationId s) ++ ")") results

        ["plan-journey"] -> withErrorHandling "Error planning journey" $ do
            putStr "From (or 'exit' to quit): "
            hFlush stdout
            fromInput <- getLine
            checkExit fromInput
            from <- resolveStation fromInput
            
            putStr "To (or 'exit' to quit): "
            hFlush stdout
            toInput <- getLine
            checkExit toInput
            to <- resolveStation toInput
            
            modes <- selectModes
            preference <- selectPreference
            
            putStrLn "Fetching journey options..."
            json <- fetchJourney from to modes preference
            case parseJourney json of
                Left err -> putStrLn $ "Error parsing journey data: " ++ err
                Right (JourneyResponse journeys) -> do
                    let sortedJourneys = sortJourneys preference journeys
                    putStrLn $ "Found " ++ show (length sortedJourneys) ++ " options:"
                    forM_ (zip [1..] sortedJourneys) $ \(i, journey) -> do
                        let modes = map (Types.mName . Types.mode) (Types.legs journey)
                        let modeStr = T.intercalate (T.pack ", ") modes
                        putStrLn $ show i ++ ". Duration: " ++ show (Types.duration journey) ++ " min (" ++ T.unpack modeStr ++ ")"
                        putStrLn $ "   Start: " ++ show (Types.startDateTime journey) ++ ", Arrival: " ++ show (Types.arrivalDateTime journey)
                    
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

        _ -> putStrLn "Usage: stack run -- [create|loaddata|dumpdata|query <args>|plan-journey]"

withErrorHandling :: String -> IO () -> IO ()
withErrorHandling msg action = catch action handler
  where
    handler :: SomeException -> IO ()
    handler e
        | show e == "ExitSuccess" = exitSuccess
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
    case results of
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

        stations -> do
            putStrLn "Found matching stations:"
            forM_ (zip [1..] stations) $ \(i, station) -> do
                putStrLn $ show i ++ ". " ++ show (Types.commonName station) ++ " (" ++ show (Types.stationId station) ++ ")"
            
            putStr "Select a station (number), press enter to use input as is, or 'exit': "
            hFlush stdout
            selection <- getLine
            checkExit selection
            case readMaybe selection of
                Just n | n > 0 && n <= length stations -> do
                    let selected = stations !! (n - 1)
                    return (T.unpack $ Types.stationId selected)
                _ -> return input

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
        then return Nothing
        else do
            let selections = map T.strip (T.splitOn (T.pack ",") (T.pack input))
            let modes = map mapMode selections
            return $ Just (T.unpack $ T.intercalate (T.pack ",") modes)
  where
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
        "1" -> return $ Just "leasttime"
        "2" -> return $ Just "leastinterchange"
        "3" -> return $ Just "leastwalking"
        ""  -> return Nothing
        _   -> return Nothing



sortJourneys :: Maybe String -> [Types.Journey] -> [Types.Journey]
sortJourneys (Just "leasttime") = sortBy (comparing Types.duration)
sortJourneys (Just "leastinterchange") = sortBy (comparing (length . Types.legs))
sortJourneys (Just "leastwalking") = sortBy (comparing walkingTime)
  where
    walkingTime :: Types.Journey -> Int
    walkingTime j = sum [Types.legDuration l | l <- Types.legs j, Types.modeId (Types.mode l) == T.pack "walking"]
sortJourneys _ = Prelude.id
