{-|
Module      : Database
Description : SQLite database interaction module
Copyright   : (c) Author name here, 2025
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental
Portability : POSIX

This module manages the local SQLite database, including creating tables,
saving data, and querying stations and delays.
-}
{-# LANGUAGE OverloadedStrings #-}

module Database where

import Database.SQLite.Simple
import Types
import Data.Text (Text)
import qualified Data.Text as T

-- | Initialize the database and create tables
initialiseDB :: IO ()
initialiseDB = do
    conn <- open "tfl.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS lines (id TEXT PRIMARY KEY, name TEXT, modeName TEXT, statusSeverityDescription TEXT)"
    -- We are simplifying for now, just storing main line info and current status desc
    -- In a real app we need a separate table for statuses with foreign key
    -- We need to remove PRIMARY KEY from id because it's not unique across lines (e.g. 0)
    execute_ conn "CREATE TABLE IF NOT EXISTS statuses (id INTEGER, lineId TEXT, severity INTEGER, description TEXT, reason TEXT, FOREIGN KEY(lineId) REFERENCES lines(id))"
    execute_ conn "CREATE TABLE IF NOT EXISTS stations (id TEXT PRIMARY KEY, commonName TEXT, lat REAL, lon REAL)"
    execute_ conn "CREATE TABLE IF NOT EXISTS line_stations (lineId TEXT, stationId TEXT, FOREIGN KEY(lineId) REFERENCES lines(id), FOREIGN KEY(stationId) REFERENCES stations(id))"
    close conn

-- | Save data to the database
saveData :: [Line] -> IO ()
saveData lines = do
    conn <- open "tfl.db"
    -- Clear existing data for fresh load? Or append?
    -- Task says "download data ... and save to database". Usually implies refreshing or adding.
    -- Let's clear for simplicity to avoid duplicates if we run it multiple times, or use INSERT OR REPLACE.
    execute_ conn "DELETE FROM statuses"
    execute_ conn "DELETE FROM lines"
  
    
    mapM_ (insertLine conn) lines
    close conn

-- | Save stations to the database
saveStations :: Text -> [Station] -> IO ()
saveStations lineId stations = do
    conn <- open "tfl.db"
    mapM_ (insertStation conn lineId) stations
    close conn

insertLine :: Connection -> Line -> IO ()
insertLine conn line = do
    execute conn "INSERT INTO lines (id, name, modeName, statusSeverityDescription) VALUES (?, ?, ?, ?)"
        (Types.id line, Types.name line, Types.modeName line, mainStatusDesc)
    
    mapM_ (insertStatus conn (Types.id line)) (Types.lineStatuses line)
  where
    -- Just take the first status description for the summary, or "Good Service"
    mainStatusDesc :: Text
    mainStatusDesc = case Types.lineStatuses line of
        (s:_) -> Types.statusSeverityDescription s
        []    -> "Unknown"

insertStatus :: Connection -> Text -> LineStatus -> IO ()
insertStatus conn lineId status = do
    execute conn "INSERT INTO statuses (id, lineId, severity, description, reason) VALUES (?, ?, ?, ?, ?)"
        (Types.statusId status, lineId, Types.statusSeverity status, Types.statusSeverityDescription status, Types.reason status)

insertStation :: Connection -> Text -> Station -> IO ()
insertStation conn lineId station = do
    execute conn "INSERT OR IGNORE INTO stations (id, commonName, lat, lon) VALUES (?, ?, ?, ?)"
        (Types.stationId station, Types.commonName station, Types.lat station, Types.lon station)
    execute conn "INSERT OR IGNORE INTO line_stations (lineId, stationId) VALUES (?, ?)"
        (lineId, Types.stationId station)

-- | Searching stations by name
searchStations :: String -> IO [Station]
searchStations queryStr = do
    conn <- open "tfl.db"
    results <- query conn "SELECT id, commonName, lat, lon FROM stations WHERE commonName LIKE ?" (Only ("%" ++ queryStr ++ "%")) :: IO [(Text, Text, Double, Double)]
    close conn
    return $ map (\(sid, name, lat, lon) -> Station sid name lat lon) results

-- | Get lines with severe delays (severity < 10, assuming 10 is Good Service)
-- Note: TfL severity: 10 is Good Service. Lower is usually worse (e.g. 1 is Closed).
getSevereDelays :: IO [LineStatus]
getSevereDelays = do
    conn <- open "tfl.db"
    results <- query_ conn "SELECT id, lineId, severity, description, reason FROM statuses WHERE severity < 10" :: IO [(Int, Text, Int, Text, Maybe Text)]
    close conn
    return $ map (\(sid, lid, sev, desc, reas) -> LineStatus sid sev desc reas) results

-- | Now we are Retrieving data from the database
retrieveData :: IO [Line]
retrieveData = do
    conn <- open "tfl.db"
    -- For now,  we are just implementing a basic retrieval or return empty to satisfy the type signature.
    -- We need to query lines and then for each line query statuses.
    lineRows <- query_ conn "SELECT id, name, modeName, statusSeverityDescription FROM lines" :: IO [(Text, Text, Text, Text)]
    
    lines <- mapM (\(lid, lname, lmode, _) -> do
        statusRows <- query conn "SELECT id, severity, description, reason FROM statuses WHERE lineId = ?" (Only lid) :: IO [(Int, Int, Text, Maybe Text)]
        let statuses = map (\(sid, ssev, sdesc, sreason) -> LineStatus sid ssev sdesc sreason) statusRows
        return $ Line lid lname lmode Nothing Nothing statuses
        ) lineRows
        
    close conn
    return lines
