{-|
Module      : Fetch
Description : API interaction module
Copyright   : (c) Author name here, 2025
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental
Portability : POSIX

This module handles all HTTP requests to the Transport for London (TfL) API,
including fetching line statuses, stations, and journey plans.
-}
{-# LANGUAGE OverloadedStrings #-}

module Fetch where

import Network.HTTP.Simple
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T

-- | API Credentials
appId :: Text
appId = "27182301add240cba9ce4256feb7ea2a"

appKey :: Text
appKey = "ce0be199e475450ab9d80bc67a0ae261"

-- | URL for Tube Line Status
-- We use the 'tube' mode to get status for all tube lines
url :: String
url = "https://api.tfl.gov.uk/Line/Mode/tube,dlr,overground,elizabeth-line,tram/Status?app_id=" ++ T.unpack appId ++ "&app_key=" ++ T.unpack appKey

-- |Here is a Function to download data from the API
downloadData :: IO LBS.ByteString
downloadData = do
    request <- parseRequest url
    let request' = setRequestCheckStatus request
    response <- httpLBS request'
    return $ getResponseBody response

-- | Here is a Function to download stations for a given line
fetchStations :: Text -> IO LBS.ByteString
fetchStations lineId = do
    let url = "https://api.tfl.gov.uk/Line/" ++ T.unpack lineId ++ "/StopPoints?app_id=" ++ T.unpack appId ++ "&app_key=" ++ T.unpack appKey
    request <- parseRequest url
    let request' = setRequestCheckStatus request
    response <- httpLBS request'
    return $ getResponseBody response

-- |  Here is a Function to fetch journey options
fetchJourney :: String -> String -> Maybe String -> Maybe String -> IO LBS.ByteString
fetchJourney from to modes preference = do
    let baseUrl = "https://api.tfl.gov.uk/Journey/JourneyResults/" ++ from ++ "/to/" ++ to
    let modeParam = maybe "" (\m -> "&mode=" ++ m) modes
    let prefParam = maybe "" (\p -> "&journeyPreference=" ++ p) preference
    let url = baseUrl ++ "?app_id=" ++ T.unpack appId ++ "&app_key=" ++ T.unpack appKey ++ modeParam ++ prefParam
    
    request <- parseRequest url
    let request' = setRequestCheckStatus request
    response <- httpLBS request'
    return $ getResponseBody response
