{-|
Module      : Types
Description : Data types for the application
Copyright   : (c) Author name here, 2025
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental
Portability : POSIX

This module defines the core data structures used throughout the application,
mirroring the TfL API response structure.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | Data type representing a TfL Line
-- Based on https://api.tfl.gov.uk/Line/Mode/tube/Status
data Line = Line
    { id :: Text
    , name :: Text
    , modeName :: Text
    , created :: Maybe Text
    , modified :: Maybe Text
    , lineStatuses :: [LineStatus]
    } deriving (Show, Generic)

-- | This is aData type representing the status of a line
data LineStatus = LineStatus
    { statusId :: Int
    , statusSeverity :: Int
    , statusSeverityDescription :: Text
    , reason :: Maybe Text
    } deriving (Show, Generic)

-- | This is aData type representing a Station (StopPoint)
data Station = Station
    { stationId :: Text
    , commonName :: Text
    , lat :: Double
    , lon :: Double
    } deriving (Show, Generic)

-- | This is aData type representing a Journey Response
data JourneyResponse = JourneyResponse
    { journeys :: [Journey]
    } deriving (Show, Generic)

-- | This is a Data type representing a single Journey option
data Journey = Journey
    { startDateTime :: Text
    , duration :: Int
    , arrivalDateTime :: Text
    , legs :: [Leg]
    } deriving (Show, Generic)

-- | This is a Data type representing a Leg of a journey
data Leg = Leg
    { legDuration :: Int
    , instruction :: Instruction
    , mode :: Mode
    , departurePoint :: Point
    , arrivalPoint :: Point
    } deriving (Show, Generic)

-- | This is a Data type representing an instruction for a leg
data Instruction = Instruction
    { summary :: Text
    , detailed :: Maybe Text
    } deriving (Show, Generic)

-- | This is a Data type representing a transport mode
data Mode = Mode
    { modeId :: Text
    , mName :: Text
    } deriving (Show, Generic)

-- | This is a Data type representing a point (departure/arrival)
data Point = Point
    { pointName :: Text
    } deriving (Show, Generic)
