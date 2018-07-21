{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module ConvertTypes where

import Data.Text
import Data.Aeson
import GHC.Generics

data Info = Info
    { timestamp :: Int
    , rate :: Double
    } deriving (Generic, Show)

data Query = Query
    { from :: Text
    , to :: Text
    , amount :: Double
    } deriving (Generic, Show)

data Convert = Convert
    { success :: Bool
    , query :: Query
    , info :: Info
    , historical :: Maybe Text
    , date :: Text
    , result :: Double
    } deriving (Generic, Show)


instance FromJSON Info
instance ToJSON Info
instance FromJSON Query
instance ToJSON Query
instance FromJSON Convert
instance ToJSON Convert



