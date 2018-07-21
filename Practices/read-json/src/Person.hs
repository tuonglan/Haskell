{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Person where

import GHC.Generics
import Data.Aeson
import Data.Text

data Person = Person
    { firstName :: !Text
    , lastName :: !Text
    , age :: Int
    , likesPizza :: Bool
    } deriving (Show, Generic)

instance FromJSON Person
instance ToJSON Person
