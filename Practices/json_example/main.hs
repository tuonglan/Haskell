{-# LANGUAGE OverloadedStrings #-}

import GHC.Generics
import Data.Text
import Data.Aeson
import Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)


data Person = Person
    { firstName :: !String
    , lastName  :: !String
    , age       :: Int
    , likesPizza:: Bool
    } deriving (Show, Generic)

instance FromJSON Person
instance ToJSON Person

getJSON :: IO ByteString
getJSON = B.readFile "ex.json"

