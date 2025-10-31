{-# LANGUAGE DeriveGeneric #-}

module Json where

import            Data.Aeson                                  (ToJSON, encode)
import            GHC.Generics                                (Generic)
import            Data.ByteString.Lazy                        (ByteString)



data Country = Country
  {
    cname      :: String,
    continent :: String,
    ctag       :: Int
  } deriving (Generic, Show)

instance ToJSON Country


ecuador = Country "Ecuador" "South America" 1
germany = Country "Germany" "Europe" 2
japan = Country "Japan" "Asia" 4
unitedStates = Country "United States" "North America" 3

countries :: [Country]
countries = [ecuador,germany,japan, unitedStates]

encodeCountries :: [Country] -> ByteString
encodeCountries = encode
