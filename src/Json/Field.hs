{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Json.Field where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)

data Field = Field {
      name :: String
    , fieldType :: String
    , accessFlags :: [String]
    }
    deriving (Show, Eq, Generic)

instance FromJSON Field
