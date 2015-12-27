{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Json.Descriptor where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)

data Descriptor = Descriptor {
      returnType :: String
    , parameterTypes :: [String]
    }
    deriving (Show, Eq, Generic)

instance FromJSON Descriptor
