{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Json.Method where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)

import Json.Instruction
import Json.Constant
import Json.Descriptor


data Method = Method {
      name :: String
    , descriptor :: Descriptor
    , accessFlags :: [String]
    , instructions :: [Instruction]
    }
    deriving (Show, Eq, Generic)

instance FromJSON Method
