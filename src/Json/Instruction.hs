{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Json.Instruction where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)

import Json.Constant

data Instruction = Instruction {
      op :: String
    , params :: Constant
    }
    deriving (Show, Eq, Generic)

instance FromJSON Instruction
