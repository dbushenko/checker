{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Json.Class where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)

import Json.Field
import Json.Method

data Class = Class {
      className :: String
    , superClassName :: String
    , interfaces :: [String]
    , accessFlags :: [String]
    , fields :: [Field]
    , code :: [Method]
    }
    deriving (Show, Eq, Generic)

instance FromJSON Class
