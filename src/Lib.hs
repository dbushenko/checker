module Lib
    ( application
    ) where

import Prelude hiding (readFile)
import System.Environment
import Data.ByteString.Lazy (ByteString, readFile)
import Data.Aeson (eitherDecode)

import Json.Class

readJsonFile :: FilePath -> IO ()
readJsonFile fname = do
    contents <- readFile fname :: IO ByteString
    case eitherDecode contents :: Either String Class of
        Left err -> putStrLn err
        Right cl -> print cl

application :: IO ()
application = do
    args <- getArgs
    case args of
        [fname] -> readJsonFile fname
        _ -> error "Wrong command line arguments"
