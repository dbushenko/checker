module Lib
    ( application
    ) where

import Prelude hiding (readFile)
import System.Environment
import System.Directory
import Data.ByteString.Lazy (ByteString, readFile)
import Data.Aeson (eitherDecode)
import Control.Monad
import System.FilePath.Posix
import Data.String.Utils hiding (join)

import Json.Class
import qualified Json.Field as F
import Props

-- containsString :: String -> [String] -> Bool
-- containsString _ [] = False
-- containsString s (str : strs) =
--     if s == str
--         then True
--         else containsString s strs

-- containsFinal :: [String] -> Bool
-- containsFinal = containsString "final"

-- containsStatic :: [String] -> Bool
-- containsStatic = containsString "static"

-- allFieldsFinal :: [F.Field] -> Bool
-- allFieldsFinal flds =
--     case flds of
--         [] -> False
--         _ -> all (containsFinal . F.accessFlags) flds

-- allFieldStatic :: [F.Field] -> Bool
-- allFieldStatic flds =
--     case flds of
--         [] -> False
--         _ -> all (containsStatic . F.accessFlags) flds

------------------------------------------------------

readJsonFile :: FilePath -> IO (Maybe Class)
readJsonFile fname = do
    contents <- readFile fname :: IO ByteString
    case eitherDecode contents :: Either String Class of
        Left err -> putStrLn fname >> putStrLn err >> return Nothing
        Right cl -> return $ Just cl


processDir :: FilePath -> IO ()
processDir dname = do
    dir <- getDirectoryContents dname
    let files = filter (not . (startswith ".") ) dir
    klasses <- forM files (\fname -> readJsonFile $ dname </> fname)
    let wrappers = processClasses klasses
    forM_ wrappers printClassWrapperInfo
    return ()
    

-- checkIfFinal :: String -> Class -> IO ()
-- checkIfFinal fname klass =
--     let allFinal = allFieldsFinal $ fields klass
--         allStatic = allFieldStatic $ fields klass
--     in case (allFinal, allStatic) of
--             (True, False) -> putStrLn $ fname ++ ", final"
--             (True, True)  -> putStrLn $ fname ++ ", static/final"
--             _ -> return ()

-- readJsonFile :: FilePath -> IO ()
-- readJsonFile fname = do
--     contents <- readFile fname :: IO ByteString
--     case eitherDecode contents :: Either String Class of
--         Left err -> putStrLn fname >> putStrLn err
--         Right cl -> checkIfFinal fname cl

-- processDir :: FilePath -> IO ()
-- processDir dname = do
--     dir <- getDirectoryContents dname
--     forM_ (filter (\s -> not (startswith "." s)) dir)
--           (\fname -> readJsonFile $ dname </> fname)
    

application :: IO ()
application = do
    args <- getArgs
    case args of
        [dname] -> processDir dname
        _ -> error "Wrong command line arguments"
