{-# LANGUAGE OverloadedStrings #-}

module Hadoo.Persistence (loadItemsRaw, deleteItem, saveItem) where

import Hadoo.State
import System.Directory
import Control.Monad (filterM, when)

import Data.Char (isDigit)
import Text.Printf
import Data.Maybe
import Data.List (sort)


   
loadItemsRaw :: State -> IO [(String, String)]
loadItemsRaw state = mapFunc state

mapFunc :: State -> IO [(String, String)]
mapFunc state = do 
        dir <- initFolder state
        files <- listDirectory dir
        let sorted = sort files
        existingFiles <- filterM (\file -> doesFileExist (dir ++ "/" ++ file)) sorted
        mapM (readFileWithId dir) existingFiles

readFileWithId :: String -> String -> IO (String, String)
readFileWithId dir file = do
        let (id, _) = break (not . isDigit) file
        contents <- readFile (dir ++ "/" ++ file)
        return (id, contents)

initFolder :: State -> IO String
initFolder state = do
        let dir = getDirectoryPath state
        createDirectoryIfMissing True dir
        return dir

getDirectoryPath :: State -> String
getDirectoryPath state = "data/" ++ (show state)

getFilePath :: State -> Int -> String
getFilePath state id = getDirectoryPath state ++  "/" ++ (printf "%03d.txt" id)

deleteItem :: State -> Int -> IO ()
deleteItem state id = do
        let filePath = getFilePath state id
        exists <- doesFileExist filePath
        putStrLn (show exists ++ ":" ++ filePath)
        when exists (removeFile filePath)

saveItem :: Int -> State -> String -> IO ()
saveItem id state content = do
        let filePath = getFilePath state id
        writeFile filePath content
