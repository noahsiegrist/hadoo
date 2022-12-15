{-# LANGUAGE OverloadedStrings #-}

module Hadoo.Persistence (loadItemsRaw) where

import Hadoo.State
import System.Directory
import Control.Monad (filterM)

import Data.Char (isDigit)
import Data.Maybe


   
loadItemsRaw :: State -> IO [(String, String)]
loadItemsRaw state = mapFunc state

mapFunc :: State -> IO [(String, String)]
mapFunc state = do 
        dir <- initFolder state
        files <- listDirectory dir
        existingFiles <- filterM (\file -> doesFileExist (dir ++ "/" ++ file)) files
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


