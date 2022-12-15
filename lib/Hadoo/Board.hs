{-# LANGUAGE OverloadedStrings #-}

module Hadoo.Board (boardAction) where

import Web.Scotty 
import Control.Monad.IO.Class (liftIO)

import Hadoo.Helpers (htmlString, e, ea, concatArr)
import Hadoo.Lane (laneInit)
import Hadoo.State

boardAction :: ActionM ()
boardAction = do
    str <- liftIO createPage
    htmlString str

createPage :: IO String
createPage = do
    bodyB <- htmlBody
    return $
        "<!DOCTYPE html>" ++
        ea "html" [("lang", "en")] (htmlHead ++ bodyB)


htmlHead :: String
htmlHead = 
  e "head" $
    ea "link" [("rel", "stylesheet"), ("href", "/styles.css")] ""

htmlBody :: IO String
htmlBody = do
    lanes <- mapM laneInit getAllStates
    return $
        e "body" $
            ea "h1" [("class", "body")] "Hadoo" ++ 
            ea "div" [("class", "container")] (concatArr lanes)


