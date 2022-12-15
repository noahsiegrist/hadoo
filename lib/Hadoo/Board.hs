{-# LANGUAGE OverloadedStrings #-}

module Hadoo.Board (boardAction) where

import Web.Scotty 
import Control.Monad.IO.Class (liftIO)

import Hadoo.Helpers (createPage, htmlString, e, ea, concatArr)
import Hadoo.Lane (laneInit)
import Hadoo.State

boardAction :: ActionM ()
boardAction = do
    lanes <- liftIO $ mapM laneInit getAllStates
    str <- liftIO $ createPage $ 
        ea "form" [("method", "get"), ("action", "items/new"), ("class", "inline")] (
            ea "button" [("type", "submit")] "New"
        )
        ++ 
        ea "div" [("class", "container")] (concatArr lanes)
    htmlString str
