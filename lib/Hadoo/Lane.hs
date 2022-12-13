{-# LANGUAGE OverloadedStrings #-}

module Hadoo.Lane (laneInit) where

import Web.Scotty 

import Hadoo.Helpers (htmlString, e, ea)
import Hadoo.State

laneInit :: State -> String
laneInit state = ea "div" [("class", "lane")] (
        ea "div" [("class", "title")] (show state)
        
        )
    
item :: Int -> String -> String
item id content = ea "div" [("class", "item")] content
    
    


    

