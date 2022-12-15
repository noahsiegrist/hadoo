{-# LANGUAGE OverloadedStrings #-}

module Hadoo.Lane (laneInit) where

import Web.Scotty 

import Hadoo.Helpers (htmlString, ea, concatArr)
import Hadoo.State
import Hadoo.Item (loadItems, render)
import Control.Monad.IO.Class (liftIO)

laneInit :: State -> IO String
laneInit state = do
  items <- loadItems state
  return $ 
    ea "div" [("class", "lane")] $
      ea "div" [("class", "title")] (show state)
      ++ concatArr (map (render) items)
    



    


    

