{-# LANGUAGE OverloadedStrings #-}

module Hadoo.Board (boardAction) where

import Web.Scotty 

import Hadoo.Helpers (htmlString, e, ea)

boardAction :: ActionM ()
boardAction = htmlString createPage

createPage :: String
createPage = 
  "<!DOCTYPE html>" ++
  ea "html" [("lang", "en")] (htmlHead ++ htmlBody)


htmlHead :: String
htmlHead = 
  e "head" $
    ea "link" [("rel", "stylesheet"), ("href", "/styles.css")] ""

htmlBody :: String
htmlBody = 
  e "body" $
    ea "h1" [("class", "body")] "Hadoo" ++ 
    ea "div" [("class", "container")] (
     ea "div" [("class", "lane")] (
        ea "div" [("class", "title")] "Todo" 
        ++ ea "div" [("class", "item")] "Some"
        )
    )
    
    
    


    

