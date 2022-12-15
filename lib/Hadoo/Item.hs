{-# LANGUAGE OverloadedStrings #-}

module Hadoo.Item (loadItems, render) where

import Web.Scotty 

import Hadoo.Helpers (htmlString, e, ea)
import Hadoo.Persistence (loadItemsRaw)
import Hadoo.State

import Data.Maybe
import Control.Monad (filterM, mapM)
import Text.Read (readMaybe)

data Item = Item {
  id:: Int,
  state :: State,
  content:: String
  }

mkItem :: Int -> State -> String -> Item
mkItem id state content = Item { Hadoo.Item.id = id, state = state, content = content }

getContent :: Item -> String
getContent (Item _ _ content) = content 

loadItems :: State -> IO [Item]
loadItems state = do
  items <- loadItemsRaw state
  return $ map (\(id, str) -> mkItem (read id) state str) items



render :: Item -> String
render (Item id state content) = ea "div" [("class", "item")] $ 
  e "pre" ( show state ++ show id ++ ": " ++ content )
  -- TODO exclude if item is min or max bound.
  ++ renderButton "<" (Item id state content)
  ++ renderButton ">" (Item id state content)
  ++ renderButton "e" (Item id state content)
  ++ renderButton "d" (Item id state content)

  
renderButton :: String -> Item -> String
renderButton ">" i = renderForm "post" "move/Next" ">" i
renderButton "<" i = renderForm "post" "move/Back" "<" i
renderButton "e" i = renderForm "get" "edit" "Edit" i 
renderButton "d" i = renderForm "post" "delete" "Delete" i
renderButton _ _ = ""
    

renderForm :: String -> String -> String -> Item -> String
renderForm method url label (Item id state content) =
  ea "form" [("method", method), ("action", baseUrl (Item id state content) ++ url), ("class", "inline")] $
    ea "button" [("type", "submit")] label
    
baseUrl :: Item -> String
baseUrl (Item id state _) = "/items/" ++ show state ++ "/" ++ show id ++ "/"

    

