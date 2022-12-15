{-# LANGUAGE OverloadedStrings #-}

module Hadoo.Item (
  loadItems
  ,render
  ,itemEditAction
  ,itemEditPostAction
  ,itemDeleteAction
  ,itemNewAction
  ,itemNewPostAction
  ,itemMoveNextAction
  ,itemMoveBackAction
  ) where

import Web.Scotty 

import Hadoo.Helpers (createPage, htmlString, e, ea, concatArr, multiLineTextParam)
import Hadoo.Persistence (loadItemsRaw, deleteItem, saveItem)
import Hadoo.State

import Data.Maybe
import Control.Monad (filterM, mapM)
import Text.Read (readMaybe)
import Control.Monad.IO.Class (liftIO)

data Item = Item {
  id:: Int,
  state :: State,
  content:: String
  }

mkItem :: Int -> State -> String -> Item
mkItem id state content = Item { Hadoo.Item.id = id, state = state, content = content }

createItemAutoIncrement :: State -> String -> IO ()
createItemAutoIncrement state content = do
  items <- loadItems state
  saveItem (idAutoIncrement items) state content

idAutoIncrement :: [Item] -> Int
idAutoIncrement [] = 0
idAutoIncrement items = id + 1
    where (Item id _ _) = last items 

getContent :: Item -> String
getContent (Item _ _ content) = content 

loadItems :: State -> IO [Item]
loadItems state = do
  items <- loadItemsRaw state
  return $ map (\(id, str) -> mkItem (read id) state str) items

loadItem :: State -> Int -> IO Item
loadItem state itemId = do
  items     <- loadItems state
  (item:_)  <- filterM (\(Item id _ _) -> return (id == itemId)) items
  return item


render :: Item -> String
render (Item id state content) = ea "div" [("class", "item")] $ 
  e "pre" ( show state ++ show id ++ ": \n" ++ content )
  -- TODO exclude if item is min or max bound.
  ++ renderButton "<" (Item id state content)
  ++ renderButton ">" (Item id state content)
  ++ renderButton "e" (Item id state content)
  ++ renderButton "d" (Item id state content)

  
renderButton :: String -> Item -> String
renderButton ">" i = shouldRenderMoveNextButton i $ renderForm "post" "move/next" ">" i
renderButton "<" i = shouldRenderMoveBackButton i $ renderForm "post" "move/back" "<" i
renderButton "e" i = renderForm "get" "edit" "Edit" i 
renderButton "d" i = renderForm "post" "delete" "Delete" i
renderButton _ _ = ""

shouldRenderMoveNextButton :: Item -> String -> String
shouldRenderMoveNextButton (Item _ state _) s = if isMaxBound state then "" else s

shouldRenderMoveBackButton :: Item -> String -> String
shouldRenderMoveBackButton (Item _ state _) s = if isMinBound state then "" else s

renderForm :: String -> String -> String -> Item -> String
renderForm method url label (Item id state content) =
  ea "form" [("method", method), ("action", baseUrl (Item id state content) ++ url), ("class", "inline")] $
    ea "button" [("type", "submit")] label
    
baseUrl :: Item -> String
baseUrl (Item id state _) = "/items/" ++ show state ++ "/" ++ show id ++ "/"

itemEditAction :: ActionM ()
itemEditAction = do
  stateStr  <- param "state"
  itemIdStr <- param "id"
  let state =  stringToState stateStr
  let itemId = stringToId itemIdStr
  item <- liftIO (loadItem state itemId)
  str <- liftIO (createPage (renderEditForm item))
  htmlString str

itemEditPostAction :: ActionM ()
itemEditPostAction = do
  itemIdStr <- param "id"
  stateStr  <- param "state"
  let state = stringToState stateStr
  let itemId = stringToId itemIdStr
  content <- multiLineTextParam "content"
  liftIO $ saveItem itemId state content
  redirect "/"
  

itemNewAction :: ActionM ()
itemNewAction = do
  str <- liftIO $ createPage renderNewForm
  htmlString str 

itemNewPostAction :: ActionM ()
itemNewPostAction = do
  stateStr  <- param "state"
  let state = stringToState stateStr
  content <- multiLineTextParam "content"
  liftIO $ createItemAutoIncrement state content
  redirect "/"
  

stringToId :: String -> Int
stringToId str = read str :: Int

renderEditForm :: Item -> String
renderEditForm (Item id state content) =
  ea "form" [("method", "post"), ("action", baseUrl (Item id state content) ++ "edit")] $
    ea "textarea" [("name", "content"), ("rows", "12"), ("cols", "60")] content
    ++ e "br" ""
    ++ ea "button" [("type", "submit"), ("value", "Create")] "Save"

renderNewForm :: String
renderNewForm =
  ea "form" [("method", "post"), ("action", "/items/new")] $
    ea "label" [("for", "state")] "State: " ++
    ea "select" [("name", "state"), ("id", "states")] (
      concatArr (map renderOption getAllStates)
    ) 
    ++ e "br" ""
    ++ ea "textarea" [("name", "content"), ("rows", "12"), ("cols", "60")] ""
    ++ e "br" ""
    ++ ea "button" [("type", "submit"), ("value", "Create")] "Create"

renderOption :: State -> String
renderOption state = ea "option" [("value", show state)] $ show state

itemDeleteAction :: ActionM ()
itemDeleteAction = do
  stateStr  <- param "state"
  itemIdStr <- param "id"
  let state =  stringToState stateStr
  let itemId = stringToId itemIdStr
  -- (Item id state _) <- liftIO (loadItem state itemId)
  liftIO (deleteItem state itemId)
  redirect "/"

itemMoveNextAction :: ActionM ()
itemMoveNextAction = do
  itemIdStr <- param "id"
  stateStr  <- param "state"
  let state = stringToState stateStr
  if isMaxBound state then 
    redirect "/" 
  else do
    let itemId = stringToId itemIdStr
    item <- liftIO (loadItem state itemId)
    liftIO $ deleteItem state itemId
    liftIO $ createItemAutoIncrement (succ state) (getContent item)
    redirect "/"

itemMoveBackAction :: ActionM ()
itemMoveBackAction = do
  itemIdStr <- param "id"
  stateStr  <- param "state"
  let state = stringToState stateStr
  if isMinBound state then 
    redirect "/" 
  else do
    let itemId = stringToId itemIdStr
    item <- liftIO (loadItem state itemId)
    liftIO $ deleteItem state itemId
    liftIO $ createItemAutoIncrement (pred state) (getContent item)
    redirect "/"