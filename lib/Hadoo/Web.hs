{-# LANGUAGE OverloadedStrings #-}

module Hadoo.Web where

import Web.Scotty 
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.IO.Class (liftIO)

import Hadoo.Helpers (htmlString)
import Hadoo.Board (boardAction)
import Hadoo.Item (itemEditAction, itemDeleteAction, itemNewAction, itemNewPostAction, itemEditPostAction, itemMoveNextAction, itemMoveBackAction)

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev

  get "/styles.css" styles 
  get "/" boardAction
  get "/items/new" itemNewAction
  post "/items/new" itemNewPostAction
  get "/items/:state/:id/edit" itemEditAction
  post "/items/:state/:id/edit" itemEditPostAction
  post "/items/:state/:id/delete" itemDeleteAction
  post "/items/:state/:id/move/next" itemMoveNextAction
  post "/items/:state/:id/move/back" itemMoveBackAction


styles :: ActionM ()
styles = do
    setHeader "Content-Type" "text/css"
    file "static/styles.css"
