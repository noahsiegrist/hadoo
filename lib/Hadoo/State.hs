{-# LANGUAGE OverloadedStrings #-}

module Hadoo.State (State(), getAllStates, isMaxBound, isMinBound, stringToState) where

import Text.Read (read)

data State = Todo | Started | Done | Baba deriving (Show, Eq, Bounded, Enum, Read)

getAllStates :: [State]
getAllStates = [minBound..maxBound] :: [State]   

isMinBound :: State -> Bool
isMinBound x = x == minBound

isMaxBound :: State -> Bool
isMaxBound x = x == maxBound

stringToState :: String -> State
stringToState str = read str :: State