{-# LANGUAGE OverloadedStrings #-}

module Hadoo.State (State(), getAllStates, isMaxBound, isMinBound) where

data State = Todo | Started | Done | Baba deriving (Show, Eq, Bounded, Enum)

getAllStates :: [State]
getAllStates = [minBound..maxBound] :: [State]   

isMinBound :: State -> Bool
isMinBound x = x == minBound

isMaxBound :: State -> Bool
isMaxBound x = x == maxBound

