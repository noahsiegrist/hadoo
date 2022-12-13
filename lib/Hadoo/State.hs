{-# LANGUAGE OverloadedStrings #-}

module Hadoo.State (State(), getAllStates) where

data State = Todo | Started | Done deriving (Show, Eq, Bounded, Enum)

getAllStates :: [State]
getAllStates = [minBound..maxBound] :: [State]    

