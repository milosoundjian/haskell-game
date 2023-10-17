{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Level where

import Data.Tree
import DataTypes

type Obstacles = [Position]

type Spikes = [Position]

data Room = Room
    {
        spikes :: Spikes,
        obstacles :: Obstacles,
        isTerminal :: Bool,
        getSpecialPos :: Position, 
        initialCharacterPos :: Position
    }

type Level = Tree Room