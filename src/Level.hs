{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Level where

import Constants
import Data.Tree
import DataTypes

type Boxes = [Position]


type Water = [Position]

data Room = Room
  { spikes :: Spikes,
    boxes :: Boxes,
    water :: Water,
    isTerminal :: Bool,
    getSpecialPos :: Position,
    initialCharacterPos :: Position
  }

type Level = Tree Room

