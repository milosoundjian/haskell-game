module Level where

import Data.Tree
import DataTypes

type Obstacles = [Position]

type Spikes = [Position]

data Room = Room
    {
        getSpikes :: Spikes,
        getObstacles :: Obstacles,
        isTerminal :: Bool,
        getSpecialPos :: Position, 
        getInitialCharacterPos :: Position
    }

type Level = Tree Room