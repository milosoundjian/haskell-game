module Level where

import Data.Tree
import Snake

type Obstacles = [Position]

type Spikes = [Position]

data Room = Room
    {
        spikes :: Spikes,
        obstacles :: Obstacles,
        acorn :: Bool,
        pos :: Position
    }

type Level = Tree Room