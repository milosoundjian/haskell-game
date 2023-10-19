{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module DataTypes where

import Data.Tree
import System.Random
import Graphics.Gloss

import Constants


-- tuple of form (x, y)
-- reminder : coordinates start at BOTTOM LEFT of the screen
-- (0, 0) is the bottom left corner of the viewport
-- coordinates are in the range (0, 0) -> (cols - 1, rows - 1)
type Position = (Int, Int)
type Dimensions = (Int, Int)

type Character = Position

data Direction = UP | DOWN | LEFT | RIGHT deriving (Eq, Ord, Enum)

type Obstacles = [Position]
type Spikes = [Position]

data RoomState = RoomState
  {
      character :: Character,
      charRot :: Float, -- character rotation

      spikes :: Spikes,
      obstacles :: Obstacles,

      isTerminal :: Bool,
      specialPos :: Position
  }


type Level = Tree RoomState

data GameState = GameState {
  -- ui related stuff
  userText :: String,
  debugText :: String,
  isCursorVisible :: Bool,
  randomStdGen :: StdGen,

  -- game flow related stuff
  elapsedFrames :: Int,
  levelIndex :: Int,

  -- gameplay related stuff
  rooms :: [RoomState],
  moveHistory :: [GameState]
}


--data type for using external sprites
data Sprite = Sprite {
  picture :: Picture,
  dimensions :: Dimensions
}


directionVectorMap :: Direction -> (Int, Int)
directionVectorMap dir =
      [(0, 1), (0, -1), (-1, 0), (1, 0)] !! (fromEnum dir)

-- Converts direction to angle of rotation needed to match
-- that rotation (assuming start orientation is looking down)
directionAngleMap :: Direction -> Float
directionAngleMap dir = 
  [180, 0, 90, -90] !! (fromEnum dir)


clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx