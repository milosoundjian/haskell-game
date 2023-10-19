{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module DataTypes where

import Data.Tree
import qualified Data.Map as Mapping
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

data Direction = UP | DOWN | LEFT | RIGHT deriving (Eq, Ord)

type Obstacles = [Position]
type Spikes = [Position]

data RoomState = RoomState
  {
      character :: Character,

      spikes :: Spikes,
      obstacles :: Obstacles,

      isTerminal :: Bool,
      specialPos :: Position
  }


type Level = Tree RoomState

data GameState = GameState {
  -- ui related stuff
  userText :: String,
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


directionVectorMap :: Mapping.Map Direction (Int, Int)
directionVectorMap =
  Mapping.fromList $
    zip
      [UP, DOWN, LEFT, RIGHT]
      [(0, 1), (0, -1), (-1, 0), (1, 0)]


clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx