{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module DataTypes where

import Data.Tree
import System.Random
import Graphics.Gloss

import Constants

import Zippers


-- tuple of form (x, y)
-- reminder : coordinates start at BOTTOM LEFT of the screen
-- (0, 0) is the bottom left corner of the viewport
-- coordinates are in the range (0, 0) -> (cols - 1, rows - 1)
type Position = (Int, Int)
type Dimensions = (Int, Int)

type Character = Position




-- defining helper types 
type Screen = [RoomState]
type Title = String
data ScreenWrap = ScreenWrap
  {
    screen :: Screen,
    isNewLevel :: Bool
  }
-- type Level = (GameState, [Screen])
type LevelData = [ScreenWrap]

data Direction = UP | DOWN | LEFT | RIGHT deriving (Eq, Ord, Enum)


data RoomState = RoomState
  {
      character :: Character,
      charRot :: Float, -- character rotation

      waters :: [Position],
      spikes :: [Position],

      isMini :: Bool, -- mini rooms have their tile size halved
      isTerminal :: Bool,
      specialPos :: Position,

      rGameOver :: Bool
  } deriving Eq



data GameState = GameState {
  -- ui related stuff
  userText :: String,
  debugText :: String,
  isCursorVisible :: Bool,

  toInit :: Bool,   -- Need to initialize the rooms (from the screen pointed to by screenPointer) or not

  -- game flow related stuff
  elapsedFrames :: Int,
  -- levelIndex :: Int,
  -- screenIndex ::Int,
  screenPointer :: ListZip ScreenWrap,
  titlePointer :: ListZip Title,
  gameOver :: Bool,
  currLevelInitScreen :: ListZip ScreenWrap,  -- pointer to screen at the start of current Level

  -- gameplay related stuff
  rooms :: Screen,
  moveHistory :: [GameState]
}


--data type for using external sprites
data Sprite = Sprite {
  picture :: Picture,
  dimensions :: Dimensions
} deriving Eq


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