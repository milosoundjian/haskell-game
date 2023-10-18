{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module DataTypes where

import qualified Data.Map as Mapping
import System.Random
import Constants

data Direction = UP | DOWN | LEFT | RIGHT deriving (Eq, Ord)

-- tuple of form (x, y)
-- reminder : coordinates start at BOTTOM LEFT of the screen
-- (0, 0) is the bottom left corner of the viewport
-- coordinates are in the range (0, 0) -> (cols - 1, rows - 1)
type Position = (Int, Int)

type Character = Position


-- data type that encodes every value susceptible of changing over time
data RoomState = RoomState { 
  character :: Character
}

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



directionVectorMap :: Mapping.Map Direction (Int, Int)
directionVectorMap =
  Mapping.fromList $
    zip
      [UP, DOWN, LEFT, RIGHT]
      [(0, 1), (0, (-1)), ((-1), 0), (1, 0)]


clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx


-- executes the move in the given direction
-- if the move is illegal : do nothing
move :: Character -> Direction -> Character
move (charX, charY) direction =
  let 
    dirVector = directionVectorMap Mapping.! direction
    newChar = (charX + fst dirVector, charY + snd dirVector)
   in 
    (clamp 0 (cols - 1) (fst newChar), clamp 0 (rows - 1) (snd newChar))


movedRoomState :: RoomState -> Direction -> RoomState
movedRoomState rs dir = 
  rs {character = move (character rs) dir}

movedGameState :: GameState -> Direction -> GameState
movedGameState gs dir =
  gs {rooms = map (`movedRoomState` dir) (rooms gs) }


-- temp variable : delete me eventually
placeholderRooms :: [RoomState]
placeholderRooms = 
  RoomState {
    character = (10, 10)
  } : []


initialGameState :: GameState
initialGameState =
  GameState
    { 
      userText = "Lorem Ipsum",
      isCursorVisible = True,
      randomStdGen = mkStdGen 100,

      elapsedFrames = 0,
      levelIndex = -1,

      rooms = placeholderRooms,
      moveHistory = []
    }
