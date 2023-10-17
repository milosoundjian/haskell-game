module DataTypes where

import Data.Map as Map
import System.Random

data Direction = UP | DOWN | LEFT | RIGHT deriving (Eq, Ord)

-- tuple of form (x, y)
-- reminder : coordinates start at BOTTOM LEFT of the screen
-- (0, 0) is the bottom left corner of the viewport
-- coordinates are in the range (0, 0) -> (cols - 1, rows - 1)
type Position = (Int, Int)

type Character = Position

-- The most important data types in the entire project 
data CursorState = CursorState {
  isCursorVisible :: Bool,
  cursorTimer :: Int
}

data GameState = GameState { 
  character :: Character,
  userText :: String,
  randomStdGen :: StdGen,
  
  cursorState :: CursorState,
  levelIndex :: Int
}

cols :: Int
cols = 16

rows :: Int
rows = 12

cellSize :: Float
cellSize = 40

directionVectorMap :: Map Direction (Int, Int)
directionVectorMap =
  Map.fromList $
    zip
      [UP, DOWN, LEFT, RIGHT]
      [(0, 1), (0, (-1)), ((-1), 0), (1, 0)]

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

-- executes the move in the given direction
-- if the move is illegal : do nothing
move :: Character -> Direction -> Character
move (charX, charY) direction =
  let dirVector = directionVectorMap ! direction
      newChar = (charX + fst dirVector, charY + snd dirVector)
   in (clamp 0 (cols - 1) (fst newChar), clamp 0 (rows - 1) (snd newChar))

-- same as move but takes and returns game state instead of char
movedGameState :: GameState -> Direction -> GameState
movedGameState gs dir =
  gs {character = move (character gs) dir }



initialGameState :: GameState
initialGameState =
  GameState
    { 
      character = (10, 10),
      userText = "[Commands appear here]",
      randomStdGen = mkStdGen 100,

      cursorState = CursorState {
        isCursorVisible = True,
        cursorTimer = 0
      },
      levelIndex = (-1)
    }
