module Snake where

import Data.Map as Map
import System.Random

data Direction = UP | DOWN | LEFT | RIGHT deriving (Eq, Ord)

-- tuple of form (x, y)
-- reminder : coordinates start at BOTTOM LEFT of the screen
-- (0, 0) is the bottom left corner of the viewport
-- coordinates are in the range (0, 0) -> (cols - 1, rows - 1)
type Position = (Int, Int)

type Character = Position

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
  GameState
    { getCharacter = move (getCharacter gs) dir,
      getUserText = getUserText gs,
      getRandomStdGen = (getRandomStdGen gs)
    }

data GameState = GameState
  { getCharacter :: Character,
    getUserText :: String,
    getRandomStdGen :: StdGen
  }

initialGameState :: GameState
initialGameState =
  GameState
    { getCharacter = (10, 10),
      getUserText = "[Commands appear here]",
      getRandomStdGen = mkStdGen 100
    }
