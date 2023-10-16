module Snake where

import Data.Map as Map
import System.Random

data Direction = UP | DOWN | LEFT | RIGHT deriving (Eq, Ord)


-- tuple of form (x, y)
type Position = (Int, Int)

type Character = Position 

cols :: Int
cols = 32

rows :: Int
rows = 24

directionVectorMap :: Map Direction (Int, Int)
directionVectorMap =
  Map.fromList $
    zip
      [UP, DOWN, LEFT, RIGHT]
      [(0, (-1)), (0, 1), ((-1), 0), (1, 0)]

clamp :: (Ord a) => a -> a -> a -> a 
clamp mn mx = max mn . min mx 

-- executes the legal move in the given direction
move :: Character -> Direction -> Character 
move (charX, charY) direction =
  let 
    dirVector = directionVectorMap ! direction
    newChar = (charX + fst dirVector, charY + snd dirVector)
  in
    (clamp 0 cols (fst newChar), clamp 0 rows (snd newChar))


-- same as move but takes and returns game state instead of char
movedGameState :: GameState -> Direction -> GameState
movedGameState gs dir = 
  GameState {
    getCharacter = move (getCharacter gs) dir,
    isGameOver = (isGameOver gs),
    getRandomStdGen = (getRandomStdGen gs)
  }

  -- if wasFoodEaten
  --   then (True, newHead : snake)
  --   else (False, newHead : init snake)
  -- where
  --   wasFoodEaten = newHead == food
  --   newHead = directionVectorMap ! direction +: head snake
  --   (a, b) +: (c, d) = (a + c, b + d)

-- checkGameOver :: Char -> Bool
-- checkGameOver snake =
--   headX == 0
--     || headX == cols
--     || headY == 0
--     || headY == rows
--     || head' `elem` tail'
--   where
--     head' = head snake
--     (headX, headY) = head'
--     tail' = tail snake

-- generateNewFood :: Snake -> StdGen -> (Food, StdGen)
-- generateNewFood snake stdGen =
--   if newFood `elem` snake
--     then generateNewFood snake stdGen3
--     else ((foodX, foodY), stdGen3)
--   where
--     (foodX, stdGen2) = randomR (1, 31) stdGen
--     (foodY, stdGen3) = randomR (1, 23) stdGen2
--     newFood = (foodX, foodY)

data GameState = GameState
  { 
    getCharacter :: Character,
    isGameOver :: Bool,
    getRandomStdGen :: StdGen
  }

-- changeDirection :: GameState -> Direction -> GameState
-- changeDirection (GameState s f d g r) newDir = GameState s f newDir g r

initialGameState :: GameState
initialGameState  =
  GameState
    { 
      getCharacter = (10, 10),
      isGameOver = False,
      getRandomStdGen = mkStdGen 100
    }
