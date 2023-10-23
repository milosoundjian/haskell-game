{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module LevelHelper where

import System.Random

import Constants
import DataTypes
import Graphics
import LevelData



--Helper FUNCTIONS
--stores a copy of the current state in the move history list 
backup :: GameState -> GameState 
backup gs = gs {moveHistory = gs:(moveHistory gs)}

-- (ignores illegal moves)
move :: Character -> Direction -> Character
move (charX, charY) direction =
  let 
    dirVector = directionVectorMap direction
    newChar = (charX + fst dirVector, charY + snd dirVector)
   in 
    (clamp 0 (cols - 1) (fst newChar), clamp 0 (rows - 1) (snd newChar))


movedToRoomState :: RoomState -> Position -> RoomState
movedToRoomState rs (x,y)
  | rGameOver rs = rs
  | otherwise = 
  let
    legalX = clamp 0 (cols - 1) x 
    legalY = clamp 0 (rows - 1) y
    newPos = (legalX, legalY)
  in
    -- only allow purely legal moves (no spike losses either)
    if not (newPos `elem` spikes rs || newPos `elem` waters rs || newPos == specialPos rs ) then
      rs {character = (legalX, legalY)}
    else 
      rs

movedRoomState ::  RoomState  -> Direction -> RoomState
movedRoomState rs  dir
  | (rGameOver rs) = rs -- don't do anything if room is game overed
  | otherwise = 
  let
    newPos = move (character rs) dir 
  in
    -- kill player if go into spikes, prevent them from moving if go into water
    case (newPos `elem` waters rs, newPos `elem` spikes rs) of 
      (True, _) -> rs {charRot = directionAngleMap dir}
      (False, True) ->  rs {character = (-100, -100), charRot = directionAngleMap dir, rGameOver = True} 
      (False, False) ->  rs {character = newPos, charRot = directionAngleMap dir}
      




movedGameState :: GameState -> Direction -> GameState
movedGameState gs dir 
  -- use short-circuiting
  | (gameOver gs || any (rGameOver) (rooms gs)) = 
      gs {gameOver = True}
      
  | otherwise = 
    let 
      newGs = (backup gs) {rooms = map (`movedRoomState` dir) (rooms gs) }
      collidingRooms = filter isSpecialCollision (rooms newGs)
      terminalCollisions = filter isTerminal collidingRooms 
    in
      if (collidingRooms /= []) then 
        if (terminalCollisions == []) then 
          --puzzle has been solved, go to next screen
          nextGameState newGs
        else 
          --destroy all the rooms that have reached their goal
          newGs {rooms = filter (not . (`elem` terminalCollisions)) (rooms newGs)}
      else 
        -- just return the normally moved state 
        newGs





-- reverts to the last saved state, doesn't do anything if the history is empty
undoLastMove :: GameState -> GameState 
undoLastMove gs 
  | (length $ moveHistory gs) == 0 = gs
  | otherwise = head $ moveHistory gs 

-- reloads the level at the current level id 
restartLevel :: GameState -> GameState
restartLevel curGs@(GameState{levelIndex = li, screenIndex = si})
  --using short circuiting
  | 
     li < length levelsData
  && si < length ( snd (levelsData !! li) )
   = 
      curGs {
        rooms = snd (levelsData !! li) !! si,
        gameOver = False,
        moveHistory = []
      }

  | otherwise =  curGs


-- moves player to the next screen of current level
-- currently doesn't implement level transition
nextGameState :: GameState -> GameState
nextGameState curGs@(GameState {levelIndex = li, screenIndex = si})
  -- wholesome 100 short circuiting
  | 
     li < length levelsData
  && si < length ( snd (levelsData !! li)) - 1 
   = 
      curGs {
        rooms = (snd (levelsData !! li)) !! (si + 1),
        screenIndex = si + 1,
        moveHistory = []
      }

  | otherwise =  curGs

--check whether a room has a squirrel-special object collision
isSpecialCollision :: RoomState -> Bool
isSpecialCollision RoomState {character=char, specialPos = special} = 
  (char == special)

-- utility functions to add entities at runtime
addSpike :: RoomState -> Position -> RoomState
addSpike rs addPos =
  if (elem addPos (spikes rs)) then 
    rs
  else 
    rs {spikes = addPos:(spikes rs) } 

addWater :: RoomState -> Position -> RoomState
addWater rs addPos =
  if (elem addPos (waters rs)) then 
    rs
  else 
    rs {waters = addPos:(waters rs) } 

