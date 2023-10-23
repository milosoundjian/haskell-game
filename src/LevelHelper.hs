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
      --call the movedRoom function and pass it the roomState + gameState
      (backup gs) {rooms = map (`movedRoomState` dir) (rooms gs) }


-- reverts to the last saved state, doesn't do anything if the history is empty
undoLastMove :: GameState -> GameState 
undoLastMove gs 
  | (length $ moveHistory gs) == 0 = gs
  | otherwise = head $ moveHistory gs 

-- reloads the level at the current level id 
restartLevel :: GameState -> GameState
restartLevel curGs@(GameState{levelIndex = li, gsIndex = gi})
  --using short circuiting
  | (
    li < (length levelsData) 
  && gi < (length (levelsData !! li)) 
  ) = 
      (levelsData !! li) !! gi

  | otherwise =  curGs

--assumes there exists a next game state in cur level, otherwise returns the OoB scene
nextGameState :: GameState -> GameState
nextGameState curGs@(GameState {levelIndex = li, gsIndex = gi})
  -- short circuiting, WOW, epic doge, wholesome 100, you'r breathtaking
  | (
    li < (length levelsData) 
  && gi < (length (levelsData !! li) - 1) 
  ) = 
      (levelsData !! li) !! (gi + 1)

  | otherwise =  curGs
    

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

