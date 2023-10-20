{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Levels where

import System.Random

import Constants
import DataTypes



--Helper FUNCTIONS
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
    -- only allow purely legal moves (no water losses either)
    if not (newPos `elem` (boxes rs) && newPos `elem` (waters rs)) then
      rs {character = (legalX, legalY)}
    else 
      rs

movedRoomState :: RoomState -> Direction -> RoomState
movedRoomState rs dir 
  | (rGameOver rs) = rs -- don't do anything if room is game overed
  | otherwise = 
  let
    newPos = move (character rs) dir 
  in
    -- kill player if go into water, prevent them from moving if go into box
    case (newPos `elem` (boxes rs), newPos `elem` (waters rs)  ) of 
      (True, _) -> rs {charRot = directionAngleMap dir}
      (False, True) -> rs {character = newPos, charRot = directionAngleMap dir, rGameOver = True} 
      (False, False) -> rs {character = newPos, charRot = directionAngleMap dir}


movedGameState :: GameState -> Direction -> GameState
movedGameState gs dir 
  -- use short-circuiting
  | (gameOver gs || any (rGameOver) (rooms gs)) = gs {gameOver = True}
  | otherwise = gs {rooms = map (`movedRoomState` dir) (rooms gs) }

addBox :: RoomState -> Position -> RoomState
addBox rs addPos =
  if (elem addPos (boxes rs)) then 
    rs
  else 
    rs {boxes = addPos:(boxes rs) } 

addWater :: RoomState -> Position -> RoomState
addWater rs addPos =
  if (elem addPos (waters rs)) then 
    rs
  else 
    rs {waters = addPos:(waters rs) } 

--Helper INSTANCES
initialGameState :: GameState
initialGameState =
  GameState
    { 
      userText = "Lorem Ipsum",
      debugText = "Debug output :",
      isCursorVisible = True,
      randomStdGen = mkStdGen 100,

      elapsedFrames = 0,
      levelIndex = -1,
      gameOver = False,

      rooms = [debugRoom],
      moveHistory = []
    }


debugRoom :: RoomState
debugRoom = 
    RoomState 
    {
        character = (10, 10),
        charRot = 0,

        waters = [(2, 3), (3, 3), (3, 4), (10, 4)],
        boxes = [(0, 5), (7, 4)],

        isTerminal = True,
        specialPos = (15, 10),

        rGameOver = False
    }