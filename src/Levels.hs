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
movedToRoomState rs (x,y) = 
  let
    legalX = clamp 0 (cols - 1) x 
    legalY = clamp 0 (rows - 1) y
  in
    rs {character = (legalX, legalY)}

movedRoomState :: RoomState -> Direction -> RoomState
movedRoomState rs dir = 
  rs {character = move (character rs) dir, 
      charRot = directionAngleMap dir}


movedGameState :: GameState -> Direction -> GameState
movedGameState gs dir =
  gs {rooms = map (`movedRoomState` dir) (rooms gs) }



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

      rooms = [debugRoom],
      moveHistory = []
    }


debugRoom :: RoomState
debugRoom = 
    RoomState 
    {
        character = (10, 10),
        charRot = 0,

        spikes = [],
        obstacles = [],

        isTerminal = True,
        specialPos = (15, 10)
    }