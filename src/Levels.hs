{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Levels where

import System.Random

import Constants
import DataTypes
import Graphics



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
  | (null $ moveHistory gs) = gs
  | otherwise = head $ moveHistory gs 

-- reloads the level at the current level id 
restartLevel :: GameState -> GameState
restartLevel (GameState{levelIndex = li, gsIndex = gi})
  --using short circuiting
  | (
    li < (length levelsData) 
  && gi < (length (levelsData !! li)) 
  ) = 
      (levelsData !! li) !! gi

  | otherwise =  initialGameState

--assumes there exists a next game state in cur level, otherwise returns the OoB scene
nextGameState :: GameState -> GameState
nextGameState (GameState {levelIndex = li, gsIndex = gi})
  -- short circuiting, much wow, epic doggo, wholesome 100ù
  | (
    li < (length levelsData) 
  && gi < (length (levelsData !! li) - 1) 
  ) = 
      (levelsData !! li) !! (gi + 1)

  | otherwise =  initialGameState
    

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

--THIS IS WHERE we define the actual level data for all of the 

initialGameState :: GameState
initialGameState =
  GameState
    { 
      userText = "Level 0 : Getting Started !",
      debugText = "Debug output :",
      isCursorVisible = True,
      randomStdGen = mkStdGen 100,

      elapsedFrames = 0,
      levelIndex = 0,
      gsIndex = 0, -- index of the game state within the level
      gameOver = False,

      rooms = [debugRoom],
      moveHistory = []
    }


debugRoom :: RoomState
debugRoom = 
    RoomState 
    {
        character = (0, 0),
        charRot = 0,

        waters = [(2, 3), (3, 3), (3, 4)],
        spikes = [(0, 5), (7, 4)],

        isTerminal = False,
        specialPos = (5, 5),

        rGameOver = False
    }

-- combine all of the levels written above
type Name = String
type Level = [GameState]

levelsData :: [Level]
levelsData = [[initialGameState, initialGameState {gsIndex = 1, rooms = [debugRoom {isTerminal = True}, debugRoom{isTerminal = True}, debugRoom{isTerminal = True}]}]]