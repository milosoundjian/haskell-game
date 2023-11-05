{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module LevelHelper where

import System.Random

import Constants
import DataTypes
import Graphics
import LevelData

import Zippers


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
    if not (newPos `elem` spikes rs || newPos `elem` walls rs || newPos == specialPos rs ) then
      rs {character = (legalX, legalY)}
    else 
      rs



movedRoomState ::  RoomState  -> Direction -> RoomState

--special case where the squirrel is double speed
movedRoomState rs@(RoomState {isDoubleSpeed = True}) dir =
  let 
    --apply regular move twice
    movedState =  (iterate (`movedRoomState` dir) (rs {isDoubleSpeed = False}) !! 2 )
                    {isDoubleSpeed = True}
  in
    movedState


movedRoomState rs  dir
  | (rGameOver rs) = rs -- don't do anything if room is game overed
  | otherwise = 
  let
    newPos = move (character rs) dir 
  in
    -- kill player if go into spikes, prevent them from moving if go into wall
    case (newPos `elem` walls rs, newPos `elem` spikes rs) of 
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
        if (terminalCollisions == []) || length (rooms newGs) == 1  then 
          --puzzle has been solved, go to next screen
          nextGameState newGs
        else 
          --remove all the side rooms that reached their terminal goal
          newGs {rooms = filter (not . (`elem` terminalCollisions)) (rooms newGs)}
      else 
        -- just return the normally moved state 
        newGs

--functions for rotating squirrels
rotRoom :: RoomState -> Int -> RoomState
rotRoom rs@(RoomState {charRot = cr}) rot = 
  rs{charRot = fromIntegral $ mod (round cr + rot) 360}

rotAllRooms :: GameState -> Int -> GameState
rotAllRooms gs@(GameState {rooms = rs}) rot =
    gs{ rooms = map (`rotRoom` rot) rs} 


-- reverts to the last saved state, doesn't do anything if the history is empty
undoLastMove :: GameState -> GameState 
undoLastMove gs 
  | null (moveHistory gs) = gs
  | otherwise = head $ moveHistory gs

-- Initializes rooms and title from the screen pointer in the GameState provided
initRooms :: GameState -> GameState
initRooms gs@(GameState{screenPointer = (_, Leaf)}) = gs{userText = "EMPTY GAME TREE", rooms = [roomDebug]}
initRooms gs@(GameState{screenPointer = (c, B sw l r)}) = gs{userText = title sw, 
                                                            rooms = screen sw, 
                                                            screenPointer = (c, B sw{active = True, visited = True} l r),
                                                            moveHistory = []}


-- reloads the level at the current level id 
restartLevel :: GameState -> GameState
restartLevel curGs@(GameState{currLevelInitScreen = scr}) = initRooms curGs{
  screenPointer = scr,      -- screenPointer will point to the screen at the start of the level
  gameOver = False
}

-- finds next unsolved screen in the pre order traversal of the tree
findNextScreen :: BinZip ScreenWrap -> BinZip ScreenWrap
findNextScreen sp@(_, Leaf) = findNextScreen $ goDown sp  -- reached leaf go back 
findNextScreen (cxt, B sw l r)
  | not $ solved sw = (cxt, B sw l r)      -- found unsolved screen
  | not $ leftSolved sw = findNextScreen $ goLeft (cxt, B sw{leftSolved = True} l r)  -- look in the left and mark left solved
  | not $ rightSolved sw = findNextScreen $ goRight (cxt, B sw{rightSolved = True} l r)  -- look in the right and mark right solved
  | otherwise = findNextScreen $ goDown (cxt, B sw{solved = False, leftSolved = False, rightSolved = False} l r)
  -- subtree is solved entirely. Mark everything unsolved to allow the game to just loop through levels again. This also
  -- avoids infinite looping in case cxt is Hole

-- moves player to the next screen of current level
-- currently doesn't implement level transition

nextGameState :: GameState -> GameState
nextGameState curGs@(GameState{screenPointer = (_, Leaf)}) = gsBase {isCredits = True}
nextGameState curGs@(GameState {currLevelInitScreen = clis, screenPointer = (c, B sw l r)}) =
    let 
      newSp@(_, B screenInfo _ _) = findNextScreen (c, B sw{solved = True, active = False} l r)   
      isNewLvl = isNewLevel screenInfo    
    
    in
      initRooms curGs{
        currLevelInitScreen = if isNewLvl then newSp else clis, 
        screenPointer = newSp,
        isCredits = isRoot newSp
      }

-- Function to navigate the game tree while game is paused. Implemented to make sure that after switching, nextGameState
-- can find the next room in the tree based on the level succession defined originally. i.e this doesn't mess up nextGameState (hopefully)
switchRooms :: GameState -> Direction -> GameState
switchRooms gs@(GameState{screenPointer = sp}) dir = 
  let 
    (cxt, B sw l r) = switchRoomsAux sp dir
  in
    -- only allow user to go to previously visited (unlocked) rooms and make sure
    if visited sw then 
      initRooms gs{screenPointer = (cxt, B sw{active = True, solved = False, leftSolved = False, rightSolved = False} l r)} 
      else gs

-- Navigate based on the direction while setting the appropriate flags in the screen wrappers
switchRoomsAux :: BinZip ScreenWrap -> Direction -> BinZip ScreenWrap
switchRoomsAux (cxt, B sw lsub rsub) UP = goDown (cxt, B sw{solved = False, leftSolved = False, rightSolved = False, active = False} lsub rsub)
switchRoomsAux (cxt, B sw lsub rsub) LEFT = goLeft (cxt, B sw{solved = True, leftSolved = True, rightSolved = False, active = False} lsub rsub)
switchRoomsAux (cxt, B sw lsub rsub) RIGHT = goRight (cxt, B sw{solved = True, leftSolved = True, rightSolved = True, active = False} lsub rsub)


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

addWall :: RoomState -> Position -> RoomState
addWall rs addPos =
  if (elem addPos (walls rs)) then 
    rs
  else 
    rs {walls = addPos:(walls rs) } 

