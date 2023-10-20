{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Constants where 

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game



-- Meta constants: 
debugModeEnabled :: Bool
debugModeEnabled = True 

-- Bindings
-- WARNING : space bar AND backspace are already reserved and can't be bound here 
upInput:: SpecialKey  
downInput :: SpecialKey 
rightInput :: SpecialKey 
leftInput :: SpecialKey 
undoInput :: SpecialKey

upInput = KeyUp
downInput = KeyDown
rightInput = KeyRight
leftInput = KeyLeft

undoInput = KeyShiftR
resetInput = KeyTab

submitTextInput = KeyEnter
clearTextInput = KeyDelete



--  GAME CONSTANTS start here :
-- screen dimensions are float to reduce number of type casts in computation
rows :: Int
rows = 6

cols :: Int
cols = 8

cellSize :: Float
cellSize = 100


gameWidth :: Float
gameWidth = fromIntegral cols * cellSize

gameHeight :: Float
gameHeight = fromIntegral rows * cellSize

framerate :: Int
framerate = 20


backgroundCol :: Color
backgroundCol = white

cursorFlickerDuration :: Int
cursorFlickerDuration = 5
cursorCharacter :: String
cursorCharacter = "|"