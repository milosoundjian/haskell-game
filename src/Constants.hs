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
resetInput :: SpecialKey
submitTextInput :: SpecialKey
clearTextInput :: SpecialKey
togglePause :: SpecialKey
-- etc... all of them must be special key constructors

upInput = KeyUp
downInput = KeyDown
rightInput = KeyRight
leftInput = KeyLeft

undoInput = KeyShiftL
resetInput = KeyCtrlL

submitTextInput = KeyEnter
clearTextInput = KeyDelete
togglePause = KeyTab



--  GAME CONSTANTS start here :
-- screen dimensions are float to reduce number of type casts in computation
-- Rows and Columns MUST BE divisible by 2 
-- or the program isn't guaranteed to work
rows :: Int
rows = 8

cols :: Int
cols = 10

cellSize :: Float
cellSize = 100


gameWidth :: Float
gameWidth = fromIntegral cols * cellSize

gameHeight :: Float
gameHeight = fromIntegral rows * cellSize

framerate :: Int
framerate = 20


backgroundCol :: Color
backgroundCol = black

cursorFlickerDuration :: Int
cursorFlickerDuration = 5
cursorCharacter :: String
cursorCharacter = "|"

danceCooldown :: Int
danceCooldown = 4