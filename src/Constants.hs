{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Constants where 

import Graphics.Gloss


-- Meta constants: 
debugModeEnabled :: Bool
debugModeEnabled = True 

--  GAME CONSTANTS start here :
-- screen dimensions are float to reduce number of type casts in computation
rows :: Int
rows = 8

cols :: Int
cols = 12

cellSize :: Float
cellSize = 80


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