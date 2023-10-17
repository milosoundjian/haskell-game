{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Constants where 

import Graphics.Gloss
import GHC.Float (float2Int)


--  GAME CONSTANTS start here :
-- screen dimensions are float to reduce number of type casts in computation
gameWidth :: Float
gameWidth = 640

gameHeight :: Float
gameHeight = 480

cellSize :: Float
cellSize = 32

cols :: Int
cols = float2Int(gameWidth / cellSize)

rows :: Int
rows = float2Int(gameHeight / cellSize)

background :: Color
background = yellow

cursorFlickerDuration :: Int
cursorFlickerDuration = 5
cursorCharacter :: String
cursorCharacter = "|"