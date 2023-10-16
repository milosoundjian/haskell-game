module Constants where 

import Graphics.Gloss


--  GAME CONSTANTS start here :
-- screen dimensions are float to reduce number of type casts in computation
gameWidth :: Float
gameWidth = 640

gameHeight :: Float
gameHeight = 480

background :: Color
background = yellow

cursorFlickerDuration :: Int
cursorFlickerDuration = 3
cursorCharacter :: String
cursorCharacter = "|"