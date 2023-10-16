module Main where

import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Snake

--  GAME CONSTANTS start here :
-- screen dimensions are float to reduce number of type casts in computation
gameWidth :: Float
gameWidth = 640

gameHeight :: Float
gameHeight = 480

window :: Display
window = InWindow "Haskell Puzzle Game" (round gameWidth, round gameHeight) (100, 100)

background :: Color
background = yellow

cursorFlickerDuration :: Int
cursorFlickerDuration = 10

-- useful shorthand
toFloat (x, y) = (fromIntegral x, fromIntegral y)

-- fills up the cell at the input coordinates with the input color
fillCell :: Position -> Color -> Picture
fillCell (posX, posY) col =
  let centerRec = color col (rectangleSolid cellSize cellSize)
      topLeftRec =
        translate
          (-gameWidth / 2 + cellSize / 2)
          (-gameHeight / 2 + cellSize / 2)
          centerRec
   in translate ((fromIntegral posX) * cellSize) ((fromIntegral posY) * cellSize) topLeftRec

-- as of now : just display the user character + the current text input
render :: GameState -> Picture
render gameState =
  let playerSprite = fillCell (getCharacter gameState) black

      userText =
        color red $ translate (-gameWidth / 2 + cellSize / 2) (-gameHeight / 2 + cellSize / 2) $
          scale 0.25 0.25 (text $ getUserText gameState)
   in pictures $
        playerSprite
          : userText
          : []


-- currently : just increment the cursor timer =
update :: Float -> GameState -> GameState
update seconds gameState = 
  let 
    curTimer = getCursorTimer . getCursorState $ gameState
    newTimer = mod (curTimer + 1)  cursorFlickerDuration
    
  in
    gameState {
      getCursorState = getCursorState gameState 
    }

handleKeys :: Event -> GameState -> GameState
-- handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) gameState = movedGameState gameState LEFT
-- handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gameState = movedGameState gameState RIGHT
-- handleKeys (EventKey (SpecialKey KeyUp) Down _ _) gameState = movedGameState gameState UP
-- handleKeys (EventKey (SpecialKey KeyDown) Down _ _) gameState = movedGameState gameState DOWN




-- player movement
handleKeys (EventKey (SpecialKey key) Down _ _) gameState
  | key == KeyLeft = movedGameState gameState LEFT
  | key == KeyRight = movedGameState gameState RIGHT
  | key == KeyUp = movedGameState gameState UP
  | key == KeyDown = movedGameState gameState DOWN




-- typing into user input
handleKeys (EventKey (Char ch) Down _ _) gs =
  gs
    { 
      getUserText = getUserText gs ++ [ch]
    }

handleKeys (EventKey (SpecialKey KeySpace) Down _ _) gs =
  gs 
  {
    getUserText = getUserText gs ++ " "
  }



-- removing characters from user input
handleKeys (EventKey (SpecialKey KeyDelete) Down _ _) gs =
  if (length (getUserText gs) > 0)
    then
      gs
        { 
          getUserText = init (getUserText gs)
        }
    else gs

--shortcut to clear the text input
handleKeys (EventKey (SpecialKey KeyEnd) Down _ _ ) gs = 
  gs {
    getUserText = ""
  }
  

handleKeys _ gameState = gameState





main :: IO ()
main = play window background 10 (initialGameState) render handleKeys update
