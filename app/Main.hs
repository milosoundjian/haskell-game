module Main where

import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Snake

-- screen dimensions are float to reduce number of type casts in computation
gameWidth :: Float 
gameWidth = 640

gameHeight :: Float
gameHeight = 480

window :: Display
window = InWindow "Haskell Puzzle Game" (round gameWidth, round gameHeight) (100, 100)

background :: Color
background = white

--useful shorthand
toFloat (x, y) = (fromIntegral x, fromIntegral y)


--fills up the cell at the input coordinates with the input color
fillCell :: Position -> Color -> Picture 
fillCell (posX, posY) col =
  let 
    centerRec = color col (rectangleSolid  cellSize cellSize)
    topLeftRec = translate (-gameWidth/2  + cellSize/2) 
                           (-gameHeight/2 + cellSize/2) 
                           centerRec 
  in
    translate ( (fromIntegral posX) * cellSize ) ( (fromIntegral posY) * cellSize) topLeftRec



--as of now : just display the user character + the current text input 
render :: GameState -> Picture
render gameState =
  let 
    playerSprite = fillCell (getCharacter gameState) black

    userText = scale 0.25 0.25 $  
               translate (-gameWidth * 2) (-gameHeight * 2 + cellSize) (text $ getUserText gameState)
  in
    pictures $
              playerSprite
              : userText 
              : []




update :: Float -> GameState -> GameState
update seconds gameState = gameState


handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) gameState = movedGameState gameState LEFT
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gameState = movedGameState gameState RIGHT
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) gameState = movedGameState gameState UP
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) gameState = movedGameState gameState DOWN

handleKeys _ gameState = gameState

main :: IO ()
main = play window background 10 (initialGameState) render handleKeys update
