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

-- useful shorthand
toFloat :: (Integral a1, Integral a2, Num a3, Num b) => (a1, a2) -> (a3, b)
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

-- creates a grid made up of list of lines
grid :: [Picture]
grid = verticalLines ++ horizontalLines
  where
    verticalLines = [Line [(x, -gameHeight / 2), (x, gameHeight / 2)] | x <- [-gameWidth / 2, -gameWidth / 2 + cellSize .. gameWidth / 2]]
    horizontalLines = [Line [(-gameWidth / 2, y), (gameWidth / 2, y)] | y <- [-gameHeight / 2, -gameHeight / 2 + cellSize .. gameHeight / 2]]

gameOverScreen :: [Picture]
gameOverScreen = [bg, txt]
  where
    bg = color black $ rectangleSolid gameWidth gameHeight
    txt = color red $ translate (-gameWidth / 2 + 150) 0 $ scale 0.5 0.5 (Text "YOU DIED")

-- as of now : just display the user character + the current text input
render :: GameState -> Picture
render gameState =
  let playerSprite = fillCell (getCharacter gameState) black
      userText =
        color red $
          translate (-gameWidth / 2 + cellSize / 2) (-gameHeight / 2 + cellSize / 2) $
            scale 0.25 0.25 (text $ getUserText gameState)
   in pictures ([playerSprite, userText] ++ grid)

-- in pictures gameOverScreen

update :: Float -> GameState -> GameState
update seconds gameState = gameState

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
  GameState
    { getCharacter = getCharacter gs,
      getUserText = getUserText gs ++ [ch],
      getRandomStdGen = getRandomStdGen gs
    }
-- removing characters from user input
handleKeys (EventKey (SpecialKey KeyDelete) Down _ _) gs =
  if not (null (getUserText gs))
    then
      GameState
        { getCharacter = getCharacter gs,
          getUserText = init (getUserText gs),
          getRandomStdGen = getRandomStdGen gs
        }
    else gs
handleKeys _ gameState = gameState

main :: IO ()
main = play window background 10 initialGameState render handleKeys update
