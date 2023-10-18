module Main (main) where

--external imports
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment

--internal imports
import DataTypes
import Constants
import Levels





-- useful shorthand
toFloat :: (Integral a1, Integral a2, Num a3, Num b) => (a1, a2) -> (a3, b)
toFloat (x, y) = (fromIntegral x, fromIntegral y)

-- fills up the cell at the input coordinates with the input color
fillCell :: Position -> Color -> Picture
fillCell (posX, posY) col =
  let 

    centerRec = color col (rectangleSolid cellSize cellSize)
    topLeftRec =
      translate
        (-gameWidth / 2 + cellSize / 2)
        (-gameHeight / 2 + cellSize / 2)
        centerRec

   in 
    translate ((fromIntegral posX) * cellSize) ((fromIntegral posY) * cellSize) topLeftRec

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

outOfBoundsScreen :: [Picture]
outOfBoundsScreen = [bg, txt] 
  where 
    bg = color black $ rectangleSolid gameWidth gameHeight
    txt = color red $ translate (-gameWidth / 2 + 150) 0 $ scale 0.5 0.5 (Text "OUT OF BOUNDS")


-- display the user character + the current text input
renderRoom :: [Sprite] -> RoomState -> Picture
renderRoom sprites roomState = 
  let 
    --display the player
    playerSprite = fillCell (character roomState) black

  in
    -- combine everything
    pictures ([playerSprite] ++ grid ++ (map picture sprites))


-- display each game room at the proper position
render :: [Sprite] -> GameState  -> Picture
render sprites gameState =
  let 
    --print the text and the cursor
    cursorSuffix = if (isCursorVisible gameState) 
                   then cursorCharacter else ""

    textContent = userText gameState ++ cursorSuffix 
    displayText =
      color red $
        translate (-gameWidth / 2 + cellSize / 2) (-gameHeight / 2 + cellSize / 2) $
          scale 0.25 0.25 (text textContent) :: Picture

    --render only the rooms we need thanks to lazy eval
    firstRoom = renderRoom sprites (rooms gameState !! 0)
    secondRoom = renderRoom sprites (rooms gameState !! 1)
    thirdRoom = renderRoom sprites (rooms gameState !! 2)
    fourthRoom = renderRoom sprites (rooms gameState !! 3)

   in 
    case (length . rooms $ gameState) of 
      0 -> pictures outOfBoundsScreen
      1 -> pictures [firstRoom, displayText]
      2 -> undefined
      3 -> undefined
      4 -> undefined 



update :: Float -> GameState -> GameState
update seconds gameState = 
  let 
    --increment the timer value
    newElapsed = (elapsedFrames gameState) + 1
    newVisibility = not (isCursorVisible gameState)

  in
    --handle all timer related utilities
    if (mod newElapsed cursorFlickerDuration == 0) then 
      gameState {elapsedFrames = newElapsed, isCursorVisible = newVisibility}
    else
      gameState {elapsedFrames = newElapsed}



handleKeys :: Event -> GameState -> GameState

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
      userText = userText gs ++ [ch]
    }

handleKeys (EventKey (SpecialKey KeySpace) Down _ _) gs =
  gs 
  {
    userText = userText gs ++ " "
  }


-- removing characters from user input
handleKeys (EventKey (SpecialKey KeyDelete) Down _ _) gs
  | [] <- userText gs = gs
  | otherwise = gs { userText = init (userText gs) }

--shortcut to clear the text input
handleKeys (EventKey (SpecialKey KeyEnd) Down _ _ ) gs = 
  gs {
    userText = ""
  }
  

handleKeys _ gameState = gameState

main :: IO ()
main = do
  -- load the assets for the render function
  grass <- loadBMP "assets/grass.bmp"
  squirrel <- loadBMP "assets/squirrel.bmp"
  
  --TODO : complete this pls ! 
  let sprites = Sprite { picture = grass, dimensions = (32, 32) }:
                Sprite { picture = squirrel, dimensions = (32, 32) }:
                []
  

  --place the game window in the center of the screen
  screenSize <- getScreenSize 

  let xCentered = (fromIntegral (fst screenSize) - gameWidth) / 2.0
  let yCentered = (fromIntegral (snd screenSize) - gameHeight) / 2.0 
  let window = InWindow "Haskell Puzzle Game" (round gameWidth, round gameHeight) 
               (round xCentered, round yCentered)

  play window background framerate initialGameState (render sprites) handleKeys update
