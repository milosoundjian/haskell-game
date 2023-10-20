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
import Interpreter
import Graphics



-- display the user character + the current text input
renderRoom :: Picture -> [Sprite] -> RoomState -> Picture
renderRoom backgroundP sprites@(squirrelS:boxS:waterS:_) roomState = 
  let 
    --display the player
    rotation = charRot roomState
    player = spriteCell (character roomState) rotation (squirrelS)

    renderWaters = (\pos -> spriteCell pos 0 waterS) 
    renderBoxes = (\pos -> spriteCell pos 0 boxS)

    watersP = map renderWaters (waters roomState)  
    boxesP = map renderBoxes (boxes roomState)

  in
    -- combine everything
    pictures ([backgroundP, grid, player] ++ watersP ++ boxesP)

-- fallback in case one of the sprites arguments wasn't passed in 
renderRoom _ _ _ = grid 


-- display each game room at the proper position
render :: Picture -> [Sprite] -> GameState  -> Picture
render backgroundP sprites@(squirrelS:boxS:waterS:_) gameState =
  let 
    --print the text and the cursor
    cursorSuffix = if (isCursorVisible gameState) 
                   then cursorCharacter else ""

    textContent = userText gameState ++ cursorSuffix 
    userDisplay =
      color red $
        translate (-gameWidth / 2 + cellSize / 2) (-gameHeight / 2 + cellSize / 2) $
          scale 0.25 0.25 (text textContent) :: Picture

    debugDisplay = 
      translate (-gameWidth /2 + cellSize/2) (gameHeight/2 - cellSize / 2) $
        scale 0.1 0.1 (text . debugText $ gameState)




    --render only the rooms we need thanks to lazy eval
    firstRoom = renderRoom backgroundP sprites (rooms gameState !! 0)
    secondRoom = renderRoom backgroundP sprites (rooms gameState !! 1)
    thirdRoom = renderRoom backgroundP sprites (rooms gameState !! 2)
    fourthRoom = renderRoom backgroundP sprites (rooms gameState !! 3)

   in 
    case (length . rooms $ gameState) of 
      0 -> nullScreen
      1 -> pictures [firstRoom, userDisplay, debugDisplay]
      2 -> undefined
      3 -> undefined
      4 -> undefined 

render _ _ _ = displayErrorScreen


update :: Float -> GameState -> GameState
update seconds gameState = 
  let 
    --increment the timer value
    newElapsed = elapsedFrames gameState + 1
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


-- removing characters from user input
handleKeys (EventKey (SpecialKey KeyDelete) Down _ _) gs
  | [] <- userText gs = gs
  | otherwise = gs { userText = init (userText gs) }

handleKeys (EventKey (Char '\b') Down _ _) gs
  | [] <- userText gs = gs
  | otherwise = gs { userText = init (userText gs) }


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




--shortcut to clear the text input
handleKeys (EventKey (SpecialKey KeyEnd) Down _ _ ) gs = 
  gs {
    userText = ""
  }

handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) gs = 
  (interpret (userText gs) gs) {userText = ""}
  

handleKeys _ gameState = gameState

main :: IO ()
main = do
  -- load the assets for the render function
  grass <- loadBMP "assets/grass.bmp"
  squirrel <- loadBMP "assets/squirrel.bmp"
  box <- loadBMP ("assets/box.bmp")
  water <- loadBMP ("assets/water.bmp")

  
  let grassS = Sprite  {picture = grass, dimensions = (32,32)} --only used rn for the background

  let squirrelS = Sprite  {picture = squirrel, dimensions = (32,32)}
  let boxS = Sprite {picture = box, dimensions = (32, 32)}
  let waterS = Sprite {picture = water, dimensions = (32, 32)}

  let sprites = [squirrelS, boxS, waterS]
                

  -- generate the background once, for all future uses
  let backgroundP = getBackground grassS
  

  --place the game window in the center of the screen
  screenSize <- getScreenSize 

  let xCentered = (fromIntegral (fst screenSize) - gameWidth) / 2.0
  let yCentered = (fromIntegral (snd screenSize) - gameHeight) / 2.0 
  let window = InWindow "Haskell Puzzle Game" (round gameWidth, round gameHeight) 
               (round xCentered, round yCentered)

  play window backgroundCol framerate initialGameState 
       (render backgroundP sprites) handleKeys update
