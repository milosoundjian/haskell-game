module Main (main) where

--external imports
--import Debug.Trace
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
renderRoom backgroundP sprites@(squirrelS:spikeS:waterS:_) roomState = 
  let 
    --display the player
    rotation = charRot roomState
    player = spriteCell (character roomState) rotation (squirrelS)

    renderWaters pos = spriteCell pos 0 waterS
    renderSpikes pos = spriteCell pos 0 spikeS

    watersP = map renderWaters (waters roomState)  
    spikesP = map renderSpikes (spikes roomState)

  in
    -- combine everything
    pictures ([backgroundP, grid, player] ++ watersP ++ spikesP)

-- fallback in case one of the sprites arguments wasn't passed in 
renderRoom _ _ _ = grid 


-- display each game room at the proper position
render :: Picture -> [Sprite] -> GameState  -> Picture
render backgroundP sprites@(squirrelS:spikeS:waterS:_) gameState =
  let 
    -- rendering all of the UI elements 
    cursorSuffix = if (isCursorVisible gameState) 
                   then cursorCharacter else ""

    textContent = userText gameState ++ cursorSuffix 
    userDisplay =
      color white $
        translate (-gameWidth / 2 + cellSize / 2) (-gameHeight / 2 + cellSize / 2) $
          scale 0.25 0.25 (text textContent) 

    debugDisplay = 
      translate (-gameWidth /2 + cellSize/2) (gameHeight/2 - cellSize / 2) $
        scale 0.1 0.1 (text . debugText $ gameState)

    uiDisplay = (if debugModeEnabled then pictures [userTextBackdrop, userDisplay, debugDisplay]  
                                     else pictures [userTextBackdrop, userDisplay])


    -- show the game over screen if applicable
    gameOverOverlay = if (gameOver gameState) then [gameOverScreen] else []

    --render only the rooms we're gonna use thanks to lazy evaluation
    firstRoom = renderRoom backgroundP sprites (rooms gameState !! 0)
    secondRoom = renderRoom backgroundP sprites (rooms gameState !! 1)
    thirdRoom = renderRoom backgroundP sprites (rooms gameState !! 2)
    fourthRoom = renderRoom backgroundP sprites (rooms gameState !! 3)

    firstRoomMini = translate (-gameWidth/4) (gameHeight/4) $ 
                    scale 0.5 0.5 firstRoom 
    secondRoomMini = translate (gameWidth/4) (gameHeight/4) $
                     scale 0.5 0.5 secondRoom
    thirdRoomMini = translate (-gameWidth/4) (-gameHeight/4) $
                    scale 0.5 0.5 thirdRoom
    fourthRoomMini = translate (gameWidth/4) (-gameHeight / 4) $ 
                 scale 0.5 0.5 fourthRoom

    fillerDouble = Blank
    fillerSingle = Blank
      


   in 
    case (length $ rooms gameState) of 
      0 -> nullScreen
      1 -> pictures ([firstRoom, uiDisplay] ++ gameOverOverlay) 
      2 -> pictures ([firstRoomMini, secondRoomMini, fillerDouble, uiDisplay] ++ gameOverOverlay)
      3 -> pictures ([firstRoomMini, secondRoomMini, thirdRoomMini, fillerSingle, uiDisplay] ++ gameOverOverlay)
      4 -> pictures ([firstRoomMini, secondRoomMini, thirdRoomMini, fourthRoomMini, uiDisplay] ++ gameOverOverlay) 
      _ -> outOfBoundsScreen

render _ _ _ = displayErrorScreen


update :: Float -> GameState -> GameState
update seconds gameState = 
  let 
    --increment the timer value
    newElapsed = elapsedFrames gameState + 1
    newVisibility = not (isCursorVisible gameState)

    -- check that none of the rooms have gamed overed
    gameOverState = (gameOver gameState || any rGameOver (rooms gameState))

  in
    --handle all timer related utilities
    if (mod newElapsed cursorFlickerDuration == 0) then 
      gameState {gameOver = gameOverState, elapsedFrames = newElapsed, isCursorVisible = newVisibility}
    else
      gameState {gameOver = gameOverState, elapsedFrames = newElapsed}



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

-- send input for processing
handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) gs = 
  (interpret (userText gs) gs) {userText = ""}
  
-- Right shift undoes the last action
handleKeys (EventKey (SpecialKey KeyShiftR) Down _ _) gs = 
  undoLastMove gs

handleKeys _ gameState = gameState

main :: IO ()
main = do
  -- load the assets for the render function
  grass <- loadBMP "assets/grass.bmp"
  squirrel <- loadBMP "assets/squirrel.bmp"
  spike <- loadBMP "assets/spike.bmp"
  water <- loadBMP "assets/water.bmp"

  
  let grassS = Sprite  {picture = grass, dimensions = (32,32)} --only used rn for the background

  let squirrelS = Sprite  {picture = squirrel, dimensions = (32,32)}
  let spikeS = Sprite {picture = spike, dimensions = (32, 32)}
  let waterS = Sprite {picture = water, dimensions = (32, 32)}

  let sprites = [squirrelS, spikeS, waterS]
                

  -- generate the background once, for all future uses
  let backgroundP = getBackground grassS
  

  --place the game window in the center of the screen
  screenSize <- getScreenSize 

  let xCentered = (fromIntegral (fst screenSize) - gameWidth) / 2.0
  let yCentered = (fromIntegral (snd screenSize) - gameHeight) / 2.0 
  let window = InWindow "Haskell Puzzle Game" (round gameWidth, round gameHeight) 
               (round xCentered, round yCentered)


  --load the first level in list
  play window backgroundCol framerate (head levelsData) 
       (render backgroundP sprites) handleKeys update
