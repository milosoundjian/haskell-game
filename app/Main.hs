module Main (main) where

--external imports
--import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment


--internal imports
import DataTypes
import Constants
import LevelHelper
import LevelData
import Interpreter
import Graphics
import PauseScreen


  

-- display the user character + the current text input
renderRoom :: Picture -> [Sprite] -> RoomState -> Picture
renderRoom backgroundP 
           sprites@(squirrelS:spikeS:boxS:acornS:branchS:sonicS:_) 
           roomState = 
  let 
    --display the player
    rotation = charRot roomState
    playerSprite = if (isDoubleSpeed roomState)  
                      then sonicS
                      else squirrelS 

    player = spriteCell (character roomState) rotation playerSprite

    -- display the grid entities
    renderBoxes pos = spriteCell pos 0 boxS
    renderSpikes pos = spriteCell pos 0 spikeS

    boxesP = map renderBoxes (walls roomState)  
    spikesP = map renderSpikes (spikes roomState)

    --display the special tile
    -- TODO : replace the solid blocks with real assets
    specialDraw = if (isTerminal roomState) 
                    then (\x -> spriteCell x 0 acornS)
                    else (\x -> spriteCell x 0 branchS)
    specialP = specialDraw $ specialPos roomState

  in
    -- combine everything
    pictures ([backgroundP, grid, player, specialP] ++ boxesP ++ spikesP)

-- fallback in case one of the sprites arguments wasn't passed in 
renderRoom _ _ _ = displayErrorScreen 



-- display each game room at the proper position
render :: Picture -> [Sprite] -> GameState  -> Picture
render backgroundP sprites@(squirrelS:spikeS:boxS:acornS:_) gameState@(GameState{paused = True, screenPointer = sp}) = 
  let 
    tpic = getTreePic squirrelS acornS spikeS sp
  in
    pictures [tpic, pauseScreenPrompt]
render backgroundP sprites (GameState{isCredits = True}) = creditsScreen
render backgroundP sprites gameState =
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
    newT = elapsedFrames gameState + 1
    oldVis = isCursorVisible gameState  
    newVisibility = if (mod newT cursorFlickerDuration == 0) then not oldVis else oldVis 


    -- check that none of the rooms have gamed overed
    gameOverState = (gameOver gameState || any rGameOver (rooms gameState))

    --create the new gamestate
    newGs = gameState {gameOver = gameOverState, elapsedFrames = newT, isCursorVisible = newVisibility}

  in
    --handle all timer related utilities
    if isDancing gameState && (mod newT danceCooldown == 0) then 
      rotAllRooms newGs 90
    else 
      newGs




handleKeys :: Event -> GameState -> GameState

handleKeys (EventKey (SpecialKey key) Down _ _) gameState@(GameState{isCredits = True}) = gameState

handleKeys (EventKey (SpecialKey key) Down _ _) gameState@(GameState{paused = True})
  -- allow user to navigate previously visited rooms from the pause screen
  | key == togglePause = gameState{paused = False}
  | key == upInput = switchRooms gameState UP
  | key == leftInput = switchRooms gameState LEFT
  | key == rightInput = switchRooms gameState RIGHT 

-- HANDLE all special characters in one BIG boolean guard
handleKeys (EventKey (SpecialKey key) Down _ _) gameState

  | key == leftInput = movedGameState gameState LEFT
  | key == rightInput = movedGameState gameState RIGHT
  | key == upInput = movedGameState gameState UP
  | key == downInput = movedGameState gameState DOWN
  | key == togglePause = gameState{paused = not $ paused gameState}

  | key == undoInput = undoLastMove gameState

  | key == resetInput = restartLevel gameState

  | key == clearTextInput = gameState {userText = ""}
  | key == submitTextInput = (interpret (userText gameState) gameState) {userText = ""}



-- removing characters from user input
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

--catch all for unused inputs
handleKeys _ gs = gs

main :: IO ()
main = do
  -- load the assets for the render function
  grass <- loadBMP "assets/grass.bmp"
  squirrel <- loadBMP "assets/squirrel.bmp"
  sonic <- loadBMP "assets/sonic.bmp"
  spike <- loadBMP "assets/spike.bmp"
  box <- loadBMP "assets/box.bmp"
  acorn <- loadBMP "assets/acorn.bmp"
  branch <- loadBMP "assets/branch_glitched.bmp"
  
  let grassS = Sprite  {picture = grass, dimensions = (32,32)} --only used rn for the background

  let squirrelS = Sprite  {picture = squirrel, dimensions = (32,32)}
  let sonicS = Sprite {picture = sonic, dimensions= (32, 32)}
  let spikeS = Sprite {picture = spike, dimensions = (32, 32)}
  let boxS = Sprite {picture = box, dimensions = (32, 32)}
  let acornS = Sprite {picture = acorn, dimensions = (32, 32)}
  let branchS = Sprite {picture = branch, dimensions = (32, 32)}

  let sprites = [squirrelS, spikeS, boxS, acornS, branchS, sonicS]
                

  -- generate the background once, for all future uses
  let backgroundP = getBackground grassS
  

  --place the game window in the center of the screen
  screenSize <- getScreenSize 

  let xCentered = (fromIntegral (fst screenSize) - gameWidth) / 2.0
  --let yCentered = (fromIntegral (snd screenSize) - gameHeight) / 2.0 
  let window = InWindow "Haskell Puzzle Game" (round gameWidth, round gameHeight) 
               (round xCentered, 0)

  -- load the first level in list 
  -- let firstLevel = (fst $ head levelsData) {rooms = head $ snd $ head levelsData} 
  let initialState = initRooms gsBase

  --load the first level in list
  play window backgroundCol framerate initialState
       (render backgroundP sprites) handleKeys update
