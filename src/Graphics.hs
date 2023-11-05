module Graphics where 

import Graphics.Gloss

import DataTypes
import Constants


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

-- fills up the cell at input GAME coordinates with the input sprite
-- and at the input rotation
spriteCell :: Position -> Float -> Sprite -> Picture
spriteCell (x, y) rotation (Sprite{picture=pic, dimensions=(w, h)}) = 
  let 
    centerSprite = rotate rotation $ 
                   scale (cellSize / (fromIntegral w)) (cellSize / (fromIntegral h)) pic

    finSprite = translate  (-gameWidth / 2 + cellSize / 2 + (fromIntegral x) * cellSize)
                           (-gameHeight / 2 + cellSize / 2 + (fromIntegral y) * cellSize)
                           centerSprite
  in
    finSprite



-- all of the pictures that only need to be computed once 
userTextBackdrop :: Picture
userTextBackdrop =  color (makeColor 0 0 0 0.5) $
                    translate 0 (-gameHeight/2) (rectangleSolid (gameWidth) 220)


makeGrid :: Float -> Float -> Picture
makeGrid borderThickness tileSize = 
  pictures (verticalLines ++ horizontalLines ++ vertBorder ++ horBorder)
  where
    vertBorder = color (makeColorI 139 69 19 255) <$> 
                    [Polygon [(x - borderThickness/2, -gameHeight/2), (x + borderThickness/2, -gameHeight/2),
                              (x + borderThickness/2, gameHeight/2), (x - borderThickness/2, gameHeight/2)] 
                    | x <- [-gameWidth/2, gameWidth/2]]

    horBorder = color (makeColorI 139 69 19 255) <$> 
                    [Polygon [(-gameWidth/2, y - borderThickness/2), (gameWidth/2, y- borderThickness/2),
                              (gameWidth/2, y + borderThickness/2), (-gameWidth/2, y + borderThickness/2)] 
                    | y <- [-gameHeight/2, gameHeight/2]]


    verticalLines = [Line [(x, -gameHeight / 2), (x, gameHeight / 2)] | 
                          x <- [-gameWidth/2 + tileSize, -gameWidth/2 + 2*tileSize .. gameWidth / 2 - tileSize]]

    horizontalLines = [Line [(-gameWidth / 2, y), (gameWidth / 2, y)] | 
                          y <- [-gameHeight/2 + tileSize, -gameHeight/2 + 2*tileSize .. gameHeight / 2 - tileSize]]

--main (rows * cols) grid
grid :: Picture
grid = makeGrid 6.0 cellSize

-- grid for halved "resolution" room
miniGrid :: Picture
miniGrid = makeGrid 6.0 (2*cellSize)

textOnScreen :: String -> Float -> Color -> Color -> Picture
textOnScreen textString textWidth textCol backCol =
    let 
        bg = color backCol $ rectangleSolid gameWidth gameHeight

        -- txtWidth = fromIntegral (stringWidth Roman textString)
        -- txtHeight = fromIntegral . fontHeight $ Roman

        txt = color textCol $ translate (-textWidth / 4) 0 $ scale 0.5 0.5 (Text textString)

        -- txt = color textCol $ translate (gameWidth/2 - txtWidth/2) 
        --                                 (gameHeight/2 - txtHeight/2) $ 
        --                       scale 0.5 0.5 (Text textString)

    in
        pictures [bg, txt]

creditsScreen :: Picture
creditsScreen = res 
  where 
      bg = color black $ rectangleSolid gameWidth gameHeight

      txt1 = color white $ translate (-633 / 2 + 20) (gameHeight / 2 - 100) $ scale 0.5 0.5 $ Text "Credits"
      txt2 = color white $ translate (-633 / 2 + 20) (gameHeight / 2 - 200) $ scale 0.5 0.5 $ Text "Rali Lahlou"
      txt3 = color white $ translate (-633 / 2 + 20) (gameHeight / 2 - 300) $ scale 0.5 0.5 $ Text "Brook Nigatu"
      txt4 = color white $ translate (-633 / 2 + 20) (gameHeight / 2 - 400) $ scale 0.5 0.5 $ Text "Milos Oundjian"

      res = pictures [bg, txt1, txt2, txt3, txt4]

pauseScreenPrompt :: Picture
pauseScreenPrompt = Color red $ translate (-gameWidth / 2 + gameWidth / 10) (-gameHeight / 4) $ scale 0.2 0.2 $ Text "Use the Arrow keys to navigate to a previously visited puzzle"

gameOverScreen :: Picture
gameOverScreen = textOnScreen "YOU DIED" 633 red (withAlpha 0.7 black)

outOfBoundsScreen :: Picture
outOfBoundsScreen = textOnScreen "HOW?" 352 black white

displayErrorScreen :: Picture 
displayErrorScreen = textOnScreen "Render function error" 1369 red black

nullScreen :: Picture 
nullScreen = textOnScreen "0" 70 black white

-- tessellates the input sprite to form a background cover 
getBackground :: Sprite -> Picture 
getBackground spr = 
    pictures [spriteCell (x, y) 0 spr | x <- [0..(cols-1)], y <- [0..(rows-1)] ]
