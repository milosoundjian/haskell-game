module Graphics where 

import Graphics.Gloss
import Graphics.UI.GLUT.Fonts

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
grid :: Picture
grid = pictures (verticalLines ++ horizontalLines)
  where
    verticalLines = [Line [(x, -gameHeight / 2), (x, gameHeight / 2)] | x <- [-gameWidth / 2, -gameWidth / 2 + cellSize .. gameWidth / 2]]
    horizontalLines = [Line [(-gameWidth / 2, y), (gameWidth / 2, y)] | y <- [-gameHeight / 2, -gameHeight / 2 + cellSize .. gameHeight / 2]]

textOnScreen :: String -> Color -> Color -> Picture
textOnScreen textString textCol backCol =
    let 
        bg = color backCol $ rectangleSolid gameWidth gameHeight

        -- txtWidth = fromIntegral (stringWidth Roman textString)
        -- txtHeight = fromIntegral . fontHeight $ Roman

        txt = color textCol $ translate 0 0 $ scale 0.5 0.5 (Text textString)

        -- txt = color textCol $ translate (gameWidth/2 - txtWidth/2) 
        --                                 (gameHeight/2 - txtHeight/2) $ 
        --                       scale 0.5 0.5 (Text textString)

    in
        pictures [bg, txt]

gameOverScreen :: Picture
gameOverScreen = textOnScreen "YOU DIED" red black

outOfBoundsScreen :: Picture
outOfBoundsScreen = textOnScreen "OUT OF BOUNDS" red black

displayErrorScreen :: Picture 
displayErrorScreen = textOnScreen "COULDN'T RENDER SCENE :(" red black

nullScreen :: Picture 
nullScreen = textOnScreen "0" black white



-- tessellates the input sprite to form a background cover 
getBackground :: Sprite -> Picture 
getBackground spr = 
    pictures [spriteCell (x, y) 0 spr | x <- [0..(cols-1)], y <- [0..(rows-1)] ]