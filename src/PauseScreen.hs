module PauseScreen where

import Zippers
import DataTypes
import Constants

import Graphics.Gloss


-- Compute the final picture to be drawn on the pause screen from the sprites and the screen pointer in GameState
-- First Sprite is for the current Position in the tree.
-- Second Sprite is for unlocked puzzles.
-- Third Sprite is for locked puzzles.
getTreePic :: Sprite -> Sprite -> Sprite -> BinZip ScreenWrap -> Picture
getTreePic activePic visitedPic toVisitPic sp =
  let 
    -- convert the screen tree to a sprite tree
    spriteTree = getScreenSprite activePic visitedPic toVisitPic <$> toTree sp
  in
    pictures $ fromSpriteTree (0, gameHeight / 2 - gameHeight / 10, gameWidth / 4, 50) spriteTree

-- compute the list of pictures to be rendered from the sprite tree constructed above
fromSpriteTree :: (Float, Float, Float, Float) -> BinTree Sprite -> [Picture]
fromSpriteTree _ Leaf = []
fromSpriteTree (x, y, dx, dy) (B s@(Sprite{picture = pic}) lsub rsub) =
  let 
    -- recursive computation
    lpics = fromSpriteTree (x - dx, y - dy, dx / 2, dy) lsub
    rpics = fromSpriteTree (x + dx, y - dy, dx / 2, dy) rsub
    
    -- red colored lines for edges
    col = makeColor 1.0 0 0 1.0 
    linel = color col $ Line [(x, y), (x - dx, y - dy)]
    liner = color col $ Line [(x, y), (x + dx, y - dy)]

    -- only draw lines for non-leaf children
    lpics' = if null lpics then lpics else linel : lpics
    rpics' = if null rpics then rpics else liner : rpics

  in
    lpics' ++ rpics' ++ [translate x y pic]



getScreenSprite :: Sprite -> Sprite -> Sprite -> ScreenWrap -> Sprite
getScreenSprite activePic visitedPic toVisitPic sw@(ScreenWrap{active = act, visited = vis})
  | act = activePic
  | vis = visitedPic
  | otherwise = toVisitPic