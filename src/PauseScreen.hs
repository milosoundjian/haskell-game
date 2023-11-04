module PauseScreen where

import Zippers
import DataTypes
import Constants

import Graphics.Gloss


getTreePic :: Sprite -> Sprite -> Sprite -> BinZip ScreenWrap -> Picture
getTreePic activePic visitedPic toVisitPic sp =
  let 
    spriteTree = getScreenSprite activePic visitedPic toVisitPic <$> toTree sp
  in
    fromSpriteTree (0, gameHeight / 2 - 80, gameWidth / 4, 50) spriteTree

fromSpriteTree :: (Float, Float, Float, Float) -> BinTree Sprite -> Picture
fromSpriteTree _ Leaf = pictures []
fromSpriteTree (x, y, dx, dy) (B s@(Sprite{dimensions = (w, h), picture = pic}) lsub rsub) =
  let 
    lpic = fromSpriteTree (x - dx, y - dy, dx / 2, dy) lsub
    rpic = fromSpriteTree (x + dx, y - dy, dx / 2, dy) rsub
  in
    pictures [translate x y pic, lpic, rpic]



getScreenSprite :: Sprite -> Sprite -> Sprite -> ScreenWrap -> Sprite
getScreenSprite activePic visitedPic toVisitPic sw@(ScreenWrap{active = act, visited = vis})
  | act = activePic
  | vis = visitedPic
  | otherwise = toVisitPic