{-|
Module      : Render
Description : This module takes the state and renders it to a picture
Copyright   : Santhosh
License     : POPL-2
Maintainer  : cs16btech11003@iith.ac.in
Stability   : experimental
Portability : POSIX
@This module takes the state and renders it to a picture@
-}
module Render(renderstate) where

import State
import Gameview
import Graphics.Gloss

atomsize = 35   -- The size of each atom
border = quot (768 - (20 * atomsize)) 2
gridwidth = 10 * atomsize --The width of the grid
gridheight = 20 * atomsize --the height of the grid

wallwidth = gridwidth + 2*border
wallheight = gridheight + 2*border

gridcolor = black           
wallcolor = dark (dark blue)
-- | 'viewtoscreen' takes the coordinates in the grid and converts them to screen coordinates 
viewtoscreen ::(Int,Int) -> (Int,Int)
viewtoscreen (tempx,tempy) = (x1,y1)
 where 
    x1 = quot (tempx*atomsize) 2 
    y1 = (11 *atomsize) + (tempy * atomsize) `quot` 2
-- | 'renderatom' takes a coordinate and color and converts it to a picture filled with that color at that position
renderatom:: (Int,Int) ->Color ->Picture
renderatom (tempx,tempy) col = translate (fromIntegral x1) (fromIntegral y1) (color col (rectangleSolid size size))
  where
     x1 = fst transformed
     y1 = snd transformed
     size = 0.9* (fromIntegral atomsize)
     transformed = viewtoscreen (tempx,tempy)
-- | 'rendergrid' takes the grid and converts it to picture
rendergrid:: Grid->Picture
rendergrid grid = pictures (map atomtopic (atompos grid))
  where 
    atomtopic (tempx,tempy,a)
     | tempy > (-3) = pictures []
     | a == Empty = pictures []
     | otherwise = renderatom (tempx,tempy) (atomcol a)
-- | 'renderstate' takes the state of the grid and converts it to picture
renderstate :: State -> Picture
renderstate s = pictures [walls, gameview, activeblock, scoreDis]
  where 
   walls = color wallcolor (rectangleSolid (fromIntegral wallwidth) (fromIntegral wallheight))
   gameview = pictures 
     [ color gridcolor (rectangleSolid (fromIntegral gridwidth) (fromIntegral gridheight))
     , rendergrid (grid s)
     ]
   activeblock = rendergrid (blockrender (block s) (blockpos s) emptygrid)
   scoreDis = translate (-600.0) (200.0) (scale 0.2 0.2 (pictures [playerScore]))
      where
        playerScore = color black (Text scoreText)
        scoreText = "Score: " ++ (show (score s))