{-|
Module      : Main
Description : The main module
Copyright   : Nakul
License     : POPL-2
Maintainer  : cs16btech11023@iith.ac.in
Stability   : experimental
Portability : POSIX
@The main module@
-}
module Main(main) where

import Graphics.Gloss
import State
import Render
import Run
import System.Random
-- | 'window' displays the window
window::Display
window = InWindow "Tetris Game " (1280,768) (200,200)
-- | 'backcol' initializes the background color
backcol :: Color
backcol = white
fps = 60
-- | 'main' this is the function that is invoked when the game is executed
main::IO()
main = do
  sd<-newStdGen
  play window backcol fps initState renderstate eventhandling update