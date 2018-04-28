{-|
Module      : Run
Description : This module descirbes the functions to take input and move blocks in the grid accordingly
Copyright   : Sricharan
License     : POPL-2
Maintainer  : cs16btech11044@iith.ac.in
Stability   : experimental
Portability : POSIX
@This module descirbes the functions to take input and move blocks in the grid accordingly@
-}
module Run ( update, eventhandling) where

import State
import Block
import Gameview
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

-- | 'blockspeed' initialises the speed of a block
blockspeed:: Float
blockspeed = 7
-- | 'blockGap' initialises the time interval between the release of two blocks
blockGap :: State->Float
blockGap s = 1.0 / (7.0)
-- | 'eventhandling' takes the input from the keyboard and the current state of the game and moves the blocks accordingly
eventhandling :: Event->State->State
eventhandling (EventKey (SpecialKey KeyLeft) Down _ _) s = moveblock (-2) s
eventhandling (EventKey (SpecialKey KeyRight) Down _ _) s = moveblock (2) s
eventhandling (EventKey (SpecialKey KeyUp) Down _ _) s = clockwise s
eventhandling (EventKey (SpecialKey KeyDown) Down _ _) s = anticlock s
eventhandling _ s = s 

-- | 'moveblock' takes an integer and a state and moves the block by the specified many units vertically
moveblock :: Int->State->State 
moveblock moveby s 
    | posvalidity (block s) pos (grid s) = s {blockpos = pos}
    | otherwise = s
       where 
        pos =(fst (blockpos s) + moveby, snd (blockpos s))
-- | 'transformBlock' takes a function and a state and transforms the block according to the given function
transformBlock :: (Block->Block) -> State -> State
transformBlock transform s
  | posvalidity block1 (blockpos s) (grid s) = s {block= block1}
  | otherwise = s
     where 
       block1 = transform (block s)

-- | 'clockwise' rotates the block in the input state clockwise
clockwise :: State->State
clockwise = transformBlock rotateclock
-- | 'anticlock' rotates the block in the input state anti-clockwise
anticlock :: State->State
anticlock = transformBlock rotateanti

-- | 'update' takes time and a state and updates the state according to the time 
update::Float->State->State
update t s = uUpdate (s {time = (time s + t), dtime = t})
-- | 'uUpdate' takes state and updates 
uUpdate :: State->State
uUpdate s
  | nextrend clockupdate <=0 = applyMove clockupdate { nextrend = blockGap s}
  | otherwise  = clockupdate
     where 
        clockupdate = s {nextrend = (nextrend s)- (dtime s)}
-- | 'posvalidity' takes a block and a position and the grid and returns true if the block can be placed at that position
posvalidity:: Block->(Int,Int)->Grid->Bool
posvalidity block pos grid =insideView && (not colliding)
 where
   insideView = validposition pos block 
   colliding = doescollide block pos grid

-- | 'applyMove' takes a state and moves the block downwards
applyMove:: State->State
applyMove s
  | nextInvalid = handleFullRows (fixBlock s)
  | otherwise = s {blockpos= pos1}
     where
       nextInvalid = not(posvalidity (block s) pos1 (grid s))
       pos1 = (fst (blockpos s), snd (blockpos s) - 2)
-- | 'fixBlock' takes a state and fixes the block in the current location
fixBlock::State->State
fixBlock s 
  | ((snd (blockpos s)) > (2)) = reset s 
  | otherwise = s
     { grid = blockrender (block s) (blockpos s) (grid s)
     , block = randomblock (fst reseed), blockpos =(0,0) , seed = snd reseed
     }
      where 
         reseed :: (Double, StdGen)
         reseed = randomR (0.0,1.0) (seed s)
-- | 'handleFullRows' takes a state and checks if there are any full rows and removes them and adds to the score
handleFullRows :: State->State
handleFullRows s = s {grid = fst result, score = (score s) + linesToScore (snd result)}
  where result = clearFilledRows (grid s)
-- | 'linesToScore' returns the score to be added when 0 or 1 lines are removed
linesToScore :: Int -> Int
linesToScore 0 = 0
linesToScore 1 = 10