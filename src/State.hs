{-|
Module      : State
Description : This module encapsulates the state of the grid
Copyright   : Nakul
License     : POPL-2
Maintainer  : cs16btech11023@iith.ac.in
Stability   : experimental
Portability : POSIX
@This module encapsulates the state of the grid and defines a initial state of the grid@
-}
module State(State(..), initState, reset) where
import Gameview
import Block
import System.Random
-- |'State' encapsulates the state of the grid 
data State = State
     { grid :: Grid
     , time :: Float
     , dtime :: Float
     , nextrend :: Float
     , block :: Block
     , blockpos :: (Int,Int)
     , seed :: StdGen
     , score:: Int
     } deriving (Show)
-- |'initState' takes Nothing and returns the initial state 
initState :: State
initState = State
   {grid = emptygrid
   , time =0
   , dtime =0
   , nextrend =0
   , block = shapeO
   , blockpos =(0,0)
   , seed =mkStdGen 0
   , score = 0
   }
-- |'reset' takes a state and returns the initial state with the seed retained
reset :: State->State
reset s = initState {seed = (seed s)}