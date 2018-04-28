{-|
Module      : Gameview
Description : This module creates a Grid as a playing space for the tetris
Copyright   : Nakul
License     : POPL-2
Maintainer  : cs16btech11023@iith.ac.in
Stability   : experimental
Portability : POSIX
@This creates a Grid as a list of rows where each row is a list of atoms and also has functions to clear a filled row and check 
whether the blocks are colliding@
-}
module Gameview(Grid,emptygrid,atompos,blockrender,doescollide,rownumber,atomindex,Atom(..),atomcol,clearFilledRows)
where 
import Block 
import Graphics.Gloss
import Data.List
-- |the 'Atom' is the basic unit of a block which is filled with a particular color 
data Atom = Empty | FilledWith Color deriving (Show, Eq)
-- |the 'atomcol' takes an atom and assigns a color to it 
atomcol:: Atom->Color
atomcol (FilledWith ff)=ff
atomcol _ = white
-- | The 'Row' is a List of atoms to build a grid 
data Row = RowOfAtoms [Atom] deriving (Show)
-- | 'emptyrow' is an empty row with 10 empty atoms in it 
emptyrow= RowOfAtoms (replicate 10 Empty)
-- | 'Grid' is a List of rows representing the playing space 
data Grid =GridOfRows [Row] deriving (Show)
-- | 'emptygrid' is a list of empty rows 
emptygrid = GridOfRows (replicate 22 emptyrow)
-- | 'rownumber' assigns the row numbers to the rows 
rownumber::Grid->[(Int,Row)]
rownumber (GridOfRows cs) = zip[1,-1..(-41)] cs 
-- | 'atomindex' assigns the atom index to the atoms in a single row
atomindex::Row->[(Int,Atom)]
atomindex (RowOfAtoms cs) = zip[-9,-7..9] cs 
-- | 'atompos' assigns the atom coordinates to all the atoms in the grid
atompos::Grid->[(Int,Int,Atom)]
atompos g=concat (map exats (rownumber g))
    where 
       exats (y,cs) = map exat (atomindex cs)
          where 
           exat (x,c) = (x,y,c)
-- | 'blockrender' takes a block and a coordinate point and a grid and returns a grid with the given block in it at the specified location 
blockrender::Block->(Int,Int)->Grid->Grid
blockrender block (tempx,tempy) grid 
 |odd tempx || odd tempy = error "The given coordinates cannot be used"
 |otherwise = GridOfRows (map rowrender (rownumber grid))
     where  rowrender (y, row) = RowOfAtoms (map atomrender (atomindex row))
               where atomrender (x,c)
                       |c/= Empty = c
                      |blockcheck (x-tempx,y-tempy) block = FilledWith (blockcolor block)
                           |otherwise = Empty
-- | 'doescollide' takes a block and a coordinate and a grid and returns true if the given block collides with any atom in the grid
doescollide::Block->(Int,Int)->Grid->Bool
doescollide block pos grid = gridcolli render grid 
     where 
        render = blockrender block pos emptygrid
        gridcolli (GridOfRows rs1) (GridOfRows rs2) = or (map rowcolli (zip rs1 rs2))
        rowcolli (RowOfAtoms cs1, RowOfAtoms cs2) = or (map atomscolli (zip cs1 cs2))
        atomscolli (x,y) = (x /= Empty) && (y /=Empty)
-- | 'clearFilledRows' takes the grid and returns a grid with the filled rows removed and the number of filled rows removed
clearFilledRows :: Grid-> (Grid, Int)
clearFilledRows (GridOfRows rs) = (grid1 , count)
 where
   grid1 = GridOfRows (clear ++ rem)
   rem = filter notFull rs
   count::Int
   count = (length rs) - (length rem)
   clear :: [Row]
   clear = replicate count emptyrow
   notFull (RowOfAtoms cs) = not (and (map (\c -> c /=Empty) cs))