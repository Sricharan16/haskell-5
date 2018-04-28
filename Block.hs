{-|
Module      : Block
Description : This module is constructor for the Blocks in the tetris-game
Copyright   : (c) Some Guy, 2013
                  Someone Else, 2014
License     : POPL-2
Maintainer  : cs16btech110
Stability   : experimental
Portability : POSIX

@This creates a type Block and enumerates it to different possible shapes and randomly selects one of them to insert 
at the top of the game view@
-}
module Block (Block,validposition,blockcolor,randomblock,rotateclock,rotateanti,shapeT,
                 shapeJ,shapeS,shapeZ,shapeA,shapeI,shapeL,shapeO,blockcheck) where
import Graphics.Gloss
import Data.List
-- | 'Block' is a data type of List of Integer pairs(coordinates) and a color of the block 
data Block = Blockpos [(Int,Int)] Color deriving (Show)
-- | 'shapeO' defines a square of size 2x2
shapeO = Blockpos [(-1,1),(-1,-1),(1,-1),(1,1)] red
-- | 'shapeL' defines a shape of L
shapeL =Blockpos [(-3,-1),(1,-1),(-1,-1),(1,1)] green
-- | 'shapeI' defines a shape of 1x4 rectangle
shapeI= Blockpos [(-1,-1),(1,-1),(3,-1),(-3,-1)] blue
-- | 'shapeZ' defines a 90degrees rotated Z
shapeZ=Blockpos [(-1,-1),(-3,1),(-1,1),(1,-1)] yellow
-- | 'shapeA' defines a single block of size 1x1
shapeA = Blockpos [(1,1)] cyan
-- | 'shapeS' defines a mirror view of the z shape
shapeS=Blockpos [(3,1),(-1,-1),(1,1),(1,-1)] magenta
-- | 'shapeJ' defines a mirror view of the L shape
shapeJ = Blockpos [(3,-1),(-1,1),(-1,-1),(1,-1)] rose
-- | 'shapeT' defines a shape of T 
shapeT = Blockpos [(3,-1),(-1,-1),(1,-1),(1,1)] violet

-- |the 'validposition' checks whether the position of the block is within the bounds of the grid or not
validposition::(Int,Int)-- ^The 'Int,Int' argument is the block position 
                ->Block-- ^the Block is input Block
                   ->Bool -- ^the return value is true if it is valid else false
validposition (x,y) (Blockpos bs _) = and (map valid bs)
     where 
         valid (tempx, tempy)= (tempx+x>= -9)&&(tempy+y>= -41)&&(tempx+x<=9)&&(tempy+y<=1)

-- |the 'blockcheck' takes a coordinate point and a block and checks whether the point lies in the block or not 
blockcheck :: (Int,Int) -> Block ->Bool
blockcheck b (Blockpos bs _) = elem b bs
-- |the 'rotateclock' takes a block and rotates it clockwise and returns the rotated block 
rotateclock:: Block-> Block 
rotateclock (Blockpos bs col)= Blockpos (map  rc bs) col 
     where 
       rc (x,y) = (y,-x)

-- |the 'rotateanti' takes a block and rotates it anti clockwise and returns the rotated block 
rotateanti:: Block-> Block 
rotateanti (Blockpos bs col)= Blockpos (map  ra bs) col 
     where 
       ra (x,y) = (-y,x)
-- |the 'blockcolor' takes a block as input and assigns a color to it 
blockcolor::Block->Color
blockcolor (Blockpos _ col )= col


-- |the 'randomblock' takes a random seed and returns a block corresponding to it 
randomblock::Double->Block
randomblock temp =case (mod (truncate(temp*1000))  8) of 
    0->shapeT
    1->shapeJ
    2->shapeS
    3->shapeA
    4->shapeZ
    5->shapeL
    6->shapeO
    7->shapeI
