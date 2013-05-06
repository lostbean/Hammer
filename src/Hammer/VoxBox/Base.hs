{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Hammer.VoxBox.Base
  ( module Hammer.VoxBox.Types
  , (%@), (%@?), (%#), (%#?), (#-#), (#+#), (#!)
  , checkPosBound
  , getVoxelID
  , getCrossPos
  , isInBox
  , sizeVoxBoxRange
  , splitInTwoBox
  , mergeVoxBoxRange
  , mkStdVoxBoxRange
  , getBoxLinRange
  , getVoxBoxDim
  , evalLinPos
  , getRangePos
  , xDir, yDir, zDir
  , deltaXYplus, deltaXYminus, deltaXZplus, deltaXZminus
  , deltaYXplus, deltaYXminus, deltaYZplus, deltaYZminus
  , deltaZXplus, deltaZXminus, deltaZYplus, deltaZYminus
  , getXYplus, getXYminus, getXZplus, getXZminus
  , getYXplus, getYXminus, getYZplus, getYZminus
  , getZXplus, getZXminus, getZYplus, getZYminus
                                      
  , isFaceX, isFaceY, isFaceZ
  , toFaceVoxelPos, toFacePos
  , isEdgeX, isEdgeY, isEdgeZ
  , toEdgeVoxelPos, toEdgePos
                    
  , (#*), (#/)
  , evalDisplacedVoxelPos
  , evalVoxelPos
  , evalFacePos
  , findIntersection
  , getEdgeEndPoints
  , getEdgeEndPos
  , posSet2VoxBox
  , posRange2VoxBox
  , isOnEdge
  , isMinRange
  , fastEvalVoxelPos

  , Plane (..)
  , Line  (..)
    
  ) where

import qualified Data.Vector           as V
import qualified Data.Vector.Unboxed   as U

import           Data.Vector           (Vector, (!))
import           Data.Bits             ((.&.), (.|.), complement)

import Hammer.VoxBox.Types
import Hammer.Math.Algebra

-- ==============================================================================

{-# INLINE checkPosBound #-}    
checkPosBound :: VoxBoxRange -> VoxelPos -> Bool
checkPosBound VoxBoxRange{..} pos = let
  (VoxBoxDim maxX maxY maxZ) = vbrDim
  (VoxelPos x y z)           = pos #-# vbrOrigin
  in x>=0 && y>=0 && z>=0 && x<maxX && y<maxY && z<maxZ

{-# INLINE unsafeGetVoxelID #-}    
unsafeGetVoxelID :: VoxBoxRange -> VoxelPos -> Int
unsafeGetVoxelID VoxBoxRange{..} pos = let
  (VoxBoxDim maxX maxY _) = vbrDim
  (VoxelPos x y z)        = pos #-# vbrOrigin
  in abs x + abs (maxX*y) + abs (maxX*maxY*z)

{-# INLINE getVoxelID #-}    
getVoxelID :: VoxBoxRange -> VoxelPos -> Maybe Int
getVoxelID vbr pos
  | checkPosBound vbr pos = return $ unsafeGetVoxelID vbr pos
  | otherwise             = Nothing
                            
{-# INLINE unsafeGetVoxelPos #-}    
unsafeGetVoxelPos :: VoxBoxRange -> Int -> VoxelPos
unsafeGetVoxelPos VoxBoxRange{..} i = vbrOrigin #+# VoxelPos qx qy qz
  where
    (VoxBoxDim dx dy _) = vbrDim
    (qz, rz) = i  `quotRem` (dx*dy)
    (qy, ry) = rz `quotRem` dx
    qx       = ry
                           
{-# INLINE getVoxelPos #-}    
getVoxelPos :: VoxBoxRange -> Int -> Maybe VoxelPos
getVoxelPos VoxBoxRange{..} i 
  | dx <= 0 && dy <= 0 && dz <= 0 = Nothing
  | qx < dx && qy < dy && qz < dz = return $ vbrOrigin #+# VoxelPos qx qy qz
  | otherwise                     = Nothing
  where
    (VoxBoxDim dx dy dz) = vbrDim
    (qz, rz) = i  `quotRem` (dx*dy)
    (qy, ry) = rz `quotRem` dx
    qx       = ry

{-# INLINE (%@)  #-} 
(%@) :: VoxBoxRange -> VoxelPos -> Int
vbr %@ pos = unsafeGetVoxelID vbr pos

{-# INLINE (%@?) #-} 
(%@?) :: VoxBoxRange -> VoxelPos -> Maybe Int
vbr %@? pos = getVoxelID vbr pos
              
{-# INLINE (%#) #-} 
(%#) :: VoxBoxRange -> Int -> VoxelPos
vbr %# i = unsafeGetVoxelPos vbr i

{-# INLINE (%#?) #-} 
(%#?) :: VoxBoxRange -> Int -> Maybe VoxelPos
vbr %#? i = getVoxelPos vbr i

{-# INLINE (#+#) #-} 
(#+#) :: VoxelPos -> VoxelPos -> VoxelPos
(VoxelPos x1 y1 z1) #+# (VoxelPos x2 y2 z2) = VoxelPos (x1+x2) (y1+y2) (z1+z2)

{-# INLINE (#-#) #-} 
(#-#) :: VoxelPos -> VoxelPos -> VoxelPos
(VoxelPos x1 y1 z1) #-# (VoxelPos x2 y2 z2) = VoxelPos (x1-x2) (y1-y2) (z1-z2)

{-# INLINE (#*) #-} 
(#*) :: VoxelPos -> Int -> VoxelPos
(VoxelPos x y z) #* k = VoxelPos (x*k) (y*k) (z*k)

(#/) :: VoxelPos -> Int -> (VoxelPos, VoxelPos)
(VoxelPos x y z) #/ k = let
  (q1,r1) = x `quotRem` k
  (q2,r2) = y `quotRem` k
  (q3,r3) = z `quotRem` k
  q = VoxelPos q1 q2 q3
  r = VoxelPos r1 r2 r3
  in (q, r)

{-# INLINE (#!) #-}    
(#!) :: VoxBox a -> VoxelPos -> Maybe a
VoxBox{..} #! pos
  | checkPosBound dimension pos = let
    i = unsafeGetVoxelID dimension pos
    in return $ grainID ! i 
  | otherwise                   = Nothing

-- ======================== Directions functions ==================================

xDir :: VoxelPos
xDir = VoxelPos 1 0 0

yDir :: VoxelPos
yDir = VoxelPos 0 1 0

zDir :: VoxelPos
zDir = VoxelPos 0 0 1

{-# INLINE getCrossPos #-}
getCrossPos :: CartesianDir -> VoxelPos -> Vector (VoxelPos, CrossDir)
getCrossPos dir p = case dir of
  XDir -> V.fromList [getXYplus p, getXYminus p, getXZplus p, getXZminus p]
  YDir -> V.fromList [getYXplus p, getYXminus p, getYZplus p, getYZminus p]
  ZDir -> V.fromList [getZXplus p, getZXminus p, getZYplus p, getZYminus p]
  
deltaXYplus, deltaXYminus, deltaXZplus, deltaXZminus :: VoxelPos
deltaYXplus, deltaYXminus, deltaYZplus, deltaYZminus :: VoxelPos
deltaZXplus, deltaZXminus, deltaZYplus, deltaZYminus :: VoxelPos
deltaXYplus  = VoxelPos   1    1    0   
deltaXYminus = VoxelPos   1  (-1)   0   
deltaXZplus  = VoxelPos   1    0    1
deltaXZminus = VoxelPos   1    0  (-1)
deltaYXplus  = VoxelPos   1    1    0   
deltaYXminus = VoxelPos (-1)   1    0   
deltaYZplus  = VoxelPos   0    1    1
deltaYZminus = VoxelPos   0    1  (-1)
deltaZXplus  = VoxelPos   1    0    1
deltaZXminus = VoxelPos (-1)   0    1
deltaZYplus  = VoxelPos   0    1    1
deltaZYminus = VoxelPos   0  (-1)   1

getXYplus, getXYminus, getXZplus, getXZminus :: VoxelPos -> (VoxelPos, CrossDir)
getYXplus, getYXminus, getYZplus, getYZminus :: VoxelPos -> (VoxelPos, CrossDir)
getZXplus, getZXminus, getZYplus, getZYminus :: VoxelPos -> (VoxelPos, CrossDir)
getXYplus  (VoxelPos x y z) = (VoxelPos (x+1) (y+1)  z   , XYplus )
getXYminus (VoxelPos x y z) = (VoxelPos (x+1) (y-1)  z   , XYminus)
getXZplus  (VoxelPos x y z) = (VoxelPos (x+1)  y    (z+1), XZplus )
getXZminus (VoxelPos x y z) = (VoxelPos (x+1)  y    (z-1), XZminus)
getYXplus  (VoxelPos x y z) = (VoxelPos (x+1) (y+1)  z   , YXplus )
getYXminus (VoxelPos x y z) = (VoxelPos (x-1) (y+1)  z   , YXminus)
getYZplus  (VoxelPos x y z) = (VoxelPos  x    (y+1) (z+1), YZplus )
getYZminus (VoxelPos x y z) = (VoxelPos  x    (y+1) (z-1), YZminus)
getZXplus  (VoxelPos x y z) = (VoxelPos (x+1)  y    (z+1), ZXplus )
getZXminus (VoxelPos x y z) = (VoxelPos (x-1)  y    (z+1), ZXminus)
getZYplus  (VoxelPos x y z) = (VoxelPos  x    (y+1) (z+1), ZYplus )
getZYminus (VoxelPos x y z) = (VoxelPos  x    (y-1) (z+1), ZYminus)
                                  
-- ======================== Geometric planes and lines ==================================
                              
newtype Plane v u = Plane (v, u) deriving (Show, Eq)
newtype Line  v u = Line  (v, u) deriving (Show, Eq)

-- =========================== Generic tools ===================================

evalLinPos :: VoxBox a -> CartesianDir -> Int -> Double
evalLinPos vbox dir pos
  | dir == XDir  = vx
  | dir == YDir  = vy
  | otherwise    = vz
  where 
    VoxBoxOrigin  ix iy iz = origin  vbox
    VoxelDim      dx dy dz = spacing vbox
    vx = (ix - dx/2) + dx * (fromIntegral pos)
    vy = (iy - dy/2) + dy * (fromIntegral pos)
    vz = (iz - dz/2) + dz * (fromIntegral pos)

evalDisplacedVoxelPos :: VoxBox a -> CartesianDir -> VoxelPos -> Vec3
evalDisplacedVoxelPos vbox dir vpos = let
  v = evalVoxelPos vbox vpos
  VoxelDim dx dy dz = spacing vbox
  in case dir of
  XDir -> v &+ Vec3 dx 0  0
  YDir -> v &+ Vec3 0  dy 0
  ZDir -> v &+ Vec3 0  0  dz

evalVoxelPos :: VoxBox a -> VoxelPos -> Vec3
evalVoxelPos vbox (VoxelPos x y z) = let
  VoxBoxOrigin  ix iy iz = origin  vbox
  VoxelDim      dx dy dz = spacing vbox
  vx = (ix - dx/2) + dx * (fromIntegral x)
  vy = (iy - dy/2) + dy * (fromIntegral y)
  vz = (iz - dz/2) + dz * (fromIntegral z)
  in Vec3 vx vy vz

evalFacePos :: VoxBox a -> FaceVoxelPos -> Plane Vec3 Normal3
evalFacePos vbox face = let
  func (a, b) = Plane $ (evalVoxelPos vbox a, toNormalUnsafe b)
  in func $ case face of
    Fx p -> (p, Vec3 1 0 0)
    Fy p -> (p, Vec3 0 1 0)
    Fz p -> (p, Vec3 0 0 1)

findIntersection :: (DotProd u, UnitVector v u)=> Plane v u -> Line v u -> Maybe v
findIntersection (Plane (p0, n)) (Line (l0, l))
  | kb == 0         = Nothing
  | k > 0 && k <= 1 = Just $ k *& (fromNormal l) &+ l0
  | otherwise       = Nothing
  where
    ka = (p0 &- l0) &. (fromNormal n)
    kb = l &. n
    k = ka/kb

getEdgeEndPoints :: VoxBox a -> (VoxelPos, VoxelPos) -> (Vec3, Vec3)
getEdgeEndPoints vbox (v1,v2) = let
  func = evalVoxelPos vbox
  in (func v1, func v2)

getEdgeEndPos :: EdgeVoxelPos -> (VoxelPos, VoxelPos)
getEdgeEndPos edge = case edge of
  Ex p -> (p, p #+# (VoxelPos 1 0 0))
  Ey p -> (p, p #+# (VoxelPos 0 1 0))
  Ez p -> (p, p #+# (VoxelPos 0 0 1))

getVoxBoxDim :: VoxBoxDim -> (Int, Int, Int)
getVoxBoxDim (VoxBoxDim x y z) = (x,y,z)

{-# INLINE getBoxLinRange  #-}
getBoxLinRange :: VoxBoxRange -> (Int, Int, Int, Int, Int, Int)
getBoxLinRange (VoxBoxRange (VoxelPos lx ly lz) (VoxBoxDim dx dy dz)) = let
  ux = func dx lx
  uy = func dy ly
  uz = func dz lz
  {-# INLINE func #-}
  func dk k
    | dk > 0    = k + (dk - 1)
    | otherwise = k 
  in (lx, ux, ly, uy, lz, uz)

{-# INLINE splitInTwoBox #-}    
splitInTwoBox :: VoxBoxRange -> Maybe (VoxBoxRange, VoxBoxRange)
splitInTwoBox box@(VoxBoxRange org boxdim)
  | sizeVoxBoxRange box <= 1 = Nothing
  | otherwise                = let
    getDir (VoxBoxDim dx dy dz)
      | dx > (max dy dz) = XDir
      | dy > dz          = YDir
      | otherwise        = ZDir

    divDim dir (VoxBoxDim dx dy dz)
      | dir == XDir = let
        x = dx `quot` 2
        in (VoxBoxDim x dy dz, VoxBoxDim (dx-x) dy dz)
      | dir == YDir = let
        y = dy `quot` 2
        in (VoxBoxDim dx y dz, VoxBoxDim dx (dy-y) dz)
      | otherwise   = let
        z = dz `quot` 2
        in (VoxBoxDim dx dy z, VoxBoxDim dx dy (dz-z))

    getNewOrigin dir (VoxBoxDim dx dy dz)
      | dir == XDir = org #+# (VoxelPos dx  0  0)
      | dir == YDir = org #+# (VoxelPos  0 dy  0)
      | otherwise   = org #+# (VoxelPos  0  0 dz)

    divDir       = getDir boxdim
    (dim1, dim2) = divDim divDir boxdim
    org2         = getNewOrigin divDir dim1
    in return (VoxBoxRange org dim1, VoxBoxRange org2 dim2)

{-# INLINE mergeVoxBoxRange #-}    
mergeVoxBoxRange :: VoxBoxRange -> VoxBoxRange -> Maybe (VoxBoxRange, CartesianDir, WallBoxRange)
mergeVoxBoxRange b1 b2
  | b1 == b2                 = Nothing
  | ux1 + 1 == lx2 && testYZ = out XDir (box lx1 ux2 ly1 uy1 lz1 uz1) (box ux1 lx2 ly1 uy1 lz1 uz1)  
  | ux2 + 1 == lx1 && testYZ = out XDir (box lx2 ux1 ly1 uy1 lz1 uz1) (box ux2 lx1 ly1 uy1 lz1 uz1) 

  | uy1 + 1 == ly2 && testXZ = out YDir (box lx1 ux1 ly1 uy2 lz1 uz1) (box lx1 ux1 uy1 ly2 lz1 uz1)
  | uy2 + 1 == ly1 && testXZ = out YDir (box lx1 ux1 ly2 uy1 lz1 uz1) (box lx1 ux1 uy2 ly1 lz1 uz1)

  | uz1 + 1 == lz2 && testXY = out ZDir (box lx1 ux1 ly1 uy1 lz1 uz2) (box lx1 ux1 ly1 uy1 uz1 lz2)
  | uz2 + 1 == lz1 && testXY = out ZDir (box lx1 ux1 ly1 uy1 lz2 uz1) (box lx1 ux1 ly1 uy1 uz2 lz1)
  | otherwise                = Nothing
  where
    out dir br wl = return $ (br, dir, WallBoxRange wl)
    (!lx1, !ux1, !ly1, !uy1, !lz1, !uz1) = getBoxLinRange b1
    (!lx2, !ux2, !ly2, !uy2, !lz2, !uz2) = getBoxLinRange b2
    testYZ = ly1 == ly2 && uy1 == uy2 && lz1 == lz2 && uz1 == uz2 
    testXZ = lx1 == lx2 && ux1 == ux2 && lz1 == lz2 && uz1 == uz2 
    testXY = lx1 == lx2 && ux1 == ux2 && ly1 == ly2 && uy1 == uy2 

    box lx ux ly uy lz uz = let 
      dimbox  = VoxBoxDim (1 + ux - lx) (1 + uy - ly) (1 + uz - lz)
      new_org = VoxelPos lx ly lz
      in VoxBoxRange new_org dimbox

posSet2VoxBox :: Vector VoxelPos -> VoxBoxRange
posSet2VoxBox ps = let
  zeroBox
    | V.length ps > 0 = V.head ps
    | otherwise       = VoxelPos 0 0 0
  func (vl, vu) (VoxelPos x y z) = let
    (VoxelPos lx ly lz) = vl
    (VoxelPos ux uy uz) = vu
    vmin = VoxelPos (min lx x) (min ly y) (min lz z)
    vmax = VoxelPos (max ux x) (max uy y) (max uz z)
    in (vmin, vmax)
  (v1, v2) = V.foldl' func (zeroBox, zeroBox) ps
  in posRange2VoxBox v1 v2

{-# INLINE posRange2VoxBox #-}    
posRange2VoxBox :: VoxelPos -> VoxelPos -> VoxBoxRange
posRange2VoxBox (VoxelPos x1 y1 z1) (VoxelPos x2 y2 z2) = let 
  dimbox    = VoxBoxDim (foo x1 x2) (foo y1 y2) (foo z1 z2)
  foo !a !b = 1 + abs (b - a)
  lx      = min x1 x2
  ly      = min y1 y2
  lz      = min z1 z2
  new_org = VoxelPos lx ly lz
  in VoxBoxRange new_org dimbox
     
isInBox :: VoxBoxRange -> VoxelPos -> Bool
isInBox box (VoxelPos x y z) = let
  (lx, ux, ly, uy, lz, uz) = getBoxLinRange box
  in x >= lx && x <= ux && y >= ly && y <= uy && z >= lz && z <= uz 

isOnEdge :: VoxBoxRange -> VoxelPos -> Bool
isOnEdge box (VoxelPos x y z) = let
  (lx, ux, ly, uy, lz, uz) = getBoxLinRange box
  in x == lx || x == ux || y == ly || y == uy || z == lz || z == uz 

isMinRange :: VoxBoxRange -> Bool
isMinRange (VoxBoxRange _ (VoxBoxDim dx dy dz)) =
  dx <= 1 && dy <= 1 && dz <= 1
                                                   
mkStdVoxBoxRange :: VoxBoxDim -> VoxBoxRange 
mkStdVoxBoxRange = VoxBoxRange (VoxelPos 0 0 0)

sizeVoxBoxRange :: VoxBoxRange -> Int
sizeVoxBoxRange = let
  func (x,y,z)
    | x > 0 && y > 0 && z > 0 = x*y*z
    | otherwise = error "[VoxBox.Base] Invalid box dimension. Its size is negative"
  in func . getVoxBoxDim . vbrDim
 
getRangePos :: VoxBoxRange -> [VoxelPos]
getRangePos VoxBoxRange{..} = let
  VoxelPos ix iy iz = vbrOrigin 
  (dx, dy, dz)      = getVoxBoxDim vbrDim
  xmax = ix + dx -1
  ymax = iy + dy -1
  zmax = iz + dz -1
  in [VoxelPos x y z | z <- [ix..zmax], y <- [iy..ymax], x <- [iz..xmax]]
 
-- ================================ Fast and compact list of Voxel ===========================

{-# INLINE fastEvalVoxelPos #-}    
fastEvalVoxelPos :: VoxBox a -> Int -> Vec3
fastEvalVoxelPos VoxBox{..} i = let
  VoxBoxOrigin  ix iy iz = origin
  VoxelDim      dx dy dz = spacing
  VoxelPos      ibx iby ibz = vbrOrigin dimension
  VoxBoxDim     dbx dby _   = vbrDim    dimension

  (!z, !rz) = i  `quotRem` (dbx*dby)
  (!y, !ry) = rz `quotRem` dbx
  !x        = ry

  vx = (ix - dx/2) + dx * (fromIntegral $ ibx + x)
  vy = (iy - dy/2) + dy * (fromIntegral $ iby + y)
  vz = (iz - dz/2) + dz * (fromIntegral $ ibz + z)
  in Vec3 vx vy vz

-- -------------------------------------------- Edges in matrix ----------------------------------------------------

toEdgePos :: EdgeVoxelPos -> VoxelPos 
toEdgePos f = let
  foo (VoxelPos x y z) = VoxelPos (x*2) (y*2) (z*2)
  in case f of
    Ex p -> foo p #-# VoxelPos 0 1 1
    Ey p -> foo p #-# VoxelPos 1 0 1
    Ez p -> foo p #-# VoxelPos 1 1 0

toEdgeVoxelPos :: EdgePos -> EdgeVoxelPos
toEdgeVoxelPos f@(EdgePos (VoxelPos x y z))
  | isEdgeX f = Ex $ p #+# VoxelPos 0 1 1
  | isEdgeY f = Ey $ p #+# VoxelPos 1 0 1
  | isEdgeZ f = Ez $ p #+# VoxelPos 1 1 0
  | otherwise = error "Can't convert to FaceVoxelPos."
  where p = VoxelPos (div x 2) (div y 2) (div z 2)

{-# INLINE isEdgeX #-}
isEdgeX :: EdgePos -> Bool
isEdgeX (EdgePos (VoxelPos x y z)) = even x && odd y && odd z

{-# INLINE isEdgeY #-}
isEdgeY :: EdgePos -> Bool
isEdgeY (EdgePos (VoxelPos x y z)) = odd x && even y && odd z

{-# INLINE isEdgeZ #-}
isEdgeZ :: EdgePos -> Bool
isEdgeZ (EdgePos (VoxelPos x y z)) = odd x && odd y && even z

-- -------------------------------------------- Faces in matrix ----------------------------------------------------

toFacePos :: FaceVoxelPos -> VoxelPos 
toFacePos f = let
  foo (VoxelPos x y z) = VoxelPos (x*2) (y*2) (z*2)
  in case f of
    Fx p -> foo p #-# VoxelPos 1 0 0
    Fy p -> foo p #-# VoxelPos 0 1 0
    Fz p -> foo p #-# VoxelPos 0 0 1

toFaceVoxelPos :: FacePos -> FaceVoxelPos
toFaceVoxelPos f@(FacePos (VoxelPos x y z))
  | isFaceX f = Fx $ p #+# VoxelPos 1 0 0
  | isFaceY f = Fy $ p #+# VoxelPos 0 1 0
  | otherwise = Fz $ p #+# VoxelPos 0 0 1
  where p = VoxelPos (div x 2) (div y 2) (div z 2)

-- | Fast bitwise test for FacePos:
-- a -> even, b -> even, c -> odd
{-# INLINE faceTest #-}
faceTest :: Int -> Int -> Int -> Bool
faceTest a b c = 0 == 1 .&. ((a .|. b) .|. complement c) 

{-# INLINE isFaceX #-}
isFaceX :: FacePos -> Bool
isFaceX (FacePos (VoxelPos x y z)) = faceTest y z x -- odd x && even y && even z

{-# INLINE isFaceY #-}
isFaceY :: FacePos -> Bool
isFaceY (FacePos (VoxelPos x y z)) = faceTest x z y -- even x && odd y && even z

{-# INLINE isFaceZ #-}
isFaceZ :: FacePos -> Bool
isFaceZ (FacePos (VoxelPos x y z)) = faceTest x y z -- even x && even y && odd z
