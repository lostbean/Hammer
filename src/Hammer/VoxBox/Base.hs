{-# LANGUAGE RecordWildCards #-}

module Hammer.VoxBox.Base where

import qualified Data.Vector           as V

import Control.DeepSeq
import Data.Vector                     (Vector, (!))
import Data.Hashable                   (Hashable, hashWithSalt)
import Hammer.Math.Vector              hiding (Vector)

-- ================================================================================

data VoxBoxOrigin = VoxBoxOrigin
                    {-# UNPACK #-} !Double
                    {-# UNPACK #-} !Double
                    {-# UNPACK #-} !Double
                  deriving (Show, Eq, Ord)
                            
data VoxBoxRange  = VoxBoxRange
                    { vbrOrigin :: {-# UNPACK #-} !VoxelPos
                    , vbrDim    :: {-# UNPACK #-} !VoxBoxDim
                    } deriving (Eq, Show)
                           
-- | Dimension of voxel box where x, y and z >= 1
data VoxBoxDim    = VoxBoxDim
                    {-# UNPACK #-} !Int
                    {-# UNPACK #-} !Int
                    {-# UNPACK #-} !Int
                  deriving (Show, Eq, Ord)

data VoxelDim     = VoxelDim
                    {-# UNPACK #-} !Double
                    {-# UNPACK #-} !Double
                    {-# UNPACK #-} !Double
                  deriving (Show, Eq, Ord)
                                                            
data VoxelPos     = VoxelPos
                    {-# UNPACK #-} !Int
                    {-# UNPACK #-} !Int
                    {-# UNPACK #-} !Int
                  deriving (Show, Eq, Ord)

data EdgeVoxelPos = Ex {-# UNPACK #-} !VoxelPos
                  | Ey {-# UNPACK #-} !VoxelPos
                  | Ez {-# UNPACK #-} !VoxelPos
                  deriving (Show, Eq)
                        
data FaceVoxelPos = Fx {-# UNPACK #-} !VoxelPos
                  | Fy {-# UNPACK #-} !VoxelPos
                  | Fz {-# UNPACK #-} !VoxelPos
                  deriving (Show, Eq)

data VoxBox a     = VoxBox
                    { dimension :: VoxBoxRange
                    , origin    :: VoxBoxOrigin
                    , spacing   :: VoxelDim 
                    , grainID   :: Vector a
                    } deriving (Show)

data CartesianDir = XDir | YDir | ZDir deriving (Eq, Show)

data CrossDir = XYplus | XYminus | XZplus | XZminus
              | YXplus | YXminus | YZplus | YZminus
              | ZXplus | ZXminus | ZYplus | ZYminus
              deriving (Show, Eq)

type VID = Int  -- Voxel's serial position


-- ---------------------------- Hashable instances ------------------------------
           
instance Hashable VoxelPos where
  hashWithSalt i (VoxelPos a b c) = hashWithSalt i (a,b,c)

-- ---------------------------- DeepSeq instances -------------------------------
           
instance NFData VoxelPos where
  rnf (VoxelPos x y z) = rnf x `seq` rnf y `seq` rnf z

instance NFData VoxBoxDim where
  rnf (VoxBoxDim dx dy dz) = rnf dx `seq` rnf dy `seq` rnf dz
  
instance NFData VoxBoxRange where
  rnf (VoxBoxRange org dimbox) = rnf org `seq` rnf dimbox

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
    (qz, rz) = i  `divMod` (dx*dy)
    (qy, ry) = rz `divMod` dx
    qx       = ry
                           
{-# INLINE getVoxelPos #-}    
getVoxelPos :: VoxBoxRange -> Int -> Maybe VoxelPos
getVoxelPos VoxBoxRange{..} i 
  | dx <= 0 && dy <= 0 && dz <= 0 = Nothing
  | qx < dx && qy < dy && qz < dz = return $ vbrOrigin #+# VoxelPos qx qy qz
  | otherwise                     = Nothing
  where
    (VoxBoxDim dx dy dz) = vbrDim
    (qz, rz) = i  `divMod` (dx*dy)
    (qy, ry) = rz `divMod` dx
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
  (q1,r1) = x `divMod` k
  (q2,r2) = y `divMod` k
  (q3,r3) = z `divMod` k
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

getCrossPos :: CartesianDir -> VoxelPos -> Vector (VoxelPos, CrossDir)
getCrossPos dir p = V.map (\x -> (p #+# getDeltaPos x, x)) $ case dir of
  XDir -> V.fromList [XYplus, XYminus, XZplus, XZminus]
  YDir -> V.fromList [YXplus, YXminus, YZplus, YZminus]
  ZDir -> V.fromList [ZXplus, ZXminus, ZYplus, ZYminus]
  
getXYplus, getXYminus, getXZplus, getXZminus :: VoxelPos
getYXplus, getYXminus, getYZplus, getYZminus :: VoxelPos
getZXplus, getZXminus, getZYplus, getZYminus :: VoxelPos
getXYplus  = VoxelPos 1    1   0
getXYminus = VoxelPos 1  (-1)  0
getXZplus  = VoxelPos 1    0   1
getXZminus = VoxelPos 1    0 (-1)
getYXplus  = VoxelPos 1    1   0
getYXminus = VoxelPos (-1) 1   0
getYZplus  = VoxelPos 0    1   1
getYZminus = VoxelPos 0    1 (-1)
getZXplus  = VoxelPos 1    0   1
getZXminus = VoxelPos (-1) 0   1
getZYplus  = VoxelPos 0    1   1
getZYminus = VoxelPos 0  (-1)  1

getDeltaPos :: CrossDir -> VoxelPos
getDeltaPos cd = case cd of
  XYplus  -> getXYplus
  XYminus -> getXYminus
  XZplus  -> getXZplus
  XZminus -> getXZminus
  YXplus  -> getYXplus
  YXminus -> getYXminus 
  YZplus  -> getYZplus
  YZminus -> getYZminus
  ZXplus  -> getZXplus
  ZXminus -> getZXminus
  ZYplus  -> getZYplus
  ZYminus -> getZYminus

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

newtype Plane v u = Plane (v, u) deriving (Show, Eq)
newtype Line  v u = Line (v, u)  deriving (Show, Eq)

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

getVoxBoxDim :: VoxBoxDim -> (Int, Int, Int)
getVoxBoxDim (VoxBoxDim x y z) = (x,y,z)

getBox :: VoxBoxRange -> ( VoxelPos, VoxelPos, VoxelPos, VoxelPos
                         , VoxelPos, VoxelPos, VoxelPos, VoxelPos )
getBox br = let
  (lx, ux, ly, uy, lz, uz) = getBoxLinRange br
  
  bll = VoxelPos lx ly lz
  bul = VoxelPos lx uy lz
  bur = VoxelPos ux uy lz
  blr = VoxelPos ux ly lz

  tur = VoxelPos ux uy uz
  tlr = VoxelPos ux ly uz
  tll = VoxelPos lx ly uz
  tul = VoxelPos lx uy uz

  in (bll, bul, bur, blr, tur, tlr, tll, tul) 

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
        x = dx `div` 2
        in (VoxBoxDim x dy dz, VoxBoxDim (dx-x) dy dz)
      | dir == YDir = let
        y = dy `div` 2
        in (VoxBoxDim dx y dz, VoxBoxDim dx (dy-y) dz)
      | otherwise   = let
        z = dz `div` 2
        in (VoxBoxDim dx dy z, VoxBoxDim dx dy (dz-z))

    getNewOrigin dir (VoxBoxDim dx dy dz)
      | dir == XDir = org #+# (VoxelPos dx  0  0)
      | dir == YDir = org #+# (VoxelPos  0 dy  0)
      | otherwise   = org #+# (VoxelPos  0  0 dz)

    divDir       = getDir boxdim
    (dim1, dim2) = divDim divDir boxdim
    org2         = getNewOrigin divDir dim1
    in return (VoxBoxRange org dim1, VoxBoxRange org2 dim2)

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

posRange2VoxBox :: VoxelPos -> VoxelPos -> VoxBoxRange
posRange2VoxBox (VoxelPos x1 y1 z1) (VoxelPos x2 y2 z2) = let 
  dimbox  = VoxBoxDim (foo x1 x2) (foo y1 y2) (foo z1 z2)
  foo a b = 1 + abs (b-a)
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
                                                   
maskVoxelPos :: CartesianDir -> VoxelPos -> VoxelPos 
maskVoxelPos dir (VoxelPos x y z) = case dir of
  XDir -> VoxelPos x 0 0
  YDir -> VoxelPos 0 y 0
  ZDir -> VoxelPos 0 0 z

mkStdVoxBoxRange :: VoxBoxDim -> VoxBoxRange 
mkStdVoxBoxRange = VoxBoxRange (VoxelPos 0 0 0)

sizeVoxBoxRange :: VoxBoxRange -> Int
sizeVoxBoxRange = let
  func (x,y,z)
    | x > 0 && y > 0 && z > 0 = x*y*z
    | otherwise = error "[VoxBox.Base] Invalid box dimension. Its size is negative"
  in func . getVoxBoxDim . vbrDim

getRangePos :: VoxBoxRange -> [VoxelPos]
getRangePos (VoxBoxRange (VoxelPos x y z) (VoxBoxDim dx dy dz)) = 
  [ VoxelPos i j k | i <- [x .. x + dx]
                   , j <- [y .. y + dy]
                   , k <- [z .. z + dz]]
 
