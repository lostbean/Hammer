{-# LANGUAGE RecordWildCards #-}

module Hammer.VoxBox.Base where

import Data.Vector                     (Vector, (!))  
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

data EdgePos      = Ex {-# UNPACK #-} !VoxelPos
                  | Ey {-# UNPACK #-} !VoxelPos
                  | Ez {-# UNPACK #-} !VoxelPos
                  deriving (Show, Eq)
                        
data FacePos      = Fx {-# UNPACK #-} !VoxelPos
                  | Fy {-# UNPACK #-} !VoxelPos
                  | Fz {-# UNPACK #-} !VoxelPos
                  deriving (Show, Eq)

data VoxBox a     = VoxBox
                    { dimension :: VoxBoxDim
                    , origin    :: VoxBoxOrigin
                    , spacing   :: VoxelDim 
                    , grainID   :: Vector a
                    } deriving (Show)

data CartesianDir = XDir | YDir | ZDir deriving (Eq, Show)

type VID = Int  -- Voxel's serial position

-- ==============================================================================

{-# INLINE unsafeGetVoxelID #-}    
unsafeGetVoxelID :: VoxBoxDim -> VoxelPos -> Int
unsafeGetVoxelID (VoxBoxDim maxX maxY _) (VoxelPos x y z) =
  x + maxX*y + maxX*maxY*z

{-# INLINE checkPosBound #-}    
checkPosBound :: VoxBoxDim -> VoxelPos -> Bool
checkPosBound (VoxBoxDim maxX maxY maxZ) (VoxelPos x y z) =
  x>=0 && y>=0 && z>=0 && x<maxX && y<maxY && z<maxZ

{-# INLINE getVoxelID #-}    
getVoxelID :: VoxBoxDim -> VoxelPos -> Maybe Int
getVoxelID bdim pos
  | checkPosBound bdim pos = return $ unsafeGetVoxelID bdim pos
  | otherwise              = Nothing

{-# INLINE getVoxelPos #-}    
getVoxelPos :: VoxBoxDim -> Int -> Maybe VoxelPos
getVoxelPos (VoxBoxDim dx dy dz) i
  | dx <= 0 && dy <= 0 && dz <= 0 = Nothing
  | qx < dx && qy < dy && qz < dz = return $ VoxelPos qx qy qz
  | otherwise                     = Nothing
  where
    (qz, rz) = i  `divMod` (dx*dy)
    (qy, ry) = rz `divMod` dx
    qx       = ry

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

evalVoxelPos :: VoxBox a -> VoxelPos -> Vec3
evalVoxelPos vbox (VoxelPos x y z) = let
  VoxBoxOrigin  ix iy iz = origin  vbox
  VoxelDim      dx dy dz = spacing vbox
  vx = (ix - dx/2) + dx * (fromIntegral x)
  vy = (iy - dy/2) + dy * (fromIntegral y)
  vz = (iz - dz/2) + dz * (fromIntegral z)
  in Vec3 vx vy vz

evalFacePos :: VoxBox a -> FacePos -> Plane Vec3 Normal3
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

getEdgeEndPos :: EdgePos -> (VoxelPos, VoxelPos)
getEdgeEndPos edge = case edge of
  Ex p -> (p, p #+# (VoxelPos 1 0 0))
  Ey p -> (p, p #+# (VoxelPos 0 1 0))
  Ez p -> (p, p #+# (VoxelPos 0 0 1))

getBoxLinRange :: VoxBoxRange -> (Int, Int, Int, Int, Int, Int)
getBoxLinRange (VoxBoxRange (VoxelPos x1 y1 z1) (VoxBoxDim dx dy dz)) = let
  x2 = func dx x1
  y2 = func dy y1
  z2 = func dz z1
  func dk k
    | dk > 0    = k + (dk - 1)
    | otherwise = k 
  ux = max x1 x2
  lx = min x1 x2
  uy = max y1 y2
  ly = min y1 y2
  uz = max z1 z2
  lz = min z1 z2
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

     
splitBox :: VoxBoxRange -> ( Maybe VoxBoxRange, Maybe VoxBoxRange, Maybe VoxBoxRange
                           , Maybe VoxBoxRange, Maybe VoxBoxRange, Maybe VoxBoxRange
                           , Maybe VoxBoxRange, Maybe VoxBoxRange )
splitBox box@(VoxBoxRange org dim) = let
  fakePos (VoxBoxDim dx dy dz) = VoxelPos dx dy dz
  
  (q, r) = (fakePos dim) #/ 2
  bc_org = (org #+# q #+# r) #-# VoxelPos 1 1 1
  bc_dim = VoxBoxDim 2 2 2

  (cbll, cbul, cbur, cblr, ctur, ctlr, ctll, ctul) = getBox (VoxBoxRange bc_org bc_dim)
  (bbll, bbul, bbur, bblr, btur, btlr, btll, btul) = getBox box
  
  getStdBox bx cx
    | isInBox box cx = return $ posRange2VoxBox bx cx
    | otherwise      = Nothing
                  
  bll = getStdBox bbll cbll
  bul = getStdBox bbul cbul
  bur = getStdBox bbur cbur
  blr = getStdBox bblr cblr
  
  tur = getStdBox btur ctur
  tlr = getStdBox btlr ctlr
  tll = getStdBox btll ctll
  tul = getStdBox btul ctul

  in (bll, bul, bur, blr, tur, tlr, tll, tul)


posRange2VoxBox :: VoxelPos -> VoxelPos -> VoxBoxRange
posRange2VoxBox (VoxelPos x1 y1 z1) (VoxelPos x2 y2 z2) = let 
  dim     = VoxBoxDim (foo x1 x2) (foo y1 y2) (foo z1 z2)
  foo a b = 1 + abs (b-a)
  lx      = min x1 x2
  ly      = min y1 y2
  lz      = min z1 z2
  new_org = VoxelPos lx ly lz
  in VoxBoxRange new_org dim

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
