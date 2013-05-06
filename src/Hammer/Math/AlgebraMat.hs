{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hammer.Math.AlgebraMat where

import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Generic.Base    as G
import qualified Data.Vector.Generic.Mutable as M

import Data.Vector                     (Vector)
import Control.Monad                   (liftM)

import Foreign
import System.Random  
  
import Hammer.Math.AlgebraBase
import Hammer.Math.AlgebraVec()

--------------------------------------------------------------------------------                    
-- Mat2 instances

instance HasCoordinates Mat2 Vec2 where
  _1 (Mat2 x _) = x
  _2 (Mat2 _ y) = y
  _3 _ = error "has only 2 coordinates"
  _4 _ = error "has only 2 coordinates"

instance Matrix Mat2 where
  transpose (Mat2 row1 row2) = 
    Mat2 (Vec2 (_1 row1) (_1 row2)) 
         (Vec2 (_2 row1) (_2 row2)) 
  idmtx = Mat2 (Vec2 1 0) (Vec2 0 1)
  inverse (Mat2 (Vec2 a b) (Vec2 c d)) = 
    Mat2 (Vec2 (d*r) (-b*r)) (Vec2 (-c*r) (a*r)) 
    where r = 1.0 / (a*d - b*c)

instance AbelianGroup Mat2 where
  (&+) (Mat2 r1 r2) (Mat2 s1 s2) = Mat2 (r1 &+ s1) (r2 &+ s2)
  (&-) (Mat2 r1 r2) (Mat2 s1 s2) = Mat2 (r1 &- s1) (r2 &- s2)
  neg  (Mat2 r1 r2)              = Mat2 (neg r1) (neg r2)  
  zero = Mat2 zero zero  
  
instance MultiVec Mat2 where
  scalarMul s (Mat2 r1 r2) = Mat2 (g r1) (g r2) where g = scalarMul s
  mapVec    f (Mat2 r1 r2) = Mat2 (g r1) (g r2) where g = mapVec f

instance MultSemiGroup Mat2 where
  (.*.) (Mat2 r1 r2) n = 
    let (Mat2 c1 c2) = transpose n
    in Mat2 (Vec2 (r1 &. c1) (r1 &. c2))
            (Vec2 (r2 &. c1) (r2 &. c2))
  one = idmtx 

instance Ring Mat2

instance LeftModule Mat2 Vec2 where
  lmul (Mat2 row1 row2) v = Vec2 (row1 &. v) (row2 &. v) 
  
instance RightModule Vec2 Mat2 where
  rmul v mt = lmul (transpose mt) v

instance Diagonal Vec2 Mat2 where
  diag (Vec2 x y) = Mat2 (Vec2 x 0) (Vec2 0 y)

instance Tensor Mat2 Vec2 where
  outer (Vec2 a b) (Vec2 x y) = Mat2
    (Vec2 (a*x) (a*y))
    (Vec2 (b*x) (b*y))

instance Determinant Mat2 where
  det (Mat2 (Vec2 a b) (Vec2 c d)) = a*d - b*c 

{-
instance Show Mat2 where
  show (Mat2 r1 r2) = show r1 ++ "\n" ++ show r2
-}

instance Storable Mat2 where
  sizeOf    _ = 2 * sizeOf (undefined::Vec2)
  alignment _ = alignment  (undefined::Vec2)
  
  peek q = do
    let p = castPtr q :: Ptr Vec2
        k = sizeOf (undefined::Vec2)
    r1 <- peek        p 
    r2 <- peekByteOff p k
    return (Mat2 r1 r2)
    
  poke q (Mat2 r1 r2) = do
    let p = castPtr q :: Ptr Vec2
        k = sizeOf (undefined::Vec2)
    poke        p   r1
    pokeByteOff p k r2

instance Random Mat2 where
  random = randomR (Mat2 v1 v1 , Mat2 v2 v2) where 
    v1 = Vec2 (-1) (-1) 
    v2 = Vec2   1    1
  randomR (Mat2 a b, Mat2 c d) gen = 
    let (x,gen1) = randomR (a,c) gen
        (y,gen2) = randomR (b,d) gen1
    in (Mat2 x y, gen2)
          
instance Dimension Mat2 where dim _ = 2
     
instance MatrixNorms Mat2 where 
  frobeniusNorm (Mat2 r1 r2) =  
    sqrt $
      normsqr r1 + 
      normsqr r2
     
instance Pointwise Mat2 where
  pointwise (Mat2 x1 y1) (Mat2 x2 y2) = Mat2 (x1 &! x2) (y1 &! y2)
       
--------------------------------------------------------------------------------   
-- Mat3 instances

instance HasCoordinates Mat3 Vec3 where
  _1 (Mat3 x _ _) = x
  _2 (Mat3 _ y _) = y
  _3 (Mat3 _ _ z) = z
  _4 _ = error "has only 3 coordinates"  

instance Matrix Mat3 where

  transpose (Mat3 row1 row2 row3) = 
    Mat3 (Vec3 (_1 row1) (_1 row2) (_1 row3)) 
         (Vec3 (_2 row1) (_2 row2) (_2 row3)) 
         (Vec3 (_3 row1) (_3 row2) (_3 row3)) 
         
  idmtx = Mat3 (Vec3 1 0 0) (Vec3 0 1 0) (Vec3 0 0 1)
  
  inverse (Mat3 (Vec3 a b c) (Vec3 e f g) (Vec3 i j k)) = 
    Mat3 (Vec3 (d11*r) (d21*r) (d31*r))  
         (Vec3 (d12*r) (d22*r) (d32*r))  
         (Vec3 (d13*r) (d23*r) (d33*r))  
    where
      r = 1.0 / ( a*d11 + b*d12 + c*d13 )

      d11 = f*k - g*j
      d12 = g*i - e*k
      d13 = e*j - f*i

      d31 = b*g - c*f
      d32 = c*e - a*g
      d33 = a*f - b*e

      d21 = c*j - b*k 
      d22 = a*k - c*i 
      d23 = b*i - a*j 

instance AbelianGroup Mat3 where
  (&+) (Mat3 r1 r2 r3) (Mat3 s1 s2 s3) = Mat3 (r1 &+ s1) (r2 &+ s2) (r3 &+ s3)
  (&-) (Mat3 r1 r2 r3) (Mat3 s1 s2 s3) = Mat3 (r1 &- s1) (r2 &- s2) (r3 &- s3)
  neg  (Mat3 r1 r2 r3)                 = Mat3 (neg r1) (neg r2) (neg r3) 
  zero = Mat3 zero zero zero 

instance MultiVec Mat3 where
  scalarMul s (Mat3 r1 r2 r3) = Mat3 (g r1) (g r2) (g r3) where g = scalarMul s
  mapVec    f (Mat3 r1 r2 r3) = Mat3 (g r1) (g r2) (g r3) where g = mapVec f

instance MultSemiGroup Mat3 where
  (.*.) (Mat3 r1 r2 r3) n = 
    let (Mat3 c1 c2 c3) = transpose n
    in Mat3 (Vec3 (r1 &. c1) (r1 &. c2) (r1 &. c3))
            (Vec3 (r2 &. c1) (r2 &. c2) (r2 &. c3))
            (Vec3 (r3 &. c1) (r3 &. c2) (r3 &. c3))
  one = idmtx 

instance Ring Mat3

instance LeftModule Mat3 Vec3 where
  lmul (Mat3 row1 row2 row3) v = Vec3 (row1 &. v) (row2 &. v) (row3 &. v)
  
instance RightModule Vec3 Mat3 where
  rmul v mt = lmul (transpose mt) v

instance Diagonal Vec3 Mat3 where
  diag (Vec3 x y z) = Mat3 (Vec3 x 0 0) (Vec3 0 y 0) (Vec3 0 0 z)

instance Tensor Mat3 Vec3 where
  outer (Vec3 a b c) (Vec3 x y z) = Mat3
    (Vec3 (a*x) (a*y) (a*z))
    (Vec3 (b*x) (b*y) (b*z))
    (Vec3 (c*x) (c*y) (c*z))

instance Determinant Mat3 where
  det (Mat3 r1 r2 r3) = det (r1,r2,r3)

{-
instance Show Mat3 where
  show (Mat3 r1 r2 r3) = show r1 ++ "\n" ++ show r2 ++ "\n" ++ show r3
-}

instance Storable Mat3 where
  sizeOf    _ = 3 * sizeOf (undefined::Vec3)
  alignment _ = alignment  (undefined::Vec3)
  
  peek q = do
    let p = castPtr q :: Ptr Vec3
        k = sizeOf (undefined::Vec3)
    r1 <- peek        p 
    r2 <- peekByteOff p (k  )
    r3 <- peekByteOff p (k+k)
    return (Mat3 r1 r2 r3)
    
  poke q (Mat3 r1 r2 r3) = do
    let p = castPtr q :: Ptr Vec3
        k = sizeOf (undefined::Vec3)
    poke        p       r1
    pokeByteOff p (k  ) r2
    pokeByteOff p (k+k) r3

instance Random Mat3 where
  random = randomR (Mat3 v1 v1 v1 , Mat3 v2 v2 v2) where
    v1 = Vec3 (-1) (-1) (-1)
    v2 = Vec3   1    1    1
  randomR (Mat3 a b c, Mat3 d e f) gen = 
    let (x,gen1) = randomR (a,d) gen
        (y,gen2) = randomR (b,e) gen1
        (z,gen3) = randomR (c,f) gen2  
    in (Mat3 x y z, gen3)
   
instance Dimension Mat3 where dim _ = 3
  
instance MatrixNorms Mat3 where 
  frobeniusNorm (Mat3 r1 r2 r3)  = 
    sqrt $
      normsqr r1 + 
      normsqr r2 + 
      normsqr r3 

instance Pointwise Mat3 where
  pointwise (Mat3 x1 y1 z1) (Mat3 x2 y2 z2) = Mat3 (x1 &! x2) (y1 &! y2) (z1 &! z2)
    
--------------------------------------------------------------------------------
-- Mat4 instances

instance HasCoordinates Mat4 Vec4 where
  _1 (Mat4 x _ _ _) = x
  _2 (Mat4 _ y _ _) = y
  _3 (Mat4 _ _ z _) = z
  _4 (Mat4 _ _ _ w) = w

instance Matrix Mat4 where
  transpose (Mat4 row1 row2 row3 row4) = 
    Mat4 (Vec4 (_1 row1) (_1 row2) (_1 row3) (_1 row4)) 
         (Vec4 (_2 row1) (_2 row2) (_2 row3) (_2 row4)) 
         (Vec4 (_3 row1) (_3 row2) (_3 row3) (_3 row4)) 
         (Vec4 (_4 row1) (_4 row2) (_4 row3) (_4 row4)) 
  idmtx = Mat4 (Vec4 1 0 0 0) (Vec4 0 1 0 0) (Vec4 0 0 1 0) (Vec4 0 0 0 1)
  inverse = error "inverse/Mat4: not implemented yet"

instance AbelianGroup Mat4 where
  (&+) (Mat4 r1 r2 r3 r4) (Mat4 s1 s2 s3 s4) = Mat4 (r1 &+ s1) (r2 &+ s2) (r3 &+ s3) (r4 &+ s4)
  (&-) (Mat4 r1 r2 r3 r4) (Mat4 s1 s2 s3 s4) = Mat4 (r1 &- s1) (r2 &- s2) (r3 &- s3) (r4 &- s4)
  neg  (Mat4 r1 r2 r3 r4)                    = Mat4 (neg r1) (neg r2) (neg r3) (neg r4) 
  zero = Mat4 zero zero zero zero
  
instance MultiVec Mat4 where
  scalarMul s (Mat4 r1 r2 r3 r4) = Mat4 (g r1) (g r2) (g r3) (g r4) where g = scalarMul s
  mapVec    f (Mat4 r1 r2 r3 r4) = Mat4 (g r1) (g r2) (g r3) (g r4) where g = mapVec f

instance MultSemiGroup Mat4 where
  (.*.) (Mat4 r1 r2 r3 r4) n = 
    let (Mat4 c1 c2 c3 c4) = transpose n
    in Mat4 (Vec4 (r1 &. c1) (r1 &. c2) (r1 &. c3) (r1 &. c4))
            (Vec4 (r2 &. c1) (r2 &. c2) (r2 &. c3) (r2 &. c4))
            (Vec4 (r3 &. c1) (r3 &. c2) (r3 &. c3) (r3 &. c4))
            (Vec4 (r4 &. c1) (r4 &. c2) (r4 &. c3) (r4 &. c4))
  one = idmtx 

instance Ring Mat4

instance LeftModule Mat4 Vec4 where
  lmul (Mat4 row1 row2 row3 row4) v = Vec4 (row1 &. v) (row2 &. v) (row3 &. v) (row4 &. v)
  
instance RightModule Vec4 Mat4 where
  rmul v mt = lmul (transpose mt) v

instance Diagonal Vec4 Mat4 where
  diag (Vec4 x y z w) = Mat4 (Vec4 x 0 0 0) (Vec4 0 y 0 0) (Vec4 0 0 z 0) (Vec4 0 0 0 w)

instance Tensor Mat4 Vec4 where
  outer (Vec4 a b c d) (Vec4 x y z w) = Mat4
    (Vec4 (a*x) (a*y) (a*z) (a*w))
    (Vec4 (b*x) (b*y) (b*z) (b*w))
    (Vec4 (c*x) (c*y) (c*z) (c*w))
    (Vec4 (d*x) (d*y) (d*z) (d*w))

instance Determinant Mat4 where
  det = error "det/Mat4: not implemented yet" 
  -- det (Mat4 r1 r2 r3 r4) = 

{-
instance Show Mat4 where
  show (Mat4 r1 r2 r3 r4) = show r1 ++ "\n" ++ show r2 ++ "\n" ++ show r3 ++ "\n" ++ show r4
-}

instance Storable Mat4 where
  sizeOf    _ = 4 * sizeOf (undefined::Vec4)
  alignment _ = alignment  (undefined::Vec4)
  
  peek q = do
    let p = castPtr q :: Ptr Vec4
        k = sizeOf (undefined::Vec4)
    r1 <- peek        p 
    r2 <- peekByteOff p (k  )
    r3 <- peekByteOff p (k+k)
    r4 <- peekByteOff p (3*k)
    return (Mat4 r1 r2 r3 r4)
    
  poke q (Mat4 r1 r2 r3 r4) = do
    let p = castPtr q :: Ptr Vec4
        k = sizeOf (undefined::Vec4)
    poke        p       r1
    pokeByteOff p (k  ) r2
    pokeByteOff p (k+k) r3
    pokeByteOff p (3*k) r4

instance Random Mat4 where
  random = randomR (Mat4 v1 v1 v1 v1, Mat4 v2 v2 v2 v2) where
    v1 = Vec4 (-1) (-1) (-1) (-1)
    v2 = Vec4   1    1    1    1
  randomR (Mat4 a b c d, Mat4 e f g h) gen = 
    let (x,gen1) = randomR (a,e) gen
        (y,gen2) = randomR (b,f) gen1
        (z,gen3) = randomR (c,g) gen2  
        (w,gen4) = randomR (d,h) gen3  
    in (Mat4 x y z w, gen4)
    
instance Dimension Mat4 where dim _ = 4
   
instance MatrixNorms Mat4 where 
  frobeniusNorm (Mat4 r1 r2 r3 r4) = 
    sqrt $
      normsqr r1 + 
      normsqr r2 + 
      normsqr r3 + 
      normsqr r4  
    
instance Pointwise Mat4 where
  pointwise (Mat4 x1 y1 z1 w1) (Mat4 x2 y2 z2 w2) = Mat4 (x1 &! x2) (y1 &! y2) (z1 &! z2) (w1 &! w2)
    
--------------------------------------------------------------------------------
-- Orthogonal matrices

instance Orthogonal Mat2 Ortho2 where
  fromOrtho (Ortho2 o) = o
  toOrthoUnsafe = Ortho2

instance Orthogonal Mat3 Ortho3 where
  fromOrtho (Ortho3 o) = o
  toOrthoUnsafe = Ortho3 

instance Orthogonal Mat4 Ortho4 where
  fromOrtho (Ortho4 o) = o
  toOrthoUnsafe = Ortho4

------

instance Matrix Ortho2 where
  transpose (Ortho2 o) = Ortho2 (transpose o)
  idmtx = Ortho2 idmtx
  inverse = transpose

instance Matrix Ortho3 where
  transpose (Ortho3 o) = Ortho3 (transpose o)
  idmtx = Ortho3 idmtx
  inverse = transpose

instance Matrix Ortho4 where
  transpose (Ortho4 o) = Ortho4 (transpose o)
  idmtx = Ortho4 idmtx
  inverse = transpose

------

instance Random Ortho2 where
  random g = let (o,h) = _rndOrtho2 g in (toOrthoUnsafe (_flip1stRow2 o), h)
  randomR _ = random

instance Random Ortho3 where
  random g = let (o,h) = _rndOrtho3 g in (toOrthoUnsafe (             o), h)
  randomR _ = random

instance Random Ortho4 where
  random g = let (o,h) = _rndOrtho4 g in (toOrthoUnsafe (_flip1stRow4 o), h)
  randomR _ = random

------

-- determinant will be -1
_rndOrtho2 :: RandomGen g => g -> (Mat2, g)
_rndOrtho2 g = (h2, g1) where
  h2 = householder u2 :: Mat2 
  (u2,g1) = random g   

-- generates a uniformly random orthogonal 3x3 matrix 
-- /with determinant +1/, with respect to the Haar measure of SO3.
--
-- see Theorem 4 in:
-- Francesco Mezzadri: How to Generate Random Matrices from the Classical Compact Groups 
-- Notices of the AMS, May 2007 issue
-- <http://www.ams.org/notices/200705/fea-mezzadri-web.ps>
_rndOrtho3 :: RandomGen g => g -> (Mat3, g) 
_rndOrtho3 g = ( (h3 .*. m3), g2) where
  m3 = (extendTailWith :: Double -> Mat2 -> Mat3) 1 o2 
  h3 = householder u3 :: Mat3
  (u3,g1) = random g
  (o2,g2) = _rndOrtho2 g1

-- determinant will be -1
_rndOrtho4 :: RandomGen g => g -> (Mat4, g) 
_rndOrtho4 g = ( (h4 .*. m4), g2) where
  m4 = (extendTailWith :: Double -> Mat3 -> Mat4) 1 o3 
  h4 = householder u4 :: Mat4
  (u4,g1) = random g
  (o3,g2) = _rndOrtho3 g1

------

_flip1stRow2 :: Mat2 -> Mat2
_flip1stRow2 (Mat2 a b) = Mat2 (neg a) b

_flip1stRow3 :: Mat3 -> Mat3
_flip1stRow3 (Mat3 a b c) = Mat3 (neg a) b c

_flip1stRow4 :: Mat4 -> Mat4
_flip1stRow4 (Mat4 a b c d) = Mat4 (neg a) b c d

--------------------------------------------------------------------------------
-- projective matrices
  
instance Projective Vec2 Mat2 Ortho2 Mat3 Proj3 where
  fromProjective (Proj3 m) = m
  toProjectiveUnsafe = Proj3
  orthogonal = Proj3 . extendTailWith 1 . fromOrtho
  linear     = Proj3 . extendTailWith 1
  translation v = Proj3 $ Mat3 (Vec3 1 0 0) (Vec3 0 1 0) (extendTailWith 1 v)
  scaling     v = Proj3 $ diag (extendTailWith 1 v)
  
instance Projective Vec3 Mat3 Ortho3 Mat4 Proj4 where
  fromProjective (Proj4 m) = m
  toProjectiveUnsafe = Proj4
  orthogonal = Proj4 . extendTailWith 1 . fromOrtho 
  linear     = Proj4 . extendTailWith 1
  translation v = Proj4 $ Mat4 (Vec4 1 0 0 0) (Vec4 0 1 0 0) (Vec4 0 0 1 0) (extendTailWith 1 v)
  scaling     v = Proj4 $ diag (extendTailWith 1 v)

instance Matrix Proj3 where
  idmtx = Proj3 idmtx
  transpose (Proj3 m) = Proj3 (transpose m)
  inverse = _invertProj3

instance Matrix Proj4 where
  idmtx = Proj4 idmtx
  transpose (Proj4 m) = Proj4 (transpose m)
  inverse = _invertProj4

_invertProj3 :: Proj3 -> Proj3
_invertProj3 (Proj3 mat@(Mat3 _ _ t)) = 
  Proj3 $ Mat3 (extendTailZero a) (extendTailZero b) (extendTailWith 1 t') 
  where
    t' = neg $ (trimTail t :: Vec2) .* invm2 
    invm2@(Mat2 a b) = inverse $ (trimTail mat :: Mat2)

-- Inverts a projective 4x4 matrix. But you can simply use "inverse" instead.
-- We assume that the bottom-right corner is 1.
_invertProj4 :: Proj4 -> Proj4
_invertProj4 (Proj4 mat@(Mat4 _ _ _ t)) = 
  Proj4 $ Mat4 (extendTailZero a) (extendTailZero b) (extendTailZero c) (extendTailWith 1 t') 
  where
    t' = neg $ (trimTail t :: Vec3) .* invm3 
    invm3@(Mat3 a b c) = inverse $ (trimTail mat :: Mat3)

-- | Orthogonal matrices.
--
-- Note: the "Random" instances generates orthogonal matrices with determinant 1
-- (that is, orientation-preserving orthogonal transformations)!
newtype Ortho2 = Ortho2 Mat2 deriving (Read,Show,Storable,MultSemiGroup,Determinant,Dimension)
newtype Ortho3 = Ortho3 Mat3 deriving (Read,Show,Storable,MultSemiGroup,Determinant,Dimension)
newtype Ortho4 = Ortho4 Mat4 deriving (Read,Show,Storable,MultSemiGroup,Determinant,Dimension)

-- | Projective matrices, encoding affine transformations in dimension one less.
newtype Proj3 = Proj3 Mat3 deriving (Read,Show,Storable,MultSemiGroup)
newtype Proj4 = Proj4 Mat4 deriving (Read,Show,Storable,MultSemiGroup)

--------------------------------------------------------------------------------
-- Extend instances
                                    
instance Extend Mat2 Mat3 where
  extendHeadZero   (Mat2 p q) = Mat3 zero (extendHeadZero p) (extendHeadZero q)
  extendHeadWith w (Mat2 p q) = Mat3 (Vec3 w 0 0) (extendHeadZero p) (extendHeadZero q)
  trimHead       (Mat3 _ p q) = Mat2 (trimHead p) (trimHead q)
  extendTailZero   (Mat2 p q) = Mat3 (extendTailZero p) (extendTailZero q) zero
  extendTailWith w (Mat2 p q) = Mat3 (extendTailZero p) (extendTailZero q) (Vec3 0 0 w)
  trimTail       (Mat3 p q _) = Mat2 (trimTail p) (trimTail q)

instance Extend Mat2 Mat4 where
  extendHeadZero   (Mat2 p q) = Mat4 zero zero (extendHeadZero p) (extendHeadZero q)
  extendHeadWith w (Mat2 p q) = Mat4 (Vec4 w 0 0 0) (Vec4 0 w 0 0) (extendHeadZero p) (extendHeadZero q)
  trimHead     (Mat4 _ _ p q) = Mat2 (trimHead p) (trimHead q)
  extendTailZero   (Mat2 p q) = Mat4 (extendTailZero p) (extendTailZero q) zero zero
  extendTailWith w (Mat2 p q) = Mat4 (extendTailZero p) (extendTailZero q) (Vec4 0 0 w 0) (Vec4 0 0 0 w)
  trimTail     (Mat4 p q _ _) = Mat2 (trimTail p) (trimTail q)

instance Extend Mat3 Mat4 where
  extendHeadZero   (Mat3 p q r) = Mat4 zero (extendHeadZero p) (extendHeadZero q) (extendHeadZero r)
  extendHeadWith w (Mat3 p q r) = Mat4 (Vec4 w 0 0 0) (extendHeadZero p) (extendHeadZero q) (extendHeadZero r)
  trimHead       (Mat4 _ p q r) = Mat3 (trimHead p) (trimHead q) (trimHead r)
  extendTailZero   (Mat3 p q r) = Mat4 (extendTailZero p) (extendTailZero q) (extendTailZero r) zero
  extendTailWith w (Mat3 p q r) = Mat4 (extendTailZero p) (extendTailZero q) (extendTailZero r) (Vec4 0 0 0 w)
  trimTail (Mat4 p q r _) = Mat3 (trimTail p) (trimTail q) (trimTail r)



-- -------------------------------------------- Unbox Mat2 ----------------------------------------------------

newtype instance U.MVector s Mat2 = MV_Mat2 (U.MVector s (Vec2, Vec2))
newtype instance U.Vector    Mat2 = V_Mat2  (U.Vector    (Vec2, Vec2))

instance U.Unbox Mat2

instance M.MVector U.MVector Mat2 where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Mat2 v)                     = M.basicLength v
  basicUnsafeSlice i n (MV_Mat2 v)            = MV_Mat2 $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Mat2 v1) (MV_Mat2 v2)     = M.basicOverlaps v1 v2
  basicUnsafeNew n                            = MV_Mat2 `liftM` M.basicUnsafeNew n
  basicUnsafeReplicate n (Mat2 x y)           = MV_Mat2 `liftM` M.basicUnsafeReplicate n (x, y)
  basicUnsafeRead (MV_Mat2 v) i               = M.basicUnsafeRead v i >>= (\(x, y) -> return $ Mat2 x y)
  basicUnsafeWrite (MV_Mat2 v) i (Mat2 x y)   = M.basicUnsafeWrite v i (x, y)
  basicClear (MV_Mat2 v)                      = M.basicClear v
  basicSet (MV_Mat2 v) (Mat2 x y)             = M.basicSet v (x, y)
  basicUnsafeCopy (MV_Mat2 v1) (MV_Mat2 v2)   = M.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MV_Mat2 v) n               = MV_Mat2 `liftM` M.basicUnsafeGrow v n

instance G.Vector U.Vector Mat2 where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Mat2 v)           = V_Mat2 `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Mat2 v)              = MV_Mat2 `liftM` G.basicUnsafeThaw v
  basicLength (V_Mat2 v)                  = G.basicLength v
  basicUnsafeSlice i n (V_Mat2 v)         = V_Mat2 $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Mat2 v) i          = G.basicUnsafeIndexM v i >>= (\(x, y) -> return $ Mat2 x y)
  basicUnsafeCopy (MV_Mat2 mv) (V_Mat2 v) = G.basicUnsafeCopy mv v
  elemseq _ (Mat2 x y) t                  = G.elemseq (undefined :: Vector a) x $
                                            G.elemseq (undefined :: Vector a) y t


-- -------------------------------------------- Unbox Mat3 ----------------------------------------------------

newtype instance U.MVector s Mat3 = MV_Mat3 (U.MVector s (Vec3, Vec3, Vec3))
newtype instance U.Vector    Mat3 = V_Mat3  (U.Vector    (Vec3, Vec3, Vec3))

instance U.Unbox Mat3

instance M.MVector U.MVector Mat3 where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Mat3 v)                     = M.basicLength v
  basicUnsafeSlice i n (MV_Mat3 v)            = MV_Mat3 $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Mat3 v1) (MV_Mat3 v2)     = M.basicOverlaps v1 v2
  basicUnsafeNew n                            = MV_Mat3 `liftM` M.basicUnsafeNew n
  basicUnsafeReplicate n (Mat3 x y z)         = MV_Mat3 `liftM` M.basicUnsafeReplicate n (x, y, z)
  basicUnsafeRead (MV_Mat3 v) i               = M.basicUnsafeRead v i >>= (\(x,y,z) -> return $ Mat3 x y z)
  basicUnsafeWrite (MV_Mat3 v) i (Mat3 x y z) = M.basicUnsafeWrite v i (x, y, z)
  basicClear (MV_Mat3 v)                      = M.basicClear v
  basicSet (MV_Mat3 v) (Mat3 x y z)           = M.basicSet v (x, y, z)
  basicUnsafeCopy (MV_Mat3 v1) (MV_Mat3 v2)   = M.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MV_Mat3 v) n               = MV_Mat3 `liftM` M.basicUnsafeGrow v n

instance G.Vector U.Vector Mat3 where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Mat3 v)           = V_Mat3 `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Mat3 v)              = MV_Mat3 `liftM` G.basicUnsafeThaw v
  basicLength (V_Mat3 v)                  = G.basicLength v
  basicUnsafeSlice i n (V_Mat3 v)         = V_Mat3 $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Mat3 v) i          = G.basicUnsafeIndexM v i >>= (\(x,y,z) -> return $ Mat3 x y z)
  basicUnsafeCopy (MV_Mat3 mv) (V_Mat3 v) = G.basicUnsafeCopy mv v
  elemseq _ (Mat3 x y z) t                = G.elemseq (undefined :: Vector a) x $
                                            G.elemseq (undefined :: Vector a) y $
                                            G.elemseq (undefined :: Vector a) z t


-- -------------------------------------------- Unbox Mat4 ----------------------------------------------------

newtype instance U.MVector s Mat4 = MV_Mat4 (U.MVector s (Vec4, Vec4, Vec4, Vec4))
newtype instance U.Vector    Mat4 = V_Mat4  (U.Vector    (Vec4, Vec4, Vec4, Vec4))

instance U.Unbox Mat4

instance M.MVector U.MVector Mat4 where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Mat4 v)                       = M.basicLength v
  basicUnsafeSlice i n (MV_Mat4 v)              = MV_Mat4 $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Mat4 v1) (MV_Mat4 v2)       = M.basicOverlaps v1 v2
  basicUnsafeNew n                              = MV_Mat4 `liftM` M.basicUnsafeNew n
  basicUnsafeReplicate n (Mat4 x y z w)         = MV_Mat4 `liftM` M.basicUnsafeReplicate n (x, y, z, w)
  basicUnsafeRead (MV_Mat4 v) i                 = M.basicUnsafeRead v i >>= (\(x, y, z, w) -> return $ Mat4 x y z w)
  basicUnsafeWrite (MV_Mat4 v) i (Mat4 x y z w) = M.basicUnsafeWrite v i (x, y, z, w)
  basicClear (MV_Mat4 v)                        = M.basicClear v
  basicSet (MV_Mat4 v) (Mat4 x y z w)           = M.basicSet v (x, y, z, w)
  basicUnsafeCopy (MV_Mat4 v1) (MV_Mat4 v2)     = M.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MV_Mat4 v) n                 = MV_Mat4 `liftM` M.basicUnsafeGrow v n

instance G.Vector U.Vector Mat4 where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Mat4 v)           = V_Mat4 `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Mat4 v)              = MV_Mat4 `liftM` G.basicUnsafeThaw v
  basicLength (V_Mat4 v)                  = G.basicLength v
  basicUnsafeSlice i n (V_Mat4 v)         = V_Mat4 $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Mat4 v) i          = G.basicUnsafeIndexM v i >>= (\(x, y, z, w) -> return $ Mat4 x y z w)
  basicUnsafeCopy (MV_Mat4 mv) (V_Mat4 v) = G.basicUnsafeCopy mv v
  elemseq _ (Mat4 x y z w) t              = G.elemseq (undefined :: Vector a) x $
                                            G.elemseq (undefined :: Vector a) y $
                                            G.elemseq (undefined :: Vector a) z $
                                            G.elemseq (undefined :: Vector a) w t

