{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hammer.Math.AlgebraVec where

import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Generic.Base    as G
import qualified Data.Vector.Generic.Mutable as M

import Foreign
import System.Random  
  
import Data.Vector                     (Vector)
import Control.Monad                   (liftM)

import Hammer.Math.AlgebraBase
  
--------------------------------------------------------------------------------
-- Unit vectors
  
instance UnitVector Vec2 Normal2 where
  mkNormal v = Normal2 (normalize v)
  fromNormal (Normal2 v) = v 
  toNormalUnsafe = Normal2

instance UnitVector Vec3 Normal3 where
  mkNormal v = Normal3 (normalize v)
  fromNormal (Normal3 v) = v 
  toNormalUnsafe = Normal3

instance UnitVector Vec4 Normal4 where
  mkNormal v = Normal4 (normalize v)
  fromNormal (Normal4 v) = v 
  toNormalUnsafe = Normal4

_rndUnit :: (RandomGen g, Random v, MultiVec v, DotProd v) => g -> (v,g)
_rndUnit g = 
  if d > 0.01
    then ( v &* (1.0/d) , h )
    else _rndUnit h
  where
    (v,h) = random g
    d = norm v
    
instance Random Normal2 where
  random g = let (v,h) = _rndUnit g in (Normal2 v, h)  
  randomR _ = random

instance Random Normal3 where
  random g = let (v,h) = _rndUnit g in (Normal3 v, h)  
  randomR _ = random

instance Random Normal4 where
  random g = let (v,h) = _rndUnit g in (Normal4 v, h)  
  randomR _ = random

instance CrossProd Normal3 where
  crossprod (Normal3 v) (Normal3 w) = mkNormal (crossprod v w)

-- | The assumption when dealing with these is always that they are of unit length.
-- Also, interpolation works differently.
newtype Normal2 = Normal2 Vec2 deriving ( Eq, Read, Show, Storable, DotProd, Dimension
                                        , G.Vector U.Vector, M.MVector U.MVector, U.Unbox )
                                        
newtype Normal3 = Normal3 Vec3 deriving (Eq, Read, Show, Storable, DotProd, Dimension 
                                        , G.Vector U.Vector, M.MVector U.MVector, U.Unbox )
                                        
newtype Normal4 = Normal4 Vec4 deriving (Eq, Read, Show, Storable, DotProd, Dimension 
                                        , G.Vector U.Vector, M.MVector U.MVector, U.Unbox ) 

--------------------------------------------------------------------------------
-- Vec2 instances

instance HasCoordinates Vec2 Double where
  _1 (Vec2 x _) = x
  _2 (Vec2 _ y) = y
  _3 _ = error "has only 2 coordinates"
  _4 _ = error "has only 2 coordinates"

instance AbelianGroup Vec2 where
  (&+) (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1+x2) (y1+y2) 
  (&-) (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1-x2) (y1-y2)
  neg  (Vec2 x y)                = Vec2 (-x) (-y)
  zero = Vec2 0 0
  
instance MultiVec Vec2 where
  scalarMul s (Vec2 x y) = Vec2 (s*x) (s*y)
  mapVec    f (Vec2 x y) = Vec2 (f x) (f y)
  
instance DotProd Vec2 where
  (&.) (Vec2 x1 y1) (Vec2 x2 y2) = x1*x2 + y1*y2

instance Pointwise Vec2 where
  pointwise (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1*x2) (y1*y2)

instance Determinant (Vec2,Vec2) where
  det (Vec2 x1 y1 , Vec2 x2 y2) = x1*y2 - x2*y1  

{-     
instance Show Vec2 where
  show (Vec2 x y) = "( " ++ show x ++ " , " ++ show y ++ " )"
-}

instance Random Vec2 where
  random = randomR (Vec2 (-1) (-1),Vec2 1 1)
  randomR (Vec2 a b, Vec2 c d) gen = 
    let (x,gen1) = randomR (a,c) gen
        (y,gen2) = randomR (b,d) gen1
    in (Vec2 x y, gen2)
     
instance Storable Vec2 where
  sizeOf    _ = 2 * sizeOf (undefined::Double)
  alignment _ = sizeOf (undefined::Double)
  
  peek q = do
    let p = castPtr q :: Ptr Double
        k = sizeOf (undefined::Double)
    x <- peek        p 
    y <- peekByteOff p k
    return (Vec2 x y)
    
  poke q (Vec2 x y) = do
    let p = castPtr q :: Ptr Double
        k = sizeOf (undefined::Double)
    poke        p   x
    pokeByteOff p k y

instance Dimension Vec2 where dim _ = 2

instance Ord Vec2 where
  compare (Vec2 a1 a2) (Vec2 b1 b2)
    | c1 == EQ  = c2
    | otherwise = c1
    where
      c1 = compare a1 b1
      c2 = compare a2 b2
                                      
--------------------------------------------------------------------------------     
-- Vec3 instances

instance HasCoordinates Vec3 Double where
  _1 (Vec3 x _ _) = x
  _2 (Vec3 _ y _) = y
  _3 (Vec3 _ _ z) = z
  _4 _ = error "has only 3 coordinates"

instance AbelianGroup Vec3 where
  (&+) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1+x2) (y1+y2) (z1+z2) 
  (&-) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1-x2) (y1-y2) (z1-z2) 
  neg  (Vec3 x y z)                    = Vec3 (-x) (-y) (-z)
  zero = Vec3 0 0 0
  
instance MultiVec Vec3 where
  scalarMul s (Vec3 x y z) = Vec3 (s*x) (s*y) (s*z)
  mapVec    f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

instance DotProd Vec3 where
  (&.) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

instance Pointwise Vec3 where
  pointwise (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1*x2) (y1*y2) (z1*z2)

{-
instance Show Vec3 where
  show (Vec3 x y z) = "( " ++ show x ++ " , " ++ show y ++ " , " ++ show z ++ " )"
-}

instance Random Vec3 where
  random = randomR (Vec3 (-1) (-1) (-1),Vec3 1 1 1)
  randomR (Vec3 a b c, Vec3 d e f) gen = 
    let (x,gen1) = randomR (a,d) gen
        (y,gen2) = randomR (b,e) gen1
        (z,gen3) = randomR (c,f) gen2  
    in (Vec3 x y z, gen3)
      
instance CrossProd Vec3 where
  crossprod (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (y1*z2-y2*z1) (z1*x2-z2*x1) (x1*y2-x2*y1) 

instance Determinant (Vec3,Vec3,Vec3) where
  det (u,v,w) = u &. (v &^ w)  
 
instance Storable Vec3 where
  sizeOf    _ = 3 * sizeOf (undefined::Double)
  alignment _ = sizeOf (undefined::Double)
  
  peek q = do
    let p = castPtr q :: Ptr Double
        k = sizeOf (undefined::Double)
    x <- peek        p 
    y <- peekByteOff p (k  )
    z <- peekByteOff p (k+k)
    return (Vec3 x y z)
    
  poke q (Vec3 x y z) = do
    let p = castPtr q :: Ptr Double
        k = sizeOf (undefined::Double)
    poke        p       x
    pokeByteOff p (k  ) y
    pokeByteOff p (k+k) z

instance Dimension Vec3 where dim _ = 3

instance Ord Vec3 where
  compare (Vec3 a1 a2 a3) (Vec3 b1 b2 b3)
    | c1 == EQ  = if c2 == EQ then c3 else c2
    | otherwise = c1
    where
      c1 = compare a1 b1
      c2 = compare a2 b2
      c3 = compare a3 b3

--------------------------------------------------------------------------------
-- Vec4 instances

instance HasCoordinates Vec4 Double where
  _1 (Vec4 x _ _ _) = x
  _2 (Vec4 _ y _ _) = y
  _3 (Vec4 _ _ z _) = z
  _4 (Vec4 _ _ _ w) = w

instance AbelianGroup Vec4 where
  (&+) (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = Vec4 (x1+x2) (y1+y2) (z1+z2) (w1+w2)
  (&-) (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = Vec4 (x1-x2) (y1-y2) (z1-z2) (w1-w2)
  neg  (Vec4 x y z w)                        = Vec4 (-x) (-y) (-z) (-w)
  zero = Vec4 0 0 0 0
  
instance MultiVec Vec4 where
  scalarMul s (Vec4 x y z w) = Vec4 (s*x) (s*y) (s*z) (s*w)
  mapVec    f (Vec4 x y z w) = Vec4 (f x) (f y) (f z) (f w)

instance DotProd Vec4 where
  (&.) (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = x1*x2 + y1*y2 + z1*z2 + w1*w2

instance Pointwise Vec4 where
  pointwise (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = Vec4 (x1*x2) (y1*y2) (z1*z2) (w1*w2)

{-
instance Show Vec4 where
  show (Vec4 x y z w) = "( " ++ show x ++ " , " ++ show y ++ " , " ++ show z ++ " , " ++ show w ++ " )"
-}

instance Random Vec4 where
  random = randomR (Vec4 (-1) (-1) (-1) (-1),Vec4 1 1 1 1)
  randomR (Vec4 a b c d, Vec4 e f g h) gen = 
    let (x,gen1) = randomR (a,e) gen
        (y,gen2) = randomR (b,f) gen1
        (z,gen3) = randomR (c,g) gen2  
        (w,gen4) = randomR (d,h) gen3  
    in (Vec4 x y z w, gen4)
           
instance Storable Vec4 where
  sizeOf    _ = 4 * sizeOf (undefined::Double)
  alignment _ = sizeOf (undefined::Double)
  
  peek q = do
    let p = castPtr q :: Ptr Double
        k = sizeOf (undefined::Double)
    x <- peek        p 
    y <- peekByteOff p (k  )
    z <- peekByteOff p (k+k)
    w <- peekByteOff p (3*k)
    return (Vec4 x y z w)
    
  poke q (Vec4 x y z w) = do
    let p = castPtr q :: Ptr Double
        k = sizeOf (undefined::Double)
    poke        p       x
    pokeByteOff p (k  ) y
    pokeByteOff p (k+k) z
    pokeByteOff p (3*k) w

instance Dimension Vec4 where dim _ = 4

--------------------------------------------------------------------------------
-- Extend instances

instance Extend Vec2 Vec3 where
  extendHeadZero   (Vec2 x y) = Vec3 0 x y
  extendHeadWith t (Vec2 x y) = Vec3 t x y
  trimHead (Vec3 _ x y)       = Vec2 x y
  extendTailZero   (Vec2 x y) = Vec3 x y 0
  extendTailWith t (Vec2 x y) = Vec3 x y t
  trimTail (Vec3 x y _)       = Vec2 x y

instance Extend Vec2 Vec4 where
  extendHeadZero   (Vec2 x y) = Vec4 0 0 x y
  extendHeadWith t (Vec2 x y) = Vec4 t t x y
  trimHead (Vec4 _ _ x y)     = Vec2 x y 
  extendTailZero   (Vec2 x y) = Vec4 x y 0 0
  extendTailWith t (Vec2 x y) = Vec4 x y t t
  trimTail (Vec4 x y _ _)     = Vec2 x y 

instance Extend Vec3 Vec4 where
  extendHeadZero   (Vec3 x y z) = Vec4 0 x y z
  extendHeadWith t (Vec3 x y z) = Vec4 t x y z
  trimHead (Vec4 _ x y z)       = Vec3 x y z
  extendTailZero   (Vec3 x y z) = Vec4 x y z 0
  extendTailWith t (Vec3 x y z) = Vec4 x y z t
  trimTail (Vec4 x y z _)       = Vec3 x y z

--------------------------------------------------------------------------------

-- -------------------------------------------- Unbox Vec2 ----------------------------------------------------

newtype instance U.MVector s Vec2 = MV_Vec2 (U.MVector s (Double, Double))
newtype instance U.Vector    Vec2 = V_Vec2  (U.Vector    (Double, Double))

instance U.Unbox Vec2

instance M.MVector U.MVector Vec2 where
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
  basicLength (MV_Vec2 v)                     = M.basicLength v
  basicUnsafeSlice i n (MV_Vec2 v)            = MV_Vec2 $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Vec2 v1) (MV_Vec2 v2)     = M.basicOverlaps v1 v2
  basicUnsafeNew n                            = MV_Vec2 `liftM` M.basicUnsafeNew n
  basicUnsafeReplicate n (Vec2 x y)           = MV_Vec2 `liftM` M.basicUnsafeReplicate n (x, y)
  basicUnsafeRead (MV_Vec2 v) i               = M.basicUnsafeRead v i >>= (\(x, y) -> return $ Vec2 x y)
  basicUnsafeWrite (MV_Vec2 v) i (Vec2 x y)   = M.basicUnsafeWrite v i (x, y)
  basicClear (MV_Vec2 v)                      = M.basicClear v
  basicSet (MV_Vec2 v) (Vec2 x y)             = M.basicSet v (x, y)
  basicUnsafeCopy (MV_Vec2 v1) (MV_Vec2 v2)   = M.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MV_Vec2 v) n               = MV_Vec2 `liftM` M.basicUnsafeGrow v n

instance G.Vector U.Vector Vec2 where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Vec2 v)           = V_Vec2 `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Vec2 v)              = MV_Vec2 `liftM` G.basicUnsafeThaw v
  basicLength (V_Vec2 v)                  = G.basicLength v
  basicUnsafeSlice i n (V_Vec2 v)         = V_Vec2 $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Vec2 v) i          = G.basicUnsafeIndexM v i >>= (\(x, y) -> return $ Vec2 x y)
  basicUnsafeCopy (MV_Vec2 mv) (V_Vec2 v) = G.basicUnsafeCopy mv v
  elemseq _ (Vec2 x y) t                  = G.elemseq (undefined :: Vector a) x $
                                            G.elemseq (undefined :: Vector a) y t


-- -------------------------------------------- Unbox Vec3 ----------------------------------------------------

newtype instance U.MVector s Vec3 = MV_Vec3 (U.MVector s (Double, Double, Double))
newtype instance U.Vector    Vec3 = V_Vec3  (U.Vector    (Double, Double, Double))

instance U.Unbox Vec3

instance M.MVector U.MVector Vec3 where
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
  basicLength (MV_Vec3 v)                     = M.basicLength v
  basicUnsafeSlice i n (MV_Vec3 v)            = MV_Vec3 $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Vec3 v1) (MV_Vec3 v2)     = M.basicOverlaps v1 v2
  basicUnsafeNew n                            = MV_Vec3 `liftM` M.basicUnsafeNew n
  basicUnsafeReplicate n (Vec3 x y z)         = MV_Vec3 `liftM` M.basicUnsafeReplicate n (x, y, z)
  basicUnsafeRead (MV_Vec3 v) i               = M.basicUnsafeRead v i >>= (\(x,y,z) -> return $ Vec3 x y z)
  basicUnsafeWrite (MV_Vec3 v) i (Vec3 x y z) = M.basicUnsafeWrite v i (x, y, z)
  basicClear (MV_Vec3 v)                      = M.basicClear v
  basicSet (MV_Vec3 v) (Vec3 x y z)           = M.basicSet v (x, y, z)
  basicUnsafeCopy (MV_Vec3 v1) (MV_Vec3 v2)   = M.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MV_Vec3 v) n               = MV_Vec3 `liftM` M.basicUnsafeGrow v n

instance G.Vector U.Vector Vec3 where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Vec3 v)           = V_Vec3 `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Vec3 v)              = MV_Vec3 `liftM` G.basicUnsafeThaw v
  basicLength (V_Vec3 v)                  = G.basicLength v
  basicUnsafeSlice i n (V_Vec3 v)         = V_Vec3 $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Vec3 v) i          = G.basicUnsafeIndexM v i >>= (\(x,y,z) -> return $ Vec3 x y z)
  basicUnsafeCopy (MV_Vec3 mv) (V_Vec3 v) = G.basicUnsafeCopy mv v
  elemseq _ (Vec3 x y z) t                = G.elemseq (undefined :: Vector a) x $
                                            G.elemseq (undefined :: Vector a) y $
                                            G.elemseq (undefined :: Vector a) z t


-- -------------------------------------------- Unbox Vec4 ----------------------------------------------------

newtype instance U.MVector s Vec4 = MV_Vec4 (U.MVector s (Double, Double, Double, Double))
newtype instance U.Vector    Vec4 = V_Vec4  (U.Vector    (Double, Double, Double, Double))

instance U.Unbox Vec4

instance M.MVector U.MVector Vec4 where
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
  basicLength (MV_Vec4 v)                       = M.basicLength v
  basicUnsafeSlice i n (MV_Vec4 v)              = MV_Vec4 $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Vec4 v1) (MV_Vec4 v2)       = M.basicOverlaps v1 v2
  basicUnsafeNew n                              = MV_Vec4 `liftM` M.basicUnsafeNew n
  basicUnsafeReplicate n (Vec4 x y z w)         = MV_Vec4 `liftM` M.basicUnsafeReplicate n (x, y, z, w)
  basicUnsafeRead (MV_Vec4 v) i                 = M.basicUnsafeRead v i >>= (\(x, y, z, w) -> return $ Vec4 x y z w)
  basicUnsafeWrite (MV_Vec4 v) i (Vec4 x y z w) = M.basicUnsafeWrite v i (x, y, z, w)
  basicClear (MV_Vec4 v)                        = M.basicClear v
  basicSet (MV_Vec4 v) (Vec4 x y z w)           = M.basicSet v (x, y, z, w)
  basicUnsafeCopy (MV_Vec4 v1) (MV_Vec4 v2)     = M.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MV_Vec4 v) n                 = MV_Vec4 `liftM` M.basicUnsafeGrow v n

instance G.Vector U.Vector Vec4 where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Vec4 v)           = V_Vec4 `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Vec4 v)              = MV_Vec4 `liftM` G.basicUnsafeThaw v
  basicLength (V_Vec4 v)                  = G.basicLength v
  basicUnsafeSlice i n (V_Vec4 v)         = V_Vec4 $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Vec4 v) i          = G.basicUnsafeIndexM v i >>= (\(x, y, z, w) -> return $ Vec4 x y z w)
  basicUnsafeCopy (MV_Vec4 mv) (V_Vec4 v) = G.basicUnsafeCopy mv v
  elemseq _ (Vec4 x y z w) t              = G.elemseq (undefined :: Vector a) x $
                                            G.elemseq (undefined :: Vector a) y $
                                            G.elemseq (undefined :: Vector a) z $
                                            G.elemseq (undefined :: Vector a) w t
