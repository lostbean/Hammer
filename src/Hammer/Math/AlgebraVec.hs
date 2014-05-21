{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Hammer.Math.AlgebraVec where

import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Generic.Base    as G
import qualified Data.Vector.Generic.Mutable as M

import           Control.Monad               (liftM)
import           Data.Vector                 (Vector)
 
import           Foreign
import           System.Random
import           Control.DeepSeq

import           Text.Printf (printf)

import           Hammer.Math.AlgebraBase
import           Hammer.Math.Unboxed ()

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
newtype Normal2 = Normal2 Vec2 deriving ( Eq, Read, Show
                                        , Storable, DotProd, Dimension
                                        , AbelianGroup, MultiVec )

newtype Normal3 = Normal3 Vec3 deriving ( Eq, Read, Show
                                        , Storable, DotProd, Dimension
                                        , AbelianGroup, MultiVec )

newtype Normal4 = Normal4 Vec4 deriving ( Eq, Read, Show
                                        , Storable, DotProd, Dimension
                                        , AbelianGroup, MultiVec )
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

instance Determinant (Vec2, Vec2) where
  det (Vec2 x1 y1 , Vec2 x2 y2) = x1*y2 - x2*y1

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

instance VecFunctor Vec2 Double where
  vecMap     f (Vec2 a b) = Vec2 (f a) (f b)
  vecFoldr   f (Vec2 a b) = f a b
  vecZipWith f (Vec2 a1 b1) (Vec2 a2 b2) = Vec2 (f a1 a2) (f b1 b2)

instance PrettyShow Vec2 where
  showPretty (Vec2 a b) = printf "| %3.3e %3.3e |" a b

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

instance VecFunctor Vec3 Double where
  vecMap     f (Vec3 a b c) = Vec3 (f a) (f b) (f c)
  vecFoldr   f (Vec3 a b c) = f a (f b c)
  vecZipWith f (Vec3 a1 b1 c1) (Vec3 a2 b2 c2) = Vec3 (f a1 a2) (f b1 b2) (f c1 c2)

instance PrettyShow Vec3 where
  showPretty (Vec3 a b c) = printf "| %3.3e %3.3e %3.3e |" a b c

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

instance VecFunctor Vec4 Double where
  vecMap     f (Vec4 a b c d) = Vec4 (f a) (f b) (f c) (f d)
  vecFoldr   f (Vec4 a b c d) = f a (f b (f c d))
  vecZipWith f (Vec4 a1 b1 c1 d1) (Vec4 a2 b2 c2 d2) = Vec4 (f a1 a2) (f b1 b2) (f c1 c2) (f d1 d2)

instance PrettyShow Vec4 where
  showPretty (Vec4 a b c d) = printf "| %3.3e %3.3e %3.3e %3.3e |" a b c d

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
-- NFData

instance NFData Vec2 where
  rnf (Vec2 a b) = a `seq` b `seq` ()

instance NFData Vec3 where
  rnf (Vec3 a b c) = a `seq` b `seq` c `seq` ()

instance NFData Vec4 where
  rnf (Vec4 a b c d) = a `seq` b `seq` c `seq` d `seq` ()

-- -------------------------------------------- Unbox Normal2 ----------------------------------------------------

newtype instance U.MVector s Normal2 = MV_Normal2 (U.MVector s Vec2)
newtype instance U.Vector    Normal2 = V_Normal2  (U.Vector    Vec2)

instance U.Unbox Normal2

instance M.MVector U.MVector Normal2 where
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
  basicLength (MV_Normal2 v)                      = M.basicLength v
  basicUnsafeSlice i n (MV_Normal2 v)             = MV_Normal2 $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Normal2 v1) (MV_Normal2 v2)   = M.basicOverlaps v1 v2
  basicUnsafeNew n                                = MV_Normal2 `liftM` M.basicUnsafeNew n
  basicUnsafeReplicate n (Normal2 x)              = MV_Normal2 `liftM` M.basicUnsafeReplicate n x
  basicUnsafeRead (MV_Normal2 v) i                = M.basicUnsafeRead v i >>= (return . Normal2)
  basicUnsafeWrite (MV_Normal2 v) i (Normal2 x)   = M.basicUnsafeWrite v i x
  basicClear (MV_Normal2 v)                       = M.basicClear v
  basicSet (MV_Normal2 v) (Normal2 x)             = M.basicSet v x
  basicUnsafeCopy (MV_Normal2 v1) (MV_Normal2 v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MV_Normal2 v) n                = MV_Normal2 `liftM` M.basicUnsafeGrow v n

instance G.Vector U.Vector Normal2 where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Normal2 v)              = V_Normal2 `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Normal2 v)                 = MV_Normal2 `liftM` G.basicUnsafeThaw v
  basicLength (V_Normal2 v)                     = G.basicLength v
  basicUnsafeSlice i n (V_Normal2 v)            = V_Normal2 $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Normal2 v) i             = G.basicUnsafeIndexM v i >>= (return . Normal2)
  basicUnsafeCopy (MV_Normal2 mv) (V_Normal2 v) = G.basicUnsafeCopy mv v
  elemseq _ (Normal2 x) t                       = G.elemseq (undefined :: Vector a) x t

-- -------------------------------------------- Unbox Normal3 ----------------------------------------------------

newtype instance U.MVector s Normal3 = MV_Normal3 (U.MVector s Vec3)
newtype instance U.Vector    Normal3 = V_Normal3  (U.Vector    Vec3)

instance U.Unbox Normal3

instance M.MVector U.MVector Normal3 where
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
  basicLength (MV_Normal3 v)                      = M.basicLength v
  basicUnsafeSlice i n (MV_Normal3 v)             = MV_Normal3 $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Normal3 v1) (MV_Normal3 v2)   = M.basicOverlaps v1 v2
  basicUnsafeNew n                                = MV_Normal3 `liftM` M.basicUnsafeNew n
  basicUnsafeReplicate n (Normal3 x)              = MV_Normal3 `liftM` M.basicUnsafeReplicate n x
  basicUnsafeRead (MV_Normal3 v) i                = M.basicUnsafeRead v i >>= (return . Normal3)
  basicUnsafeWrite (MV_Normal3 v) i (Normal3 x)   = M.basicUnsafeWrite v i x
  basicClear (MV_Normal3 v)                       = M.basicClear v
  basicSet (MV_Normal3 v) (Normal3 x)             = M.basicSet v x
  basicUnsafeCopy (MV_Normal3 v1) (MV_Normal3 v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MV_Normal3 v) n                = MV_Normal3 `liftM` M.basicUnsafeGrow v n

instance G.Vector U.Vector Normal3 where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Normal3 v)              = V_Normal3 `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Normal3 v)                 = MV_Normal3 `liftM` G.basicUnsafeThaw v
  basicLength (V_Normal3 v)                     = G.basicLength v
  basicUnsafeSlice i n (V_Normal3 v)            = V_Normal3 $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Normal3 v) i             = G.basicUnsafeIndexM v i >>= (return . Normal3)
  basicUnsafeCopy (MV_Normal3 mv) (V_Normal3 v) = G.basicUnsafeCopy mv v
  elemseq _ (Normal3 x) t                       = G.elemseq (undefined :: Vector a) x t


-- -------------------------------------------- Unbox Normal4 ----------------------------------------------------

newtype instance U.MVector s Normal4 = MV_Normal4 (U.MVector s Vec4)
newtype instance U.Vector    Normal4 = V_Normal4  (U.Vector    Vec4)

instance U.Unbox Normal4

instance M.MVector U.MVector Normal4 where
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
  basicLength (MV_Normal4 v)                      = M.basicLength v
  basicUnsafeSlice i n (MV_Normal4 v)             = MV_Normal4 $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Normal4 v1) (MV_Normal4 v2)   = M.basicOverlaps v1 v2
  basicUnsafeNew n                                = MV_Normal4 `liftM` M.basicUnsafeNew n
  basicUnsafeReplicate n (Normal4 x)              = MV_Normal4 `liftM` M.basicUnsafeReplicate n x
  basicUnsafeRead (MV_Normal4 v) i                = M.basicUnsafeRead v i >>= (return . Normal4)
  basicUnsafeWrite (MV_Normal4 v) i (Normal4 x)   = M.basicUnsafeWrite v i x
  basicClear (MV_Normal4 v)                       = M.basicClear v
  basicSet (MV_Normal4 v) (Normal4 x)             = M.basicSet v x
  basicUnsafeCopy (MV_Normal4 v1) (MV_Normal4 v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MV_Normal4 v) n                = MV_Normal4 `liftM` M.basicUnsafeGrow v n

instance G.Vector U.Vector Normal4 where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Normal4 v)              = V_Normal4 `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Normal4 v)                 = MV_Normal4 `liftM` G.basicUnsafeThaw v
  basicLength (V_Normal4 v)                     = G.basicLength v
  basicUnsafeSlice i n (V_Normal4 v)            = V_Normal4 $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Normal4 v) i             = G.basicUnsafeIndexM v i >>= (return . Normal4)
  basicUnsafeCopy (MV_Normal4 mv) (V_Normal4 v) = G.basicUnsafeCopy mv v
  elemseq _ (Normal4 x) t                       = G.elemseq (undefined :: Vector a) x t
