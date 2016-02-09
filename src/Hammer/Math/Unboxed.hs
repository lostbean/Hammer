{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Hammer.Math.Unboxed where

import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Generic.Base    as G
import qualified Data.Vector.Generic.Mutable as M

import           Control.Monad               (liftM)
import           Data.Vector                 (Vector)

import           Foreign
  
import           Hammer.Math.AlgebraBase

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
  basicInitialize (MV_Vec2 v)                 = M.basicInitialize v
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
  basicInitialize (MV_Vec3 v)                 = M.basicInitialize v
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
  basicInitialize (MV_Mat2 v)                 = M.basicInitialize v
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
  basicInitialize (MV_Mat3 v)                 = M.basicInitialize v
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
  basicInitialize (MV_Mat4 v)                 = M.basicInitialize v
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

