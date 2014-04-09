{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hammer.VoxBox.Types where

import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Generic.Base    as G
import qualified Data.Vector.Generic.Mutable as M

import Control.DeepSeq
import Control.Monad                   (liftM)
import Data.Hashable                   (Hashable, hashWithSalt)
import Data.Vector.Unboxed             (Vector)

import Foreign

-- =======================================================================================

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

newtype WallBoxRange = WallBoxRange VoxBoxRange deriving (Show, Eq)

-- -------------------------------- Hashable instances -----------------------------------

instance Hashable VoxelPos where
  hashWithSalt i (VoxelPos a b c) = hashWithSalt i (a,b,c)

-- ---------------------------------- DeepSeq instances ----------------------------------

instance NFData VoxelPos where
  rnf (VoxelPos x y z) = rnf x `seq` rnf y `seq` rnf z

instance NFData VoxBoxDim where
  rnf (VoxBoxDim dx dy dz) = rnf dx `seq` rnf dy `seq` rnf dz

instance NFData VoxBoxOrigin where
  rnf (VoxBoxOrigin x0 y0 z0) = rnf x0 `seq` rnf y0 `seq` rnf z0

instance NFData VoxelDim where
  rnf (VoxelDim x y z) = rnf x `seq` rnf y `seq` rnf z

instance NFData VoxBoxRange where
  rnf (VoxBoxRange org dimbox) = rnf org `seq` rnf dimbox

instance (NFData a)=> NFData (VoxBox a) where
  rnf (VoxBox dimbox org sizevox vec) = rnf dimbox  `seq` rnf org `seq`
                                        rnf sizevox `seq` rnf vec

-- -------------------------------- Storable instances -----------------------------------

instance Storable VoxelPos where
  sizeOf _    = sizeOf (undefined :: Int) * 3
  alignment _ = alignment (undefined :: Int)

  {-# INLINE peek #-}
  peek p = do
    a <- peekElemOff q 0
    b <- peekElemOff q 1
    c <- peekElemOff q 2
    return (VoxelPos a b c)
    where
      q = castPtr p
  {-# INLINE poke #-}
  poke p (VoxelPos a b c) = do
    pokeElemOff q 0 a
    pokeElemOff q 1 b
    pokeElemOff q 2 c
    where
      q = castPtr p

-- --------------------------------- Faces in matrix -------------------------------------

newtype FacePos =
  FacePos VoxelPos
  deriving ( Eq, Hashable, NFData, G.Vector U.Vector, M.MVector U.MVector, U.Unbox)

instance Show FacePos where
  show (FacePos (VoxelPos x y z)) = "FacePos " ++ show x ++ " " ++ show y ++ " " ++ show z

-- -------------------------------- Edges in matrix --------------------------------------

newtype EdgePos =
  EdgePos VoxelPos
  deriving ( Eq, Hashable, NFData, G.Vector U.Vector, M.MVector U.MVector, U.Unbox)

instance Show EdgePos where
  show (EdgePos (VoxelPos x y z)) = "EdgePos " ++ show x ++ " " ++ show y ++ " " ++ show z

-- =======================================================================================
-- --------------------------------- Unbox instances -------------------------------------
-- =======================================================================================

-- ----------------------------------- Unbox VoxelPos ------------------------------------

newtype instance U.MVector s VoxelPos = MV_VoxelPos (U.MVector s (Int, Int, Int))
newtype instance U.Vector    VoxelPos = V_VoxelPos  (U.Vector    (Int, Int, Int))

instance U.Unbox VoxelPos

instance M.MVector U.MVector VoxelPos where
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
  basicLength (MV_VoxelPos v)                       = M.basicLength v
  basicUnsafeSlice i n (MV_VoxelPos v)              = MV_VoxelPos $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_VoxelPos v1) (MV_VoxelPos v2)   = M.basicOverlaps v1 v2
  basicUnsafeNew n                                  = MV_VoxelPos `liftM` M.basicUnsafeNew n
  basicUnsafeReplicate n (VoxelPos x y z)           = MV_VoxelPos `liftM` M.basicUnsafeReplicate n (x, y, z)
  basicUnsafeRead (MV_VoxelPos v) i                 = M.basicUnsafeRead v i >>= (\(x,y,z) -> return $ VoxelPos x y z)
  basicUnsafeWrite (MV_VoxelPos v) i (VoxelPos x y z) = M.basicUnsafeWrite v i (x, y, z)
  basicClear (MV_VoxelPos v)                        = M.basicClear v
  basicSet (MV_VoxelPos v) (VoxelPos x y z)         = M.basicSet v (x, y, z)
  basicUnsafeCopy (MV_VoxelPos v1) (MV_VoxelPos v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MV_VoxelPos v) n                 = MV_VoxelPos `liftM` M.basicUnsafeGrow v n

instance G.Vector U.Vector VoxelPos where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_VoxelPos v)   = V_VoxelPos `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_VoxelPos v)      = MV_VoxelPos `liftM` G.basicUnsafeThaw v
  basicLength (V_VoxelPos v)          = G.basicLength v
  basicUnsafeSlice i n (V_VoxelPos v) = V_VoxelPos $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_VoxelPos v) i  = G.basicUnsafeIndexM v i >>= (\(x,y,z) -> return $ VoxelPos x y z)
  basicUnsafeCopy (MV_VoxelPos mv) (V_VoxelPos v) = G.basicUnsafeCopy mv v
  elemseq _ (VoxelPos x y z) t        = G.elemseq (undefined :: Vector a) x $
                                        G.elemseq (undefined :: Vector a) y $
                                        G.elemseq (undefined :: Vector a) z t


-- ------------------------------- Unbox CrossDir ----------------------------------------

newtype instance U.MVector s CrossDir = MV_CrossDir (U.MVector s Word8)
newtype instance U.Vector    CrossDir = V_CrossDir  (U.Vector    Word8)

instance U.Unbox CrossDir

instance M.MVector U.MVector CrossDir where
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
  basicLength (MV_CrossDir v)                       = M.basicLength v
  basicUnsafeSlice i n (MV_CrossDir v)              = MV_CrossDir $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_CrossDir v1) (MV_CrossDir v2)   = M.basicOverlaps v1 v2
  basicUnsafeNew n                                  = MV_CrossDir `liftM` M.basicUnsafeNew n
  basicUnsafeReplicate n x                          = MV_CrossDir `liftM` M.basicUnsafeReplicate n (castCrossDir x)
  basicUnsafeRead (MV_CrossDir v) i                 = unCastCrossDir `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_CrossDir v) i                = M.basicUnsafeWrite v i . castCrossDir
  basicClear (MV_CrossDir v)                        = M.basicClear v
  basicSet (MV_CrossDir v)                          = M.basicSet v . castCrossDir
  basicUnsafeCopy (MV_CrossDir v1) (MV_CrossDir v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MV_CrossDir v) n                 = MV_CrossDir `liftM` M.basicUnsafeGrow v n

instance G.Vector U.Vector CrossDir where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_CrossDir v)   = V_CrossDir `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_CrossDir v)      = MV_CrossDir `liftM` G.basicUnsafeThaw v
  basicLength (V_CrossDir v)          = G.basicLength v
  basicUnsafeSlice i n (V_CrossDir v) = V_CrossDir $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_CrossDir v) i  = unCastCrossDir `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_CrossDir mv) (V_CrossDir v) = G.basicUnsafeCopy mv v
  elemseq _ x t                       = G.elemseq (undefined :: Vector a) (castCrossDir x) t


{-# INLINE castCrossDir #-}
castCrossDir :: CrossDir -> Word8
castCrossDir cd = case cd of
  XYplus  -> 0
  XYminus -> 1
  XZplus  -> 2
  XZminus -> 3
  YXplus  -> 4
  YXminus -> 5
  YZplus  -> 6
  YZminus -> 7
  ZXplus  -> 8
  ZXminus -> 9
  ZYplus  -> 10
  ZYminus -> 11

{-# INLINE unCastCrossDir #-}
unCastCrossDir :: Word8 -> CrossDir
unCastCrossDir x
  | x == 0  = XYplus
  | x == 1  = XYminus
  | x == 2  = XZplus
  | x == 3  = XZminus
  | x == 4  = YXplus
  | x == 5  = YXminus
  | x == 6  = YZplus
  | x == 7  = YZminus
  | x == 8  = ZXplus
  | x == 9  = ZXminus
  | x == 10 = ZYplus
  | otherwise = ZYminus

-- ------------------------------- Unbox FaceVoxelPos ------------------------------------

newtype instance U.MVector s FaceVoxelPos = MV_FaceVoxelPos (U.MVector s (Word8, VoxelPos))
newtype instance U.Vector    FaceVoxelPos = V_FaceVoxelPos  (U.Vector    (Word8, VoxelPos))

instance U.Unbox FaceVoxelPos

instance M.MVector U.MVector FaceVoxelPos where
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
  basicLength (MV_FaceVoxelPos v)                           = M.basicLength v
  basicUnsafeSlice i n (MV_FaceVoxelPos v)                  = MV_FaceVoxelPos $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_FaceVoxelPos v1) (MV_FaceVoxelPos v2)   = M.basicOverlaps v1 v2
  basicUnsafeNew n                                          = MV_FaceVoxelPos `liftM` M.basicUnsafeNew n
  basicUnsafeReplicate n x                                  = MV_FaceVoxelPos `liftM` M.basicUnsafeReplicate n (castFaceVoxelPos x)
  basicUnsafeRead (MV_FaceVoxelPos v) i                     = unCastFaceVoxelPos `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_FaceVoxelPos v) i                    = M.basicUnsafeWrite v i . castFaceVoxelPos
  basicClear (MV_FaceVoxelPos v)                            = M.basicClear v
  basicSet (MV_FaceVoxelPos v)                              = M.basicSet v . castFaceVoxelPos
  basicUnsafeCopy (MV_FaceVoxelPos v1) (MV_FaceVoxelPos v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MV_FaceVoxelPos v) n                     = MV_FaceVoxelPos `liftM` M.basicUnsafeGrow v n

instance G.Vector U.Vector FaceVoxelPos where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_FaceVoxelPos v)                   = V_FaceVoxelPos `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_FaceVoxelPos v)                      = MV_FaceVoxelPos `liftM` G.basicUnsafeThaw v
  basicLength (V_FaceVoxelPos v)                          = G.basicLength v
  basicUnsafeSlice i n (V_FaceVoxelPos v)                 = V_FaceVoxelPos $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_FaceVoxelPos v) i                  = unCastFaceVoxelPos `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_FaceVoxelPos mv) (V_FaceVoxelPos v) = G.basicUnsafeCopy mv v
  elemseq _ x t                                           = G.elemseq (undefined :: Vector a) (castFaceVoxelPos x) t


{-# INLINE castFaceVoxelPos #-}
castFaceVoxelPos :: FaceVoxelPos -> (Word8, VoxelPos)
castFaceVoxelPos cd = case cd of
  Fx p -> (0, p)
  Fy p -> (1, p)
  Fz p -> (2, p)

{-# INLINE unCastFaceVoxelPos #-}
unCastFaceVoxelPos :: (Word8, VoxelPos) -> FaceVoxelPos
unCastFaceVoxelPos (x, p)
  | x == 0    = Fx p
  | x == 1    = Fy p
  | otherwise = Fz p

-- ----------------------------- Unbox EdgeVoxelPos --------------------------------------

newtype instance U.MVector s EdgeVoxelPos = MV_EdgeVoxelPos (U.MVector s (Word8, VoxelPos))
newtype instance U.Vector    EdgeVoxelPos = V_EdgeVoxelPos  (U.Vector    (Word8, VoxelPos))

instance U.Unbox EdgeVoxelPos

instance M.MVector U.MVector EdgeVoxelPos where
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
  basicLength (MV_EdgeVoxelPos v)                           = M.basicLength v
  basicUnsafeSlice i n (MV_EdgeVoxelPos v)                  = MV_EdgeVoxelPos $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_EdgeVoxelPos v1) (MV_EdgeVoxelPos v2)   = M.basicOverlaps v1 v2
  basicUnsafeNew n                                          = MV_EdgeVoxelPos `liftM` M.basicUnsafeNew n
  basicUnsafeReplicate n x                                  = MV_EdgeVoxelPos `liftM` M.basicUnsafeReplicate n (castEdgeVoxelPos x)
  basicUnsafeRead (MV_EdgeVoxelPos v) i                     = unCastEdgeVoxelPos `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_EdgeVoxelPos v) i                    = M.basicUnsafeWrite v i . castEdgeVoxelPos
  basicClear (MV_EdgeVoxelPos v)                            = M.basicClear v
  basicSet (MV_EdgeVoxelPos v)                              = M.basicSet v . castEdgeVoxelPos
  basicUnsafeCopy (MV_EdgeVoxelPos v1) (MV_EdgeVoxelPos v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MV_EdgeVoxelPos v) n                     = MV_EdgeVoxelPos `liftM` M.basicUnsafeGrow v n

instance G.Vector U.Vector EdgeVoxelPos where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_EdgeVoxelPos v)                   = V_EdgeVoxelPos `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_EdgeVoxelPos v)                      = MV_EdgeVoxelPos `liftM` G.basicUnsafeThaw v
  basicLength (V_EdgeVoxelPos v)                          = G.basicLength v
  basicUnsafeSlice i n (V_EdgeVoxelPos v)                 = V_EdgeVoxelPos $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_EdgeVoxelPos v) i                  = unCastEdgeVoxelPos `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_EdgeVoxelPos mv) (V_EdgeVoxelPos v) = G.basicUnsafeCopy mv v
  elemseq _ x t                                           = G.elemseq (undefined :: Vector a) (castEdgeVoxelPos x) t


{-# INLINE castEdgeVoxelPos #-}
castEdgeVoxelPos :: EdgeVoxelPos -> (Word8, VoxelPos)
castEdgeVoxelPos cd = case cd of
  Ex p -> (0, p)
  Ey p -> (1, p)
  Ez p -> (2, p)

{-# INLINE unCastEdgeVoxelPos #-}
unCastEdgeVoxelPos :: (Word8, VoxelPos) -> EdgeVoxelPos
unCastEdgeVoxelPos (x, p)
  | x == 0    = Ex p
  | x == 1    = Ey p
  | otherwise = Ez p
