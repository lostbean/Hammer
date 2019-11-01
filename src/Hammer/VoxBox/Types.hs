{-# LANGUAGE
    FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , TypeSynonymInstances
  , TypeFamilies
  , UndecidableInstances
  , RankNTypes
  , TemplateHaskell
  #-}

module Hammer.VoxBox.Types
  ( VoxelPos (..)
  , EdgeVoxelPos (..)
  , FaceVoxelPos (..)
  , VoxBoxOrigin (..)
  , VoxBoxRange (..)
  , VoxBoxDim (..)
  , VoxelDim (..)
  , VoxBox (..)
  , CartesianDir (..)
  , CrossDir (..)
  , WallBoxRange (..)
  , FacePos (..)
  , EdgePos (..)
  , VID
  ) where

import Control.DeepSeq
import Data.Hashable       (Hashable, hashWithSalt)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed.Deriving

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
  deriving ( Eq, Hashable, NFData )

instance Show FacePos where
  show (FacePos (VoxelPos x y z)) = "FacePos " ++ show x ++ " " ++ show y ++ " " ++ show z

-- -------------------------------- Edges in matrix --------------------------------------

newtype EdgePos =
  EdgePos VoxelPos
  deriving ( Eq, Hashable, NFData )

instance Show EdgePos where
  show (EdgePos (VoxelPos x y z)) = "EdgePos " ++ show x ++ " " ++ show y ++ " " ++ show z

-- =======================================================================================
-- --------------------------------- Unbox instances -------------------------------------
-- =======================================================================================

-- ----------------------------------- Unbox VoxelPos ------------------------------------

derivingUnbox "VoxelPos"
    [t| VoxelPos -> (Int, Int, Int) |]
    [| \ (VoxelPos x y z) -> (x, y, z) |]
    [| \ (x, y, z) -> (VoxelPos x y z) |]

-- ------------------------------- Unbox CrossDir ----------------------------------------

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

derivingUnbox "CrossDir"
    [t| CrossDir -> Word8 |]
    [| castCrossDir |]
    [| unCastCrossDir |]

-- ------------------------------- Unbox FaceVoxelPos ------------------------------------

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

derivingUnbox "FaceVoxelPos"
    [t| FaceVoxelPos -> (Word8, VoxelPos) |]
    [| castFaceVoxelPos |]
    [| unCastFaceVoxelPos |]

-- ----------------------------- Unbox EdgeVoxelPos --------------------------------------

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

derivingUnbox "EdgeVoxelPos"
    [t| EdgeVoxelPos -> (Word8, VoxelPos) |]
    [| castEdgeVoxelPos |]
    [| unCastEdgeVoxelPos |]

-- -------------------------------------------- Unbox FacePos ----------------------------------------------------

derivingUnbox "FacePos"
    [t| FacePos -> VoxelPos |]
    [| \(FacePos p) -> p |]
    [| FacePos |]

-- -------------------------------------------- Unbox EdgePos ----------------------------------------------------

derivingUnbox "EdgePos"
    [t| EdgePos -> VoxelPos |]
    [| \(EdgePos p) -> p |]
    [| EdgePos |]
