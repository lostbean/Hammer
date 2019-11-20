{-# LANGUAGE
    RecordWildCards
  , FlexibleInstances
  , FlexibleContexts
  , TypeFamilies
  #-}

module Hammer.VoxBox.Render
  ( getExtendedVoxBox
  , getVoxBoxCornersPoints
  , RenderVox(..)
  ) where

import qualified Data.Vector.Unboxed   as U

import           Data.Vector.Unboxed   (Vector, Unbox)
import           Linear.Vect
import           Hammer.VoxBox

-- =======================================================================================

newtype VoxBoxExt a = VoxBoxExt (VoxBox a)

class (Unbox elem, Unbox (Pointer elem))=> RenderVox elem where
  type Pointer elem :: *
  renderVox :: VoxBoxExt a -> elem -> Pointer elem

instance RenderVox FaceVoxelPos where
  type Pointer FaceVoxelPos = (Int, Int, Int, Int)
  renderVox = renderFacePos

instance RenderVox EdgeVoxelPos where
  type Pointer EdgeVoxelPos = (Int, Int)
  renderVox = renderEdgePos

instance RenderVox VoxelPos where
  type Pointer VoxelPos = Int
  renderVox = renderVoxelPos

renderVoxelVolume :: VoxBoxExt a -> VoxelPos -> (((Int, Int), (Int, Int)), ((Int, Int), (Int, Int)))
renderVoxelVolume (VoxBoxExt vbox) pos = let
  e p  = case getVoxelID (dimension vbox) p of
    Just x -> x
    _      -> error ("[VTK.VoxBox] " ++ show pos ++ show (dimension vbox))
  f000 = e
  f100 = e . (#+# VoxelPos 1 0 0)
  f010 = e . (#+# VoxelPos 0 1 0)
  f110 = e . (#+# VoxelPos 1 1 0)
  f001 = e . (#+# VoxelPos 0 0 1)
  f101 = e . (#+# VoxelPos 1 0 1)
  f011 = e . (#+# VoxelPos 0 1 1)
  f111 = e . (#+# VoxelPos 1 1 1)
  -- face struct -> (((rtf, rbf), (ltf, lbf)), ((rtb, rbb), (ltb, lbb)))
  in (((f110 pos, f100 pos), (f010 pos, f000 pos)), ((f111 pos, f101 pos), (f011 pos, f001 pos)))

renderFacePos :: VoxBoxExt a -> FaceVoxelPos -> (Int, Int, Int, Int)
renderFacePos (VoxBoxExt vbox) face = let
  e p  = case getVoxelID (dimension vbox) p of
    Just x -> x
    _      -> error ("[VTK.VoxBox] " ++ show face ++ show (dimension vbox))
  f000 = e
  f100 = e . (#+# VoxelPos 1 0 0)
  f010 = e . (#+# VoxelPos 0 1 0)
  f110 = e . (#+# VoxelPos 1 1 0)
  f001 = e . (#+# VoxelPos 0 0 1)
  f101 = e . (#+# VoxelPos 1 0 1)
  f011 = e . (#+# VoxelPos 0 1 1)
  in case face of
    Fx p -> (f000 p, f010 p, f011 p, f001 p)
    Fy p -> (f000 p, f100 p, f101 p, f001 p)
    Fz p -> (f000 p, f100 p, f110 p, f010 p)

renderEdgePos :: VoxBoxExt a -> EdgeVoxelPos -> (Int, Int)
renderEdgePos (VoxBoxExt vbox) edge = let
  e p  = case getVoxelID (dimension vbox) p of
    Just x -> x
    _      -> error ("[VTK.VoxBox] " ++ show edge ++ show (dimension vbox))
  f000 = e
  f100 = e . (#+# VoxelPos 1 0 0)
  f010 = e . (#+# VoxelPos 0 1 0)
  f001 = e . (#+# VoxelPos 0 0 1)
  in case edge of
    Ex p -> (f000 p, f100 p)
    Ey p -> (f000 p, f010 p)
    Ez p -> (f000 p, f001 p)

renderVoxelPos :: VoxBoxExt a -> VoxelPos -> Int
renderVoxelPos (VoxBoxExt vbox) p = case getVoxelID (dimension vbox) p of
  Just x -> x
  _      -> error ("[VTK.VoxBox] " ++ show p ++ show (dimension vbox))

-- | Extends the range ('VoxBoxDim') of an given 'VoxBox' in order to calculate all voxel
-- corner points. See 'getVoxBoxCornersPoints'.
getExtendedVoxBox :: VoxBox a -> VoxBoxExt a
getExtendedVoxBox vbox = let
  func (VoxBoxDim x y z) = VoxBoxDim (x+1) (y+1) (z+1)
  range = let vbr = dimension vbox
          in vbr { vbrDim = func $ vbrDim vbr}
  in VoxBoxExt $ vbox { dimension = range }

-- | Calculate all the voxel's corner points (vertices from voxel box) by calculating the
-- base point of each 'VoxelPos' in a extended range.
getVoxBoxCornersPoints :: VoxBoxExt a -> Vector Vec3D
getVoxBoxCornersPoints (VoxBoxExt vbox) = let
  dimData = dimension vbox
  origin  = vbrOrigin dimData
  size    = sizeVoxBoxRange dimData
  VoxBoxDim dbx dby _ = vbrDim dimData
  foo i = let
    (z, rz) = i  `quotRem` (dbx * dby)
    (y, ry) = rz `quotRem` dbx
    x       = ry
    in evalVoxelPos vbox (origin #+# VoxelPos x y z)
  in U.generate size foo
