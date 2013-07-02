{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Hammer.Render.VoxBoxVTK where

import qualified Data.List             as L
import qualified Data.Vector           as V
import qualified Data.Vector.Unboxed   as U
import qualified Data.HashMap.Strict   as HM

import           Data.Maybe            (mapMaybe)
import           Data.Vector.Unboxed   (Vector, Unbox)

import           Hammer.MicroGraph
import           Hammer.Math.Algebra
import           Hammer.VoxBox.Base
import           Hammer.VoxBox.MicroVoxel

import           Hammer.Render.VTK.VTKRender

import           Debug.Trace
--dbg a = trace ("@@@@@@@>> " ++ show a) a

-- =======================================================================================

newtype VoxBoxExt a = VoxBoxExt (VoxBox a)

class (Unbox elem, Unbox (VTKElem elem))=> RenderVox elem where
  type VTKElem elem :: *
  renderVox    :: VoxBoxExt a -> elem -> VTKElem elem

instance RenderVox FaceVoxelPos where
  type VTKElem FaceVoxelPos = (Int, Int, Int, Int)
  renderVox = renderFacePos

instance RenderVox EdgeVoxelPos where
  type VTKElem EdgeVoxelPos = (Int, Int)
  renderVox = renderEdgePos

instance RenderVox VoxelPos where
  type VTKElem VoxelPos = Int
  renderVox = renderVoxelPos

renderFacePos :: VoxBoxExt a -> FaceVoxelPos -> (Int, Int, Int, Int)
renderFacePos (VoxBoxExt vbox) face = let
  e p  = case getVoxelID (dimension vbox) p of
    Just x -> x
    _      -> trace ("-----------> " ++ show face ++ show (dimension vbox)) 0
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
    _      -> trace ("-----------> " ++ show edge ++ show (dimension vbox)) 0
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
  _      -> trace ("-----------> " ++ show p ++ show (dimension vbox)) 0

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
getVoxBoxCornersPoints :: VoxBoxExt a -> Vector Vec3
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

-- =======================================================================================

-- | Render to VTK using RectlinearGrid.
-- A @VoxBoxDim@ of x, y and z will result in x*z*y cells
-- and (x+1)*(y+1)*(z+1) points
renderVoxBoxVTK :: VoxBox a -> [VTKAttrCell Double] -> VTK Double
renderVoxBoxVTK vbox attrs = let
  (dx,dy,dz) = getVoxBoxDim . vbrDim $ dimension vbox
  vx  = U.generate (dx+1) (evalLinPos vbox XDir)
  vy  = U.generate (dy+1) (evalLinPos vbox YDir)
  vz  = U.generate (dz+1) (evalLinPos vbox ZDir)
  vtk = mkRLGVTK "MicroGraph" vx vy vz
  in L.foldl' addDataCells vtk attrs

renderVoxElemListVTK :: (RenderCell (VTKElem elem), RenderVox elem)=>
                        VoxBox a -> [elem] -> VTK Vec3
renderVoxElemListVTK vbox v = let
  vbext = getExtendedVoxBox vbox
  ps    = getVoxBoxCornersPoints vbext
  color = U.generate (length v) id
  rs    = map (renderVox vbext) v
  attr  = mkCellAttr "color" (\a _ _ -> color U.! a)
  vtk   = mkUGVTK "MicroGraph" ps rs
  in addDataCells vtk attr

renderVoxElemVTK :: (Unbox elem, RenderCell (VTKElem elem), RenderVox elem)=>
                    VoxBox a -> [V.Vector elem] -> VTK Vec3
renderVoxElemVTK vbox v = let
  vbext = getExtendedVoxBox vbox
  ps    = getVoxBoxCornersPoints vbext
  color = U.concat $ map (\(fid, x) -> U.replicate (V.length x) fid) $ zip [(1::Int) ..] v
  rs    = V.map (renderVox vbext) $ V.concat v
  attr  = mkCellAttr "color" (\a _ _ -> color U.! a)
  vtk   = mkUGVTK "MicroGraph" ps rs
  in addDataCells vtk attr

renderAllElemProp :: (RenderCell (VTKElem elem), RenderVox elem, HasPropValue prop)
                  => VoxBox a -> [prop (V.Vector elem)] -> VTK Vec3
renderAllElemProp vbox vec = let
  ps = mapMaybe getPropValue vec
  in renderVoxElemVTK vbox ps

renderMicroGrainsVTK :: VoxBox a -> MicroVoxel -> VTK Vec3
renderMicroGrainsVTK vbox mv = renderAllElemProp vbox (HM.elems $ microGrains mv)

renderMicroEdgesVTK :: VoxBox a -> MicroVoxel -> VTK Vec3
renderMicroEdgesVTK vbox mv = renderAllElemProp vbox (HM.elems $ microEdges mv)

renderMicroFacesVTK :: VoxBox a -> MicroVoxel -> VTK Vec3
renderMicroFacesVTK vbox mv = renderAllElemProp vbox (HM.elems $ microFaces mv)

renderMicroVertexVTK :: VoxBox a -> MicroVoxel -> VTK Vec3
renderMicroVertexVTK vbox mv = let
  ps = mapMaybe getPropValue (HM.elems $ microVertex mv)
  in renderVoxElemListVTK vbox ps

instance RenderCell (Int, Int, Int, Int) where
  makeCell (a,b,c,d) = U.fromList [a,b,c,d]
  getType _          = VTK_QUAD
