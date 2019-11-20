{-# LANGUAGE
    RecordWildCards
  , FlexibleInstances
  , FlexibleContexts
  , TypeFamilies
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hammer.VTK
  ( module Hammer.VTK
  , module Data.VTK
  ) where

import qualified Data.Vector           as V
import qualified Data.Vector.Unboxed   as U
import qualified Data.HashMap.Strict   as HM

import           Data.Maybe            (mapMaybe)
import           Data.Monoid           ((<>))
import           Data.Vector.Unboxed   (Unbox)
import           Data.VTK
import           Linear.Vect
import           Linear.Mat

import           Hammer.MicroGraph
import           Hammer.VoxBox
import           Hammer.VoxBox.Render

class (Unbox elem, Unbox (Pointer elem))=> RenderVox elem where
  type Pointer elem :: *
  renderVox :: VoxBoxExt a -> elem -> Pointer elem

instance RenderVox FaceVoxelPos where
  type Pointer FaceVoxelPos = (Int, Int, Int, Int)
  renderVox = renderVoxelFace

instance RenderVox EdgeVoxelPos where
  type Pointer EdgeVoxelPos = (Int, Int)
  renderVox = renderVoxelEdge

instance RenderVox VoxelPos where
  type Pointer VoxelPos = Int
  renderVox = renderVoxel

-- | Render to VTK using RectlinearGrid as points.
-- A @VoxBoxDim@ of x, y and z will result in x*z*y points
renderVoxBoxVTK :: VoxBox a -> [VTKAttrPoint Double] -> VTK Double
renderVoxBoxVTK vbox attrs = let
  (dx,dy,dz) = getVoxBoxDim . vbrDim $ dimension vbox
  vx  = U.generate dx (evalLinPos vbox XDir)
  vy  = U.generate dy (evalLinPos vbox YDir)
  vz  = U.generate dz (evalLinPos vbox ZDir)
  in mkRLGVTK "VoxBoxPoints" vx vy vz attrs

renderVoxElemListVTK :: (RenderCell (Pointer elem), RenderVox elem)=>
                        VoxBox a -> [elem] -> VTK Vec3D
renderVoxElemListVTK vbox v = let
  vbext = getExtendedVoxBox vbox
  ps    = getVoxBoxCornersPoints vbext
  color = U.generate (length v) id
  rs    = map (renderVox vbext) v
  attr  = mkCellAttr "color" (\a _ _ -> color U.! a)
  in mkPolyDataVTK "MicroGraph" ps rs [] [attr]

renderVoxElemVTK :: (Unbox elem, RenderCell (Pointer elem), RenderVox elem)=>
                    VoxBox a -> [V.Vector elem] -> VTK Vec3D
renderVoxElemVTK vbox v = let
  vbext = getExtendedVoxBox vbox
  ps    = getVoxBoxCornersPoints vbext
  color = U.concat $ map (\(fid, x) -> U.replicate (V.length x) fid) $ zip [(1::Int) ..] v
  rs    = V.map (renderVox vbext) $ V.concat v
  attr  = mkCellAttr "color" (\a _ _ -> color U.! a)
  in mkPolyDataVTK "MicroGraph" ps rs [] [attr]

renderAllElemProp :: (RenderCell (Pointer elem), RenderVox elem, HasPropValue prop)
                  => VoxBox a -> [prop (V.Vector elem)] -> VTK Vec3D
renderAllElemProp vbox vec = let
  ps = mapMaybe getPropValue vec
  in renderVoxElemVTK vbox ps

renderMicroGrainsVTK :: VoxBox a -> MicroVoxel -> VTK Vec3D
renderMicroGrainsVTK vbox mv = renderAllElemProp vbox (HM.elems $ microGrains mv)

renderMicroEdgesVTK :: VoxBox a -> MicroVoxel -> VTK Vec3D
renderMicroEdgesVTK vbox mv = renderAllElemProp vbox (HM.elems $ microEdges mv)

renderMicroFacesVTK :: VoxBox a -> MicroVoxel -> VTK Vec3D
renderMicroFacesVTK vbox mv = renderAllElemProp vbox (HM.elems $ microFaces mv)

renderMicroVertexVTK :: VoxBox a -> MicroVoxel -> VTK Vec3D
renderMicroVertexVTK vbox mv = let
  ps = mapMaybe getPropValue (HM.elems $ microVertex mv)
  in renderVoxElemListVTK vbox ps

-- =======================================================================================

instance RenderCell (Int, Int, Int, Int) where
  makeCell (a,b,c,d) = U.fromList [a,b,c,d]
  getType _          = VTK_QUAD

instance RenderElemVTK Normal3D
instance RenderElemVTK Vec3D
instance RenderElemVTK Vec2D
instance RenderElemVTK Mat3D

instance RenderPoint Mat3D where
  renderPoint (Mat3 x y z) =
    renderPoint x <>
    renderPoint y <>
    renderPoint z
  renderBinaryPoint (Mat3 x y z) =
    renderBinaryPoint x <>
    renderBinaryPoint y <>
    renderBinaryPoint z
  pointNumberType = const VTK_Float
  pointNumberComp = const 9

instance RenderPoint Vec3D where
  renderPoint (Vec3 x y z) =
    renderPoint x <>
    renderPoint y <>
    renderPoint z
  renderBinaryPoint (Vec3 x y z) =
    renderBinaryPoint x <>
    renderBinaryPoint y <>
    renderBinaryPoint z
  pointNumberType = const VTK_Float
  pointNumberComp = const 3

instance RenderPoint Vec2D where
  renderPoint (Vec2 x y) =
    renderPoint x <>
    renderPoint y <>
    renderPoint (0 :: Double)
  renderBinaryPoint (Vec2 x y) =
    renderBinaryPoint x <>
    renderBinaryPoint y <>
    renderBinaryPoint (0 :: Double)
  pointNumberType = const VTK_Float
  pointNumberComp = const 3

instance RenderPoint Normal3D where
  renderPoint = renderPoint . fromNormal
  renderBinaryPoint = renderBinaryPoint . fromNormal
  pointNumberType = const VTK_Float
  pointNumberComp = const 3
