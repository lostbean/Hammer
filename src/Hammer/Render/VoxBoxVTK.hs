{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Hammer.Render.VoxBoxVTK where

import qualified Data.IntMap           as IM
import qualified Data.List             as L
import qualified Data.Vector           as V
import qualified Data.HashMap.Strict   as HM
import qualified Data.HashSet          as HS

import           Control.Applicative   ((<$>))
import           Data.HashMap.Strict   (HashMap)
import           Data.HashSet          (HashSet)
import           Data.Maybe            (mapMaybe)
import           Data.Vector           (Vector)

import           Hammer.MicroGraph
import           Hammer.Math.Vector             hiding (Vector)
import           Hammer.VoxBox.Base
import           Hammer.VoxBox.MicroVoxel

import           Hammer.Render.VTK.VTKRender

import           Debug.Trace

-- ==========================================================================================

newtype VoxBoxExt a = VoxBoxExt (VoxBox a)
  
renderFacePos :: VoxBoxExt a -> FacePos -> (Int, Int, Int, Int)
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

renderEdgePos :: VoxBoxExt a -> EdgePos -> (Int, Int)
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

getExtendedVoxBox :: VoxBox a -> VoxBoxExt a
getExtendedVoxBox vbox = let
  func (VoxBoxDim x y z) = VoxBoxDim (x+1) (y+1) (z+1)
  range = let vbr = dimension vbox
          in vbr { vbrDim = func $ vbrDim vbr} 
  in VoxBoxExt $ vbox { dimension = range }
     
getVoxBoxCornersPoints :: VoxBoxExt a -> Vector Vec3 
getVoxBoxCornersPoints (VoxBoxExt vbox) = let
  range = dimension vbox
  size  = sizeVoxBoxRange range
  in V.generate size (evalVoxelPos vbox . (range %#))

-- ==========================================================================================

-- | Render to VTK using RectlinearGrid.
-- A @VoxBoxDim@ of x, y and z will result in x*z*y cells
-- and (x+1)*(y+1)*(z+1) points
renderVoxBoxVTK :: VoxBox a -> [VTKAttrCell Double] -> VTK Double
renderVoxBoxVTK vbox attrs = let
  (dx,dy,dz) = getVoxBoxDim . vbrDim $ dimension vbox
  vx  = V.generate (dx+1) (evalLinPos vbox XDir)
  vy  = V.generate (dy+1) (evalLinPos vbox YDir)
  vz  = V.generate (dz+1) (evalLinPos vbox ZDir)
  vtk = mkRLGVTK "MicroGraph" vx vy vz
  in L.foldl' addDataCells vtk attrs


renderVTKAllVoxelEdges :: VoxBox a -> MicroVoxel -> VTK Vec3
renderVTKAllVoxelEdges vbox mv = let
  es    = mapMaybe (\x -> V.fromList <$> getPropValue x) . HM.elems $ microEdges mv
  vbext = getExtendedVoxBox vbox
  ps    = getVoxBoxCornersPoints vbext
  rs    = V.map (renderEdgePos vbext) $ V.concat es
  in mkUGVTK "MicroGraph" ps rs

renderVTKAllVoxelFaces :: VoxBox a -> MicroVoxel -> VTK Vec3
renderVTKAllVoxelFaces vbox mv = let
  es    = mapMaybe (\x -> V.fromList <$> getPropValue x) . HM.elems $ microFaces mv
  vbext = getExtendedVoxBox vbox
  ps    = getVoxBoxCornersPoints vbext
  rs    = V.map (renderFacePos vbext) $ V.concat es
  in mkUGVTK "MicroGraph" ps rs

renderVTKAllVoxelVertecies :: VoxBox a -> MicroVoxel -> VTK Vec3
renderVTKAllVoxelVertecies vbox mv = let
  es    = mapMaybe getPropValue . HM.elems $ microVertex mv
  vbext = getExtendedVoxBox vbox
  ps    = getVoxBoxCornersPoints vbext
  rs    = V.map (renderVoxelPos vbext) $ V.fromList es
  in mkUGVTK "MicroGraph" ps rs

-- ==========================================================================================

instance RenderCell (Vector Int) where
  makeCell  = id
  getType _ = VTK_POLYGON

dbg a = trace ("@@@@@@@>> " ++ show a) a

renderVTKBaseTripleLinesFaceID :: VoxBox a -> MicroVoxel -> FaceID -> VTK Vec3
renderVTKBaseTripleLinesFaceID vbox mv face = let
  m     = renderMicro mv
  m'    = V.foldl' (V.++) V.empty m
  m''   = V.foldl' (V.++) V.empty m'
  
  vbext = getExtendedVoxBox vbox
  ps    = getVoxBoxCornersPoints vbext
  renderSimpleEdge es = let
    f (OpenSeq _ _ os) = V.map render $ V.fromList os
    f (LoopSeq os)     = V.map render $ V.fromList os
    render (a, b) = let foo =  renderVoxelPos vbext in (foo a ,foo b)
    in V.concatMap f es
       
  rs'    = (renderSimpleEdge . V.fromList . dbg . classifySegs . V.toList)
          <$> getFaceElements' mv face

  rs = case rs' of
    Just x -> x
    _      -> V.empty

  attr  = mkCellAttr "color" (\a _ _ -> a)
  vtk   = mkUGVTK "MicroGraph" ps rs
  in addDataCells vtk attr
        
renderVTKBaseTripleLines :: VoxBox a -> MicroVoxel -> VTK Vec3
renderVTKBaseTripleLines vbox mv = let
  m     = renderMicro mv
  m'    = V.foldl' (V.++) V.empty m
  m''   = V.foldl' (V.++) V.empty m'
  
  vbext = getExtendedVoxBox vbox
  ps    = getVoxBoxCornersPoints vbext
  rs    = V.map (V.map (renderVoxelPos vbext)) m'

  attr  = mkCellAttr "color" (\a _ _ -> a)
  vtk   = mkUGVTK "MicroGraph" ps rs
  in addDataCells vtk attr

renderVTKp :: VoxBox a -> MicroVoxel -> VTK Vec3
renderVTKp vbox mv = let
  m    = renderMicro' mv
  m'   = V.foldl' (V.++) V.empty m
  m''  = V.foldl' (V.++) V.empty m'

  vbext = getExtendedVoxBox vbox
  ps    = getVoxBoxCornersPoints vbext
  rs    = V.map func m''

  func (a,b) = let
    f = renderVoxelPos vbext
    in (f a, f b)

  in mkUGVTK "MicroGraph" ps rs

-- ==========================================================================================

renderMicro' :: MicroVoxel -> Vector (Vector (Vector (VoxelPos, VoxelPos)))
renderMicro' mv = let
  gids = getGrainIDList mv
  gs   = mapMaybe (renderGrain' mv) gids
  in V.fromList gs

renderGrain' :: MicroVoxel -> GrainID -> Maybe (Vector (Vector (VoxelPos, VoxelPos)))
renderGrain' mv gid = do
  faces <- getGrainProp gid mv >>= getPropConn
  let s = mapMaybe (getFaceElements' mv) $ HS.toList faces
  return $ V.fromList s
  
getFaceElements' :: MicroVoxel -> FaceID -> Maybe (Vector (VoxelPos, VoxelPos))
getFaceElements' mv fid = do
  mf   <- getFaceProp fid mv >>= getPropConn
  let s = mapMaybe (getEdgeElements mv) $ HS.toList mf
  return (V.fromList s)
 



renderMicro :: MicroVoxel -> Vector (Vector (Vector VoxelPos))
renderMicro mv = let
  gids = getGrainIDList mv
  gs   = mapMaybe (renderGrain mv) gids
  in V.fromList gs

renderGrain :: MicroVoxel -> GrainID -> Maybe (Vector (Vector VoxelPos))
renderGrain mv gid = do
  faces <- getGrainProp gid mv >>= getPropConn
  let s = mapMaybe (getFaceElements mv) $ HS.toList faces
  return $ V.fromList s

getFaceElements :: MicroVoxel -> FaceID -> Maybe (Vector VoxelPos)
getFaceElements mv fid = let
  getClosedRing = closeSeq . mapMaybe (getEdgeElements mv) . HS.toList
  getVertecies  = V.map fst . V.fromList
  in do
    mf   <- getFaceProp fid mv >>= getPropConn
    ring <- getClosedRing mf
    return $ getVertecies ring

getEdgeElements :: MicroVoxel -> EdgeID -> Maybe (VoxelPos, VoxelPos)
getEdgeElements mv eid = do
  let table = microVertex mv
  ep <- getEdgeProp eid mv
  me <- getPropConn ep
  case me of
    FullEdge vid1 vid2 -> do
      a <- getPropValue =<< HM.lookup vid1 table
      b <- getPropValue =<< HM.lookup vid2 table
      return (a, b)
    _ -> Nothing
