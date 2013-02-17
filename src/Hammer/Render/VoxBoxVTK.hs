{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Hammer.Render.VoxBoxVTK where

import qualified Data.IntMap           as IM
import qualified Data.List             as L
import qualified Data.Vector           as V
import qualified Data.HashMap.Strict   as HM

import           Data.Vector           (Vector)
import           Data.Maybe            (mapMaybe)
import           Data.HashMap.Strict   (HashMap)

import           Hammer.MicroGraph.GrainsGraph
import           Hammer.Math.Vector             hiding (Vector)
import           Hammer.VoxBox.Base
import           Hammer.VoxBox.MicroVoxel 

import           Hammer.Render.VTK.VTKRender

import           Debug.Trace

renderFacePos :: VoxBox a -> FacePos -> (Vec3, Vec3, Vec3, Vec3)
renderFacePos vbox face = let
  e    = evalVoxelPos vbox
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

renderEdgePos :: VoxBox a -> EdgePos -> (Vec3, Vec3)
renderEdgePos vbox face = let
  e    = evalVoxelPos vbox
  f000 = e
  f100 = e . (#+# VoxelPos 1 0 0)
  f010 = e . (#+# VoxelPos 0 1 0)
  f001 = e . (#+# VoxelPos 0 0 1)
  in case face of
    Ex p -> (f000 p, f100 p)
    Ey p -> (f000 p, f010 p)
    Ez p -> (f000 p, f001 p)

renderVoxBoxVTK :: VoxBox a -> [VTKAttrCell Double] -> VTK Double
renderVoxBoxVTK vbox attrs = let
  (dx,dy,dz) = getVoxBoxDim $ dimension vbox
  vx  = V.generate (dx+1) (evalLinPos vbox XDir)
  vy  = V.generate (dy+1) (evalLinPos vbox YDir)
  vz  = V.generate (dz+1) (evalLinPos vbox ZDir)
  vtk = mkRLGVTK "MicroGraph" vx vy vz
  in L.foldl' addDataCells vtk attrs

renderVoxelPos :: VoxBox a -> VoxelPos -> Vec3
renderVoxelPos = evalVoxelPos

instance RenderCell (Vector Int) where
  makeCell  = id
  getType _ = VTK_POLYGON

renderVTKf :: VoxBox a -> MicroVoxel -> Vector (VTK Vec3)
renderVTKf vbox mv = let
  (vps, m) = renderMicro mv
  m'       = V.foldl' (V.++) V.empty m
  ps       = V.map (renderVoxelPos vbox) vps
  
  attr     = mkCellAttr "color" (\a _ _ -> a)
  vtk      = mkUGVTK "MicroGraph" ps
  addAttr v = addDataCells v attr
  
  in V.map (addAttr . vtk) m

renderVTKp :: VoxBox a -> MicroVoxel -> VTK Vec3
renderVTKp vbox mv = let
  (vps, m) = renderMicro' mv
  m'       = V.foldl' (V.++) V.empty m
  m''      = V.foldl' (V.++) V.empty m'
  ps       = V.map (renderVoxelPos vbox) vps
  pids     = V.generate (V.length ps) id
  in mkUGVTK "MicroGraph" ps m'' --pids

type Table = HashMap VertexID Int



renderMicro' :: MicroVoxel -> (Vector VoxelPos, Vector (Vector (Vector (Int, Int))))
renderMicro' mv = let
  gids = getGrainIDList mv
  gs   = mapMaybe (renderGrain' mv table) gids
  (table, vps) = getVertexTable mv
  in (vps, V.fromList gs)

renderGrain' :: MicroVoxel -> Table -> GrainID -> Maybe (Vector (Vector (Int, Int)))
renderGrain' mv table gid = do
  faces <- getGrainProp gid mv >>= getPropConn
  let s = mapMaybe (getFaceElements' mv table) $ IM.elems faces
  return $ V.fromList s
  
getFaceElements' :: MicroVoxel -> Table -> FaceID -> Maybe (Vector (Int, Int))
getFaceElements' mv table fid = do
  mf   <- getFaceProp fid mv >>= getPropConn
  let s = mapMaybe (getEdgeElements mv table) $ IM.elems mf
  return (V.fromList s)
 



renderMicro :: MicroVoxel -> (Vector VoxelPos, Vector (Vector (Vector Int)))
renderMicro mv = let
  gids = getGrainIDList mv
  gs   = mapMaybe (renderGrain mv table) gids
  (table, vps) = getVertexTable mv
  in (vps, V.fromList gs)

renderGrain :: MicroVoxel -> Table -> GrainID -> Maybe (Vector (Vector Int))
renderGrain mv table gid = do
  faces <- getGrainProp gid mv >>= getPropConn
  let s = mapMaybe (getFaceElements mv table) $ IM.elems faces
  return $ V.fromList s
  
getFaceElements :: MicroVoxel -> Table -> FaceID -> Maybe (Vector Int)
getFaceElements mv table fid = do
  mf   <- getFaceProp fid mv >>= getPropConn
  let s = mapMaybe (getEdgeElements mv table) $ IM.elems mf
  ring <- closeSeq $ traceShow (fid, s) s
  return (V.map fst $ V.fromList ring)
  

getEdgeElements :: MicroVoxel -> Table -> EdgeID -> Maybe (Int, Int)
getEdgeElements mv table eid = do
 ep <- getEdgeProp eid mv
 me <- getPropConn ep
 case trace ("> edge: " ++ show ep) me of
   FullEdge vid1 vid2 -> do
     a <- HM.lookup vid1 table
     b <- HM.lookup vid2 table
     return (a, b)
   _ -> Nothing

getVertexTable :: MicroVoxel -> (Table, Vector VoxelPos)
getVertexTable mv = let
  hmv = microVertex mv
  ini = (HM.empty, V.empty, 0)
  (table, vps, _) = HM.foldlWithKey' func ini hmv
  func acc@(tb, ps, count) k v = case getPropValue v of
    Just [x]  -> (HM.insert k count tb, V.cons x ps, count + 1) -- Fix for different sizes
    _         -> acc
  in (table, vps)
