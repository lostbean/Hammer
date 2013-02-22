{-# LANGUAGE RecordWildCards #-}

module Hammer.VoxBox.MicroVoxel
  ( MicroVoxel
  , getMicroVoxel
  ) where

import           Control.Applicative   ((<$>))
import           Data.Maybe            (isJust, catMaybes)

import           Data.List

import           Hammer.MicroGraph.GrainsGraph
import           Hammer.VoxBox.Base

import           Debug.Trace

-- ================================ Find Microstruture Graph =====================================

type MicroVoxel = MicroGraph [VoxelPos] [FacePos] [EdgePos] [VoxelPos] -- Insert proper v (vertex) type

getMicroVoxel :: VoxBox GrainID -> MicroVoxel
getMicroVoxel vbox = foldl (getInterfaces vbox) initMicroGraph (scanMicro vbox)

scanMicro :: VoxBox GrainID -> [VoxelPos]
scanMicro VoxBox{..} = let
  VoxBoxDim xmax ymax zmax = vbrDim dimension
  in [VoxelPos x y z | z <- [0..zmax-1], y <- [0..ymax-1], x <- [0..xmax-1]]

getInterfaces :: VoxBox GrainID -> MicroVoxel -> VoxelPos -> MicroVoxel
getInterfaces vbox@VoxBox{..} micrograph pos = let
  --dbg a = trace (show pos ++ ": " ++ show a) a
  
  v    = pos
  vx   = pos #+# (VoxelPos (-1)   0    0 )
  vy   = pos #+# (VoxelPos 0    (-1)   0 )
  vz   = pos #+# (VoxelPos 0      0  (-1))
  vxy  = pos #+# (VoxelPos (-1) (-1)   0 )
  vyz  = pos #+# (VoxelPos 0    (-1) (-1))
  vzx  = pos #+# (VoxelPos (-1)   0  (-1))
  vxyz = pos #+# (VoxelPos (-1) (-1) (-1))

  p    = vbox#!v
  px   = vbox#!vx
  py   = vbox#!vy
  pz   = vbox#!vz
  pxy  = vbox#!vxy
  pyz  = vbox#!vyz
  pzx  = vbox#!vzx
  pxyz = vbox#!vxyz

  fx = mkFaceID' <$> checkFace p px
  fy = mkFaceID' <$> checkFace p py
  fz = mkFaceID' <$> checkFace p pz

  ex = mkEdgeID' <$> checkEgde p py pz pyz
  ey = mkEdgeID' <$> checkEgde p pz px pzx
  ez = mkEdgeID' <$> checkEgde p px py pxy

  emx = mkEdgeID' <$> checkEgde px pxy pzx pxyz
  emy = mkEdgeID' <$> checkEgde py pyz pxy pxyz
  emz = mkEdgeID' <$> checkEgde pz pzx pyz pxyz

  vertex = checkVertex p px py pz pxy pyz pzx pxyz

  checkVertex p0 p1 p2 p3 p4 p5 p6 p7
    | ne == 1   = traceShow msg Nothing
    | ne == 3   = traceShow msg Nothing
    | ne == 4   = getV
    | ne >= 5   = traceShow "-----" getV
    | otherwise = Nothing
    where
      msg  = "[VoxBoxReader] Oh my Gosh! A vertex with " ++
             show ne ++ " edge(s) with GrainIDs: " ++ show ps
      ne   = length $ catMaybes [ex, ey, ez, emx, emy, emz]
      ps   = catMaybes [p0, p1, p2, p3, p4, p5, p6, p7]
      getV = case nub ps of
        [a,b,c,d]   -> traceShow (mkVertexID' (a,b,c,d), pos) $ Just $ mkVertexID' (a,b,c,d)
        (a:b:c:d:_) -> traceShow (">>>" ++ msg) Nothing
        _           -> Nothing

  fs = [(fx, Fx pos), (fy, Fy pos), (fz, Fz pos)]
  es = [(ex, Ex pos), (ey, Ey pos), (ez, Ez pos)]

  insertList func ls micro
    | null ls   = micro
    | otherwise = let
      foo acc (Just key, value) = func key [value] acc
      foo acc _                 = acc
      in foldl' foo micro ls

  insertOne func (Just key, value) = func key value
  insertOne _    _                 = id
   
  newGrain  = insertOne  (insertNewGrain  (++)) (p, [pos])
  newFace   = insertList (insertNewFace   (++)) fs 
  newTriple = insertList (insertNewEdge   (++)) es
  newQuadri = insertOne  (insertNewVertex (errV (vertex, pos, fs, es))) (vertex, [pos])

  errV x a b = traceShow ("[VoxBoxReader] Overlaping verticies. " ++ show a ++ " <> " ++ show b ++ " *** " ++ show x ) (a ++ b)
                          
  in newGrain . newFace . newQuadri . newTriple $ micrograph


checkFace :: (Eq a)=> Maybe a -> Maybe a -> Maybe (a, a)
checkFace (Just a) (Just b)
  | a /= b    = Just (a, b)
  | otherwise = Nothing
checkFace _ _ = Nothing

--
--   D | A*
--   -----
--   C | B
--
-- Scan the AB -> BC -> DC -> DA
checkEgde :: (Eq a)=> Maybe a -> Maybe a -> Maybe a -> Maybe a -> Maybe (a, a, a) 
checkEgde Nothing _ _ _ = Nothing
checkEgde p@(Just id0) pb pc pd
  | ab && ad && ca && bc && dc && bd = traceShow "++++[VoxBox] Edge with more than 3 faces." Nothing
  | ab && bc && ca = Just (id0, getID pb, getID pc)
  | ad && dc && ca = Just (id0, getID pc, getID pd)
  | ab && ad && bd = Just (id0, getID pb, getID pd)
  | otherwise      = Nothing
  where
    getID (Just x) = x
    getID _        = error "[VoxBoxReader] Oops, the imposible has happened.."
    ab = isJust $ checkFace p  pb
    ad = isJust $ checkFace p  pd
    ca = isJust $ checkFace pc p
    bc = isJust $ checkFace pb pc
    dc = isJust $ checkFace pd pc
    bd = isJust $ checkFace pb pd

