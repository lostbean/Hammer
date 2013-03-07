{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Hammer.VoxBox.MicroVoxel
  ( MicroVoxel
  , getMicroVoxel
  ) where

import qualified Data.IntSet           as IS
import qualified Data.HashMap.Strict   as HM
import qualified Data.HashSet          as HS
  
import           Data.IntSet           (IntSet)
import           Data.HashMap.Strict   (HashMap)
import           Data.Maybe            (isJust, catMaybes, mapMaybe)

import           Control.Applicative
import           Data.List

import           Hammer.MicroGraph
import           Hammer.VoxBox.Base

import           Debug.Trace

-- ================================ Find Microstruture Graph ================================

-- Insert proper v (vertex) type
type MicroVoxel = MicroGraph [VoxelPos] [FacePos] [EdgePos] VoxelPos

getMicroVoxel :: VoxBox GrainID -> MicroVoxel
getMicroVoxel vbox = fix $ foldl (getInterfaces vbox) initMicroGraph (scanMicro vbox)

scanMicro :: VoxBox GrainID -> [VoxelPos]
scanMicro VoxBox{..} = let
  VoxBoxDim xmax ymax zmax = vbrDim dimension
  in [VoxelPos x y z | z <- [0..zmax-1], y <- [0..ymax-1], x <- [0..xmax-1]]

getInterfaces :: VoxBox GrainID -> MicroVoxel -> VoxelPos -> MicroVoxel
getInterfaces vbox@VoxBox{..} micrograph pos = let
  
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

  fx = checkFace p px
  fy = checkFace p py
  fz = checkFace p pz

  ex = checkEgde p py pyz pz
  ey = checkEgde p pz pzx px
  ez = checkEgde p py pxy px

  emx = checkEgde px pxy pxyz pzx
  emy = checkEgde py pyz pxyz pxy
  emz = checkEgde pz pyz pxyz pzx
  
  validEdges = catMaybes [ex, ey, ez, emx, emy, emz]
  validNeigh = catMaybes [p, px, py, pz, pxy, pyz, pzx, pxyz]
  
  vertex = checkVertex validNeigh validEdges

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
   
  newGrain  = insertOne  (insertNewGrainAutoConn  (++)) (p, [pos])
  newFace   = insertList (insertNewFaceAutoConn   (++)) fs 
  newTriple = insertList (\(eid, conn) prop -> insertNewEdge   (++) eid prop conn) es
  newQuadri = insertOne  (\(vid, conn) prop -> insertNewVertex errV vid prop conn) (vertex, pos)

  errV a b = traceShow ("[MicroVoxel] Overlaping verticies. "
                        ++ show a ++ " <> " ++ show b
                        ++ " *** " ++ show (vertex, pos, fs, es))
             a
                          
  in newGrain . newFace . newQuadri . newTriple $ micrograph




dbg a = trace ("::::::>> " ++ show a) a

checkFace :: Maybe GrainID -> Maybe GrainID -> Maybe FaceID
checkFace (Just a) (Just b)
  | a /= b    = return $ mkFaceID' (a, b)
  | otherwise = Nothing
checkFace _ _ = Nothing

-- | WARNING: In this function the sequence of the arguments matters!!! 
-- Insert in the following sequence: A -> B -> C -> D where A* is the
-- current @VoxelPos@.
--
--   D | A*
--   -----
--   C | B
-- 
-- The function will scan the faces: AB, BC, DC and DA
checkEgde :: Maybe GrainID -> Maybe GrainID -> Maybe GrainID
          -> Maybe GrainID -> Maybe (EdgeID, [FaceID]) 
checkEgde pa pb pc pd
  | not (isJust pa)     = Nothing
  | nun == 4 && nf == 4 = out
  | nun == 3 && nf == 3 = out
  | otherwise           = Nothing
  where
    out = do
      newE <- mkMultiEdgeID' ns
      return (newE, fs)
    nun = length uns
    nf  = length fs
    uns = nub ns       -- list of unique neighbours
    ns = catMaybes [pa, pb, pc, pd]
    fs = catMaybes [ab, bc, cd, da]
    ab = checkFace pa pb
    bc = checkFace pb pc
    cd = checkFace pc pd
    da = checkFace pd pa
    
checkVertex :: [GrainID] -> [(EdgeID, a)] -> Maybe (VertexID, [EdgeID])
checkVertex ns es
  | nun <= 3  = Nothing
  | ne  >= 3  = getV
  | otherwise = Nothing
  where
    ne   = length es
    nun  = length uns
    uns  = nub ns       -- list of unique neighbours
    getV = do
      newV <- mkMultiVertexID' uns
      return (newV, map fst es) 

    
-- -------------------------- Fix Bad Edges ---------------------------------------

fix :: MicroVoxel -> MicroVoxel
fix mg@MicroGraph{..} = let
  bs = HM.toList $ HM.filter isBad microEdges
  me = HM.filter (not . isBad) microEdges
  isBad (EdgeProp (BadEdge _) _) = True
  isBad _                        = False
  in traceShow microFaces $ foldl' foofix (mg {microEdges=me}) bs

foofix :: MicroVoxel -> (EdgeID, EdgeProp [EdgePos]) -> MicroVoxel
foofix mg@MicroGraph{..} (k,v) = let
  eps = fix' microVertex v

  func (acc, eids@(eid:_)) newProp = let
    newEM  = HM.insert eid newProp acc
    newEID = getAlterEdgeID eid
    in traceShow (newProp, newEID:eids) (newEM, newEID:eids)
  func (acc, []) newProp = (HM.insert k newProp acc, [k])
  
  (me, newEIDs) = foldl' func (microEdges, [k]) eps
  mf            = updateFaces k newEIDs microFaces

  in traceShow (k,v) $ mg { microEdges = me, microFaces = mf }

updateFaces :: EdgeID -> [EdgeID] -> HashMap FaceID (FaceProp a) -> HashMap FaceID (FaceProp a)
updateFaces initEdge newEdges hmf = let
  subs       = generateSubLevelConn initEdge
  foo hm fid = HM.adjust (modPropConn func) fid hm
  modPropConn f (FaceProp hm ps ) = FaceProp (f hm) ps
  modPropConn f (NullFaceProp hm) = NullFaceProp (f hm)
  func hm
    | HS.member initEdge hm = (HS.delete initEdge hm) `HS.union` (HS.fromList newEdges)
    | otherwise             = hm
  in foldl' foo hmf subs

fix' :: HashMap VertexID (VertexProp VoxelPos) -> EdgeProp [EdgePos] -> [EdgeProp [EdgePos]]
fix' hmv (EdgeProp (BadEdge vs) prop) = let
  func vid = case HM.lookup vid hmv of
    Just (VertexProp pos) -> return (pos, vid)
    _                     -> Nothing
  ps = mapMaybe func vs
  in fixBadEdge prop ps
fix' _ _ = []

instance SeqSeg EdgePos where
  type SeqUnit EdgePos = VoxelPos
  seqHead = fst . getEdgeEndPos
  seqTail = snd . getEdgeEndPos
  seqInv  = id 

fixBadEdge :: [EdgePos] -> [(VoxelPos, VertexID)] -> [EdgeProp [EdgePos]]
fixBadEdge es vvs = let
  getElem x  = snd <$> find ((== x) . fst) vvs
  out xs i f = EdgeProp (FullEdge i f) xs
  foo (OpenSeq i f xs) = (out xs) <$> getElem i <*> getElem f
  foo _                = Nothing
  in case splitOpenLoop $ classifySegs es of
    (ss, []) -> mapMaybe foo ss
    _        -> [] 